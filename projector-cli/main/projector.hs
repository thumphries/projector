{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}

import qualified BuildInfo_projector_cli as BuildInfo

import           Control.Applicative (many)
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Monad.Catch as Catch
import           Control.Monad.IO.Class (MonadIO (..))

import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Options.Applicative ((<**>))
import qualified Options.Applicative as Options

import qualified Projector.Core as Projector
import qualified Projector.Html as Projector
import qualified Projector.Html.Backend.Haskell as Haskell
import qualified Projector.Html.Core.Machinator as Projector
import qualified Projector.Html.Data.Module as Projector
import qualified Projector.Html.Interpreter as Interpreter
import qualified Projector.Html.Interpreter.Hydrant as Interpreter
import qualified Projector.Html.Machinator as Machinator

import           Projector.Core.Prelude

import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath
import           System.FilePath.Find ((==?))
import qualified System.FilePath.Find as Find
import qualified System.FSNotify as Notify
import           System.IO (IO, FilePath)
import qualified System.IO as IO
import qualified System.Exit as Exit

data Watch =
    Watch
  | NoWatch
    deriving (Eq, Ord, Show)

data Verbose =
    Verbose
  | Quiet
    deriving (Eq, Ord, Show)

newtype TemplateDirectory =
  TemplateDirectory {
      templateDirectory :: FilePath
    } deriving (Eq, Ord, Show)

newtype HaskellDirectory =
    HaskellDirectory FilePath
    deriving (Eq, Ord, Show)

newtype ExampleDirectory =
    ExampleDirectory FilePath
    deriving (Eq, Ord, Show)

data RelativeFileContent a =
    RelativeFileContent FilePath a
    deriving (Eq, Ord, Show, Functor)

data Command =
    Command Verbose Watch [TemplateDirectory] [TemplateDirectory] (Maybe HaskellDirectory) (Maybe ExampleDirectory) (Maybe Projector.ModuleName)
  | Version
    deriving (Eq, Ord, Show)

data Error =
    ErrorMessage Text
  | HaskellErrors [Haskell.HaskellError]
  | HtmlErrors [Projector.HtmlError]
  | MachinatorError Machinator.MachinatorError
    deriving (Eq, Show)

renderError :: Error -> Text
renderError err =
  case err of
    ErrorMessage message ->
      mconcat ["[projector:build:error] ", message]
    HaskellErrors errors ->
      Text.unlines . join $ [
          ["[projector:build:error] error generating haskell"]
        , Haskell.renderHaskellError <$> errors
        ]
    HtmlErrors errors ->
      Text.unlines . join $ [
          ["[projector:build:error] error compiling templates"]
        , Projector.renderHtmlError <$> errors
        ]
    MachinatorError e ->
      Text.unlines [
          "[projector:build:error] error compiling data types"
        , Machinator.renderMachinatorError e
        ]

parser :: Options.Parser Command
parser =
  Command
    <$> pverbose
    <*> pwatch
    <*> many ptemplates
    <*> many pexamples
    <*> Options.optional phaskell
    <*> Options.optional phtml
    <*> Options.optional pmodulePrefix

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  dispatch (Version <$ pversion <|> parser) >>= \command ->
    case command of
      Command verbose NoWatch templates examples haskell html prefix ->
        runEitherT (run verbose templates examples haskell html prefix) >>= \r ->
          case r of
            Left err -> do
              Text.hPutStrLn IO.stderr $ renderError err
              Exit.exitFailure
            Right _ ->
              Exit.exitSuccess
      Command verbose Watch templates examples haskell html prefix ->
        watch verbose templates examples haskell html prefix
      Version ->
        Text.putStrLn . mconcat $ ["projector-", Text.pack BuildInfo.buildInfoVersion]

watch :: Verbose -> [TemplateDirectory] -> [TemplateDirectory] -> Maybe HaskellDirectory -> Maybe ExampleDirectory -> Maybe Projector.ModuleName -> IO ()
watch verbose templates examples haskell html prefix = do
  lock <- MVar.newMVar ()
  let
    conf =
      Notify.defaultConfig { Notify.confDebounce = Notify.Debounce 0.1 }
    exploded e =
      log . mconcat $ ["[projector:explosion] ", Text.pack . show $ e]
    errored e =
      log . renderError $ e
    handler =
      (\(e :: Catch.SomeException) -> exploded e)
    safely =
      Catch.handle handler (runEitherT (run verbose templates examples haskell html prefix) >>= either errored (const $ pure ()))
    check x =
      and [
          not (List.isInfixOf ".#" $ Notify.eventPath x)
        , (List.isSuffixOf ".prj" $ Notify.eventPath x) || (List.isSuffixOf ".mcn" $ Notify.eventPath x)
        ]
  runEitherT (run verbose templates examples haskell html prefix) >>= either errored (const $ pure ())
  void . Notify.withManagerConf conf $ \manager -> do
    for_ (templates <> examples) $ \(TemplateDirectory template) -> do
      log $ mconcat ["[projector:watch] watching directory ", Text.pack template]
      void $ Notify.watchTree manager template check (const $ MVar.withMVar lock . const $ safely)
    forever $ Concurrent.threadDelay 1000000

machinators :: [TemplateDirectory] -> EitherT Error IO [RelativeFileContent Machinator.DefinitionFile]
machinators templates = do
  mcns <- liftIO $ gatherBy templates ".mcn"
  for mcns $ \mcn -> do
    machinator <- liftIO $ Text.readFile mcn
    Machinator.Versioned _ (Machinator.DefinitionFile _file definitions) <-
      firstT MachinatorError . newEitherT . pure $
        Machinator.parseDefinitionFile mcn machinator
    pure $ RelativeFileContent mcn $ Machinator.DefinitionFile mcn definitions

projectors :: [TemplateDirectory] -> IO [RelativeFileContent Text]
projectors templates =  do
  prjs <- gatherBy templates ".prj"
  for prjs $ \prj -> do
    RelativeFileContent prj <$> Text.readFile prj

examplars :: [TemplateDirectory] -> [FilePath] -> Maybe Projector.ModuleName -> IO (Set Projector.ModuleName)
examplars templates absolutes prefix =  do
  examples <- liftIO $ gatherBy templates ".prj"
  pure . Set.fromList . with examples $
    Projector.pathToModuleName (moduleNamer absolutes prefix)

absolutize :: [TemplateDirectory] -> IO [FilePath]
absolutize templates =
  for (templateDirectory <$> templates) $ fmap (<> "/") . Directory.makeAbsolute

run :: Verbose -> [TemplateDirectory] -> [TemplateDirectory] -> Maybe HaskellDirectory -> Maybe ExampleDirectory -> Maybe Projector.ModuleName -> EitherT Error IO ()
run verbose templates examples haskells htmls prefix = do
  detail verbose "[projector:build:begin]"

  absolutes <- liftIO $ absolutize (templates <> examples)
  mcns <- machinators (templates <> examples)
  prjs <- liftIO $ projectors (templates <> examples)
  exas <- liftIO $ examplars examples absolutes prefix

  let
    declarations =
      with mcns $ \(RelativeFileContent mcn (Machinator.DefinitionFile _ definitions)) ->
        (mcn,  Projector.unTypeDecls . Projector.machinatorDecls $ definitions)

    plain =
      Projector.machinatorDecls $
        mcns >>= \(RelativeFileContent _ (Machinator.DefinitionFile _ definitions)) -> definitions

  artefacts <- firstT HtmlErrors . hoistEither $
    Projector.runBuild
      (Projector.Build (moduleNamer absolutes prefix) [])
      (Projector.UserDataTypes declarations)
      (Projector.UserConstants Map.empty)
      (Projector.RawTemplates $ with prjs $ \(RelativeFileContent prj raw) -> (prj, raw))

  firstT HtmlErrors . hoistEither $
    Projector.warnModules plain $ Projector.buildArtefactsHtmlModules artefacts

  code <- firstT HaskellErrors . hoistEither $
    Projector.codeGen
      Haskell.haskellBackend
      (codeGenNamer absolutes)
      (Projector.PlatformConstants Map.empty)
      artefacts

  detail verbose "[projector:build:html]"

  liftIO $ for_ (Map.toList $ Projector.buildArtefactsHtmlModules $ artefacts) $ \(name, modulex) ->
    when (Set.member name exas) $ do
      for_ htmls $ \(ExampleDirectory d) -> do
        for_ (Map.elems $ Projector.moduleExprs modulex) $ \(Projector.ModuleExpr _ty expr) ->
          case Interpreter.interpret plain (Projector.extractModuleExprs . Projector.buildArtefactsHtmlModules $ artefacts) expr of
            Left _ ->
              pure ()
            Right html -> do
              writeOnChange (d </> (Text.unpack . Projector.unModuleName $ name) <> ".html") (Interpreter.toText html)

  detail verbose "[projector:build:haskell]"

  for_ haskells $ \(HaskellDirectory d) ->
    liftIO $ forM_ code $ \(m, name, haskell) -> do
      unless (Set.member m exas) $
        writeOnChange (d </> name) haskell

  log "[projector:build:done]"


log :: MonadIO m => Text -> m ()
log msg =
  liftIO $ Text.hPutStrLn IO.stderr msg

detail :: MonadIO m => Verbose -> Text -> m ()
detail v msg =
  case v of
    Verbose ->
      liftIO $ Text.hPutStrLn IO.stderr msg
    Quiet ->
      pure ()

gatherBy :: [TemplateDirectory] -> FilePath -> IO [FilePath]
gatherBy templates extension =
  fmap join . for templates $ \t ->
    gather t extension

gather :: TemplateDirectory -> FilePath -> IO [FilePath]
gather (TemplateDirectory directory) extension = do
  exists <- Directory.doesDirectoryExist directory
  if not exists then
    pure []
  else do
    absolute <- Directory.makeAbsolute directory
    Find.find Find.always (Find.extension ==? extension) directory >>= \files ->
      (pure . with files $ \file ->
        absolute </> FilePath.makeRelative directory file)

stripPrefix :: [FilePath] -> FilePath -> FilePath
stripPrefix prefixes  path =
  foldr (\el acc -> maybe acc id $ List.stripPrefix el acc) path (List.reverse . List.sortOn length $ prefixes)

-- Note: This obtuse namer ensures that we can use absolute FilePath's but get
-- sensible names out. Absolute paths are required to generate error messages
-- that make sense. Sensible names, are sensible. It uses the provided prefixes
-- to relativise the module paths as it constructs the name.
-- Note: Module names are Dot.Separated.TitleCase
-- Note: Expression names are slash/separated/file-paths
moduleNamer :: [FilePath] -> Maybe Projector.ModuleName -> Projector.ModuleNamer
moduleNamer prefixes prefix =
  let
    defaultModuleNamer =
      Projector.moduleNamerSimple prefix
  in
    defaultModuleNamer {
       Projector.pathToModuleName =
         Projector.pathToModuleName defaultModuleNamer . stripPrefix prefixes
     , Projector.pathToDataModuleName =
         Projector.pathToDataModuleName defaultModuleNamer . stripPrefix prefixes
     , Projector.filePathToExprName =
         Projector.Name . Text.pack . FilePath.dropExtension . stripPrefix prefixes
     }

-- Note: See moduleNamer for absolute path name explanation.
-- Note: Backend names are camel case, i.e. slashSeparatedFilePaths produced from slash/separated/file-paths.
codeGenNamer :: [FilePath] -> Projector.CodeGenNamer
codeGenNamer prefixes =
  Projector.CodeGenNamer {
      Projector.templateDefToBackendDef =
        \n _ _ ->
          Projector.filePathToExprNameSimple . stripPrefix prefixes . Text.unpack . Projector.unName $ n
    , Projector.templateNameToBackendName =
        \n _ _ ->
          Projector.filePathToExprNameSimple . stripPrefix prefixes . Text.unpack . Projector.unName $ n
    }

writeOnChange :: FilePath -> Text -> IO ()
writeOnChange path content = do
  Directory.createDirectoryIfMissing True (FilePath.takeDirectory path)
  exists <- Directory.doesFileExist path
  case exists of
    False ->
      Text.writeFile path content
    True -> do
      old <- Text.readFile path
      when (old /= content) $
        Text.writeFile path content

dispatch :: Options.Parser a -> IO a
dispatch p = do
  Options.customExecParser
    (Options.prefs . mconcat $ [
        Options.showHelpOnEmpty
      , Options.showHelpOnError
      ])
    (Options.info
      (p <**> Options.helper)
      (mconcat [
          Options.fullDesc
        , Options.progDesc "Compile projector templates to haskell or html."
        , Options.header "projector template compiler."
        ]))

pwatch :: Options.Parser Watch
pwatch =
  Options.flag NoWatch Watch . mconcat $ [
      Options.long "watch"
    , Options.short 'w'
    ]

pverbose :: Options.Parser Verbose
pverbose =
  Options.flag Quiet Verbose . mconcat $ [
      Options.long "verbose"
    , Options.short 'v'
    ]

ptemplates :: Options.Parser TemplateDirectory
ptemplates =
  fmap TemplateDirectory . Options.strOption . mconcat $ [
      Options.long "templates"
    , Options.short 't'
    , Options.metavar "DIRECTORY"
    ]

pexamples :: Options.Parser TemplateDirectory
pexamples =
  fmap TemplateDirectory . Options.strOption . mconcat $ [
      Options.long "examples"
    , Options.short 'e'
    , Options.metavar "DIRECTORY"
    ]

phaskell :: Options.Parser HaskellDirectory
phaskell =
  fmap HaskellDirectory . Options.strOption . mconcat $ [
      Options.long "haskell-output"
    , Options.short 'o'
    , Options.metavar "DIRECTORY"
    ]

phtml :: Options.Parser ExampleDirectory
phtml =
  fmap ExampleDirectory . Options.strOption . mconcat $ [
      Options.long "html-output"
    , Options.short 'h'
    , Options.metavar "DIRECTORY"
    ]

pmodulePrefix :: Options.Parser Projector.ModuleName
pmodulePrefix =
  fmap (Projector.ModuleName . Text.pack) . Options.strOption . mconcat $ [
      Options.long "module-prefix"
    , Options.short 'm'
    , Options.metavar "MODULE"
    ]

pversion :: Options.Parser ()
pversion =
  Options.flag' () . mconcat $ [
      Options.short 'V'
    , Options.long "version"
    , Options.help "Version information"
    ]
