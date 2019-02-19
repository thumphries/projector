-- | Projector build tool.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           BuildInfo_projector_cli
import           DependencyInfo_projector_cli

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Options.Applicative (Parser, ReadM, long, short, help, hidden)
import qualified Options.Applicative as O

import qualified Projector.Html.Machinator as MC

import           Projector.Core.Prelude

import qualified Projector.Core as PC
import           Projector.Html
import           Projector.Html.Backend
import           Projector.Html.Backend.Haskell
import           Projector.Html.Backend.Purescript
import qualified Projector.Html.Core.Machinator as HMC

import           System.Directory
import qualified System.Exit as Exit
import qualified System.FilePath.Glob as Glob
import           System.FilePath.Posix ((</>), makeRelative, takeDirectory)
import           System.IO  (FilePath, IO)
import qualified System.IO as IO


data BackendT
  = Haskell
  | Purescript
  deriving (Eq, Ord, Show)

data BackendError
  = HaskellBackendError HaskellError
  | PurescriptBackendError PurescriptError
  deriving (Eq, Ord, Show)

data CinemaError
  = GlobError Text
  | BuildError [HtmlError]
  | BackendError [BackendError]
  | DataError Text -- MC.MachinatorError FIX
  deriving (Eq, Show)

renderCinemaError :: CinemaError -> Text
renderCinemaError ce =
  case ce of
    GlobError t ->
      "Invalid glob: " <> t
    BuildError h ->
      "Build errors:\n" <> T.unlines (fmap renderHtmlError h)
    BackendError h ->
      "Build errors:\n" <> T.unlines (fmap renderBackendError h)
    DataError me ->
      "Data errors:\n" <> me -- FIX

renderBackendError :: BackendError -> Text
renderBackendError e =
  case e of
    HaskellBackendError he ->
      renderHaskellError he
    PurescriptBackendError pe ->
      renderPurescriptError pe

data CinemaArgs = CinemaArgs {
    caBackend :: Maybe BackendT
  , caModulePrefix :: Maybe ModuleName
  , caDataModules :: [DataModuleName]
  , caStripPrefix :: Maybe StripPrefix
  , caTemplateGlob :: Glob
  , caDataGlob :: Maybe Glob
  , caOutputPath :: FilePath
  } deriving (Eq, Show)

newtype Glob = Glob {
    unGlob :: [Char]
  } deriving (Eq, Show)

newtype StripPrefix = StripPrefix {
    unStripPrefix :: FilePath
  } deriving (Eq, Show)


data Command =
    VersionCommand
  | DependencyCommand
  | CinemaCommand CinemaArgs

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  cmd <- dispatch (VersionCommand <$ versionFlag <|> DependencyCommand <$ dependencyFlag <|> CinemaCommand <$> cinemaP)
  case cmd of
    VersionCommand ->
      T.putStrLn ("cinema-" <> (T.pack buildInfoVersion))
    DependencyCommand ->
      traverse_ (T.putStrLn . T.pack) dependencyInfo
    CinemaCommand c ->
      run c

-- -----------------------------------------------------------------------------

run :: CinemaArgs -> IO ()
run args =
  orDie renderCinemaError $
    cinemaBuild
      (Build (moduleNamerSimple (caModulePrefix args)) (caDataModules args))
      (caBackend args)
      (caStripPrefix args)
      (caTemplateGlob args)
      (caDataGlob args)
      (caOutputPath args)

cinemaBuild :: Build -> Maybe BackendT -> Maybe StripPrefix -> Glob -> Maybe Glob -> FilePath -> EitherT CinemaError IO ()
cinemaBuild b mb msp tg mdg o = do
  -- Load all our datatypes from disk
  dfs <- maybe (pure []) globSafe mdg
  udts <- parseDataFiles msp dfs
  -- Load all our template files from disk
  tfs <- globSafe tg
  rts <- liftIO . fmap RawTemplates . for tfs $ \f -> do
    body <- T.readFile f
    let stripPrefix = maybe f (\sp -> makeRelative (unStripPrefix sp) f) msp
    pure (stripPrefix, body)
  -- Run the build
  ba <- hoistEither (first BuildError (runBuild b udts mempty rts))
  out <- maybe (pure mempty) (\b' -> hoistEither (first BackendError (codeGen (getBackend b') codeGenNamerSimple mempty ba))) mb
  -- Write out any artefacts
  liftIO . for_ out $ \(f, body) -> do
    let ofile = o </> f
    IO.putStrLn ("Generating " <> ofile)
    createDirectoryIfMissing True (takeDirectory ofile)
    T.writeFile ofile body

-- | Parse machinator files, discard version info.
parseDataFiles :: Maybe StripPrefix -> [FilePath] -> EitherT CinemaError IO UserDataTypes
parseDataFiles msp fps = do
  defs <- for fps $ \f -> do
    m <- liftIO (T.readFile f)
    MC.Versioned _ (MC.DefinitionFile _ defs) <- hoistEither (first (DataError . T.pack . show) (MC.parseDefinitionFile f m))
    let stripPrefix = maybe f (\sp -> makeRelative (unStripPrefix sp) f) msp
    pure (stripPrefix, PC.unTypeDecls (HMC.machinatorDecls defs))
  pure (UserDataTypes defs)

getBackend :: BackendT -> Backend a BackendError
getBackend b =
  case b of
    Haskell ->
      fmap HaskellBackendError haskellBackend

    Purescript ->
      fmap PurescriptBackendError purescriptBackend

-- -----------------------------------------------------------------------------

-- There's some partial code in Glob, you wanna be careful with that.
globSafe :: Glob -> EitherT CinemaError IO [FilePath]
globSafe g = do
  pat <- hoistEither (first (GlobError . T.pack)
           (Glob.tryCompileWith Glob.compDefault {Glob.errorRecovery = False} (unGlob g)))
  -- This is not Windows-safe, I believe you'd need to remove the Drive prefix.
  cwd <- liftIO getCurrentDirectory
  liftIO (Glob.globDir1 pat cwd)

-- -----------------------------------------------------------------------------
-- optparse

-- cinema --backend haskell --prefix "Bikeshed.Projector.NWO" --data '**/*.mcn' \
--   --templates '**/*.prj' -o "dist/build/Bikeshed/Projector/NWO/"
-- cinema -b purescript -p "Bikeshed.Foo" -d '**/*.mcn' -t '**/*.prj' \
--   -o "dist/purs/build/Bikeshed/Foo"

cinemaP :: Parser CinemaArgs
cinemaP =
  CinemaArgs
    <$> optional backendP
    <*> optional prefixP
    <*> many dataModuleNameP
    <*> optional stripPrefixP
    <*> templatesP
    <*> optional dataP
    <*> outputP

backendP :: Parser BackendT
backendP =
  O.option backendR $ fold [
      O.short 'b'
    , O.long "backend"
    , O.help "The backend to use for code generation."
    , O.metavar "BACKEND"
    ]

prefixP :: Parser ModuleName
prefixP =
  fmap (ModuleName . T.pack) $ O.strOption $ fold [
      O.short 'p'
    , O.long "prefix"
    , O.help "The module prefix to use for generated code."
    , O.metavar "MODULE_PREFIX"
    ]

dataModuleNameP :: Parser DataModuleName
dataModuleNameP =
  O.option dataModuleNameR $ fold [
      O.short 'i'
    , O.long "import"
    , O.help "The module name containing your datatypes, to be imported."
    , O.metavar "DATA_MODULE_NAME"
    ]

dataModuleNameR :: ReadM DataModuleName
dataModuleNameR =
  fmap (DataModuleName . ModuleName . T.pack) O.str

backendR :: ReadM BackendT
backendR =
  eitherTextReader id $ \s ->
    case s of
      "haskell" ->
        pure Haskell
      "purescript" ->
        pure Purescript
      _ ->
        Left ("Unknown backend. Please choose from: purescript, haskell")

templatesP :: Parser Glob
templatesP =
  O.option globR $ fold [
      O.short 't'
    , O.long "templates"
    , O.help "A glob pointing to your template files."
    , O.metavar "TEMPLATES"
    ]

dataP :: Parser Glob
dataP =
  O.option globR $ fold [
      O.short 'd'
    , O.long "data"
    , O.help "A glob to your Machinator-format data files"
    , O.metavar "DATATYPES"
    ]

outputP :: Parser FilePath
outputP =
  O.option O.str $ fold [
      O.short 'o'
    , O.long "output"
    , O.help "The destination directory for generated code."
    , O.metavar "OUTPUT_PATH"
    ]

globR :: ReadM Glob
globR =
  fmap Glob O.str

stripPrefixP :: Parser StripPrefix
stripPrefixP =
  O.option stripPrefixR $ fold [
      O.short 's'
    , O.long "strip-prefix"
    , O.help "The portion of each filename to ignore when generating names."
    , O.metavar "STRIP_PREFIX"
    ]

stripPrefixR :: ReadM StripPrefix
stripPrefixR =
  fmap StripPrefix O.str

orDie :: (e -> Text) -> EitherT e IO a -> IO a
orDie render e =
  runEitherT e >>=
    either (\err -> (IO.hPutStrLn IO.stderr . T.unpack . render) err >> Exit.exitFailure) pure

versionFlag :: Parser ()
versionFlag =
  O.flag' () $
       short 'v'
    <> long "version"
    <> help "Version information"

dependencyFlag :: Parser ()
dependencyFlag =
  O.flag' () $
       long "dependencies"
    <> hidden

dispatch :: Parser a -> IO a
dispatch parser = do
  O.customExecParser
    (O.prefs . mconcat $ [
        O.showHelpOnEmpty
      , O.showHelpOnError
      ])
    (O.info
      (parser <**> O.helper)
      O.fullDesc)

eitherTextReader :: (e -> Text) -> (Text -> Either e a) -> ReadM a
eitherTextReader render f =
  O.eitherReader $
    either (Left . T.unpack . render) Right . f . T.pack
