-- | A stupid repl tool
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           Control.Monad.IO.Class  (MonadIO(..))
import           Control.Monad.Trans.Class  (MonadTrans(..))
import           Control.Monad.Trans.State  (StateT, runStateT, gets, modify')

import qualified Data.List as L
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           P hiding (bind)

import qualified Projector.Core as Core
import           Projector.Html
import qualified Projector.Html as Html
import           Projector.Html.Data.Annotation
import qualified Projector.Html.Backend.Haskell as Haskell
import qualified Projector.Html.Backend.Purescript as Purescript
import qualified Projector.Html.Pretty as HP

import           System.Console.Haskeline as HL
import           System.IO  (IO, FilePath)

import           X.Control.Monad.Trans.Either



main :: IO ()
main = do
  repl defaultReplState

repl :: ReplState -> IO ()
repl bs =
  HL.runInputT HL.defaultSettings (loop bs)

loop :: ReplState -> HL.InputT IO ()
loop bs = do
  minput <- getInput bs
  case minput of
    Nothing ->
      pure ()
    Just line ->
      ecase (readCommand line) (\_ -> HL.outputStrLn "unknown command" *> loop bs) $ \cmd -> do
        eres <- lift (runRepl bs (runReplCommand cmd))
        ecase eres
          (\e -> HL.outputStrLn (T.unpack (renderReplError e)) *> loop bs)
          (\(resp, bs') ->
            case resp of
              ReplSuccess t -> do
                HL.outputStrLn (T.unpack t)
                loop bs'
              ReplVoid -> do
                loop bs'
              ReplExit ->
                pure ())

getInput :: ReplState -> HL.InputT IO (Maybe Text)
getInput bs =
  let prompt = "slideshow> " in
  if replMultiline bs
    then getMultilineInput prompt
    else fmap (fmap T.pack) (HL.getInputLine prompt)

getMultilineInput :: [Char] -> HL.InputT IO (Maybe Text)
getMultilineInput prompt = do
  mfst <- HL.getInputLine prompt
  maybe (pure Nothing) (go . (:[]))  mfst
  where
    go xs = do
      msnd <- HL.getInputLine " |"
      maybe (pure (Just (unesc (T.pack (L.unlines (L.reverse xs)))))) (go . (:xs)) msnd
    -- fold up escaped newlines
    unesc = T.replace "\\\n" ""


readCommand :: Text -> Either ReplError ReplCommand
readCommand t =
  case T.words t of
    [":load", foo, path] ->
      pure (LoadTemplate foo (T.unpack path))
    (":let" : foo : xs) ->
      pure (LetTemplate foo (T.unwords xs))
    [":template", foo] ->
      pure (DumpTemplate foo)
    [":core", foo] ->
      pure (DumpCore foo)
    [":type", foo] ->
      pure (DumpType foo)
    [":t", foo] ->
      pure (DumpType foo)
    [":haskell", foo] ->
      pure (DumpHaskell foo)
    [":purescript", foo] ->
      pure (DumpPurescript foo)
    [":multiline"] ->
      pure SetMultiline
    [":nomultiline"] ->
      pure SetNoMultiline
    [":exit"] ->
      pure ExitRepl
    _ ->
      Left ReplBadCommand

data ReplCommand
  = LoadTemplate Text FilePath
  | LetTemplate Text Text
  | DumpTemplate Text
  | DumpCore Text
  | DumpType Text
  | DumpHaskell Text
  | DumpPurescript Text
  | SetMultiline
  | SetNoMultiline
  | ExitRepl
  deriving (Eq, Read, Show)

data ReplError
  = ReplError HtmlError
  | ReplUnbound Text
  | ReplNotATemplate Text
  | ReplBadCommand
  deriving (Eq, Show)

renderReplError :: ReplError -> Text
renderReplError re =
  case re of
    ReplError h ->
      renderHtmlError h
    ReplUnbound v ->
      "'" <> v <> "' is not bound"
    ReplNotATemplate b ->
      "'" <> b <> "' is a repl expression, there is no template to render!"
    ReplBadCommand ->
      "unknown command"

data ReplResponse
  = ReplSuccess Text
  | ReplVoid
  | ReplExit
  deriving (Eq, Show)

data ReplState = ReplState {
    replBindings :: Bindings
  , replKnown :: Map Text (HtmlType, SrcAnnotation)
  , replMultiline :: Bool
  } deriving (Eq, Show)

defaultReplState :: ReplState
defaultReplState = ReplState {
    replBindings = mempty
  , replKnown = mempty
  , replMultiline = False
  }

newtype Bindings = Bindings { unBindings :: Map Text Binding }
  deriving (Eq, Show, Monoid)

bind :: Text -> Binding -> Bindings -> Bindings
bind n b (Bindings m) =
  Bindings (M.insert n b m)

bindM :: Text -> Binding -> Repl ()
bindM n b =
  Repl . modify' $ \(ReplState bs kn m) ->
    ReplState (bind n b bs) (M.insert n (Core.extractAnnotation (boundCore b)) kn) m

withBind :: Text -> (Binding -> Repl a) -> Repl a
withBind n f = do
  mbind <- Repl $ gets (M.lookup n . unBindings . replBindings)
  maybe (err (ReplUnbound n)) f mbind

data Binding
  = TBind (Template Range) HtmlType (HtmlExpr (HtmlType, SrcAnnotation))
  | TFBind FilePath (Template Range) HtmlType (HtmlExpr (HtmlType, SrcAnnotation))
  | EBind HtmlType (HtmlExpr (HtmlType, SrcAnnotation))
  deriving (Eq, Show)

boundType :: Binding -> HtmlType
boundType b =
  case b of
    TBind _ t _ ->
      t
    TFBind _ _ t _ ->
      t
    EBind t _ ->
      t

boundCore :: Binding -> HtmlExpr (HtmlType, SrcAnnotation)
boundCore b =
  case b of
    TBind _ _ c ->
      c
    TFBind _ _ _ c ->
      c
    EBind _ c ->
      c

boundTemplate :: Binding -> Maybe (Template Range)
boundTemplate b =
  case b of
    TBind t _ _ ->
      Just t
    TFBind _ t _ _ ->
      Just t
    EBind _ _ ->
      Nothing

newtype Repl a = Repl { unRepl :: StateT ReplState (EitherT ReplError IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

runRepl :: ReplState -> Repl a -> IO (Either ReplError (a, ReplState))
runRepl b f =
  runEitherT (runStateT (unRepl f) b)

runReplCommand :: ReplCommand -> Repl ReplResponse
runReplCommand cmd =
  case cmd of
    LoadTemplate name path -> do
      (ast, ty, core) <- loadTemplate path
      bindM name (TFBind path ast ty core)
      pure (ReplSuccess (name <> " : " <> Core.ppType ty))
    LetTemplate name temp -> do
      (ast, ty, core) <- parseTemplate' "<repl>" temp
      bindM name (TBind ast ty core)
      pure (ReplSuccess (name <> " : " <> Core.ppType ty))
    DumpTemplate name -> do
      let dump t = pure (ReplSuccess (HP.uglyPrintTemplate t))
      withBind name $ maybe (err (ReplNotATemplate name)) dump . boundTemplate
    DumpCore name -> do
      let dump = pure . ReplSuccess . Core.ppExpr
      withBind name $ dump . boundCore
    DumpType name -> do
      let dump = pure . ReplSuccess . Core.ppType
      withBind name $ dump . boundType
    DumpHaskell name -> do
      let dump = pure . ReplSuccess . Haskell.renderExpr (Core.Name name)
      withBind name $ dump . boundCore
    DumpPurescript name -> do
      let dump = pure . ReplSuccess . Purescript.renderExpr (Core.Name name)
      withBind name $ dump . boundCore
    SetMultiline -> do
      Repl (modify' (\s -> s { replMultiline = True }))
      pure ReplVoid
    SetNoMultiline -> do
      Repl (modify' (\s -> s { replMultiline = False }))
      pure ReplVoid
    ExitRepl ->
      pure ReplExit

loadTemplate :: FilePath -> Repl (Template Range, HtmlType, HtmlExpr (HtmlType, SrcAnnotation))
loadTemplate f = do
  t <- liftIO (T.readFile f)
  parseTemplate' f t

parseTemplate' :: FilePath -> Text -> Repl (Template Range, HtmlType, HtmlExpr (HtmlType, SrcAnnotation))
parseTemplate' f t =
  Repl $ do
    k <- gets replKnown
    lift . firstEitherT ReplError $ do
      ast <- hoistEither (Html.parseTemplate f t)
      (ty, core) <- hoistEither (Html.checkTemplateIncremental mempty k ast)
      pure (ast, ty, core)

err :: ReplError -> Repl a
err =
  Repl . lift . left
