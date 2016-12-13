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
import qualified Projector.Html.Backend.Haskell as Haskell
import qualified Projector.Html.Pretty as HP

import           System.IO  (IO, FilePath)
import qualified System.IO as IO
import qualified System.IO.Error as IOError

import           X.Control.Monad.Trans.Either



main :: IO ()
main = do
  IO.hSetBuffering IO.stdin IO.LineBuffering --IO.NoBuffering
  IO.hSetBuffering IO.stdout IO.NoBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  repl defaultReplState

repl :: ReplState -> IO ()
repl bs = do
  T.putStr "slideshow> "
  mline <- getInput bs
  mcase mline (pure ()) $ \line ->
    ecase (readCommand line) (\_ -> T.hPutStrLn IO.stderr "unknown command" *> repl bs) $ \cmd -> do
      eres <- runRepl bs (runReplCommand cmd)
      ecase eres
        (\e -> IO.hPutStrLn IO.stderr (show e) *> repl bs)
        (\(resp, bs') ->
          case resp of
            ReplSuccess t -> do
              T.putStrLn t
              repl bs'
            ReplVoid -> do
              repl bs'
            ReplExit ->
              pure ())

getInput :: ReplState -> IO (Maybe Text)
getInput (ReplState _ ml) =
  if ml
    then getContents
    else fmap Just T.getLine

getContents :: IO (Maybe Text)
getContents =
  fmap (T.pack . L.reverse) <$> go Nothing
  where
    go ms = do
      c <- IOError.tryIOError IO.getChar
      ecase c (const (pure ms)) $ \s ->
        go
          (case ms of
             Just xs ->
               Just (s : xs)
             Nothing ->
               Just [s])

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

data ReplResponse
  = ReplSuccess Text
  | ReplVoid
  | ReplExit
  deriving (Eq, Show)

data ReplState = ReplState {
    replBindings :: Bindings
  , replMultiline :: Bool
  } deriving (Eq, Show)

defaultReplState :: ReplState
defaultReplState = ReplState {
    replBindings = mempty
  , replMultiline = False
  }

newtype Bindings = Bindings { unBindings :: Map Text Binding }
  deriving (Eq, Show, Monoid)

bind :: Text -> Binding -> Bindings -> Bindings
bind n b (Bindings m) =
  Bindings (M.insert n b m)

bindM :: Text -> Binding -> Repl ()
bindM n b =
  Repl . modify' $ \(ReplState bs m) ->
    ReplState (bind n b bs) m

withBind :: Text -> (Binding -> Repl a) -> Repl a
withBind n f = do
  mbind <- Repl $ gets (M.lookup n . unBindings . replBindings)
  maybe (err (ReplUnbound n)) f mbind

data Binding
  = TBind (Template Range) HtmlType (HtmlExpr Range)
  | TFBind FilePath (Template Range) HtmlType (HtmlExpr Range)
  | EBind HtmlType (HtmlExpr Range)
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

boundCore :: Binding -> HtmlExpr Range
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
    SetMultiline -> do
      Repl (modify' (\s -> s { replMultiline = True }))
      pure ReplVoid
    SetNoMultiline -> do
      Repl (modify' (\s -> s { replMultiline = False }))
      pure ReplVoid
    ExitRepl ->
      pure ReplExit

loadTemplate :: FilePath -> Repl (Template Range, HtmlType, HtmlExpr Range)
loadTemplate f = do
  t <- liftIO (T.readFile f)
  parseTemplate' f t

parseTemplate' :: FilePath -> Text -> Repl (Template Range, HtmlType, HtmlExpr Range)
parseTemplate' f t =
  Repl . lift . firstEitherT ReplError $ do
    ast <- hoistEither (Html.parseTemplate f t)
    (ty, core) <- hoistEither (Html.checkTemplate ast)
    pure (ast, ty, core)

err :: ReplError -> Repl a
err =
  Repl . lift . left
