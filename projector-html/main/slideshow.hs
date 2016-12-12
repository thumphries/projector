-- | A stupid repl tool
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           Control.Monad.IO.Class  (MonadIO(..))
import           Control.Monad.Trans.Class  (MonadTrans(..))
import           Control.Monad.Trans.State  (StateT, runStateT, gets, modify')

import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict as M
import           Data.Text.IO as T

import           P hiding (bind)

import qualified Projector.Core as Core
import           Projector.Html
import qualified Projector.Html as Html
import qualified Projector.Html.Backend.Haskell as Haskell
import qualified Projector.Html.Pretty as HP

import           System.IO  (IO, FilePath)
import qualified System.IO as IO

import           X.Control.Monad.Trans.Either



main :: IO ()
main = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  repl mempty

repl :: Bindings -> IO ()
repl bs = do
  T.putStr "slideshow> "
  line <- IO.getLine
  -- FIX  parse commands :foo bar /foo/
  -- TODO read until EOF
  -- TODO multiline
  -- TODO core
  mcase (readMaybe line :: Maybe ReplCommand) (T.hPutStrLn IO.stderr "Unrecognised input" *> repl bs) $ \cmd -> do
    eres <- runRepl bs (runReplCommand cmd)
    ecase eres
      (\e -> IO.hPutStrLn IO.stderr (show e) *> repl bs)
      (\(resp, bs') ->
        case resp of
          ReplSuccess t -> do
            T.putStrLn t
            repl bs'
          ReplExit ->
            pure ())

data ReplCommand
  = LoadTemplate Text FilePath
  | DumpTemplate Text
  | DumpCore Text
  | DumpType Text
  | DumpHaskell Text
  | ExitRepl
  deriving (Eq, Read, Show)

data ReplError
  = ReplError HtmlError
  | ReplUnbound Text
  | ReplNotATemplate Text
  deriving (Eq, Show)

data ReplResponse
  = ReplSuccess Text
  | ReplExit
  deriving (Eq, Show)

newtype Bindings = Bindings { unBindings :: Map Text Binding }
  deriving (Monoid)

bind :: Text -> Binding -> Bindings -> Bindings
bind n b (Bindings m) =
  Bindings (M.insert n b m)

withBind :: Text -> (Binding -> Repl a) -> Repl a
withBind n f = do
  mbind <- Repl $ gets (M.lookup n . unBindings)
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

newtype Repl a = Repl { unRepl :: StateT Bindings (EitherT ReplError IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

runRepl :: Bindings -> Repl a -> IO (Either ReplError (a, Bindings))
runRepl b (Repl f) =
  runEitherT (runStateT f b)

runReplCommand :: ReplCommand -> Repl ReplResponse
runReplCommand cmd =
  case cmd of
    LoadTemplate name path -> do
      (ast, ty, core) <- Repl (lift (firstEitherT ReplError (loadTemplate path)))
      Repl $ modify' (bind name (TFBind path ast ty core))
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
    ExitRepl ->
      pure ReplExit

loadTemplate :: FilePath -> EitherT HtmlError IO (Template Range, HtmlType, HtmlExpr Range)
loadTemplate f = do
  t <- liftIO (T.readFile f)
  ast <- hoistEither (Html.parseTemplate f t)
  (ty, core) <- hoistEither (Html.checkTemplate ast)
  pure (ast, ty, core)

err :: ReplError -> Repl a
err =
  Repl . lift . left
