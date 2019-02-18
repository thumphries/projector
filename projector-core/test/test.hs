{-# LANGUAGE NoImplicitPrelude #-}

import           Control.Monad ((>>=), (>>), when, mapM)

import           Prelude (($), (.), not, all, id)

import qualified System.Exit as Exit
import           System.IO (IO)
import qualified System.IO as IO

import qualified Test.Projector.Core.Check as Check
import qualified Test.Projector.Core.Eval as Eval
import qualified Test.Projector.Core.Match as Match
import qualified Test.Projector.Core.Syntax as Syntax
import qualified Test.Projector.Core.Warn as Warn


main :: IO ()
main =
  IO.hSetBuffering IO.stdout IO.LineBuffering >> mapM id [
      Check.tests
    , Eval.tests
    , Syntax.tests
    , Match.tests
    , Warn.tests
    ] >>= \rs -> when (not . all id $ rs) Exit.exitFailure
