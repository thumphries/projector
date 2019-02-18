{-# LANGUAGE NoImplicitPrelude #-}

import           Control.Monad ((>>=), (>>), when, mapM)

import           Prelude (($), (.), not, all, id)

import qualified System.Exit as Exit
import           System.IO (IO)
import qualified System.IO as IO

import qualified Test.IO.Projector.Html.Backend.Haskell as Haskell

main :: IO ()
main =
  IO.hSetBuffering IO.stdout IO.LineBuffering >> mapM id [
      Haskell.tests
    ] >>= \rs -> when (not . all id $ rs) Exit.exitFailure
