{-# LANGUAGE NoImplicitPrelude #-}

import           Control.Monad ((>>=), (>>), when, mapM)

import           Prelude (($), (.), not, all, id)

import qualified System.Exit as Exit
import           System.IO (IO)
import qualified System.IO as IO

import qualified Test.Projector.Html.Syntax as Syntax
import qualified Test.Projector.Html.Core.Elaborator as Elab
import qualified Test.Projector.Html.Data.Prim as Prim
import qualified Test.Projector.Html.ModuleGraph as ModuleGraph
import qualified Test.Projector.Html.Interpreter as Interpreter
import qualified Test.Projector.Html.Machinator.Parser as Parser
import qualified Test.Projector.Html.Machinator.Graph as Graph
import qualified Test.Projector.Html.Machinator.Lexer as Lexer

main :: IO ()
main =
  IO.hSetBuffering IO.stdout IO.LineBuffering >> mapM id [
      Syntax.tests
    , Prim.tests
    , Elab.tests
    , ModuleGraph.tests
    , Interpreter.tests
    , Parser.tests
    , Graph.tests
    , Lexer.tests
    ] >>= \rs -> when (not . all id $ rs) Exit.exitFailure
