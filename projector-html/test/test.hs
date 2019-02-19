import           Disorder.Core.Main

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
  disorderMain [
      Syntax.tests
    , Prim.tests
    , Elab.tests
    , ModuleGraph.tests
    , Interpreter.tests
    , Parser.tests
    , Graph.tests
    , Lexer.tests
    ]
