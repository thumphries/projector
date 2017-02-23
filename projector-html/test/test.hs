import           Disorder.Core.Main

import qualified Test.Projector.Html.Parser as Parser
import qualified Test.Projector.Html.Core.Elaborator as Elab
import qualified Test.Projector.Html.Data.Prim as Prim
import qualified Test.Projector.Html.ModuleGraph as ModuleGraph
import qualified Test.Projector.Html.Interpreter as Interpreter


main :: IO ()
main =
  disorderMain [
      Parser.tests
    , Prim.tests
    , Elab.tests
    , ModuleGraph.tests
    , Interpreter.tests
    ]
