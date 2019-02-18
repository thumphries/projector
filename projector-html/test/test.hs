import           Disorder.Core.Main

import qualified Test.Projector.Html.Syntax as Syntax
import qualified Test.Projector.Html.Core.Elaborator as Elab
import qualified Test.Projector.Html.Data.Prim as Prim
import qualified Test.Projector.Html.ModuleGraph as ModuleGraph
import qualified Test.Projector.Html.Interpreter as Interpreter


main :: IO ()
main =
  disorderMain [
      Syntax.tests
    , Prim.tests
    , Elab.tests
    , ModuleGraph.tests
    , Interpreter.tests
    ]
