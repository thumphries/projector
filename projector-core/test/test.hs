import           Disorder.Core.Main

import qualified Test.Projector.Core.CallGraph as CallGraph
import qualified Test.Projector.Core.Check as Check
import qualified Test.Projector.Core.Simplify as Simplify

main :: IO ()
main =
  disorderMain [
      Check.tests
    , Simplify.tests
    , CallGraph.tests
    ]
