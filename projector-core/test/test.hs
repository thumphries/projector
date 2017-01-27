import           Disorder.Core.Main

import qualified Test.Projector.Core.Check as Check
import qualified Test.Projector.Core.Simplify as Simplify
import qualified Test.Projector.Core.Syntax as Syntax

main :: IO ()
main =
  disorderMain [
      Check.tests
    , Simplify.tests
    , Syntax.tests
    ]
