import           Disorder.Core.Main

import qualified Test.Projector.Core.Check as Check
import qualified Test.Projector.Core.Syntax as Syntax
import qualified Test.Projector.Core.Warn as Warn

main :: IO ()
main =
  disorderMain [
      Check.tests
    , Syntax.tests
    , Warn.tests
    ]
