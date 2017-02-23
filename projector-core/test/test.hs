import           Disorder.Core.Main

import qualified Test.Projector.Core.Check as Check
import qualified Test.Projector.Core.Eval as Eval
import qualified Test.Projector.Core.Match as Match
import qualified Test.Projector.Core.Syntax as Syntax
import qualified Test.Projector.Core.Warn as Warn

main :: IO ()
main =
  disorderMain [
      Check.tests
    , Eval.tests
    , Syntax.tests
    , Match.tests
    , Warn.tests
    ]
