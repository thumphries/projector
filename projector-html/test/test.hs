import           Disorder.Core.Main

import qualified Test.Projector.Html.Parser as Parser
import qualified Test.Projector.Html.Core.Elaborator as Elab


main :: IO ()
main =
  disorderMain [
      Parser.tests
    , Elab.tests
    ]
