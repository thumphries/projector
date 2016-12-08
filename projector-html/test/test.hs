import           Disorder.Core.Main

import qualified Test.Projector.Html.Parser as Parser


main :: IO ()
main =
  disorderMain [
      Parser.tests
    ]
