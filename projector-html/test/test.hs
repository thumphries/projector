import           Disorder.Core.Main

import qualified Test.Projector.Html.Lexer as Lexer
import qualified Test.Projector.Html.Parser as Parser


main :: IO ()
main =
  disorderMain [
      Lexer.tests
    , Parser.tests
    ]
