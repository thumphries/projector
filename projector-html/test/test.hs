import           Disorder.Core.Main

import qualified Test.Projector.Html.Lexer as Lexer


main :: IO ()
main =
  disorderMain [
      Lexer.tests
    ]
