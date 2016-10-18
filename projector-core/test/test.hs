import           Disorder.Core.Main

import qualified Test.Projector.Core.Simplify as Simplify

main :: IO ()
main =
  disorderMain [
      Simplify.tests
    ]
