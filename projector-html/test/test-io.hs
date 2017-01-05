import           Disorder.Core.Main

import qualified Test.IO.Projector.Html.Backend.Haskell as Haskell
import qualified Test.IO.Projector.Html.Backend.Purescript as Purescript

main :: IO ()
main = do
  disorderMain [
      Purescript.tests
    , Haskell.tests
    ]
