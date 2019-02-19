{-# LANGUAGE OverloadedStrings #-}


import           Criterion.Main
import           Criterion.Types (Config(..))

import           Data.Foldable (fold)
import qualified Data.List as L
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Projector.Hydrant


thing :: Html
thing =
  fold [
      doctype "HTML"
    , parentNode (Tag "div") [Attribute (AttributeKey "blink") (AttributeValue "160bpm")]
        (textNode "marquee marquee marquee netscape navigator")
    , voidNode (Tag "img") [Attribute (AttributeKey "src") (AttributeValue "google.com")]
    , comment "html is for you and me"
    ]


linear :: Int -> Html
linear n =
  fold (L.replicate n thing)

nested :: Int -> Html
nested 0 =
  thing
nested n =
  parentNode (Tag "div") [Attribute (AttributeKey "blink") (AttributeValue "210bpm")]
    (thing <> (nested (n-1)))

escape :: Text -> Int -> Text
escape t n =
  escapeEntities (T.replicate n t)

main :: IO ()
main = do
  let cfg =
        defaultConfig {
            reportFile = Just "dist/build/hydrant-bench.html"
          , csvFile    = Just "dist/build/hydrant-bench.csv"
          }
      go f = nf toText . f
  glass <- T.readFile "test/glass.txt"
  defaultMainWith cfg [
      bgroup "linear" [
          bench "linear-100" (go linear 100)
        , bench "linear-200" (go linear 200)
        , bench "linear-500" (go linear 500)
        , bench "linear-1000" (go linear 1000)
        ]
    , bgroup "nested" [
          bench "nested-100" (go nested 100)
        , bench "nested-200" (go nested 200)
        , bench "nested-500" (go nested 500)
        , bench "nested-1000" (go nested 1000)
        ]
    , bgroup "escaping" [
          bench "escaping-100" (nf (escape glass) 100)
        , bench "escaping-200" (nf (escape glass) 200)
        , bench "escaping-500" (nf (escape glass) 500)
        , bench "escaping-1000" (nf (escape glass) 1000)
        ]
    ]
