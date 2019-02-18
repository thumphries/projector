{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Projector.Html.Expect where


import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Disorder.Core.IO (testIO)
import           Disorder.Jack

import           Projector.Core.Prelude

import           System.FilePath ((</>), (<.>))
import           System.IO (FilePath, IO)

import           Text.Show.Pretty (ppShow)


expectFile :: Show a => FilePath -> FilePath -> (x -> Text) -> (Text -> Either x a) -> Property
expectFile root fp e f =
  once . testIO $ do
    fin <- T.readFile (root </> fp <.> "in")
    out <- T.readFile (root </> fp <.> "out")
    pure $
      let result = fmap (T.pack . ppShow) (f fin) in
        (either
          (counterexample . T.unpack . e)
          (counterexample . T.unpack . textDiff out)
          result)
        (property (either (const False) (== out) result))

mkExpect :: Show a => FilePath -> FilePath -> Text -> a -> IO ()
mkExpect root fp fin out = do
  T.writeFile (root </> fp <.> "in") fin
  T.writeFile (root </> fp <.> "out") (T.pack (ppShow out))

updateExpect :: (Show a, Foldable t) => FilePath -> FilePath -> (Text -> t a) -> IO ()
updateExpect root fp f = do
  fin <- T.readFile (root </> fp <.> "in")
  for_ (f fin) $
    T.writeFile (root </> fp <.> "out") . T.pack . ppShow

textDiff :: Text -> Text -> Text
textDiff a b =
  let la = T.lines a
      lb = T.lines b
  in T.unlines (go la lb mempty)
  where
    go :: [Text] -> [Text] -> [Text] -> [Text]
    go [] [] acc = acc
    go [] ys acc = acc <> fmap ("+" <>) ys
    go xs [] acc = acc <> fmap ("-" <>) xs
    go (x:xs) (y:ys) acc =
      go xs ys $
        acc <> if x == y
                 then [" " <> x]
                 else ["-" <> x, "+" <> y]
