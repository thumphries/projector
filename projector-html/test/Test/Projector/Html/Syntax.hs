{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Syntax where


import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Disorder.Core hiding (tripping)
import           Disorder.Core.IO (testIO)
import           Disorder.Jack

import           P

import           Projector.Html
import           Projector.Html.Pretty
import           Projector.Html.Syntax

import           System.IO (IO, FilePath)

import           Test.Projector.Html.Arbitrary

import           Text.Show.Pretty (ppShow)


prop_parse_roundtrip =
  gamble genTemplate $ \t ->
    gamble (pure (uglyPrintTemplate t)) $ \_ ->
      tripping
        uglyPrintTemplate
        parse
        t

prop_parse_unit_prj =
  regressionFile "record_prec"

prop_parse_unit_indent_tag =
  regressionFile "indent_tag"

prop_parse_unit_html =
  regressionFile "html"

prop_parse_unit_indent_hell =
  regressionFile "indent_hell"

prop_parse_unit_case =
  regressionFile "case"

prop_parse_unit_brace =
  regressionFile "brace"

prop_parse_unit_case_html =
  regressionFile "case_html"

prop_parse_unit_conid =
  regressionFile "conid"

prop_parse_unit_sensitive_layout =
  regressionFile "sensitive_layout"

prop_parse_unit_sensitive_space =
  regressionFile "sensitive_space"


-- -----------------------------------------------------------------------------

this :: [Char]
this = "Test.Projector.Html.Syntax"

parse :: Text -> Either SyntaxError (Template ())
parse t =
  fmap (const ()) <$> templateFromText this t

parse' :: FilePath -> Text -> Either SyntaxError (Template ())
parse' fp t =
  fmap (const ()) <$> templateFromText fp t

regressionFile :: FilePath -> Property
regressionFile fp =
  once . testIO $ do
    fin <- T.readFile ("test/syntax/" <> fp <> ".in")
    out <- T.readFile ("test/syntax/" <> fp <> ".out")
    pure $
      let result = fmap (T.pack . ppShow) (parse' fp fin) in
        (either
          (counterexample . T.unpack . renderSyntaxError)
          (counterexample . T.unpack . textDiff out)
          result)
        (property (result == pure out))

mkRegression :: FilePath -> Text -> IO ()
mkRegression fp t = do
  T.writeFile ("test/syntax/" <> fp <> ".in") t
  for_ (parse' fp t) $
    T.writeFile ("test/syntax/" <> fp <> ".out") . T.pack . ppShow

updateRegression :: FilePath -> IO ()
updateRegression fp = do
  fin <- T.readFile ("test/syntax/" <> fp <> ".in")
  for_ (parse' fp fin) $
    T.writeFile ("test/syntax/" <> fp <> ".out") . T.pack . ppShow

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

return []
tests = $disorderCheckEnvAll TestRunNormal
