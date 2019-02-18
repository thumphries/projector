{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Projector.Html.Syntax where


import qualified Data.Text as T

import           Hedgehog

import           Projector.Core.Prelude

import           Projector.Html
import           Projector.Html.Pretty
import           Projector.Html.Syntax

import           System.IO (FilePath, IO)

import           Test.Projector.Html.Gen
import           Test.Projector.Html.Expect



prop_parse_roundtrip :: Property
prop_parse_roundtrip =
  property $ do
    t <- forAll genTemplate
    annotate . T.unpack $ uglyPrintTemplate t
    tripping t
        uglyPrintTemplate
        parse

prop_parse_unit_prj :: Property
prop_parse_unit_prj =
  regressionFile "record_prec"

prop_parse_unit_indent_tag :: Property
prop_parse_unit_indent_tag =
  regressionFile "indent_tag"

prop_parse_unit_html :: Property
prop_parse_unit_html =
  regressionFile "html"

prop_parse_unit_indent_hell :: Property
prop_parse_unit_indent_hell =
  regressionFile "indent_hell"

prop_parse_unit_case :: Property
prop_parse_unit_case =
  regressionFile "case"

prop_parse_unit_brace :: Property
prop_parse_unit_brace =
  regressionFile "brace"

prop_parse_unit_case_html :: Property
prop_parse_unit_case_html =
  regressionFile "case_html"

prop_parse_unit_conid :: Property
prop_parse_unit_conid =
  regressionFile "conid"

prop_parse_unit_sensitive_layout :: Property
prop_parse_unit_sensitive_layout =
  regressionFile "sensitive_layout"

prop_parse_unit_sensitive_space :: Property
prop_parse_unit_sensitive_space =
  regressionFile "sensitive_space"

prop_parse_unit_pipe :: Property
prop_parse_unit_pipe =
  regressionFile "pipe"

prop_parse_unit_pre :: Property
prop_parse_unit_pre =
  regressionFile "pre"

prop_parse_unit_paren_plain :: Property
prop_parse_unit_paren_plain =
  regressionFile "paren_plain"

prop_parse_unit_inline_ham :: Property
prop_parse_unit_inline_ham =
  regressionFile "inline_ham"

prop_parse_unit_ul :: Property
prop_parse_unit_ul =
  regressionFile "ul"

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
  expectFile "test/syntax" fp renderSyntaxError (parse' fp)

mkRegression :: FilePath -> Text -> IO ()
mkRegression fp t =
  ecase (parse' fp t) (fail . T.unpack . renderSyntaxError) (mkExpect "test/syntax" fp t)

updateRegression :: FilePath -> IO ()
updateRegression fp = do
  updateExpect "test/syntax" fp (parse' fp)


tests :: IO Bool
tests =
  checkParallel $$(discover)
