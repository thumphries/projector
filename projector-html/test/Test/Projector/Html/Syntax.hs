{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Syntax where


import qualified Data.Text as T

import           Disorder.Core hiding (tripping)
import           Disorder.Jack

import           P

import           Projector.Html
import           Projector.Html.Pretty
import           Projector.Html.Syntax

import           System.IO (FilePath, IO)

import           Test.Projector.Html.Arbitrary
import           Test.Projector.Html.Expect


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
  expectFile "test/syntax" fp renderSyntaxError (parse' fp)

mkRegression :: FilePath -> Text -> IO ()
mkRegression fp t =
  ecase (parse' fp t) (fail . T.unpack . renderSyntaxError) (mkExpect "test/syntax" fp t)

updateRegression :: FilePath -> IO ()
updateRegression fp = do
  updateExpect "test/syntax" fp (parse' fp)


return []
tests = $disorderCheckEnvAll TestRunNormal
