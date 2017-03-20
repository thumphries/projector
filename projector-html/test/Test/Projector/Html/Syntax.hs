{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Syntax where


import           Data.String.QQ
import qualified Data.Text as T

import           Disorder.Core hiding (tripping)
import           Disorder.Jack

import           P

import           Projector.Html.Data.Template
import           Projector.Html.Pretty
import           Projector.Html.Syntax

import           Test.Projector.Html.Arbitrary


prop_parse_roundtrip =
  gamble genTemplate $ \t ->
    gamble (pure (uglyPrintTemplate t)) $ \_ ->
      tripping
        uglyPrintTemplate
        (fmap (fmap (const ())) . (templateFromText this))
        t

-- regression for #193
prop_parse_unit_prj =
  once $
    fmap (fmap (const ())) (templateFromText this "{ foo.bar baz.quux wem.quib pil mun }")
    ===
    (Right $
      Template
        ()
        Nothing $
        TENode () (THtml
          ()
          [ TExprNode
              ()
              (TEApp
                 ()
                 (TEApp
                    ()
                    (TEApp
                       ()
                       (TEApp
                          ()
                          (TEPrj () (TEVar () (TId "foo")) (TId "bar"))
                          (TEPrj () (TEVar () (TId "baz")) (TId "quux")))
                       (TEPrj () (TEVar () (TId "wem")) (TId "quib")))
                    (TEVar () (TId "pil" )))
                 (TEVar () (TId "mun" )))
          ]))

prop_parse_unit_indent =
  once $
    isRight $ fmap (fmap (const ())) (templateFromText this regression_indent)

regression_indent :: Text
regression_indent =
  T.pack [s|case x of
  y ->
    <a b="{ x
      }">
    </a>
|]

prop_parse_unit_html =
  once $
    isRight $ fmap (fmap (const ())) (templateFromText this regression_html)

regression_html :: Text
regression_html =
  T.pack [s|{ foo (<b>bar</b>) <span>baz</span> <h1>quux</hq> }|]

prop_parse_unit_hell =
  once $
    isRight $ fmap (fmap (const ())) (templateFromText this regression_hell)

regression_hell :: Text
regression_hell =
  T.pack [s|<a>
  {<b/>}
</a>
<c></c>
|]

prop_parse_unit_case =
  once $
    isRight $ templateFromText this regression_case

regression_case :: Text
regression_case =
  T.pack [s|case foo of
  Abc ->
    "def"
  Ghi ->
    "jkl"
  Mno ->
    "pqr"
|]

prop_parse_unit_brace =
  once $ isRight $ templateFromText this regression_brace

regression_brace :: Text
regression_brace =
  T.pack [s|{ foo bar }
{ baz quux }
|]

this :: [Char]
this = "Test.Projector.Html.Syntax"

return []
tests = $disorderCheckEnvAll TestRunNormal
