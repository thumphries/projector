{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Syntax where


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
        (fmap (fmap (const ())) . (templateFromText "Test.Projector.Html.Syntax"))
        t

-- regression for #193
prop_parse_unit_prj =
  once $
    fmap (fmap (const ())) (templateFromText "Test.Projector.Html.Syntax" "{ foo.bar baz.quux wem.quib pil mun }")
    ===
    (Right $
      Template
        ()
        Nothing $
        THtml
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
          ])

return []
tests = $disorderCheckEnvAll TestRunNormal
