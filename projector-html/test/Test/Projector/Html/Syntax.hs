{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Syntax where


import           Disorder.Core hiding (tripping)
import           Disorder.Jack

import           P

import           Projector.Html.Pretty
import           Projector.Html.Syntax

import           Test.Projector.Html.Arbitrary


prop_parse_roundtrip =
  gamble genTemplate $ \t ->
    gamble (pure (uglyPrintTemplate t)) $ \_ ->
      tripping
        uglyPrintTemplate
        (fmap (fmap (const ())) . (templateFromText "Test.Projector.Html.Parser"))
        t


return []
tests = $disorderCheckEnvAll TestRunNormal
