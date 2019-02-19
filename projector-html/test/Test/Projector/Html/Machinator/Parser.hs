{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Machinator.Parser (tests) where

import           Disorder.Core hiding (tripping)
import           Disorder.Jack

import           Projector.Html.Machinator

import           Projector.Core.Prelude

import           Test.Projector.Html.Machinator.Gen


prop_tripping_v1 =
  gamble genDefinitionFileV1 $ \vdf ->
    tripping ppDefinitionFile (parseDefinitionFile "Test.Projector.Html.Machinator.Arbitrary") vdf

prop_tripping_v2 =
  gamble genDefinitionFileV2 $ \vdf ->
    tripping ppDefinitionFile (parseDefinitionFile "Test.Projector.Html.Machinator.Arbitrary") vdf
return []
tests = $disorderCheckEnvAll TestRunNormal
