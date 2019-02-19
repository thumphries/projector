{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Projector.Html.Machinator.Parser (tests) where

import           Hedgehog

import           Projector.Html.Machinator

import           Projector.Core.Prelude

import           System.IO (IO)

import           Test.Projector.Html.Machinator.Gen


prop_tripping_v1 :: Property
prop_tripping_v1 =
  property $ do
    vdf <- forAll genDefinitionFileV1
    tripping vdf ppDefinitionFile (parseDefinitionFile "Test.Projector.Html.Machinator.Arbitrary")

prop_tripping_v2 :: Property
prop_tripping_v2 =
  property $ do
    vdf <- forAll genDefinitionFileV2
    tripping vdf ppDefinitionFile (parseDefinitionFile "Test.Projector.Html.Machinator.Arbitrary")

tests :: IO Bool
tests =
  checkParallel $$(discover)
