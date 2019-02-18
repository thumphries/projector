{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Projector.Html.Data.Prim where

import           Hedgehog

import           Projector.Core.Prelude

import           Projector.Core
import           Projector.Html.Data.Prim

import           System.IO (IO)

import           Test.Projector.Html.Gen


prop_prim_parse_roundtrip :: Property
prop_prim_parse_roundtrip =
  property $ do
    x <- forAll genHtmlLitT
    tripping x ppGroundType parsePrimT

tests :: IO Bool
tests =
  checkParallel $$(discover)
