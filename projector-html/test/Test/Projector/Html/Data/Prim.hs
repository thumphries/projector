{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Data.Prim where

import           Disorder.Core (ExpectedTestSpeed (..), disorderCheckEnvAll)

import           Projector.Core.Prelude

import           Projector.Core
import           Projector.Html.Data.Prim

import           Test.Projector.Html.Arbitrary
import qualified Test.QuickCheck.Jack as J


prop_prim_parse_roundtrip =
  J.forAll genHtmlLitT $
    J.tripping ppGroundType parsePrimT


return []
tests = $disorderCheckEnvAll TestRunNormal
