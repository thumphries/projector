{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Core.Match where


import           Disorder.Core
import           Disorder.Jack

import           P

import           Projector.Core.Match

import           Test.Projector.Core.Arbitrary


prop_matchtree_monoid_assoc =
  gamble genMatchTree $ \x ->
  gamble genMatchTree $ \y ->
  gamble genMatchTree $ \z ->
    (x <> y) <> z === x <> (y <> z)

prop_matchtree_monoid_left_id =
  gamble genMatchTree $ \x ->
    mempty <> x === x

prop_matchtree_monoid_right_id =
  gamble genMatchTree $ \x ->
    x <> mempty === x

genMatchTree :: Jack MatchTree
genMatchTree =
  buildMatchTree <$> (listOf (genPattern genConstructor genName))


return []
tests = $disorderCheckEnvAll TestRunNormal
