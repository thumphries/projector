{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Projector.Core.Match where


import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Projector.Core.Prelude

import           Projector.Core.Match

import           System.IO (IO)

import           Test.Projector.Core.Gen


prop_matchtree_monoid_assoc :: Property
prop_matchtree_monoid_assoc =
  withTests 20 . property $ do
    x <- forAll genMatchTree
    y <- forAll genMatchTree
    z <- forAll genMatchTree
    (x <> y) <> z === x <> (y <> z)

prop_matchtree_monoid_left_id :: Property
prop_matchtree_monoid_left_id =
  withTests 20 . property $ do
    x <- forAll genMatchTree
    mempty <> x === x

prop_matchtree_monoid_right_id :: Property
prop_matchtree_monoid_right_id =
  withTests 20 . property $ do
    x <- forAll genMatchTree
    x <> mempty === x

genMatchTree :: Gen MatchTree
genMatchTree =
  Gen.sized $ \n -> do
    k <- Gen.int (Range.linear 0 (fromIntegral n))
    buildMatchTree <$> (Gen.list (Range.linear 0 k) (genPattern genConstructor genName))

tests :: IO Bool
tests =
  checkParallel $$(discover)
