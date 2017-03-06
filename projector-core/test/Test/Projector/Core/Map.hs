{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Core.Map where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Disorder.Core

import           P

import           Projector.Core.Map

import           Test.Projector.Core.Arbitrary
import           Test.QuickCheck.Jack ((===))
import qualified Test.QuickCheck.Jack as J
import           Test.QuickCheck.Instances ()


prop_map_merge =
  J.forAll J.arbitrary $ \(m :: Map Int (Maybe Int)) ->
  J.forAll J.arbitrary $ \(n :: Map Int (Maybe Int)) ->
    mergeA
      (traverseMissing (const id))
      (traverseMissing (const id))
      (zipWithAMatched (\_ x -> liftA2 (+) x))
      m
      n
    ===
    if any isNothing $ Map.elems m <> Map.elems n then
      Nothing
    else
      Just (fmap (fromMaybe 0) $ Map.unionWith (liftA2 (+)) m n <> m <> n)


return []
tests = $disorderCheckEnvAll TestRunNormal
