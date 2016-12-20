{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Core.CallGraph where


import           Data.Char  (isAsciiLower)
import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Disorder.Core
import           Disorder.Corpus
import           Disorder.Jack

import           P

import           Projector.Core.Syntax
import           Projector.Core.CallGraph

import           Test.Projector.Core.Arbitrary


prop_cycles =
  neg . gamble genCyclicExprs $ \me ->
    detectCycles (buildCallGraph me) === pure ()

prop_no_cycles =
  gamble genAcyclicExprs $ \me ->
    detectCycles (buildCallGraph me) === pure ()

genCyclicExprs :: Jack (Map Name (Expr TestLitT ()))
genCyclicExprs = sized $ \n -> do
  k <- chooseInt (2, n + 2)
  names <- (L.nub <$> vectorOf k genName) `suchThat` ((== k) . L.length)
  pure $ case names of
    (x:y:xs) ->
      M.fromList ((x, var y) : genem x y xs)
    _ ->
      mempty -- shouldn't happen
  where
    genem x y [] = [(y, var x)]
    genem x y (z:zs) =
      (y, var z) : genem x z zs


genAcyclicExprs :: Jack (Map Name (Expr TestLitT ()))
genAcyclicExprs =
  sized $ \n -> do
    k <- chooseInt (0, n)
    names <- L.nub <$> vectorOf k genName
    pure $ case names of
      [] ->
        mempty
      (x:xs) ->
        M.fromList (genem x xs)
  where
    genem _ [] =
      []
    genem x (y:ys) =
      (y, var x) : genem y ys

genName :: Jack Name
genName =
  frequency [
      (1, Name <$> elements muppets)
    , (10, Name . T.pack <$> vectorOf 8 (arbitrary `suchThat` isAsciiLower))
    ]

return []
tests = $disorderCheckEnvAll TestRunNormal
