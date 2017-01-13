{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Core.CallGraph where


import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           Disorder.Core
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
genCyclicExprs =
  sized $ \n -> do
    k <- chooseInt (2, n + 2)
    names <- (L.nub <$> vectorOf k genName) `suchThat` ((== k) . L.length)
    -- take partitions of at least two
    sets <- partition 2 names
    pure (fold (fmap genCycle sets))

genAcyclicExprs :: Jack (Map Name (Expr TestLitT ()))
genAcyclicExprs =
  sized $ \n -> do
    k <- chooseInt (0, n)
    names <- (L.nub <$> vectorOf k genName) `suchThat` ((== k) . L.length)
    sets <- partition 1 names
    pure (fold (fmap genAcycle sets))

partition :: Int -> [a] -> Jack [[a]]
partition _ [] = pure []
partition i xs =
  sized $ \n -> do
    k <- chooseInt (i, n+i)
    let (y, ys) = L.splitAt k xs
    fmap (y:) (partition i ys)

genCycle :: [Name] -> Map Name (Expr TestLitT ())
genCycle names = do
  case names of
    (x:y:xs) ->
      M.fromList ((x, var y) : genem x y xs)
    _ ->
      mempty -- shouldn't happen
  where
    genem x y [] = [(y, var x)]
    genem x y (z:zs) =
      (y, var z) : genem x z zs

genAcycle :: [Name] -> Map Name (Expr TestLitT ())
genAcycle names =
  case names of
    [] ->
      mempty
    (x:xs) ->
      M.fromList (genem x xs)
  where
    genem _ [] =
      []
    genem x (y:ys) =
      (y, var x) : genem y ys


return []
tests = $disorderCheckEnvAll TestRunNormal
