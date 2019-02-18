{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Core.Syntax where


import           Control.Monad.Trans.State

import           Disorder.Core
import           Disorder.Jack

import           Projector.Core.Prelude

import           Projector.Core.Syntax

import           Test.Projector.Core.Arbitrary


-- (foldlExpr extractAnnotation) should correspond to foldl
prop_foldlExpr =
  gamble genExprIntAnn $ \expr ->
    foldlExpr
      (\acc e -> acc <> [extractAnnotation e])
      (\acc p -> acc <> [extractPatternAnnotation p])
      []
      expr
    ===
    foldl' (\acc a -> acc <> [a]) [] expr

-- (foldlExprM extractAnnotation) should correspond to foldlM
prop_foldlExprM =
  gamble genExprIntAnn $ \expr ->
    runState
      (foldlExprM
         (\acc e -> fun acc (extractAnnotation e))
         (\acc p -> fun acc (extractPatternAnnotation p))
         0
         expr)
      []
    ===
    runState (foldlM fun 0 expr) []
  where
    fun acc a = do
      modify' (<> [a])
      pure (acc + 1 :: Integer)

-- (foldrExpr extractAnnotation) should correspond to foldr
prop_foldrExpr =
  gamble genExprIntAnn $ \expr ->
    foldrExpr
      (\e acc -> extractAnnotation e : acc)
      (\p acc -> extractPatternAnnotation p : acc)
      []
      expr
    ===
    foldr (\a acc -> a : acc) [] expr

-- (foldrExprM extractAnnotation) should correspond to foldlM
prop_foldrExprM =
  gamble genExprIntAnn $ \expr ->
    runState
      (foldrExprM
         (\e acc -> fun (extractAnnotation e) acc)
         (\p acc -> fun (extractPatternAnnotation p) acc)
         0
         expr)
      []
  ===
  runState (foldrM fun 0 expr) []
  where
    fun a acc = do
      modify' (a :)
      pure (acc + 1 :: Integer)

genExprIntAnn :: Jack (Expr TestLitT Int)
genExprIntAnn =
  genTestExpr >>= traverse (const arbitrary)

return []
tests = $disorderCheckEnvAll TestRunNormal
