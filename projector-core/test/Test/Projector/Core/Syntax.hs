{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Projector.Core.Syntax where

import           Control.Monad.Trans.State

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           Projector.Core.Prelude

import           Projector.Core.Syntax

import           System.IO (IO)

import           Test.Projector.Core.Gen


-- (foldlExpr extractAnnotation) should correspond to foldl
prop_foldlExpr :: Property
prop_foldlExpr =
  property $ do
    expr <- forAll genExprIntAnn
    foldlExpr
      (\acc e -> acc <> [extractAnnotation e])
      (\acc p -> acc <> [extractPatternAnnotation p])
      []
      expr === foldl' (\acc a -> acc <> [a]) [] expr

-- (foldlExprM extractAnnotation) should correspond to foldlM
prop_foldlExprM :: Property
prop_foldlExprM =
  property $ do
    expr <- forAll genExprIntAnn
    runState
      (foldlExprM
         (\acc e -> fun acc (extractAnnotation e))
         (\acc p -> fun acc (extractPatternAnnotation p))
         0
         expr)
      [] === runState (foldlM fun 0 expr) []
  where
    fun acc a = do
      modify' (<> [a])
      pure (acc + 1 :: Integer)

-- (foldrExpr extractAnnotation) should correspond to foldr
prop_foldrExpr :: Property
prop_foldrExpr =
  property $ do
    expr <- forAll genExprIntAnn
    foldrExpr
      (\e acc -> extractAnnotation e : acc)
      (\p acc -> extractPatternAnnotation p : acc)
      []
      expr === foldr (\a acc -> a : acc) [] expr

-- (foldrExprM extractAnnotation) should correspond to foldlM
prop_foldrExprM :: Property
prop_foldrExprM =
  property $ do
    expr <- forAll genExprIntAnn
    runState
      (foldrExprM
         (\e acc -> fun (extractAnnotation e) acc)
         (\p acc -> fun (extractPatternAnnotation p) acc)
         0
         expr)
      [] === runState (foldrM fun 0 expr) []
  where
    fun a acc = do
      modify' (a :)
      pure (acc + 1 :: Integer)

genExprIntAnn :: Gen (Expr TestLitT Int)
genExprIntAnn =
  genTestExpr >>= traverse (const Gen.enumBounded)

tests :: IO Bool
tests =
  checkParallel $$(discover)
