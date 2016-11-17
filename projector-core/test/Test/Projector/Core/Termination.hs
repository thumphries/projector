{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Core.Termination where


import           Disorder.Core
import           Disorder.Jack

import           P

import           Projector.Core.Termination
import           Projector.Core.Type

import           Test.Projector.Core.Arbitrary


-- Regression tests for positivity check

prop_reg_positivity_simple_1 =
  once $
    isLeft (positivityCheck ctx)
  where
    tn = TypeName "Lefty"
    td = DVariant [
        (Constructor "Lefty", [TArrow (TVar tn) (TLit TBool)])
      ]
    ctx = declareType tn td mempty

prop_reg_positivity_simple_2 =
  once $
    positivityCheck ctx === pure ()
  where
    tn = TypeName "Lefty"
    td = DVariant [
        (Constructor "Lefty", [TArrow (TLit TBool) (TVar tn)])
      ]
    ctx = declareType tn td mempty

prop_reg_positivity_simple_3 =
  once $
    isLeft (positivityCheck ctx)
  where
    tn1 = TypeName "A"
    td1 = DVariant [
        (Constructor "AB", [TVar tn2])
      ]
    tn2 = TypeName "B"
    td2 = DVariant [
        (Constructor "BA", [TArrow (TVar tn1) (TLit TBool)])
      ]
    ctx =
        declareType tn1 td1
      . declareType tn2 td2
      $ mempty

prop_reg_positivity_simple_4 =
  once $
    positivityCheck ctx === pure ()
  where
    tn1 = TypeName "A"
    td1 = DVariant [
        (Constructor "A", [TArrow (TArrow (TVar tn1) (TLit TBool)) (TLit TBool)])
      ]
    ctx = declareType tn1 td1 mempty

prop_reg_positivity_simple_5 =
  once $
    isLeft (positivityCheck ctx)
  where
    tn1 = TypeName "A"
    td1 = DVariant [
        (Constructor "A", [TArrow (TArrow (TLit TBool) (TVar tn1)) (TLit TBool)])
      ]
    ctx = declareType tn1 td1 mempty


return []
tests = $disorderCheckEnvAll TestRunNormal
