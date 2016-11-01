{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Core.Check where


import           Disorder.Core
import           Disorder.Jack

import           P

import           Projector.Core.Check (typeCheck)
import           Projector.Core.Simplify (nf, whnf)

import           Test.Projector.Core.Arbitrary


prop_welltyped =
  gamble (genType genTestLitT) $ \ty ->
    gamble (genWellTypedTestExpr ty) $ \e ->
      typeCheck e === pure ty

prop_welltyped_shrink =
  gamble (genType genTestLitT) $ \ty ->
    jackShrinkProp 1 (genWellTypedTestExpr ty) $ \e ->
      typeCheck e === pure ty

prop_illtyped =
  gamble genIllTypedTestExpr $ \e ->
    property (isLeft (typeCheck e))

prop_illtyped_shrink =
  jackShrinkProp 1 genIllTypedTestExpr $ \e ->
    property (isLeft (typeCheck e))

prop_nf_consistent =
  gamble (genType genTestLitT) $ \ty ->
    gamble (genWellTypedTestExpr ty) $ \e ->
      typeCheck (nf e) === pure ty

prop_whnf_consistent =
  gamble (genType genTestLitT) $ \ty ->
    gamble (genWellTypedTestExpr ty) $ \e ->
      typeCheck (whnf e) === pure ty


return []
tests = $disorderCheckEnvAll TestRunNormal
