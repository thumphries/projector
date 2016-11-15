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
  gamble genWellTypedTestExpr' $ \(ty, ctx, e) ->
    typeCheck ctx e === pure ty

prop_welltyped_shrink =
  jackShrinkProp 5 genWellTypedTestExpr' $ \(ty, ctx, e) ->
    typeCheck ctx e === pure ty

{-
-- prop_illtyped =
  gamble genIllTypedTestExpr $ \e ->
    property (isLeft (typeCheck e))

-- prop_illtyped_shrink =
  jackShrinkProp 5 genIllTypedTestExpr $ \e ->
    property (isLeft (typeCheck e))
-}

prop_nf_consistent =
  gamble genWellTypedTestExpr' $ \(ty, ctx, e) ->
    typeCheck ctx (nf e) === pure ty

prop_whnf_consistent =
  gamble genWellTypedTestExpr' $ \(ty, ctx, e) ->
    typeCheck ctx (whnf e) === pure ty


return []
tests = $disorderCheckEnvAll TestRunNormal
