{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Core.Simplify where


import           Disorder.Core
import           Disorder.Jack

import           P

import           Projector.Core.Simplify (anf, alpha, nf, whnf)

import           Test.Projector.Core.Arbitrary


-- These normalisation props are also true for untyped terms (if nf
-- terminates, and only up to alpha), but the generators sometimes
-- spit out fixpoints.
prop_nf_idem =
  gamble (genType genTestLitT) $ \ty ->
    gamble (genWellTypedTestExpr ty) $ \e ->
      nf (nf e) === nf e

prop_whnf_idem =
  gamble (genType genTestLitT) $ \ty ->
    gamble (genWellTypedTestExpr ty) $ \e ->
      whnf (whnf e) === whnf e

prop_whnf_nf_idem =
  gamble (genType genTestLitT) $ \ty ->
    gamble (genWellTypedTestExpr ty) $ \e ->
      whnf (nf e) === nf e

prop_anf_idem =
  gamble genTestExpr $ \e ->
    anf (anf e) === anf e

prop_alpha_idem =
  gamble genTestExpr $ \e ->
    alpha (alpha e) === alpha e


return []
tests = $disorderCheckEnvAll TestRunNormal
