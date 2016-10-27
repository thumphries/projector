{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Core.Simplify where


import           Disorder.Core
import           Disorder.Jack

import           P

import           Projector.Core.Simplify (alpha, nf, whnf)

import           Test.Projector.Core.Arbitrary


prop_nf_idem =
  gamble genTestExpr $ \e ->
    nf (nf e) === nf e

prop_whnf_idem =
  gamble genTestExpr $ \e ->
    whnf (whnf e) === whnf e

prop_alpha_idem =
  gamble genTestExpr $ \e ->
    alpha (alpha e) === alpha e


return []
tests = $disorderCheckEnvAll TestRunNormal
