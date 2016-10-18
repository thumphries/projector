{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Core.Simplify where


import           Disorder.Core
import           Disorder.Jack

import           P

import           Projector.Core.Simplify (nf, whnf)

import           Test.Projector.Core.Arbitrary


prop_nf_idem =
  gamble genTestExpr $ \e ->
    nf (nf e) === nf e

prop_whnf_idem =
  gamble genTestExpr $ \e ->
    whnf (whnf e) === whnf e


return []
tests = $disorderCheckEnvAll TestRunNormal
