{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Core.Check where


import           Disorder.Core
import           Disorder.Jack

import           P

import           Projector.Core.Check (typeCheck)
import           Projector.Core.Simplify (nf)

import           Test.Projector.Core.Arbitrary


prop_welltyped =
  gamble (genType genTestLitT) $ \ty ->
    gamble (genWellTypedTestExpr ty) $ \e ->
      typeCheck e === pure ty

prop_consistent =
  gamble (genType genTestLitT) $ \ty ->
    gamble (genWellTypedTestExpr ty) $ \e ->
      typeCheck (nf e) === pure ty


return []
tests = $disorderCheckEnvAll TestRunNormal
