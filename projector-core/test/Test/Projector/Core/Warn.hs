{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Core.Warn where


import qualified Data.List as L
import qualified Data.Set as S

import           Disorder.Core
import           Disorder.Jack

import           P

import           Projector.Core.Syntax
import           Projector.Core.Warn

import           Test.Projector.Core.Arbitrary


-- big lambda billy
buildExpr :: Int -> Expr TestLitT ()
buildExpr n = case n of
  0 -> var_ "billy"
  m -> app
    (lam_ "billy" Nothing (lit (VBool True)))
    (case_ (var_ "billy") [
        (pvar_ "billy",  (buildExpr (m - 1)))
      ])

prop_warn_shadowing =
  gamble (chooseInt (1, 1000)) $ \n ->
    warnShadowing (S.singleton (Name "billy")) (buildExpr n)
    ===
    Left (L.take (n*2) (L.repeat (ShadowedName () (Name "billy"))))

prop_warn_shadowing_vac =
  once $
    warnShadowing (S.singleton (Name "billy")) (buildExpr 0)
    ===
    Right ()


return []
tests = $disorderCheckEnvAll TestRunNormal
