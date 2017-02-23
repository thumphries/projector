{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Core.Eval where


import           Disorder.Core
import           Disorder.Jack

import           P

import           Projector.Core.Eval (nf, whnf)
import           Projector.Core.Syntax (Expr (..), lam_, var_, app)
import           Projector.Core.Type

import           Test.Projector.Core.Arbitrary


-- These normalisation props are also true for untyped terms (if nf
-- terminates, and only up to alpha), but the generators sometimes
-- spit out fixpoints.
prop_nf_idem =
  gamble genWellTypedTestExpr' $ \(_, _, e) ->
    nf mempty (nf mempty e) === nf mempty e

prop_whnf_idem =
  gamble genWellTypedTestExpr' $ \(_, _, e) ->
    whnf mempty (whnf mempty e) === whnf mempty e

prop_whnf_nf_idem =
  gamble genWellTypedTestExpr' $ \(_, _, e) ->
    whnf mempty (nf mempty e) === nf mempty e

-- -----------------------------------------------------------------------------
-- church numerals

prop_nf_church_mult =
  gamble (chooseInt (0, 20)) $ \m ->
    gamble (chooseInt (0, 20)) $ \n ->
      nf mempty (mul m n) === nf mempty (nth (m * n))

unit :: Type TestLitT
unit =
  TLit TBool

church :: Type TestLitT
church =
  TArrow (TArrow unit unit) (TArrow unit unit)

zero :: Expr TestLitT ()
zero =
  lam_ "f" (Just (TArrow unit unit))
    (lam_ "x" (Just unit) (var_ "x"))

succ :: Expr TestLitT ()
succ =
  lam_ "n" (Just church)
    (lam_ "f" (Just (TArrow unit unit))
      (lam_ "x" (Just unit)
        (app
          (var_ "f")
          (app (app (var_ "n") (var_ "f")) (var_ "x")))))

mult :: Expr TestLitT ()
mult =
  lam_ "m" (Just church)
    (lam_ "n" (Just church)
      (lam_ "f" (Just (TArrow unit unit))
        (app
          (var_ "m")
          (app (var_ "n") (var_ "f")))))

nth :: Int -> Expr TestLitT ()
nth 0 = zero
nth n = app succ (nth (n - 1))

mul :: Int -> Int -> Expr TestLitT ()
mul m n =
  app (app mult (nth m)) (nth n)


return []
tests = $disorderCheckEnvAll TestRunNormal
