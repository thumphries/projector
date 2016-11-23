{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Core.Simplify where


import           Disorder.Core
import           Disorder.Jack

import           P

import           Projector.Core.Simplify (alphaNf, alpha, nf, whnf)
import           Projector.Core.Syntax (Expr (..), lam_, var_, app)
import           Projector.Core.Type (Type(..))

import           Test.Projector.Core.Arbitrary


-- These normalisation props are also true for untyped terms (if nf
-- terminates, and only up to alpha), but the generators sometimes
-- spit out fixpoints.
prop_nf_idem =
  gamble genWellTypedTestExpr' $ \(_, _, e) ->
    nf (nf e) === nf e

prop_whnf_idem =
  gamble genWellTypedTestExpr' $ \(_, _, e) ->
    whnf (whnf e) === whnf e

prop_whnf_nf_idem =
  gamble genWellTypedTestExpr' $ \(_, _, e) ->
    whnf (nf e) === nf e

prop_alphaNf_idem =
  gamble genWellTypedTestExpr' $ \(_, _, e) ->
    alphaNf (alphaNf e) === alphaNf e

prop_alpha_idem =
  gamble genWellTypedTestExpr' $ \(_, _, e) ->
    alpha (alpha e) === alpha e


-- -----------------------------------------------------------------------------
-- church numerals

prop_nf_church_mult =
  gamble (chooseInt (0, 20)) $ \m ->
    gamble (chooseInt (0, 20)) $ \n ->
      nf (mul m n) =@@= nf (nth (m * n))

--

unit :: Type TestLitT
unit =
  TLit TBool

church :: Type TestLitT
church =
  TArrow (TArrow unit unit) (TArrow unit unit)

zero :: Expr TestLitT ()
zero =
  lam_ "f" (TArrow unit unit)
    (lam_ "x" unit (var_ "x"))

succ :: Expr TestLitT ()
succ =
  lam_ "n" church
    (lam_ "f" (TArrow unit unit)
      (lam_ "x" unit
        (app
          (var_ "f")
          (app (app (var_ "n") (var_ "f")) (var_ "x")))))

mult :: Expr TestLitT ()
mult =
  lam_ "m" church
    (lam_ "n" church
      (lam_ "f" (TArrow unit unit)
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
