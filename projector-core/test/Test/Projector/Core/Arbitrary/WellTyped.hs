{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Projector.Core.Arbitrary.WellTyped where


import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P

import           Projector.Core.Eval (whnf)
import           Projector.Core.Syntax
import           Projector.Core.Type

import           Test.Projector.Core.Arbitrary.Ground (TestLitT)
import qualified Test.Projector.Core.Arbitrary.Ground as Ground
import qualified Test.Projector.Core.Arbitrary.Name as Name


-- -----------------------------------------------------------------------------

genWellTypedTestExpr ::
     Monad m
  => TypeDecls TestLitT
  -> Type TestLitT
  -> Gen m (Expr TestLitT ())
genWellTypedTestExpr decls ty =
  genWellTypedExpr
    decls
    ty
    undefined
    Ground.genWellTypedTestLitValue

-- -----------------------------------------------------------------------------

genTypeDecls :: (Ground l, Monad m) => TypeDecls l -> Gen m l -> Gen m (TypeDecls l)
genTypeDecls tc gt =
  undefined

genWellTypedExpr ::
     (Ground l, Ord l, Monad m)
  => TypeDecls l
  -> Type l
  -> Gen m (Type l)
  -> (l -> Gen m (Value l))
  -> Gen m (Expr l ())
genWellTypedExpr decls ty genType genVal =
  undefined
