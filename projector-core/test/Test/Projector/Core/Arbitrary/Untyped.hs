{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Projector.Core.Arbitrary.Untyped (
    genType
  , genTestExpr
  , genExpr
  ) where


import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P

import           Projector.Core.Syntax
import           Projector.Core.Type

import           Test.Projector.Core.Arbitrary.Ground (TestLitT)
import qualified Test.Projector.Core.Arbitrary.Ground as Ground
import qualified Test.Projector.Core.Arbitrary.Name as Name


-- -----------------------------------------------------------------------------
-- Generating completely arbitrary expressions (mostly ill-typed)

genTestExpr :: Monad m => Gen m (Expr TestLitT ())
genTestExpr =
  genExpr
    Name.genName
    Name.genTypeName
    (genType Ground.genTestLitT)
    Ground.genTestLitValue

-- -----------------------------------------------------------------------------

genType :: Monad m => Gen m l -> Gen m (Type l)
genType g = do
  Gen.recursive Gen.choice [
      TLit <$> g
    ] [
      Gen.subterm (genType g) TList
    , Gen.subterm2 (genType g) (genType g) TArrow
    ]

genExpr ::
     Monad m
  => Gen m Name
  -> Gen m TypeName
  -> Gen m (Type l)
  -> Gen m (Value l)
  -> Gen m (Expr l ())
genExpr n tn t v =
  Gen.recursive Gen.choice [
      lit <$> v
    , var <$> n
    , foreign_ <$> n <*> t
    ] [
      Gen.subterm2 (genExpr n tn t v) (genExpr n tn t v) app
    , genLam n tn t v
    , genCon tn (genExpr n tn t v)
    , genCase (genExpr n tn t v) (genPattern Name.genConstructor n)
    , list <$> Gen.list (Range.linear 0 100) (genExpr n tn t v)
    , Gen.subterm2 (genExpr n tn t v) (genExpr n tn t v) (EMap ())
    ]

genLam ::
     Monad m
  => Gen m Name
  -> Gen m TypeName
  -> Gen m (Type l)
  -> Gen m (Value l)
  -> Gen m (Expr l ())
genLam n tn t v = do
  name <- n
  tyyp <- t
  -- Make it likely that we'll actually use the bound name
  let n' = Gen.choice [pure name, n]
  -- Randomly drop the type annotation
  typ' <- Gen.element [Just tyyp, empty]
  Gen.subterm (genExpr n' tn t v) (lam name typ')

genCon :: Monad m => Gen m TypeName -> Gen m (Expr l ()) -> Gen m (Expr l ())
genCon t v =
  con
    <$> Name.genConstructor
    <*> t
    <*> Gen.list (Range.linear 0 100) v

genCase :: Monad m => Gen m (Expr l ()) -> Gen m (Pattern ()) -> Gen m (Expr l ())
genCase e p =
  case_
    <$> e
    <*> Gen.list (Range.linear 1 100) ((,) <$> p <*> e)

genPattern :: Monad m => Gen m Constructor -> Gen m Name -> Gen m (Pattern ())
genPattern c n =
  Gen.recursive Gen.choice [
      pvar <$> n
    ] [
      pcon <$> c <*> Gen.list (Range.linear 0 20) (genPattern c n)
    ]
