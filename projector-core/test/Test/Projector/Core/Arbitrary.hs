{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Projector.Core.Arbitrary where


import qualified Bound as B
import qualified Bound.Name as B
import qualified Bound.Var as B

import Disorder.Corpus
import Disorder.Jack

import P

import Projector.Core.Syntax
import Projector.Core.Type


genType :: Jack l -> Jack (Type l)
genType g =
  let nonrec = [
          TLit <$> g
        ]

      recc = [
          TArrow <$> genType g <*> genType g
        ]

  in oneOfRec nonrec recc

genExpr :: Jack Text -> Jack (Type l) -> Jack (Value l) -> Jack (Expr l Text Text)
genExpr n t v =
  let shrink z = case z of
        ELit _ ->
          []

        EVar _ ->
          []

        EApp x y ->
          [x, y]

        ELam _ x ->
          -- need to walk under binders with fromScope
          -- and give sensible names to all the anonymous bound variables.
          -- luckily, we were tracking their source names using Bound.Name!
          [uninstantiate (B.fromScope x)]

      nonrec = [
          ELit <$> v
        , EVar <$> n
        ]

      recc = [
          EApp <$> genExpr n t v <*> genExpr n t v
        , genLam n t v
        ]
 in reshrink shrink (oneOfRec nonrec recc)

genLam :: Jack Text -> Jack (Type l) -> Jack (Value l) -> Jack (Expr l Text Text)
genLam n t v = do
  nam <- n
  typ <- t
  -- Make it likely that we'll actually use the bound name
  let n' = oneOf [pure nam, n]
  bdy <- genExpr n' t v
  pure (lam nam typ bdy)

-- Use source metadata in Bound.Name to restore names to instantiated variables.
uninstantiate :: Expr l Text (B.Var (B.Name Text ()) Text) -> Expr l Text Text
uninstantiate =
  fmap (B.unvar B.name id)

-- A simple set of literals for testing purposes
data TestLitT
  = TBool
  | TInt
  | TString
  deriving (Eq, Ord, Show)

instance Ground TestLitT where
  data Value TestLitT
    = VBool Bool
    | VInt Int
    | VString Text
    deriving (Eq, Ord, Show)

  typeOf v = case v of
    VBool _ -> TBool
    VInt _ -> TInt
    VString _ -> TString

genTestLitT :: Jack TestLitT
genTestLitT =
  elements [
      TBool
    , TInt
    , TString
    ]

genTestLitValue :: Jack (Value TestLitT)
genTestLitValue =
  oneOf [
      VBool <$> arbitrary
    , VInt <$> chooseInt (0, 100)
    , VString <$> elements muppets
    ]

genTestExpr :: Jack (Expr TestLitT Text Text)
genTestExpr =
  genExpr (elements muppets) (genType genTestLitT) genTestLitValue
