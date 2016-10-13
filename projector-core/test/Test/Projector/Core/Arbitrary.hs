{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Projector.Core.Arbitrary where


import qualified Bound as B
import qualified Bound.Var as B

import Control.Monad.State.Strict (State, evalState, get, put)

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

genExpr :: Jack (Type l) -> Jack (Value l) -> Jack (Expr l Text)
genExpr t v =
  let shrink z = case z of
        ELit _ ->
          []

        EVar _ ->
          []

        EApp x y ->
          [x, y]

        ELam _ x ->
          -- need to walk under binders with fromScope
          -- and give sensible names to all the anonymous bound variables
          [freshish (B.fromScope x)]

      nonrec = [
          ELit <$> v
        , EVar <$> elements boats
        ]

      recc = [
          EApp <$> genExpr t v <*> genExpr t v
        , lam <$> elements muppets <*> t <*> genExpr t v
        ]
 in reshrink shrink (oneOfRec nonrec recc)

-- Provide fresh-ish names for some function body.
-- The real solution here is to use something like Bound.Name
-- https://hackage.haskell.org/package/bound-1.0.7/docs/Bound-Name.html
freshish :: Expr l (B.Var () Text) -> Expr l Text
freshish =
  flip evalState 0 . traverse (B.unvar fresh pure)
  where
    fresh :: () -> State Int Text
    fresh _ = do
      i <- get
      put $! i+1
      pure $ "v" <> renderIntegral i


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

genTestExpr :: Jack (Expr TestLitT Text)
genTestExpr =
  genExpr (genType genTestLitT) genTestLitValue
