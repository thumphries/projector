{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Test.Projector.Core.Arbitrary.Ground where


import qualified Data.Text as T

import           Disorder.Corpus

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           P

import           Projector.Core.Type


-- -----------------------------------------------------------------------------
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

  ppGroundType t = case t of
    TBool -> "Bool"
    TInt -> "Int"
    TString -> "String"

  ppGroundValue v = case v of
    VBool b ->
      if b then "true" else "false"

    VInt n ->
      renderIntegral n

    VString s ->
      T.pack (show s)

genTestLitT :: Monad m => Gen m TestLitT
genTestLitT =
  Gen.element [
      TBool
    , TInt
    , TString
    ]

genTestLitValue :: Monad m => Gen m (Value TestLitT)
genTestLitValue =
  Gen.choice [
      VBool <$> Gen.bool_
    , VInt <$> Gen.enumBounded
    , VString <$> Gen.element muppets
    ]

genWellTypedTestLitValue :: Monad m => TestLitT -> Gen m (Value TestLitT)
genWellTypedTestLitValue t =
  case t of
    TBool ->
      VBool <$> Gen.bool_
    TInt ->
      VInt <$> Gen.enumBounded
    TString ->
      VString <$> Gen.element muppets
