-- | Everything in this module must be made available in the target
-- prior to codegen. i.e. these are the primitives we assume.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Projector.Html.Core.Prim (
    HtmlType
  , HtmlDecl
  , HtmlDecls
  , HtmlExpr
  , HtmlLit
  , PrimT (..)
  , Value (..)
  , types
  , tBool
  , dBool
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           P

import           Projector.Core


type HtmlType = Type PrimT
type HtmlDecl = Decl PrimT
type HtmlDecls = TypeDecls PrimT
type HtmlExpr a = Expr PrimT a
type HtmlLit = Value PrimT

data PrimT
  = TString
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

instance Ground PrimT where
  data Value PrimT
    = VString Text
    deriving (Eq, Ord, Read, Show)

  typeOf v = case v of
    VString _ -> TString

  ppGroundType t = case t of
    TString -> "String"

  ppGroundValue v = case v of
    VString s ->
      T.pack (show s)


types :: HtmlDecls
types =
  TypeDecls $ M.fromList [
      (nBool, dBool)
    ]


nBool :: TypeName
nBool =
  TypeName "Bool"

tBool :: HtmlType
tBool =
  TVar nBool

dBool :: HtmlDecl
dBool =
  DVariant [
      (Constructor "True", [])
    , (Constructor "False", [])
    ]
