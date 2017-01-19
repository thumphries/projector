{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Projector.Html.Data.Prim (
    PrimT (..)
  , Value (..)
  , HtmlType
  , HtmlDecl
  , HtmlDecls
  , HtmlExpr
  , HtmlLit
  ) where


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
