{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Projector.Html.Data.Prim (
  -- * Primitive types
    PrimT (..)
  , Value (..)
  -- * Primitive functions
  , primExprs
  -- ** String append
  , nStringAppend
  , tStringAppend
  , eStringAppend
  -- ** String concat
  , nStringConcat
  , tStringConcat
  , eStringConcat
  -- * Gross type aliases
  , HtmlType
  , HtmlDecl
  , HtmlDecls
  , HtmlExpr
  , HtmlLit
  , CheckedHtmlModule (..)
  , UncheckedHtmlModule (..)
  , parsePrimT
  ) where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           P

import           Projector.Core
import           Projector.Html.Data.Module


type HtmlType = Type PrimT
type HtmlDecl = Decl PrimT
type HtmlDecls = TypeDecls PrimT
type HtmlExpr a = Expr PrimT a
type HtmlLit = Value PrimT

newtype CheckedHtmlModule a = CheckedHtmlModule {
    unCheckedHtmlModule :: Module HtmlType PrimT a
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype UncheckedHtmlModule a = UncheckedHtmlModule {
    unUncheckedHtmlModule :: Module () PrimT a
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- -----------------------------------------------------------------------------
-- Primitive types that every backend must provide/support

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

primTLookup :: M.Map Text PrimT
primTLookup =
  M.fromList . with [minBound..maxBound] $ \v ->
    (ppGroundType v, v)

parsePrimT :: Text -> Maybe PrimT
parsePrimT =
  flip M.lookup primTLookup

-- -----------------------------------------------------------------------------

primExprs :: Map Name (HtmlType, HtmlExpr ())
primExprs =
  M.fromList [
      (nStringAppend, (tStringAppend, eStringAppend))
    , (nStringConcat, (tStringConcat, eStringConcat))
    ]

-- -----------------------------------------------------------------------------

nStringAppend :: Name
nStringAppend =
  Name "append"

tStringAppend :: HtmlType
tStringAppend =
  TArrow (TLit TString) (TArrow (TLit TString) (TLit TString))

eStringAppend :: HtmlExpr ()
eStringAppend =
  foreign_ nStringAppend tStringAppend

-- -----------------------------------------------------------------------------
-- TODO could probably implement this first class at some point

nStringConcat :: Name
nStringConcat =
  Name "concat"

tStringConcat :: HtmlType
tStringConcat =
  TArrow (TList (TLit TString)) (TLit TString)

eStringConcat :: HtmlExpr ()
eStringConcat =
  foreign_ nStringConcat tStringConcat

