{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Projector.Html.Data.Prim (
  -- * Primitive types
    PrimT (..)
  , Value (..)
  -- * Gross type aliases
  , HtmlType
  , HtmlDecl
  , HtmlDecls
  , HtmlExpr
  , HtmlLit
  , HtmlWarning
  , parsePrimT
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
type HtmlWarning a = Warning PrimT a

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
