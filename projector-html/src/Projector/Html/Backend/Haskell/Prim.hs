{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Projector.Html.Backend.Haskell.Prim (
    HaskellPrimT (..)
  , Value (..)
  , toHaskellExpr
  , toHaskellModule
  , HaskellType
  ) where


import qualified Data.Text as T

import           P

import           Projector.Core
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim


type HaskellType = Type HaskellPrimT

data HaskellPrimT
  = HTextT
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

instance Ground HaskellPrimT where
  data Value HaskellPrimT
    = HTextV Text
    deriving (Eq, Ord, Read, Show)

  typeOf v = case v of
    HTextV _ -> HTextT

  ppGroundType t = case t of
    HTextT -> "Text"

  ppGroundValue v = case v of
    HTextV s ->
      T.pack (show s)


toHaskellExpr :: Expr PrimT a -> Expr HaskellPrimT a
toHaskellExpr =
  mapGround tmap vmap

toHaskellModule :: Module HtmlType PrimT a -> Module HaskellType HaskellPrimT a
toHaskellModule (Module typs imps exps) =
  Module typs imps (with exps (\(t, e) -> (toHaskellType t, toHaskellExpr e)))

toHaskellType :: HtmlType -> HaskellType
toHaskellType =
  mapGroundType tmap
{-# INLINE toHaskellType #-}

tmap :: PrimT -> HaskellPrimT
tmap prim =
  case prim of
    TString -> HTextT
{-# INLINE tmap #-}

vmap :: Value PrimT -> Value HaskellPrimT
vmap val =
  case val of
    VString t -> HTextV t
{-# INLINE vmap #-}
