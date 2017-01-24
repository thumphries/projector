{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Projector.Html.Backend.Haskell.Prim (
    HaskellPrimT (..)
  , Value (..)
  , toHaskellPrim
  ) where


import qualified Data.Text as T

import           P

import           Projector.Core
import           Projector.Html.Data.Prim


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


toHaskellPrim :: Expr PrimT a -> Expr HaskellPrimT a
toHaskellPrim =
  mapGround tmap vmap

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
