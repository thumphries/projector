-- | Everything in this module must be made available in the target
-- prior to codegen. i.e. these are the primitives we assume.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Projector.Html.Core.Prim (
    types
  , tBool
  , dBool
  ) where

import qualified Data.Map.Strict as M

import           P

import           Projector.Core

import           Projector.Html.Data.Prim


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
