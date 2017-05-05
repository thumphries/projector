-- | Everything in this module must be made available in the target
-- prior to codegen. i.e. these are the primitives we assume.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Projector.Html.Core.Prim (
    types
  -- * Primitives types
  -- ** Bool
  , tBool
  , dBool
  , nBool
  -- * Primitive functions
  , exprs
  -- ** String append
  , nStringAppend
  , tStringAppend
  , eStringAppend
  -- ** String concat
  , nStringConcat
  , tStringConcat
  , eStringConcat
  -- ** List fold
  , nListFold
  , tListFold
  , eListFold
  -- ** isEmpty
  , nIsEmpty
  , tIsEmpty
  , eIsEmpty
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           P

import           Projector.Core

import           Projector.Html.Data.Annotation
import           Projector.Html.Data.Prim


types :: HtmlDecls
types =
  TypeDecls $ M.fromList [
      (nBool, dBool)
    ]

exprs :: Map Name (HtmlType, HtmlExpr (HtmlType, Annotation a))
exprs =
  M.fromList [
      (nStringAppend, (tStringAppend, eStringAppend))
    , (nStringConcat, (tStringConcat, eStringConcat))
    , (nListFold, (tListFold, eListFold))
    , (nIsEmpty, (tIsEmpty, eIsEmpty))
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


-- -----------------------------------------------------------------------------

nStringAppend :: Name
nStringAppend =
  Name "append"

tStringAppend :: HtmlType
tStringAppend =
  TArrow (TLit TString) (TArrow (TLit TString) (TLit TString))

eStringAppend :: HtmlExpr (HtmlType, Annotation a)
eStringAppend =
  EForeign (tStringAppend, aStringAppend) nStringAppend tStringAppend

aStringAppend :: Annotation a
aStringAppend =
  LibraryFunction nStringAppend

-- -----------------------------------------------------------------------------
-- TODO could probably implement this first class using append at some point

nStringConcat :: Name
nStringConcat =
  Name "concat"

tStringConcat :: HtmlType
tStringConcat =
  TArrow (TList (TLit TString)) (TLit TString)

eStringConcat :: HtmlExpr (HtmlType, Annotation a)
eStringConcat =
  EForeign (tStringConcat, aStringConcat) nStringConcat tStringConcat

aStringConcat :: Annotation a
aStringConcat =
  LibraryFunction nStringConcat

-- -----------------------------------------------------------------------------

nListFold :: Name
nListFold =
  Name "fold"

tListFold :: HtmlType
tListFold =
  TForall [TypeName "a"]
    (TArrow (TList (TList (TVar (TypeName "a")))) (TList (TVar (TypeName "a"))))

eListFold :: HtmlExpr (HtmlType, Annotation a)
eListFold =
  EForeign (tListFold, aListFold) nListFold tListFold

aListFold :: Annotation a
aListFold =
  LibraryFunction nListFold

-- -----------------------------------------------------------------------------

nIsEmpty :: Name
nIsEmpty =
  Name "isEmpty"

tIsEmpty :: HtmlType
tIsEmpty =
  TForall [TypeName "a"]
    (TArrow (TList (TVar (TypeName "a"))) tBool)

eIsEmpty :: HtmlExpr (HtmlType, Annotation a)
eIsEmpty =
  EForeign (tIsEmpty, aIsEmpty) nIsEmpty tIsEmpty

aIsEmpty :: Annotation a
aIsEmpty =
  LibraryFunction nIsEmpty
