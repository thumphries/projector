-- | This is the HTML abstraction that users interact with.
-- Backends may choose to transform it into something else.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Core.Library (
  -- * Collections
    types
  , exprs
  -- * Types
  -- ** Tag
  , tTag
  , nTag
  , dTag
  -- ** Attributes
  , tAttribute
  , nAttribute
  , dAttribute
  -- *** Attribute keys
  , tAttributeKey
  , nAttributeKey
  , dAttributeKey
  -- *** Attribute values
  , tAttributeValue
  , nAttributeValue
  , dAttributeValue
  -- ** Html
  , tHtml
  , nHtml
  , dHtml
  -- * Expressions
  -- ** text
  , nHtmlText
  , tHtmlText
  , eHtmlText
  -- ** attrValue
  , nHtmlAttrValue
  , tHtmlAttrValue
  , eHtmlAttrValue
  -- ** blank
  , nHtmlBlank
  , tHtmlBlank
  , eHtmlBlank
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
      (nTag, dTag)
    , (nAttribute, dAttribute)
    , (nAttributeKey, dAttributeKey)
    , (nAttributeValue, dAttributeValue)
    , (nHtml, dHtml)
    ]

exprs :: Map Name (HtmlType, HtmlExpr (HtmlType, Annotation a))
exprs =
  M.fromList [
      (nHtmlText, (tHtmlText, eHtmlText))
    , (nHtmlAttrValue, (tHtmlAttrValue, eHtmlAttrValue))
    , (nHtmlBlank, (tHtmlBlank, eHtmlBlank))
    ]

-- -----------------------------------------------------------------------------

nTag :: TypeName
nTag =
  TypeName "Tag"

tTag :: HtmlType
tTag =
  TVar nTag

dTag :: HtmlDecl
dTag =
  DVariant [
      (Constructor "Tag", [TLit TString])
    ]

-- -----------------------------------------------------------------------------

nAttribute :: TypeName
nAttribute =
  TypeName "Attribute"

tAttribute :: HtmlType
tAttribute =
  TVar nAttribute

dAttribute :: HtmlDecl
dAttribute =
  DVariant [
      (Constructor "Attribute", [tAttributeKey, tAttributeValue])
    ]

-- -----------------------------------------------------------------------------

nAttributeKey :: TypeName
nAttributeKey =
  TypeName "AttributeKey"

tAttributeKey :: HtmlType
tAttributeKey =
  TVar nAttributeKey

dAttributeKey :: HtmlDecl
dAttributeKey =
  DVariant [
      (Constructor "AttributeKey", [TLit TString])
    ]

-- -----------------------------------------------------------------------------

nAttributeValue :: TypeName
nAttributeValue =
  TypeName "AttributeValue"

tAttributeValue :: HtmlType
tAttributeValue =
  TVar nAttributeValue

dAttributeValue :: HtmlDecl
dAttributeValue =
  DVariant [
      (Constructor "AttributeValue", [TLit TString])
    ]

-- -----------------------------------------------------------------------------

nHtml :: TypeName
nHtml =
  TypeName "Html"

tHtml :: HtmlType
tHtml =
  TVar nHtml

dHtml :: HtmlDecl
dHtml =
  DVariant [
      (Constructor "Element", [tTag, TList tAttribute, tHtml])
    , (Constructor "VoidElement", [tTag, TList tAttribute])
    , (Constructor "Comment", [TLit TString])
    , (Constructor "Plain", [TLit TString])
    , (Constructor "Raw", [TLit TString])
    , (Constructor "Nested", [TList tHtml])
    ]

-- -----------------------------------------------------------------------------

nHtmlText :: Name
nHtmlText =
  Name "text"

tHtmlText :: HtmlType
tHtmlText =
  TArrow (TLit TString) tHtml

eHtmlText :: HtmlExpr (HtmlType, Annotation a)
eHtmlText =
  ELam (tHtmlText, aHtmlText) (Name "t") (Just (TLit TString))
    (ECon (tHtml, aHtmlText) (Constructor "Plain") nHtml [EVar (TLit TString, aHtmlText) (Name "t")])

aHtmlText :: Annotation a
aHtmlText =
  LibraryFunction nHtmlText

-- -----------------------------------------------------------------------------

nHtmlAttrValue :: Name
nHtmlAttrValue =
  Name "attrValue"

tHtmlAttrValue :: HtmlType
tHtmlAttrValue =
  TArrow (TLit TString) tAttributeValue

eHtmlAttrValue :: HtmlExpr (HtmlType, Annotation a)
eHtmlAttrValue =
  ELam (tHtmlAttrValue, aHtmlAttrValue) (Name "t") (Just (TLit TString))
    (ECon (tAttributeValue, aHtmlAttrValue) (Constructor "AttributeValue") nAttributeValue [EVar (TLit TString, aHtmlAttrValue) (Name "t")])

aHtmlAttrValue :: Annotation a
aHtmlAttrValue =
  LibraryFunction nHtmlAttrValue

-- -----------------------------------------------------------------------------

nHtmlBlank :: Name
nHtmlBlank =
  Name "blank"

tHtmlBlank :: HtmlType
tHtmlBlank =
  tHtml

eHtmlBlank :: HtmlExpr (HtmlType, Annotation a)
eHtmlBlank =
  ECon (tHtml, aHtmlBlank) (Constructor "Nested") nHtml
    [EList (TList tHtml, aHtmlBlank) []]

aHtmlBlank :: Annotation a
aHtmlBlank =
  LibraryFunction nHtmlBlank

-- -----------------------------------------------------------------------------
