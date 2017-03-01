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
  -- ** Attributes
  , tAttribute
  , nAttribute
  -- *** Attribute keys
  , tAttributeKey
  , nAttributeKey
  -- *** Attribute values
  , tAttributeValue
  , nAttributeValue
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

exprs :: Map Name (HtmlType, HtmlExpr (HtmlType, ()))
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
    , (Constructor "Whitespace", [])
    , (Constructor "Nested", [TList tHtml])
    ]

-- -----------------------------------------------------------------------------

nHtmlText :: Name
nHtmlText =
  Name "text"

tHtmlText :: HtmlType
tHtmlText =
  TArrow (TLit TString) tHtml

eHtmlText :: HtmlExpr (HtmlType, ())
eHtmlText =
  ELam (tHtmlText, ()) (Name "t") (Just (TLit TString))
    (ECon (tHtml, ()) (Constructor "Plain") nHtml [EVar (TLit TString, ()) (Name "t")])

-- -----------------------------------------------------------------------------

nHtmlAttrValue :: Name
nHtmlAttrValue =
  Name "attrValue"

tHtmlAttrValue :: HtmlType
tHtmlAttrValue =
  TArrow (TLit TString) tAttributeValue

eHtmlAttrValue :: HtmlExpr (HtmlType, ())
eHtmlAttrValue =
  ELam (tHtmlAttrValue, ()) (Name "t") (Just (TLit TString))
    (ECon (tAttributeValue, ()) (Constructor "AttributeValue") nAttributeValue [EVar (TLit TString, ()) (Name "t")])

-- -----------------------------------------------------------------------------

nHtmlBlank :: Name
nHtmlBlank =
  Name "blank"

tHtmlBlank :: HtmlType
tHtmlBlank =
  tHtml

eHtmlBlank :: HtmlExpr (HtmlType, ())
eHtmlBlank =
  ECon (tHtml, ()) (Constructor "Nested") nHtml
    [EList (TList tHtml, ()) []]

-- -----------------------------------------------------------------------------
