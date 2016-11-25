-- | This is the HTML abstraction that users interact with.
-- Backends may choose to transform it into something else.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Core.Library (
    types
  , tTag
  , nTag
  , tAttribute
  , nAttribute
  , tAttributeKey
  , nAttributeKey
  , tAttributeValue
  , nAttributeValue
  , tHtml
  , nHtml
  , dHtml
  , tHtmlNode
  , nHtmlNode
  , dHtmlNode
  ) where


import qualified Data.Map.Strict as M

import           P

import           Projector.Core
import           Projector.Html.Core.Prim (HtmlType, HtmlDecl, HtmlDecls)
import qualified Projector.Html.Core.Prim as Prim


types :: HtmlDecls
types =
  TypeDecls $ M.fromList [
      (nTag, dTag)
    , (nAttribute, dAttribute)
    , (nAttributeKey, dAttributeKey)
    , (nAttributeValue, dAttributeValue)
    , (nHtml, dHtml)
    , (nHtmlNode, dHtmlNode)
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
      (Constructor "Tag", [TLit Prim.TString])
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
      (Constructor "AttributeKey", [TLit Prim.TString])
    ]

-- -----------------------------------------------------------------------------

nAttributeValue :: TypeName
nAttributeValue =
  TypeName "AttributeValue"

tAttributeValue :: HtmlType
tAttributeValue =
  TVar nAttributeValue

-- TODO need to add unquoted constructor maybe?
dAttributeValue :: HtmlDecl
dAttributeValue =
  DVariant [
      (Constructor "AttributeValue", [TLit Prim.TString])
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
      (Constructor "Html", [TList tHtmlNode])
    ]


-- -----------------------------------------------------------------------------

nHtmlNode :: TypeName
nHtmlNode =
  TypeName "HtmlNode"

tHtmlNode :: HtmlType
tHtmlNode =
  TVar nHtmlNode

dHtmlNode :: HtmlDecl
dHtmlNode =
  DVariant [
      (Constructor "Element", [tTag, TList tAttribute, TList tHtmlNode])
    , (Constructor "VoidElement", [tTag, TList tAttribute])
    , (Constructor "Comment", [TLit Prim.TString])
    , (Constructor "Plain", [TLit Prim.TString])
    , (Constructor "Whitespace", [])
    ]
