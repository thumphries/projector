-- | This is the HTML abstraction that users interact with.
-- Backends may choose to transform it into something else.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Core.Library (
  -- * Collections
    types
  , exprs
  -- * Types
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
  -- * Expressions
  , nHtmlText
  , tHtmlText
  , eHtmlText
  , nHtmlAttrValue
  , tHtmlAttrValue
  , eHtmlAttrValue
  , nHtmlBlank
  , tHtmlBlank
  , eHtmlBlank
  , nStringAppend
  , tStringAppend
  , eStringAppend
  , nStringConcat
  , tStringConcat
  , eStringConcat
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
    , (nHtmlNode, dHtmlNode)
    ]

exprs :: Map Name (HtmlType, HtmlExpr ())
exprs =
  M.fromList [
      (nHtmlText, (tHtmlText, eHtmlText))
    , (nHtmlAttrValue, (tHtmlAttrValue, eHtmlAttrValue))
    , (nHtmlBlank, (tHtmlBlank, eHtmlBlank))
    , (nStringAppend, (tStringAppend, eStringAppend))
    , (nStringConcat, (tStringConcat, eStringConcat))
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
      (Constructor "Element", [tTag, TList tAttribute, tHtml])
    , (Constructor "VoidElement", [tTag, TList tAttribute])
    , (Constructor "Comment", [TLit TString])
    , (Constructor "Plain", [TLit TString])
    , (Constructor "Whitespace", [])
    , (Constructor "Nested", [tHtml])
    ]

-- -----------------------------------------------------------------------------

nHtmlText :: Name
nHtmlText =
  Name "text"

tHtmlText :: HtmlType
tHtmlText =
  TArrow (TLit TString) tHtml

eHtmlText :: HtmlExpr ()
eHtmlText =
  ELam () (Name "t") (Just (TLit TString))
    (ECon () (Constructor "Html") nHtml
      [EList () tHtmlNode [ECon () (Constructor "Plain") nHtmlNode [EVar () (Name "t")]]])

-- -----------------------------------------------------------------------------

nHtmlAttrValue :: Name
nHtmlAttrValue =
  Name "attrValue"

tHtmlAttrValue :: HtmlType
tHtmlAttrValue =
  TArrow (TLit TString) tAttributeValue

eHtmlAttrValue :: HtmlExpr ()
eHtmlAttrValue =
  lam (Name "t") (Just (TLit TString))
    (con (Constructor "AttributeValue") nAttributeValue [var (Name "t")])

-- -----------------------------------------------------------------------------

nHtmlBlank :: Name
nHtmlBlank =
  Name "blank"

tHtmlBlank :: HtmlType
tHtmlBlank =
  tHtml

eHtmlBlank :: HtmlExpr ()
eHtmlBlank =
  con (Constructor "Html") nHtml
    [list tHtmlNode []]

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

nStringConcat :: Name
nStringConcat =
  Name "concat"

tStringConcat :: HtmlType
tStringConcat =
  TArrow (TList (TLit TString)) (TLit TString)

eStringConcat :: HtmlExpr ()
eStringConcat =
  foreign_ nStringConcat tStringConcat
