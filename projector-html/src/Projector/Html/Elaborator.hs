{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Elaborator (
    elaborate
  ) where


import qualified Data.List.NonEmpty as NE

import           P

import           Projector.Core
import           Projector.Html.Core.Prim
import qualified Projector.Html.Core.Library as Lib
import           Projector.Html.Data.Template


elaborate :: Template a -> HtmlExpr a
elaborate (Template _ mts html) =
  eTypeSigs mts (eHtml html)

eTypeSigs :: Maybe (TTypeSig a) -> (HtmlExpr a -> HtmlExpr a)
eTypeSigs msigs =
  mcase msigs id $ \(TTypeSig a sigs) ->
    foldl' (\f (TId x, ty) -> f . ELam a (Name x) (eType ty)) id sigs

eType :: TType a -> HtmlType
eType ty =
  case ty of
    TTVar _ (TId x) ->
      TVar (TypeName x)

--    TTList _ t ->
--      TList (eType t)
    -- TTApp to come eventually

eHtml :: THtml a -> HtmlExpr a
eHtml (THtml a nodes) =
  ECon a (Constructor "Html") Lib.nHtml [EList a Lib.tHtmlNode (fmap eNode nodes)]

eNode :: TNode a -> HtmlExpr a
eNode node =
  case node of
    TWhiteSpace a ->
      ECon a (Constructor "WhiteSpace") Lib.nHtml []
    TPlain a (TPlainText t) ->
      ECon a (Constructor "Plain") Lib.nHtml [stringLit a t]
    TComment a (TPlainText t) ->
      ECon a (Constructor "Comment") Lib.nHtml [stringLit a t]
    TVoidElement a tag attrs ->
      ECon a (Constructor "VoidElement") Lib.nHtml [
          eTag tag
        , eAttrs a attrs
        ]
    TElement a tag attrs html ->
      ECon a (Constructor "Element") Lib.nHtml [
          eTag tag
        , eAttrs a attrs
        , eHtml html
        ]
    TExprNode _ expr ->
      eExpr expr

eTag :: TTag a -> HtmlExpr a
eTag (TTag a t) =
  ECon a (Constructor "Tag") Lib.nTag [stringLit a t]

eAttrs :: a -> [TAttribute a] -> HtmlExpr a
eAttrs a =
  EList a Lib.tAttribute . fmap eAttr

eAttr :: TAttribute a -> HtmlExpr a
eAttr attr =
  case attr of
    TAttribute a name aval ->
      ECon a (Constructor "Attribute") Lib.nAttribute [
          eAttrKey a name
        , eAttrVal aval
        ]
    TEmptyAttribute a name ->
      ECon a (Constructor "Attribute") Lib.nAttribute [
          eAttrKey a name
        , eAttrVal (TQuotedAttrValue a (TPlainText ""))
        ]

eAttrKey :: a -> TAttrName -> HtmlExpr a
eAttrKey a (TAttrName n) =
  ECon a (Constructor "AttributeKey") Lib.nAttributeKey [stringLit a n]

eAttrVal :: TAttrValue a -> HtmlExpr a
eAttrVal aval =
  let mkVal a t =
        ECon  a (Constructor "AttributeValue") Lib.nAttributeValue [t]
  in case aval of
       TQuotedAttrValue a (TPlainText t) ->
         mkVal a (stringLit a t)
       TUnquotedAttrValue a (TPlainText t) ->
         mkVal a (stringLit a t)
         -- TODO don't have a constructor in Lib, are these equivalent in HTML?
       TAttrExpr a expr ->
         mkVal a (eExpr expr)

eExpr :: TExpr a -> HtmlExpr a
eExpr expr =
  case expr of
    TEVar a (TId x) ->
      EVar a (Name x)
    TEApp a f g ->
      EApp a (eExpr f) (eExpr g)
    TECase a e alts ->
      ECase a (eExpr e) (NE.toList (fmap eAlt alts))

eAlt :: TAlt a -> (Pattern a, HtmlExpr a)
eAlt (TAlt _ pat body) =
  (ePat pat, eAltBody body)

ePat :: TPattern a -> Pattern a
ePat pat =
  case pat of
    TPVar a (TId x) ->
      PVar a (Name x)
    TPCon a (TConstructor x) pats ->
      PCon a (Constructor x) (fmap ePat pats)

eAltBody :: TAltBody a -> HtmlExpr a
eAltBody body =
  case body of
    TAltExpr _ expr ->
      eExpr expr
    TAltHtml _ html ->
      eHtml html

stringLit :: a -> Text -> HtmlExpr a
stringLit a =
  ELit a . VString
