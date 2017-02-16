{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Core.Elaborator (
    elaborate
  ) where


import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import           P

import           Projector.Core
import           Projector.Html.Data.Annotation
import           Projector.Html.Data.Prim
import qualified Projector.Html.Data.Prim as Prim
import qualified Projector.Html.Core.Library as Lib
import           Projector.Html.Data.Template


elaborate :: Template a -> HtmlExpr (Annotation a)
elaborate (Template _ mts html) =
  eTypeSigs mts (eHtml html)

eTypeSigs :: Maybe (TTypeSig a) -> (HtmlExpr (Annotation a) -> HtmlExpr (Annotation a))
eTypeSigs msigs =
  mcase msigs id $ \(TTypeSig a sigs) ->
    foldl' (\f (TId x, ty) -> f . ELam (TypeSignature a) (Name x) (Just (eType ty))) id sigs

eType :: TType a -> HtmlType
eType ty =
  case ty of
    TTVar _ (TId x) ->
      case parsePrimT x of
        Just p ->
          TLit p
        _ ->
          TVar (TypeName x)

--    TTList _ t ->
--      TList (eType t)
    -- TTApp to come eventually

eHtml :: THtml a -> HtmlExpr (Annotation a)
eHtml (THtml a nodes) =
  ECon (HtmlBlock a) (Constructor "Html") Lib.nHtml [EList (SourceAnnotation a) Lib.tHtmlNode (fmap eNode nodes)]

eNode :: TNode a -> HtmlExpr (Annotation a)
eNode node =
  case node of
    TWhiteSpace a ->
      ECon (SourceAnnotation a) (Constructor "Whitespace") Lib.nHtmlNode []
    TPlain a (TPlainText t) ->
      ECon (SourceAnnotation a) (Constructor "Raw") Lib.nHtmlNode [stringLit a t]
    TComment a (TPlainText t) ->
      ECon (SourceAnnotation a) (Constructor "Comment") Lib.nHtmlNode [stringLit a t]
    TVoidElement a tag attrs ->
      ECon (SourceAnnotation a) (Constructor "VoidElement") Lib.nHtmlNode [
          eTag tag
        , eAttrs a attrs
        ]
    TElement a tag attrs html ->
      ECon (SourceAnnotation a) (Constructor "Element") Lib.nHtmlNode [
          eTag tag
        , eAttrs a attrs
        , eHtml html
        ]
    TExprNode a expr ->
      ECon (HtmlExpression a) (Constructor "Nested") Lib.nHtmlNode [
          eExpr expr
        ]

eTag :: TTag a -> HtmlExpr (Annotation a)
eTag (TTag a t) =
  ECon (SourceAnnotation a) (Constructor "Tag") Lib.nTag [stringLit a t]

eAttrs :: a -> [TAttribute a] -> HtmlExpr (Annotation a)
eAttrs a =
  EList (SourceAnnotation a) Lib.tAttribute . fmap eAttr

eAttr :: TAttribute a -> HtmlExpr (Annotation a)
eAttr attr =
  case attr of
    TAttribute a name aval ->
      ECon (AttributeExpression a) (Constructor "Attribute") Lib.nAttribute [
          eAttrKey a name
        , eAttrVal aval
        ]
    TEmptyAttribute a name ->
      ECon (AttributeExpression a) (Constructor "Attribute") Lib.nAttribute [
          eAttrKey a name
        , eAttrVal (TQuotedAttrValue a (TIString a []))
        ]

eAttrKey :: a -> TAttrName -> HtmlExpr (Annotation a)
eAttrKey a (TAttrName n) =
  ECon (SourceAnnotation a) (Constructor "AttributeKey") Lib.nAttributeKey [stringLit a n]

eAttrVal :: TAttrValue a -> HtmlExpr (Annotation a)
eAttrVal aval =
  let mkVal a t =
        ECon  (AttributeExpression a) (Constructor "AttributeValue") Lib.nAttributeValue [t]
  in case aval of
       TQuotedAttrValue a str ->
         mkVal a (eStr str)
       TAttrExpr _ expr ->
         eExpr expr

eExpr :: TExpr a -> HtmlExpr (Annotation a)
eExpr expr =
  case expr of
    TEVar a (TId x) ->
      EVar (Variable (Name x) a) (Name x)
    TELam a bnds bdy ->
      funX a (fmap (Name . unTId) bnds) (eExpr bdy)
    TEApp a f g ->
      EApp (FunctionApplication a) (eExpr f) (eExpr g)
    TECase a e alts ->
      ECase (CaseExpression a) (eExpr e) (NE.toList (fmap eAlt alts))
    TEEach a f g ->
      ECon (SourceAnnotation a) (Constructor "Html") Lib.nHtml . pure $
        EMap (SourceAnnotation a)
          (ELam (SourceAnnotation a) (Name "x") Nothing $
            ECon (SourceAnnotation a) (Constructor "Nested") Lib.nHtmlNode
              [EApp (SourceAnnotation a) (eExpr g) (EVar (SourceAnnotation a) (Name "x"))]
            )
          (eExpr f)
    TEString _ s ->
      eStr s
    TENode a e ->
      ECon (HtmlBlock a) (Constructor "Html") Lib.nHtml [
          EList (HtmlBlock a) Lib.tHtmlNode [eNode e]
        ]

eStr :: TIString a -> HtmlExpr (Annotation a)
eStr (TIString a chunks) =
-- TODO custom annotation
  EApp
    (SourceAnnotation a)
    (fmap (const (LibraryFunction Prim.nStringConcat)) Prim.eStringConcat)
    (EList (SourceAnnotation a) (TLit TString) (fmap eChunk chunks))

eChunk :: TIChunk a -> HtmlExpr (Annotation a)
eChunk chunk =
  case chunk of
    TStringChunk a t ->
      ELit (SourceAnnotation a) (VString t)
    TExprChunk _ e ->
      eExpr e

-- curried function
funX :: a -> NonEmpty Name -> HtmlExpr (Annotation a) -> HtmlExpr (Annotation a)
funX a bnds bdy =
  foldr (\n expr -> ELam (InlineFunction n a) n Nothing expr) bdy bnds

eAlt :: TAlt a -> (Pattern (Annotation a), HtmlExpr (Annotation a))
eAlt (TAlt _ pat body) =
  (ePat pat, eExpr body)

ePat :: TPattern a -> Pattern (Annotation a)
ePat pat =
  -- TODO find something we can do with these annotations
  case pat of
    TPVar a (TId x) ->
      PVar (PatternVar (Name x) a) (Name x)
    TPCon a (TConstructor x) pats ->
      PCon (PatternCon (Name x) a) (Constructor x) (fmap ePat pats)

stringLit :: a -> Text -> HtmlExpr (Annotation a)
stringLit a =
  ELit (SourceAnnotation a) . VString
