{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Core.Elaborator (
    elaborate
  , elaborateSig
  ) where


import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import           P

import           Projector.Core
import           Projector.Html.Data.Annotation
import           Projector.Html.Data.Prim
import qualified Projector.Html.Core.Prim as Prim
import qualified Projector.Html.Core.Library as Lib
import           Projector.Html.Data.Template


elaborate :: Template a -> HtmlExpr (Annotation a)
elaborate (Template _ mts html) =
  eTypeSigs mts (eHtml html)

elaborateSig :: Template a -> Maybe HtmlType
elaborateSig (Template _ mts _) = do
  TTypeSig _ sigs ty <- mts
  Just (foldr (\(_, u) t -> TArrow (eType u) t) (eType ty) sigs)

eTypeSigs :: Maybe (TTypeSig a) -> (HtmlExpr (Annotation a) -> HtmlExpr (Annotation a))
eTypeSigs mmsigs =
  mcase mmsigs id $ \(TTypeSig a sigs _ty) ->
    -- We ignore the RHS of the type signature here, though it produces a constraint elsewhere.
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

    TTApp _ f x ->
      TArrow (eType f) (eType x)

eHtml :: THtml a -> HtmlExpr (Annotation a)
eHtml (THtml a nodes) =
  ECon (HtmlBlock a) (Constructor "Nested") Lib.nHtml [EList (SourceAnnotation a) (fmap eNode nodes)]

eNode :: TNode a -> HtmlExpr (Annotation a)
eNode node =
  case node of
    TWhiteSpace a ->
      ECon (SourceAnnotation a) (Constructor "Whitespace") Lib.nHtml []
    TPlain a (TPlainText t) ->
      ECon (SourceAnnotation a) (Constructor "Raw") Lib.nHtml [stringLit a t]
    TComment a (TPlainText t) ->
      ECon (SourceAnnotation a) (Constructor "Comment") Lib.nHtml [stringLit a t]
    TVoidElement a tag attrs ->
      ECon (SourceAnnotation a) (Constructor "VoidElement") Lib.nHtml [
          eTag tag
        , eAttrs a attrs
        ]
    TElement a tag attrs html ->
      ECon (SourceAnnotation a) (Constructor "Element") Lib.nHtml [
          eTag tag
        , eAttrs a attrs
        , eHtml html
        ]
    TExprNode a expr ->
      ECon (HtmlExpression a) (Constructor "Nested") Lib.nHtml [
          EList (SourceAnnotation a) [eExpr expr]
        ]

eTag :: TTag a -> HtmlExpr (Annotation a)
eTag (TTag a t) =
  ECon (SourceAnnotation a) (Constructor "Tag") Lib.nTag [stringLit a t]

eAttrs :: a -> [TAttribute a] -> HtmlExpr (Annotation a)
eAttrs a =
  EList (SourceAnnotation a) . fmap eAttr

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
      ECon (SourceAnnotation a) (Constructor "Nested") Lib.nHtml . pure $
        EMap (SourceAnnotation a) (eExpr g) (eExpr f)
    TEString _ s ->
      eStr s
    TENode a e ->
      ECon (HtmlBlock a) (Constructor "Nested") Lib.nHtml [
          EList (HtmlBlock a) [eNode e]
        ]
    TEList a es ->
      EList (ListLiteral a) (fmap eExpr es)
    TEPrj a e (TId fn) ->
      EPrj (RecordProjection a (FieldName fn)) (eExpr e) (FieldName fn)
    TEHole a ->
      EHole (TypedHole a)


eStr :: TIString a -> HtmlExpr (Annotation a)
eStr (TIString a chunks) =
-- TODO custom annotation
  EApp
    (SourceAnnotation a)
    (fmap (const (LibraryFunction Prim.nStringConcat)) Prim.eStringConcat)
    (EList (SourceAnnotation a) (fmap eChunk chunks))

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
