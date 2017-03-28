{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Pretty (
    uglyPrintTemplate
  , templateTokens
  ) where


import           Data.DList (DList)
import qualified Data.DList as D
import qualified Data.List as L
import           Data.List.NonEmpty  (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import           P

import           Projector.Html.Data.Template
import           Projector.Html.Data.Token


uglyPrintTemplate :: Template a -> Text
uglyPrintTemplate =
  go . templateTokens
  where
    go :: [Token] -> Text
    go ls =
      case ls of
        (x:xs) ->
          pre x <> renderToken x <> post x <> go xs
        _ -> -- GHC 7.10 bug
          mempty
    pre x =
      case x of
        TypeSigsEnd ->
          " "
        TypeSigSep ->
          " "
        TypeIdent _ ->
          " "
        AttName _ ->
          " "
        ExprIdent _ ->
          " "
        CaseStart ->
          " "
        CaseOf ->
          " "
        ExprLParen ->
          " "
        ExprEnd ->
          " "
        AltSep ->
          " "
        LamBody ->
          " "
        PatId _ ->
          " "
        PatLParen ->
          " "
        ExprHole ->
          " "
        _ ->
          mempty
    post x =
      case x of
        TypeSigsEnd ->
          "\n"
        TypeSigsSep ->
          "\n"
        ExprStart ->
          " "
        LamStart ->
          " "
        _ ->
          mempty

-- -----------------------------------------------------------------------------
-- Template to tokens

templateTokens :: Template a -> [Token]
templateTokens (Template _ mts expr) =
  D.toList $
     maybe mempty typeSigTokens mts
  <> exprTokens expr

typeSigTokens :: TTypeSig a -> DList (Token)
typeSigTokens (TTypeSig _ idts ty) =
  mconcat [
      [TypeSigsStart]
    , mconcat . L.intersperse [TypeSigsSep] $
        with idts $ \((TId tid), tty) ->
          mconcat [
              [TypeIdent tid, TypeSigSep]
            , typeTokens tty
            ]
    , if null idts then mempty else [TypeSigsSep]
    , typeTokens ty
    , [TypeSigsEnd]
    ]

typeTokens :: TType a -> DList (Token)
typeTokens ty =
  case ty of
    TTVar _ (TId t) ->
      [TypeIdent t]
    TTApp _ t1 t2 ->
      mconcat [
          [TypeLParen]
        , typeTokens t1
        , typeTokens t2
        , [TypeRParen]
        ]

htmlTokens :: THtml a -> DList (Token)
htmlTokens html =
  case html of
    (THtml _ nodes) ->
      foldMap nodeTokens nodes

nodeTokens :: TNode a -> DList (Token)
nodeTokens node =
  case node of
    TWhiteSpace _ x ->
      [WhiteSpace x]

    TNewline _ ->
      [HtmlText "\n"]

    TElement _ (TTag _ tag) attrs html ->
      mconcat [
          [TagOpen, TagIdent tag]
        , mconcat (fmap attrTokens attrs)
        , [TagClose]
        , htmlTokens html
        , [TagCloseOpen, TagIdent tag, TagClose]
        ]

    TVoidElement _ (TTag _ tag) attrs ->
      mconcat [
          [TagOpen, TagIdent tag]
        , mconcat (fmap attrTokens attrs)
        , [TagSelfClose]
        ]

    TComment _ (TPlainText t) ->
      [HtmlComment t]

    TPlain _ (TPlainText t) ->
      [HtmlText t]

    TExprNode _ expr ->
      mconcat [
          [ExprStart]
        , exprTokens expr
        , [ExprEnd]
        ]
    (THtmlWS _ nodes) ->
      mconcat [
          [ExprStartWS]
        , foldMap nodeTokens nodes
        , [ExprEndWS]
        ]


attrTokens :: TAttribute a -> DList (Token)
attrTokens attr =
  case attr of
    TAttribute _ (TAttrName an) aval ->
      mconcat [
          [AttName an, AttSep]
        , attrValueTokens aval
        ]
    TEmptyAttribute _ (TAttrName an) ->
      [AttName an]

attrValueTokens :: TAttrValue a -> DList (Token)
attrValueTokens aval =
  case aval of
    TQuotedAttrValue _ s ->
      stringTokens s
    TAttrExpr _ expr ->
      mconcat [
          [ExprStart]
        , exprTokens expr
        , [ExprEnd]
        ]

exprTokens :: TExpr a -> DList (Token)
exprTokens expr =
  case expr of
    TEVar _ (TId x) ->
      [ExprIdent x]
    TELam _ ids e ->
      mconcat [
          [ExprLParen, LamStart]
        , (fmap (\(TId x) -> ExprIdent x) (D.fromList (toList ids)))
        , [LamBody]
        , exprTokens e
        , [ExprRParen]
        ]
    TEApp _ f g ->
      mconcat [
          [ExprLParen]
        , exprTokens f
        , exprTokens g
        , [ExprRParen]
        ]
    TECase _ e alts ->
      mconcat [
          [ExprLParen, CaseStart]
        , exprTokens e
        , [CaseOf]
        , altsTokens alts
        , [ExprRParen]
        ]
    TEEach _ f g ->
      mconcat [
          [ExprLParen, Each]
        , exprTokens f
        , exprTokens g
        , [ExprRParen]
        ]
    TENode _ h ->
      mconcat [
          [ExprLParen]
        , htmlTokens h
        , [ExprRParen]
        ]
    TEString _ s ->
      stringTokens s
    TEList _ es ->
      mconcat [
          [ListStart]
        , mconcat (L.intersperse [ListSep] (fmap exprTokens es))
        , [ListEnd]
        ]
    TEPrj _ e (TId fn) ->
      mconcat [
          [ExprLParen]
        , exprTokens e
        , [ExprDot, ExprIdent fn, ExprRParen]
        ]
    TEHole _ ->
      [ExprHole]

stringTokens :: TIString a -> DList (Token)
stringTokens (TIString _ ss) =
  mconcat [
      [StringStart]
    , mconcat (fmap chunkTokens ss)
    , [StringEnd]
    ]

chunkTokens :: TIChunk a -> DList (Token)
chunkTokens chunk =
  case chunk of
    TStringChunk _ t ->
      [StringChunk t]
    TExprChunk _ e ->
      mconcat [
         [ExprStart]
       , exprTokens e
       , [ExprEnd]
       ]

altsTokens :: NonEmpty (TAlt a) -> DList (Token)
altsTokens =
  mconcat . L.intersperse (D.singleton CaseSep) . NE.toList . fmap altTokens

altTokens :: TAlt a -> DList (Token)
altTokens (TAlt _ pat body) =
  mconcat [
      patTokens pat
    , [AltSep]
    , exprTokens body
    ]

patTokens :: TPattern a -> DList (Token)
patTokens pat =
  case pat of
    TPVar _ (TId x) ->
      [PatId x]
    TPCon _ (TConstructor con) pats ->
      mconcat [
          [PatLParen]
        , [PatCon con]
        , mconcat (fmap patTokens pats)
        , [PatRParen]
        ]
