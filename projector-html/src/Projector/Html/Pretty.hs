{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Pretty (
    uglyPrintTemplate
  , templateTokens
  , testTemplate
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
        PatId _ ->
          " "
        PatLParen ->
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
        _ ->
          mempty

testTemplate :: Template ()
testTemplate =
  Template ()
    (Just (TTypeSig () ((TId "foo", (TTVar () (TId "Bar"))) :|
                        [(TId "bar", (TTVar () (TId "Baz")))])))
    (THtml () [
        TElement () (TTag () "h1") [
            TAttribute ()
              (TAttrName "display")
              (TQuotedAttrValue () (TPlainText "inline-block"))
          ] (THtml () [
           TPlain () (TPlainText "Hello, world!")
          ])
      , TExprNode () (TEApp () (TEVar () (TId "foo")) (TEVar () (TId "bar")))
      , TExprNode () (TECase () (TEVar () (TId "foo")) (
          (TAlt () (TPCon () (TConstructor "Foo") [TPVar () (TId "x")])
                   (TAltExpr () (TEVar () (TId "x"))))
          :| []))
      ])

-- -----------------------------------------------------------------------------
-- Template to tokens

templateTokens :: Template a -> [Token]
templateTokens (Template _ mts html) =
  D.toList $
     maybe mempty typeSigTokens mts
  <> htmlTokens html

typeSigTokens :: TTypeSig a -> DList (Token)
typeSigTokens (TTypeSig _ idts) =
  mconcat [
      [TypeSigsStart]
    , mconcat . L.intersperse [TypeSigsSep] . NE.toList . with idts $ \((TId tid), tty) ->
        mconcat [
            [TypeIdent tid, TypeSigSep]
          , typeTokens tty
          ]
    , [TypeSigsEnd]
    ]

typeTokens :: TType a -> DList (Token)
typeTokens ty =
  case ty of
    TTVar _ (TId t) ->
      [TypeIdent t]
--    TTList _ t ->
--      mconcat [
--          [TypeLSquare]
--        , typeTokens t
--        , [TypeRSquare]
--        ]
--    TTApp _ t1 t2 ->
--      mconcat [
--          [TypeLParen]
--        , typeTokens t1
--        , typeTokens t2
--        , [TypeRParen]
--        ]

htmlTokens :: THtml a -> DList (Token)
htmlTokens (THtml _ nodes) =
  foldMap nodeTokens nodes

nodeTokens :: TNode a -> DList (Token)
nodeTokens node =
  case node of
    TWhiteSpace _ ->
      [WhiteSpace]

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
    TQuotedAttrValue _ (TPlainText t) ->
      [AttValueQ t]
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

altsTokens :: NonEmpty (TAlt a) -> DList (Token)
altsTokens =
  mconcat . L.intersperse (D.singleton CaseSep) . NE.toList . fmap altTokens

altTokens :: TAlt a -> DList (Token)
altTokens (TAlt _ pat body) =
  mconcat [
      patTokens pat
    , [AltSep]
    , altBodyTokens body
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

altBodyTokens :: TAltBody a -> DList (Token)
altBodyTokens body =
  case body of
    TAltExpr _ expr ->
      exprTokens expr

    TAltHtml _ html ->
      htmlTokens html
