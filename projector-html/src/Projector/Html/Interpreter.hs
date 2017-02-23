{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Interpreter (
    Html (..)
  , HtmlNode (..)
  , Attribute (..)
  ---
  , InterpretError (..)
  , interpret
  ) where

import           Data.Map (Map)
import qualified Data.Text as T

import           P

import           Projector.Core.Eval
import           Projector.Core.Syntax
import           Projector.Core.Type
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim

newtype Html =
  Html [HtmlNode]
  deriving (Eq, Monoid, Show)

data HtmlNode =
    Plain !Text
  | Raw !Text
  | Whitespace !Text
  | Comment !Text
  | Element !Text ![Attribute] !Html
  | VoidElement !Text ![Attribute]
  deriving (Eq, Show)

data Attribute =
  Attribute !Text !Text
  deriving (Eq, Show)

data InterpretError a =
    InterpretInvalidExpression (HtmlExpr a)
  deriving (Eq, Show)

interpret :: Map Name (HtmlExpr a) -> HtmlExpr a -> Either (InterpretError a) Html
interpret bnds =
  interpret' . nf bnds

interpret' :: HtmlExpr a -> Either (InterpretError a) Html
interpret' e =
  case e of
    ECon a c tn es ->
      case (c, es) of
        (Constructor "Plain", [ELit _ (VString t)]) ->
           pure . html $ Plain t
        (Constructor "Raw", [ELit _ (VString t)]) ->
           pure . html $ Raw t
        (Constructor "Whitespace", []) ->
           pure . html $ Whitespace " "
        (Constructor "Comment", [ELit _ (VString t)]) ->
           pure . html $ Comment t
        (Constructor "Nested", [body]) ->
           interpret' body
        (Constructor "Element", [ECon _ (Constructor "Tag") _ [(ELit _ (VString t))], EList _ attrs, body]) -> do
           fmap html $ Element
             <$> pure t
             <*> mapM attr attrs
             <*> interpret' body
        (Constructor "VoidElement", [ECon _ (Constructor "Tag") _ [(ELit _ (VString t))], EList _ attrs]) -> do
           fmap html $ VoidElement
             <$> pure t
             <*> mapM attr attrs
        (Constructor "Html", [EList _ nodes]) ->
          fmap mconcat . mapM interpret' $ nodes
        c ->
          Left $ InterpretInvalidExpression e
    ECase _ _ _ ->
      -- FIX Not implemented, but this lets us test it without failures
      pure . html $ Plain "TODO"
    EApp _ (EVar _ (Name "text")) v ->
      html . Plain
        <$> value v
    EApp _ _ _ ->
      Left $ InterpretInvalidExpression e
    ELam _ _ _ _ ->
      Left $ InterpretInvalidExpression e
    EList _ _ ->
      Left $ InterpretInvalidExpression e
    EMap _ _ _ ->
      Left $ InterpretInvalidExpression e
    ELit _ _ ->
      Left $ InterpretInvalidExpression e
    EVar _ _ ->
      Left $ InterpretInvalidExpression e
    EForeign _ _ _ ->
      Left $ InterpretInvalidExpression e

-- | Guaranteed to return text, not html
value :: HtmlExpr a -> Either (InterpretError a) Text
value e =
  case e of
    ELit _ (VString v) ->
      pure v
    EApp _ (EForeign _ (Name "concat") _) (EList _ as) ->
      fmap mconcat . mapM value $ as
    ECase _ _ _ ->
      -- FIX Not implemented, but this lets us test it without failures
      pure "TODO"
    _ ->
      Left $ InterpretInvalidExpression e

attr :: HtmlExpr a -> Either (InterpretError a) Attribute
attr e =
  case e of
    ECon _ (Constructor "Attribute") _ [ECon _ (Constructor "AttributeKey") _ [ELit _ (VString k)], ECon _ (Constructor "AttributeValue") _ [v]] ->
      Attribute k <$> value v
    _ ->
      Left $ InterpretInvalidExpression e

-- FIX https://github.com/ambiata/projector/issues/154
html :: HtmlNode -> Html
html =
  Html . pure
