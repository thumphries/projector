{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Data.Template (
    Template (..)
  -- * AST internals
  -- ** Type signatures
  , TTypeSig (..)
  , TType (..)
  -- ** Html
  , THtml (..)
  , TNode (..)
  , TAttribute (..)
  , TAttrValue (..)
  -- ** Expressions
  , TExpr (..)
  , TAlt (..)
  , TAltBody (..)
  , TPattern (..)
  -- ** Strings
  , TId (..)
  , TPlainText (..)
  , TAttrName (..)
  , TConstructor (..)
  , TTag (..)
  ) where


import           Data.List.NonEmpty  (NonEmpty(..))

import           P


data Template a
  = Template a (Maybe (TTypeSig a)) (THtml a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data TTypeSig a
  = TTypeSig a (NonEmpty (TId, TType a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data TType a
  = TTVar a TId
  | TTApp a (TType a) (TType a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data THtml a
  = THtml a [TNode a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data TNode a
  = TElement a TTag [TAttribute a] (THtml a)
  | TVoidElement a TTag [TAttribute a]
  | TComment a TPlainText
  | TPlain a TPlainText
  | TWhiteSpace a
  | TExprNode a (TExpr a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data TAttribute a
  = TAttribute a TAttrName (TAttrValue a)
  | TEmptyAttribute a TAttrName
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data TAttrValue a
  = TQuotedAttrValue a TPlainText
  | TUnquotedAttrValue a TPlainText
  | TAttrExpr a (TExpr a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data TExpr a
  = TEVar a TId
  | TEApp a (TExpr a) (TExpr a)
  | TECase a (TExpr a) (NonEmpty (TAlt a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data TAlt a
  = TAlt a (TPattern a) (TAltBody a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- FIX only Element / VoidElements are valid here
-- should make this correct by construction
data TAltBody a
  = TAltExpr a (TExpr a)
  | TAltHtml a (THtml a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data TPattern a
  = TPVar a TId
  | TPCon a TConstructor [TPattern a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype TId = TId { unTId :: Text }
  deriving (Eq, Ord, Show)

newtype TPlainText = TPlainText { unTPlainText :: Text }
  deriving (Eq, Ord, Show)

newtype TAttrName = TAttrName { unTAttrName :: Text }
  deriving (Eq, Ord, Show)

newtype TConstructor = TConstructor { unTConstructor :: Text }
  deriving (Eq, Ord, Show)

newtype TTag = TTag { unTTag :: Text }
  deriving (Eq, Ord, Show)
