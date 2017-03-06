{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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
  , setTExprAnnotation
  , TAlt (..)
  , TPattern (..)
  , setTPatAnnotation
  , TIString (..)
  , TIChunk (..)
  -- ** Strings
  , TId (..)
  , TPlainText (..)
  , TAttrName (..)
  , TConstructor (..)
  , TTag (..)
  ) where


import           Control.Comonad  (Comonad(..))

import           Data.Data (Data, Typeable)
import           Data.List.NonEmpty  (NonEmpty(..))

import           GHC.Generics (Generic)

import           P


data Template a
  = Template a (Maybe (TTypeSig a)) (THtml a)
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Comonad Template where
  extract (Template a _ _) =
    a
  extend f t@(Template _ g h) =
    Template (f t) (fmap (extend (const (f t))) g) (extend (const (f t)) h)

data TTypeSig a
  -- TODO fix location info here, should be per sig
  = TTypeSig a (NonEmpty (TId, TType a))
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Comonad TTypeSig where
  extract (TTypeSig a _) =
    a
  extend f ts@(TTypeSig _ tss) =
    TTypeSig (f ts) (fmap (fmap (extend (const (f ts)))) tss)

data TType a
  = TTVar a TId
--  | TTList a (TType a)
--  | TTApp a (TType a) (TType a)
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Comonad TType where
  extract ty =
    case ty of
      TTVar a _ ->
        a
--      TTApp a _ _ ->
--        a
--      TTList a _ ->
--        a
  extend f ty =
    case ty of
      TTVar _ x ->
        TTVar (f ty) x
--      TTApp _ t1 t2 ->
--        TTApp (f ty) (extend f t1) (extend f t2)
--      TTList _ t ->
--        TTList (f ty) (extend f t)

data THtml a
  = THtml a [TNode a]
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Comonad THtml where
  extract (THtml a _) =
    a
  extend f h@(THtml _ nodes) =
    THtml (f h) (fmap (extend (const (f h))) nodes)


data TNode a
  = TElement a (TTag a) [TAttribute a] (THtml a)
  | TVoidElement a (TTag a) [TAttribute a]
  | TComment a TPlainText
  | TPlain a TPlainText
  | TWhiteSpace a
  | TExprNode a (TExpr a)
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Comonad TNode where
  extract node =
    case node of
      TElement a _ _ _ ->
        a
      TVoidElement a _ _ ->
        a
      TComment a _ ->
        a
      TWhiteSpace a ->
        a
      TExprNode a _ ->
        a
      TPlain a _ ->
        a
  extend f node =
    case node of
      TElement _ t a h ->
        TElement
          (f node)
          (extend (const (f node)) t)
          (fmap (extend (const (f node))) a)
          (extend (const (f node)) h)
      TVoidElement _ t a ->
        TVoidElement (f node) (extend (const (f node)) t) (fmap (extend (const (f node))) a)
      TComment _ t ->
        TComment (f node) t
      TWhiteSpace _ ->
        TWhiteSpace (f node)
      TExprNode _ e ->
        TExprNode (f node) (extend (const (f node)) e)
      TPlain _ t ->
        TPlain (f node) t


data TAttribute a
  = TAttribute a TAttrName (TAttrValue a)
  | TEmptyAttribute a TAttrName
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Comonad TAttribute where
  extract attr =
    case attr of
      TAttribute a _ _ ->
        a
      TEmptyAttribute a _ ->
        a
  extend f attr =
    case attr of
      TAttribute _ n v ->
        TAttribute (f attr) n (extend (const (f attr)) v)
      TEmptyAttribute _ n ->
        TEmptyAttribute (f attr) n

data TAttrValue a
  = TQuotedAttrValue a (TIString a)
  | TAttrExpr a (TExpr a)
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Comonad TAttrValue where
  extract val =
    case val of
      TQuotedAttrValue a _ ->
        a
      TAttrExpr a _ ->
        a
  extend f expr = case expr of
    TQuotedAttrValue _ t -> TQuotedAttrValue (f expr) (extend (const (f expr)) t)
    TAttrExpr _ e -> TAttrExpr (f expr) (extend (const (f expr)) e)

data TExpr a
  = TEVar a TId
  | TELam a (NonEmpty TId) (TExpr a)
  | TEApp a (TExpr a) (TExpr a)
  | TECase a (TExpr a) (NonEmpty (TAlt a))
  | TEEach a (TExpr a) (TExpr a)
  | TENode a (TNode a)
  | TEString a (TIString a)
  | TEList a [TExpr a]
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Comonad TExpr where
  extract expr =
    case expr of
      TEVar a _ ->
        a
      TELam a _ _ ->
        a
      TEApp a _ _ ->
        a
      TECase a _ _ ->
        a
      TEEach a _ _ ->
        a
      TENode a _ ->
        a
      TEString a _ ->
        a
      TEList a _ ->
        a
  extend f expr =
    case expr of
      TEVar _ a ->
        TEVar (f expr) a
      TELam _ ids e ->
        TELam (f expr) ids (extend f e)
      TEApp _ e1 e2 ->
        TEApp (f expr) (extend f e1) (extend f e2)
      TECase _ e alts ->
        TECase (f expr) (extend f e) (fmap (extend (const (f expr))) alts)
      TEEach _ e1 e2 ->
        TEEach (f expr) (extend f e1) (extend f e2)
      TENode _ a ->
        TENode (f expr) (extend (const (f expr)) a)
      TEString _ s ->
        TEString (f expr) (extend (const (f expr)) s)
      TEList _ es ->
        TEList (f expr) (fmap (extend f) es)

setTExprAnnotation :: a -> TExpr a -> TExpr a
setTExprAnnotation a expr =
  case expr of
    TEVar _ b ->
      TEVar a b
    TELam _ b c ->
      TELam a b c
    TEApp _ b c ->
      TEApp a b c
    TECase _ b c ->
      TECase a b c
    TEEach _ b c ->
      TEEach a b c
    TENode _ b ->
      TENode a b
    TEString _ b ->
      TEString a b
    TEList _ b ->
      TEList a b

data TIString a = TIString a [TIChunk a]
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Comonad TIString where
  extract (TIString a _) = a
  extend f str =
    case str of
      TIString _ ss ->
        TIString (f str) (fmap (extend (const (f str))) ss)

data TIChunk a
  = TStringChunk a Text
  | TExprChunk a (TExpr a)
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Comonad TIChunk where
  extract chunk =
    case chunk of
      TStringChunk a _ ->
        a
      TExprChunk a _ ->
        a
  extend f chunk =
    case chunk of
      TStringChunk _ t ->
        TStringChunk (f chunk) t
      TExprChunk _ e ->
        TExprChunk (f chunk) (extend (const (f chunk)) e)

data TAlt a
  = TAlt a (TPattern a) (TExpr a)
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Comonad TAlt where
  extract (TAlt a _ _) =
    a
  extend f a@(TAlt _ p b) =
    TAlt (f a) (extend (const (f a)) p) (extend (const (f a)) b)

data TPattern a
  = TPVar a TId
  | TPCon a TConstructor [TPattern a]
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Comonad TPattern where
  extract pat =
    case pat of
      TPVar a _ ->
        a
      TPCon a _ _ ->
        a
  extend f pat =
    case pat of
      (TPVar _ a) ->
        TPVar (f pat) a
      TPCon _ a b ->
        TPCon (f pat) a (fmap (extend f) b)

setTPatAnnotation :: a -> TPattern a -> TPattern a
setTPatAnnotation a pat =
  case pat of
    TPVar _ b ->
      TPVar a b
    TPCon _ b c ->
      TPCon a b c

data TTag a = TTag a Text
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Comonad TTag where
  extract (TTag a _) =
    a
  extend f tag =
    case tag of
      TTag _ t ->
        TTag (f tag) t

newtype TId = TId { unTId :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype TPlainText = TPlainText { unTPlainText :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype TAttrName = TAttrName { unTAttrName :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype TConstructor = TConstructor { unTConstructor :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
