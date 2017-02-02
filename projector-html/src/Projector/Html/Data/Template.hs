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
  , TLit (..)
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
  = TQuotedAttrValue a TPlainText
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
    TQuotedAttrValue _ t -> TQuotedAttrValue (f expr) t
    TAttrExpr _ e -> TAttrExpr (f expr) (extend (const (f expr)) e)

data TExpr a
  = TEVar a TId
  | TELam a (NonEmpty TId) (TExpr a)
  | TEApp a (TExpr a) (TExpr a)
  | TECase a (TExpr a) (NonEmpty (TAlt a))
  | TELit a (TLit a)
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
      TELit a _ ->
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
      TELit _ a ->
        TELit (f expr) (extend (const (f expr)) a)

data TLit a
  = TLString a Text
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Comonad TLit where
  extract (TLString a _) =
    a
  extend f a@(TLString _ s) =
    TLString (f a) s

data TAlt a
  = TAlt a (TPattern a) (TAltBody a)
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Comonad TAlt where
  extract (TAlt a _ _) =
    a
  extend f a@(TAlt _ p b) =
    TAlt (f a) (extend (const (f a)) p) (extend (const (f a)) b)

-- FIX only Element / VoidElements are valid here
-- should make this correct by construction
data TAltBody a
  = TAltExpr a (TExpr a)
--  | TAltElement a TTag [TAttribute a] (THtml a)
--  | TAltVoidElement a TTag [TAttribute a]
  | TAltHtml a (THtml a)
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Comonad TAltBody where
  extract body =
    case body of
      TAltExpr a _ ->
        a
      TAltHtml a _ ->
        a
  extend f body =
    case body of
      TAltExpr _ e ->
        TAltExpr (f body) (extend (const (f body)) e)
      TAltHtml _ h ->
        TAltHtml (f body) (extend (const (f body)) h)

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
