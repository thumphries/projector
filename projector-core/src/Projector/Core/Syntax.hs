{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Projector.Core.Syntax (
    Expr (..)
  , lam
  ) where


import           Bound (Scope (..),  (>>>=), abstract1)

import           P

import           Prelude.Extras (Eq1, Ord1, Show1)

import           Projector.Core.Type (Type (..), Ground (..))


-- | The type of Projector expressions.
--
-- The first type parameter refers to the type of literal.
--
-- This uses the Bound style, where an AST is parameterised by the type of names.
-- The Functor, Applicative, Monad, Foldable and Traversable instances operate
-- on all the free variables in the syntax tree.
data Expr l a
  = ELit (Value l)
  | EVar a
  | ELam (Type l) (Scope () (Expr l) a)
  | EApp (Expr l a) (Expr l a)
  deriving (Functor, Foldable, Traversable)

-- Default instances for various functor classes (Eq1, Show1, etc)
--
-- These are also defined in Data.Functor.Classes in transformers,
-- but only in newer versions >= 5.0. So, let's use the versions in
-- prelude-extras, which is already a transitive dependency.
--
-- We need these to achieve 'Eq' and 'Show' on 'Expr'.
instance (Eq l, Eq (Value l)) => Eq1 (Expr l)
deriving instance (Eq l, Eq (Value l), Eq a) => Eq (Expr l a)

instance (Ord l, Ord (Value l)) => Ord1 (Expr l)
deriving instance (Ord l, Ord (Value l), Ord a) => Ord (Expr l a)

instance (Show l, Show (Value l)) => Show1 (Expr l)
deriving instance (Show l, Show (Value l), Show a) => Show (Expr l a)


instance Applicative (Expr l) where
  pure = EVar
  (<*>) = ap

instance Monad (Expr l) where
  return = pure
  a >>= f = case a of
    ELit l ->
      ELit l

    EVar x ->
      f x

    ELam t x ->
      -- (>>>=) lifts bind operations over the Scope monad transformer (and family)
      -- (>>>=) :: (Monad f, Bound t) => t f a -> (a -> f c) -> t f c
      -- (>>>=) :: Scope () Expr a -> (a -> Expr c) -> Scope () Expr c
      -- x :: Scope () Expr a, f :: a -> Expr b
      -- i.e. it lets us walk under binders and apply f to all the free variables in the body.
      ELam t (x >>>= f )

    EApp x y ->
      EApp (x >>= f) (y >>= f)


-- | Construct a lambda abstraction from a name, a type, and an expression.
lam :: Eq a => a -> Type l -> Expr l a -> Expr l a
lam v t b =
  -- abstract1 constructs a scope from a name and an expr
  -- abstract1 :: (Eq a, Monad f) => a -> f a -> Scope () f a
  -- abstract1 :: a -> Expr l a -> Scope () (Expr l) a
  ELam t (abstract1 v b)
