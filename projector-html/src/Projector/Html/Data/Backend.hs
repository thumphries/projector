{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Projector.Html.Data.Backend (
  -- * Backends
    Backend (..)
  , Predicate (..)
  , PredResult (..)
  ) where


import           P

import           Projector.Core
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim

import           System.IO (FilePath)


-- | Backends take typechecked modules and expressions to target
-- platform source code, with a validation pass derived from a set of
-- predicates.
data Backend a e = Backend {
    renderModule :: HtmlDecls -> ModuleName -> Module HtmlType PrimT (HtmlType, a) -> Either e (FilePath, Text)
  , renderExpr :: HtmlDecls -> Name -> HtmlExpr (HtmlType, a) -> Either e Text
  , predicates :: [Predicate e]
  }

instance Functor (Backend a) where
  fmap f (Backend m e p) =
    Backend {
        renderModule = fmap (fmap (fmap (first f))) m
      , renderExpr = fmap (fmap (fmap (first f))) e
      , predicates = fmap (fmap f) p
      }

data Predicate e
  = ExprPredicate (forall a. HtmlExpr a -> PredResult e)
  | PatPredicate (forall a. Pattern a -> PredResult e)
  deriving (Functor)

data PredResult e
  = PredError e
  | PredOk
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
