{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Data.Backend (
  -- * Backends
    BackendT (..)
  , Backend (..)
  , Predicate (..)
  , PredResult (..)
  -- * Runtime constants
  , htmlRuntime
  , htmlRuntimePrim
  , htmlRuntimeLibrary
  ) where


import           P

import           Projector.Core
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim

import           System.IO (FilePath)


data BackendT
  = Haskell
  | Purescript
  deriving (Eq, Ord, Show)

-- | Backends take typechecked modules and expressions to target
-- platform source code, with a validation pass derived from a set of
-- predicates.
data Backend a e = Backend {
    renderModule :: ModuleName -> Module HtmlType PrimT (HtmlType, a) -> Either e (FilePath, Text)
  , renderExpr :: Name -> HtmlExpr (HtmlType, a) -> Either e Text
  , predicates :: [Predicate a e]
  }

instance Functor (Backend a) where
  fmap f (Backend m e p) =
    Backend {
        renderModule = fmap (fmap (first f)) m
      , renderExpr = fmap (fmap (first f)) e
      , predicates = fmap (fmap f) p
      }

data Predicate a e
  = ExprPredicate (HtmlExpr a -> PredResult e)
  | PatPredicate (Pattern a -> PredResult e)
  deriving (Functor)

data PredResult e
  = PredError e
  | PredOk
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

htmlRuntime :: ModuleName
htmlRuntime =
  ModuleName "Projector.Html.Runtime"

htmlRuntimePrim :: ModuleName
htmlRuntimePrim =
  ModuleName "Projector.Html.Runtime.Prim"

htmlRuntimeLibrary :: ModuleName
htmlRuntimeLibrary =
  ModuleName "Projector.Html.Runtime.Library"
