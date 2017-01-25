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

data Backend a e = Backend {
    renderModule :: ModuleName -> Module HtmlType a -> (FilePath, Text)
  , renderExpr :: Name -> HtmlExpr a -> Text
  , predicates :: [Predicate a e]
  } deriving (Functor)

newtype Predicate a e = Predicate {
    unPredicate :: (HtmlExpr a -> PredResult e)
  } deriving (Functor)

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
