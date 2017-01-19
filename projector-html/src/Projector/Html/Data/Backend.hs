{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Data.Backend (
  -- * Backends
    BackendT (..)
  , Backend (..)
  -- * Runtime constants
  , htmlRuntime
  , htmlRuntimePrim
  , htmlRuntimeLibrary
  ) where


import           P

import           Projector.Core (Name)
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim

import           System.IO (FilePath)


data BackendT
  = Haskell
  | Purescript
  deriving (Eq, Ord, Show)

data Backend a = Backend {
    renderModule :: ModuleName -> Module HtmlType a -> (FilePath, Text)
  , renderExpr :: Name -> HtmlExpr a -> Text
  }

htmlRuntime :: ModuleName
htmlRuntime =
  ModuleName "Projector.Html.Runtime"

htmlRuntimePrim :: ModuleName
htmlRuntimePrim =
  ModuleName "Projector.Html.Runtime.Prim"

htmlRuntimeLibrary :: ModuleName
htmlRuntimeLibrary =
  ModuleName "Projector.Html.Runtime.Library"
