{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Backend.Data (
    ModuleName (..)
  , Module (..)
  , Imports (..)
  , htmlRuntime
  , htmlRuntimePrim
  , htmlRuntimeLibrary
  ) where


import           Data.Map.Strict  (Map)

import           P

import           Projector.Core  (Name)
import           Projector.Html.Core.Prim (HtmlDecls, HtmlExpr, HtmlType)


newtype ModuleName = ModuleName { unModuleName :: Text }
  deriving (Eq, Ord, Show)

data Module = Module {
    moduleTypes :: HtmlDecls
  , moduleImports :: Map ModuleName Imports
  , moduleExprs :: Map Name (HtmlType, HtmlExpr)
  } deriving (Eq, Ord, Show)

instance Monoid Module where
  mempty = Module mempty mempty mempty
  mappend (Module a b c) (Module d e f) = Module {
      moduleTypes = a <> d
    , moduleImports = b <> e
    , moduleExprs = c <> f
    }

data Imports
  = OpenImport
  | OnlyImport [Name]
  deriving (Eq, Ord, Show)

htmlRuntime :: ModuleName
htmlRuntime =
  ModuleName "Projector.Html.Runtime"

htmlRuntimePrim :: ModuleName
htmlRuntimePrim =
  ModuleName "Projector.Html.Runtime.Prim"

htmlRuntimeLibrary :: ModuleName
htmlRuntimeLibrary =
  ModuleName "Projector.Html.Runtime.Library"
