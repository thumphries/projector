{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Backend.Data (
    ModuleName (..)
  , Module (..)
  , emptyModule
  ) where


import           Data.Map.Strict  (Map)

import           P

import           Projector.Core  (Name)
import           Projector.Html.Core.Prim (HtmlDecls, HtmlExpr, HtmlType)


newtype ModuleName = ModuleName { unModuleName :: Text }
  deriving (Eq, Ord, Show)

data Module = Module {
    moduleTypes :: HtmlDecls
  , moduleImports :: Map ModuleName [Name]
  , moduleExprs :: Map Name (HtmlType, HtmlExpr)
  } deriving (Eq, Ord, Show)

emptyModule :: Module
emptyModule = Module {
    moduleTypes = mempty
  , moduleImports = mempty
  , moduleExprs = mempty
  }
