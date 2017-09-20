{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Data.Module (
    ModuleName (..)
  , moduleNameAppend
  , Module (..)
  , ModuleExpr (..)
  , moduleFree
  , moduleBound
  , extractModuleBindings
  , extractModuleExprs
  , Imports (..)
  ) where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import           P

import           Projector.Core


newtype ModuleName = ModuleName { unModuleName :: Text }
  deriving (Eq, Ord, Show)

moduleNameAppend :: ModuleName -> ModuleName -> ModuleName
moduleNameAppend (ModuleName a) (ModuleName b) =
  ModuleName $
    if T.null b then a else a <> "." <> b

-- TODO might need another datatype, this bakes in a number of
-- assumptions about the backend.
data Module b l a = Module {
    moduleTypes :: TypeDecls l
  , moduleImports :: Map ModuleName Imports
  , moduleExprs :: Map Name (ModuleExpr b l a)
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Ground l => Monoid (Module b l a) where
  mempty = Module mempty mempty mempty
  mappend (Module a b c) (Module d e f) = Module {
      moduleTypes = a <> d
    , moduleImports = b <> e
    , moduleExprs = c <> f
    }

data ModuleExpr b l a = ModuleExpr {
    meParameter :: b
  , meExpr :: Expr l a
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | The names of all free variables referenced in this module.
moduleFree :: Module b l a -> Set Name
moduleFree (Module _types _imports exprs) =
  foldl' (flip S.delete) (foldMap (gatherFree . meExpr) exprs) (M.keys exprs)

-- | The names of all variables bound/exported in this module.
moduleBound :: Module b l a -> Set Name
moduleBound (Module _types _imports exprs) =
  S.fromList (M.keys exprs)

extractModuleBindings :: Map k (Module b l a) -> Map Name a
extractModuleBindings =
  foldMap (fmap (extractAnnotation . meExpr) . moduleExprs) . M.elems

extractModuleExprs :: Map k (Module b l a) -> Map Name (Expr l a)
extractModuleExprs =
  foldMap (fmap meExpr . moduleExprs) . M.elems

data Imports
  = OpenImport
  | OnlyImport [Name]
  | ImportQualified
  | ImportQualifiedAs ModuleName
  deriving (Eq, Ord, Show)
