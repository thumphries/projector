{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Data.Module (
    ModuleName (..)
  , moduleNameAppend
  , Module (..)
  , CheckedModule (..)
  , UncheckedModule (..)
  , moduleFree
  , moduleBound
  , extractModuleBindings
  , Imports (..)
  ) where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S

import           P

import           Projector.Core


-- | The name of a module.
-- This has a different meaning for each type of module! but
-- Projector, Haskell and Purescript all use the same style.
newtype ModuleName = ModuleName { unModuleName :: Text }
  deriving (Eq, Ord, Show)

-- | Append two Haskell-style module names.
moduleNameAppend :: ModuleName -> ModuleName -> ModuleName
moduleNameAppend (ModuleName a) (ModuleName b) =
  ModuleName (a <> "." <> b)

-- | A single abstract compilation / typechecking unit.
data Module b l a = Module {
    moduleTypes :: TypeDecls l
  , moduleImports :: Map ModuleName Imports
  , moduleExprs :: Map Name (b, Expr l a)
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Ground l => Monoid (Module b l a) where
  mempty = Module mempty mempty mempty
  mappend (Module a b c) (Module d e f) = Module {
      moduleTypes = a <> d
    , moduleImports = b <> e
    , moduleExprs = c <> f
    }

-- | A module that has not been typechecked.
newtype UncheckedModule l a = UncheckedModule {
    unUncheckedModule :: Module () l a
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | A module that has been successfully typechecked.
newtype CheckedModule l a = CheckedModule {
    unCheckedModule :: Module (Type l) l a
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | The names of all free variables referenced in this module.
moduleFree :: Module b l a -> Set Name
moduleFree (Module _types _imports exprs) =
  foldl' (flip S.delete) (foldMap (\(_ty, ex) -> gatherFree ex) exprs) (M.keys exprs)

-- | The names of all variables bound/exported in this module.
moduleBound :: Module b l a -> Set Name
moduleBound (Module _types _imports exprs) =
  S.fromList (M.keys exprs)

extractModuleBindings :: Map k (Module b l a) -> Map Name a
extractModuleBindings =
  foldMap (fmap (extractAnnotation . snd) . moduleExprs) . M.elems

data Imports
  = OpenImport
  | OnlyImport [Name]
  deriving (Eq, Ord, Show)
