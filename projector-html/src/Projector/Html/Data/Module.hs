{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Data.Module (
    ModuleName (..)
  , moduleNameAppend
  , Module (..)
  , moduleFree
  , moduleBound
  , Imports (..)
  ) where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S

import           P

import           Projector.Core


newtype ModuleName = ModuleName { unModuleName :: Text }
  deriving (Eq, Ord, Show)

moduleNameAppend :: ModuleName -> ModuleName -> ModuleName
moduleNameAppend (ModuleName a) (ModuleName b) =
  ModuleName (a <> "." <> b)

-- TODO might need another datatype, this bakes in a number of
-- assumptions about the backend.
data Module b l a = Module {
    moduleTypes :: TypeDecls l
  , moduleImports :: Map ModuleName Imports
  , moduleExprs :: Map Name (b, Expr l a)
  } deriving (Eq, Ord, Show)

instance Ground l => Monoid (Module b l a) where
  mempty = Module mempty mempty mempty
  mappend (Module a b c) (Module d e f) = Module {
      moduleTypes = a <> d
    , moduleImports = b <> e
    , moduleExprs = c <> f
    }

-- | The names of all free variables referenced in this module.
moduleFree :: Module b l a -> Set Name
moduleFree (Module _types _imports exprs) =
  foldl' (flip S.delete) (foldMap (\(_ty, ex) -> gatherFree ex) exprs) (M.keys exprs)

-- | The names of all variables bound/exported in this module.
moduleBound :: Module b l a -> Set Name
moduleBound (Module _types _imports exprs) =
  S.fromList (M.keys exprs)

data Imports
  = OpenImport
  | OnlyImport [Name]
  deriving (Eq, Ord, Show)
