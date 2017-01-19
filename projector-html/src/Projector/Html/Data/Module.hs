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

import           Projector.Core (Name)
import qualified Projector.Core as PC
import           Projector.Html.Data.Prim


newtype ModuleName = ModuleName { unModuleName :: Text }
  deriving (Eq, Ord, Show)

moduleNameAppend :: ModuleName -> ModuleName -> ModuleName
moduleNameAppend (ModuleName a) (ModuleName b) =
  ModuleName (a <> "." <> b)

-- TODO might need another datatype, this bakes in a number of
-- assumptions about the backend.
data Module b a = Module {
    moduleTypes :: HtmlDecls
  , moduleImports :: Map ModuleName Imports
  , moduleExprs :: Map Name (b, HtmlExpr a)
  } deriving (Eq, Ord, Show)

instance Monoid (Module b a) where
  mempty = Module mempty mempty mempty
  mappend (Module a b c) (Module d e f) = Module {
      moduleTypes = a <> d
    , moduleImports = b <> e
    , moduleExprs = c <> f
    }

-- | The names of all free variables referenced in this module.
moduleFree :: Module b a -> Set Name
moduleFree (Module _types _imports exprs) =
  foldl' (flip S.delete) (foldMap (\(_ty, ex) -> PC.gatherFree ex) exprs) (M.keys exprs)

-- | The names of all variables bound/exported in this module.
moduleBound :: Module b a -> Set Name
moduleBound (Module _types _imports exprs) =
  S.fromList (M.keys exprs)

data Imports
  = OpenImport
  | OnlyImport [Name]
  deriving (Eq, Ord, Show)
