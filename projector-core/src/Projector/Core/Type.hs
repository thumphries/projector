{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Projector.Core.Type (
    Type (..)
  , Decl (..)
  , Ground (..)
  , TypeName (..)
  , Constructor (..)
  , TypeDecls (..)
  , declareType
  , lookupType
  , subtractTypes
  ) where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           P


-- | Types.
data Type l
  = TLit l
  | TVar TypeName
  | TArrow (Type l) (Type l)
  | TList (Type l)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

-- | Declared types.
data Decl l
  = DVariant [(Constructor, [Type l])]
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

-- | The class of user-supplied primitive types.
class (Eq l, Ord l) => Ground l where
  data Value l
  typeOf :: Value l -> l
  ppGroundType :: l -> Text
  ppGroundValue :: Value l -> Text

-- | A type's name.
newtype TypeName = TypeName { unTypeName :: Text }
  deriving (Eq, Ord, Show, Read)

-- | A constructor's name.
newtype Constructor  = Constructor { unConName :: Text }
  deriving (Eq, Ord, Show, Read)

-- | Type contexts.
newtype TypeDecls l = TypeDecls { unTypeDecls :: Map TypeName (Decl l) }
  deriving (Eq, Ord, Show, Read)

instance Ord l => Monoid (TypeDecls l) where
  mempty = TypeDecls mempty
  mappend x = TypeDecls . (mappend `on` unTypeDecls) x

declareType :: Ground l => TypeName -> Decl l -> TypeDecls l -> TypeDecls l
declareType n t =
  TypeDecls . M.insert n t . unTypeDecls

lookupType :: Ground l => TypeName -> TypeDecls l -> Maybe (Decl l)
lookupType n =
  M.lookup n . unTypeDecls

subtractTypes :: Ground l => TypeDecls l -> TypeDecls l -> TypeDecls l
subtractTypes (TypeDecls m) (TypeDecls n) =
  TypeDecls (M.difference m n)
