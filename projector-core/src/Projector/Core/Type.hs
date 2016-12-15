{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Projector.Core.Type (
  -- * Types
  -- ** Interface
    Type (..)
  , pattern TLit
  , pattern TVar
  , pattern TArrow
  , pattern TList
  -- *** Type functor
  , TypeF (..)
  -- ** Declared types
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
newtype Type l = Type (TypeF l (Type l))
  deriving (Eq, Ord, Show)

pattern TLit l = Type (TLitF l)
pattern TVar x = Type (TVarF x)
pattern TArrow a b = Type (TArrowF a b)
pattern TList a = Type (TListF a)

-- | Type functor.
data TypeF l a
  = TLitF l
  | TVarF TypeName
  | TArrowF a a
  | TListF a
  deriving (Functor, Foldable, Traversable)

deriving instance (Eq l, Eq a) => Eq (TypeF l a)
deriving instance (Ord l, Ord a) => Ord (TypeF l a)
deriving instance (Show l, Show a) => Show (TypeF l a)


-- | Declared types.
data Decl l
  = DVariant [(Constructor, [Type l])]
  deriving (Eq, Ord, Show)

-- | The class of user-supplied primitive types.
class (Eq l, Ord l) => Ground l where
  data Value l
  typeOf :: Value l -> l
  ppGroundType :: l -> Text
  ppGroundValue :: Value l -> Text

-- | A type's name.
newtype TypeName = TypeName { unTypeName :: Text }
  deriving (Eq, Ord, Show)

-- | A constructor's name.
newtype Constructor  = Constructor { unConName :: Text }
  deriving (Eq, Ord, Show)

-- | Type contexts.
newtype TypeDecls l = TypeDecls { unTypeDecls :: Map TypeName (Decl l) }
  deriving (Eq, Ord, Show)

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
