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
  , TypeContext (..)
  , tempty
  , textend
  , tlookup
  ) where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           P


-- | Types.
data Type l
  = TLit l
  | TVar TypeName
  | TArrow (Type l) (Type l)
  | TVariant TypeName [(Constructor, [Type l])]
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
newtype TypeContext l = TypeContext { unTypeContext :: Map TypeName (Decl l) }
  deriving (Eq, Ord, Show, Read)

instance Ord l => Monoid (TypeContext l) where
  mempty = TypeContext mempty
  mappend x = TypeContext . (mappend `on` unTypeContext) x

tempty :: TypeContext l
tempty =
  TypeContext mempty

textend :: Ground l => TypeName -> Decl l -> TypeContext l -> TypeContext l
textend n t =
  TypeContext . M.insert n t . unTypeContext

tlookup :: Ground l => TypeName -> TypeContext l -> Maybe (Decl l)
tlookup n =
  M.lookup n . unTypeContext
