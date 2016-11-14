{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Projector.Core.Type (
    Type (..)
  , Ground (..)
  , TypeName (..)
  , Constructor (..)
  , TypeContext (..)
  , tempty
  , textend
  , tlookup
  , tresolve
  , tnormalise
  , typesEqual
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

-- | The class of user-supplied primitive types.
class Eq l => Ground l where
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
newtype TypeContext l = TypeContext { unTypeContext :: Map TypeName (Type l) }
  deriving (Eq, Ord, Show, Read)

tempty :: TypeContext l
tempty =
  TypeContext mempty

textend :: Ground l => TypeName -> Type l -> TypeContext l -> TypeContext l
textend n t =
  TypeContext . M.insert n t . unTypeContext

tlookup :: Ground l => TypeName -> TypeContext l -> Maybe (Type l)
tlookup n =
  M.lookup n . unTypeContext

tresolve :: Ground l => TypeContext l -> Type l -> Type l
tresolve tc ty =
  case ty of
    TVar tn ->
      fromMaybe ty (tlookup tn tc)
    _ ->
      ty

-- equality is bork, variants aren't top level types
-- this needs to be much better thought out
tnormalise :: Ground l => TypeContext l -> Type l -> Type l
tnormalise tc ty  =
  case ty of
    TArrow t1 t2 ->
      TArrow (tnormalise tc t1) (tnormalise tc t2)

    TVariant tn cts ->
      TVariant tn (fmap (fmap (fmap (recur tn))) cts)

    TLit _ ->
      ty

    TVar tn ->
      tnormalise tc (fromMaybe ty (tlookup tn tc))
  where
    -- conspicuously don't unfold recursive calls
    -- this is garbage, should just not allow ambiguous type specs
    recur tn ty
      | ty == (TVar tn) = ty
      | otherwise = tnormalise tc ty



typesEqual :: Ground l => TypeContext l -> Type l -> Type l -> Bool
typesEqual tc a b =
  tnormalise tc a == tnormalise tc b
