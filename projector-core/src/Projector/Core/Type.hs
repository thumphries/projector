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
  ) where


import           P


-- | Types.
data Type l
  = TLit l
  | TArrow (Type l) (Type l)
  | TVariant TypeName [(Constructor, [Type l])]
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
