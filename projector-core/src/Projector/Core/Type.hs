{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Projector.Core.Type (
    Type (..)
  , Ground (..)
  ) where


import           P


data Type l
  = TLit l
  | TArrow (Type l) (Type l)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

class Ground l where
  data Value l
  typeOf :: Value l -> l
