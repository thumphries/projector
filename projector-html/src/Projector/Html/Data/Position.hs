{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Data.Position (
    Position (..)
  , Range (..)
  , Positioned (..)
  , (<@@)
  , (@@>)
  ) where


import           Data.Semigroup (Semigroup (..))

import           P hiding ((<>))

import           System.IO (FilePath)


-- | A position in a file.
data Position = Position {
    posLine :: !Int
  , posColumn :: !Int
  , posFile :: !FilePath
  } deriving (Eq, Ord, Show)

-- | A range between two positions.
data Range = Range {
    rangeStart :: !Position
  , rangeEnd :: !Position
  } deriving (Eq, Ord, Show)

instance Semigroup Range where
  (Range a _) <> (Range _ d) = Range a d

-- | A functor for positioned tokens.
data Positioned a
  = !a :@ !Range
  deriving (Eq, Ord, Show, Functor)

-- | Absorb the item to the right.
(<@@) :: Positioned a -> Positioned b -> Positioned a
(x :@ i) <@@ (_ :@ j) =
  x :@ (i <> j)

-- | Absorb the item to the left.
(@@>) :: Positioned a -> Positioned b -> Positioned b
(_ :@ i) @@> (y :@ j) =
  y :@ (i <> j)
