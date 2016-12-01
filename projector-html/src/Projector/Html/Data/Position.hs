{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Data.Position (
    Position (..)
  , Range (..)
  , Positioned (..)
  , extractPositioned
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

emptyPosition :: Position
emptyPosition =
  Position 0 0 []

-- | A range between two positions.
data Range = Range {
    rangeStart :: !Position
  , rangeEnd :: !Position
  } deriving (Eq, Ord, Show)

instance Semigroup Range where
  (Range a _) <> (Range _ d) = Range a d

instance Monoid Range where
  mempty =
    Range emptyPosition emptyPosition
  mappend =
    (<>)

-- | A functor for positioned tokens.
data Positioned a
  = !a :@ !Range
  deriving (Eq, Ord, Show, Functor)

extractPositioned :: Positioned a -> a
extractPositioned (a :@ _) =
  a

-- | Absorb the item to the right.
(<@@) :: Positioned a -> Positioned b -> Positioned a
(x :@ i) <@@ (_ :@ j) =
  x :@ (i <> j)

-- | Absorb the item to the left.
(@@>) :: Positioned a -> Positioned b -> Positioned b
(_ :@ i) @@> (y :@ j) =
  y :@ (i <> j)
