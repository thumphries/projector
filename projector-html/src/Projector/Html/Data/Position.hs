{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Data.Position (
    Position (..)
  , Range (..)
  , renderRange
  , Positioned (..)
  , extractPosition
  , extractPositioned
  , (<@@)
  , (@@>)
  ) where


import           Data.Data (Data, Typeable)
import           Data.Semigroup (Semigroup (..))
import qualified Data.Text as T

import           GHC.Generics  (Generic)

import           P hiding ((<>))

import           System.IO (FilePath)


-- | A position in a file.
data Position = Position {
    posLine :: !Int
  , posColumn :: !Int
  , posFile :: !FilePath
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

emptyPosition :: Position
emptyPosition =
  Position 0 0 []

-- | A range between two positions.
data Range = Range {
    rangeStart :: !Position
  , rangeEnd :: !Position
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

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
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

extractPosition :: Positioned a -> Range
extractPosition (_ :@ b) =
  b

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

renderRange :: Range -> Text
renderRange (Range (Position l1 c1 file) (Position l2 c2 _)) =
  mconcat
    [ T.pack file
    , ":"
    , renderIntegral l1
    , ":"
    , renderIntegral c1
    , "-"
    , renderIntegral l2
    , ":"
    , renderIntegral c2
    ]
