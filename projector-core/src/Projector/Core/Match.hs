{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.Match (
    MatchTree (..)
  , buildMatchTree
  ) where


import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           P

import           Projector.Core.Syntax
import           Projector.Core.Type


-- | A decision tree for a pattern match.
newtype MatchTree = MatchTree {
    -- FIX should use lists for ordering reasons
    unMatchTree :: Map Pat [MatchTree]
  } deriving (Eq, Ord, Show)

instance Monoid MatchTree where
  mempty = MatchTree mempty
  mappend (MatchTree a) (MatchTree b) = MatchTree (M.unionWith (L.zipWith (<>)) a b)

-- | Key nodes in the pattern tree.
data Pat
  = WildCard
  | Con Constructor
  deriving (Eq, Ord, Show)

buildMatchTree :: [Pattern a] -> MatchTree
buildMatchTree =
  foldl' addToMatchTree mempty

addToMatchTree :: MatchTree -> Pattern a -> MatchTree
addToMatchTree mt@(MatchTree m) pat =
  case pat of
    PVar _ _ ->
      MatchTree (M.insert WildCard mempty m)
    PCon _ c ps ->
      MatchTree (M.singleton (Con c) (fmap (buildMatchTree . pure) ps)) <> mt

testPats :: [Pattern ()]
testPats = [
    pvar_ "x"
  , pcon_ "Abc" [pvar_ "y"]
  , pcon_ "Def" [pcon_ "Xyz" [pvar_ "ddd"]]
  , pcon_ "Abc" [pcon_ "Def" []]
  ]
