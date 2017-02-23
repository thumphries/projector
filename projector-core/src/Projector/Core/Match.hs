{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.Match (
    MatchTree (..)
  , Pat (..)
  , buildMatchTree
  ) where


import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           P

import           Projector.Core.Syntax
import           Projector.Core.Type


-- | A decision tree for a pattern match.
newtype MatchTree = MatchTree {
    unMatchTree :: [(Pat, [MatchTree])]
  } deriving (Eq, Ord, Show)

instance Monoid MatchTree where
  mempty = MatchTree mempty
  mappend (MatchTree a) (MatchTree b) = MatchTree (unionWith (L.zipWith (<>)) a b)

-- we have to preserve ordering at all times
unionWith :: Ord a => (b -> b -> b) -> [(a, b)] -> [(a, b)] -> [(a, b)]
unionWith f l1 l2 =
  let m2 = M.fromList l2
      k1 = S.fromList (fmap fst l1)
      l2' = filter (\(k, _v) -> if S.member k k1 then False else True) l2
  in (<> l2') . with l1 $ \(k, v) ->
    case M.lookup k m2 of
      Just v2 ->
        (k, f v v2)
      Nothing ->
        (k, v)

-- | Key nodes in the pattern tree.
data Pat
  = Var Name
  | Con Constructor
  deriving (Eq, Ord, Show)

buildMatchTree :: [Pattern a] -> MatchTree
buildMatchTree =
  foldl' addToMatchTree mempty

addToMatchTree :: MatchTree -> Pattern a -> MatchTree
addToMatchTree mt@(MatchTree m) pat =
  case pat of
    PVar _ n ->
      MatchTree (m <> [(Var n, [])])
    PCon _ c ps ->
      mt <> MatchTree [(Con c, fmap (buildMatchTree . pure) ps)]

testPats :: [Pattern ()]
testPats = [
    pvar_ "x"
  , pcon_ "Def" [pcon_ "Xyz" [pvar_ "ddd"]]
  , pcon_ "Abc" [pvar_ "y"]
  , pcon_ "Abc" [pcon_ "Def" []]
  , pcon_ "Abc" [pvar_ "foo"]
  , pcon_ "Abc" [pvar_ "abc"]
  , pcon_ "Def" [pcon_ "AAA" []]
  , pcon_ "Def" [pcon_ "Xyz" [pcon_ "Abc" [pvar_ "foo"]]]
  ]
