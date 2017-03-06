{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Projector.Core.Map (
    mergeA
  , traverseMissing
  , zipWithAMatched
  ) where

#if MIN_VERSION_containers(0,5,9)
import           Data.Map.Merge.Strict (mergeA, traverseMissing, zipWithAMatched)

#else

-- Copyright
--   - (c) Daan Leijen 2002
--   - (c) Andriy Palamarchuk 2008
-- Obtained from licensed file under BSD.
-- https://hackage.haskell.org/package/containers-0.5.9.2/docs/src/Data-Map-Strict-Internal.html
--
import           Data.Map.Strict (Map, traverseWithKey)
import qualified Data.Map.Strict as M

import           P

data WhenMissing f k x y =
  WhenMissing {
      missingSubtree :: Map k x -> f (Map k y)
    , missingKey :: k -> x -> f (Maybe y)
    }

newtype WhenMatched f k x y z =
  WhenMatched {
      _matchedKey :: k -> x -> y -> f (Maybe z)
    }

mergeA
  :: (Applicative f, Ord k)
  => WhenMissing f k a c -- ^ What to do with keys in @m1@ but not @m2@
  -> WhenMissing f k b c -- ^ What to do with keys in @m2@ but not @m1@
  -> WhenMatched f k a b c -- ^ What to do with keys in both @m1@ and @m2@
  -> Map k a -- ^ Map @m1@
  -> Map k b -- ^ Map @m2@
  -> f (Map k c)
mergeA (WhenMissing g1t g1k) (WhenMissing g2t g2k) (WhenMatched f) m1 m2 =
  let
    go wm t1' t2' =
      case (t1', t2') of
        ([], []) ->
          fmap mconcat . sequenceA . reverse $ wm
        (t1, []) ->
          go (g1t (M.fromDistinctAscList t1) : wm) [] []
        ([], t2) ->
          go (g2t (M.fromDistinctAscList t2) : wm) [] []
        ((k1, v1) : t1, (k2, v2) : t2) ->
          case compare k1 k2 of
            LT ->
              go ((maybe mempty (M.singleton k1) <$> g1k k1 v1) : wm) t1 ((k2, v2) : t2)
            GT ->
              go ((maybe mempty (M.singleton k2) <$> g2k k2 v2) : wm) ((k1, v1) : t1) t2
            EQ ->
              go ((maybe mempty (M.singleton k1) <$> f k1 v1 v2) : wm) t1 t2
  in
    go mempty (M.toList m1) (M.toList m2)
{-# INLINE mergeA #-}

traverseMissing :: Applicative f => (k -> x -> f y) -> WhenMissing f k x y
traverseMissing f =
  WhenMissing {
      missingSubtree = traverseWithKey f
    , missingKey = \k x -> (Just $!) <$> f k x
    }
{-# INLINE traverseMissing #-}

zipWithAMatched :: Applicative f => (k -> x -> y -> f z) -> WhenMatched f k x y z
zipWithAMatched f =
  WhenMatched $ \k x y ->
   (Just $!) <$> f k x y
{-# INLINE zipWithAMatched #-}

#endif
