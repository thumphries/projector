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
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           P

newtype WhenMissing f k x y =
  WhenMissing {
      missingKey :: k -> x -> f (Maybe y)
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
mergeA (WhenMissing g1k) (WhenMissing g2k) (WhenMatched f) m1 m2 =
  fmap (M.mapMaybe id) . sequenceA $
    M.mergeWithKey (\k a -> Just . f k a) (M.mapWithKey g1k) (M.mapWithKey g2k) m1 m2
{-# INLINE mergeA #-}

traverseMissing :: Applicative f => (k -> x -> f y) -> WhenMissing f k x y
traverseMissing f =
  WhenMissing {
      missingKey = \k x -> (Just $!) <$> f k x
    }
{-# INLINE traverseMissing #-}

zipWithAMatched :: Applicative f => (k -> x -> y -> f z) -> WhenMatched f k x y z
zipWithAMatched f =
  WhenMatched $ \k x y ->
   (Just $!) <$> f k x y
{-# INLINE zipWithAMatched #-}

#endif
