{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Machinator.Data.Version (
    MachinatorVersion (..)
  , versionToNumber
  , versionFromNumber
  , MachinatorFeature (..)
  , versionFeatures
  , featureEnabled
  , featureGuard
  , Versioned (..)
  ) where


import           Data.Set  (Set)
import qualified Data.Set as S

import           Projector.Core.Prelude


-- | The version of machinator in use. This will be reflected in a
-- mandatory syntax marker and will enable or disable parser and core
-- language features.
--
-- Version should be bumped for every feature addition, feature
-- removal, or syntax change.
data MachinatorVersion
  = MachinatorV1
  | MachinatorV2
  | MachinatorV3
  deriving (Eq, Ord, Enum, Show)

versionToNumber :: Integral a => MachinatorVersion -> a
versionToNumber v =
  case v of
    MachinatorV1 ->
      1
    MachinatorV2 ->
      2
    MachinatorV3 ->
      3

versionFromNumber :: (Alternative f, Integral a) => a -> f MachinatorVersion
versionFromNumber i =
  case i of
    1 ->
      pure MachinatorV1
    2 ->
      pure MachinatorV2
    3 ->
      pure MachinatorV3
    _ ->
      empty


-- | Features supported by Machinator at one time or another.
--
-- New features should be added to this list freely, but old ones
-- should never be removed, as we (almost) never want to break
-- backwards compatibility.
data MachinatorFeature
  = HasStrings
  | HasVariants
  | HasLists
  | HasRecords
  | HasBools
  | HasComments
  | HasMaybe
  | HasEither
  deriving (Eq, Ord, Enum, Show)

-- | The set of features enabled for a given version.
versionFeatures :: MachinatorVersion -> Set MachinatorFeature
versionFeatures mv =
  case mv of
    MachinatorV1 ->
      S.fromList [
          HasStrings
        , HasVariants
        , HasLists
        , HasRecords
        , HasBools
        ]

    MachinatorV2 ->
      S.fromList [
          HasStrings
        , HasVariants
        , HasLists
        , HasRecords
        , HasBools
        , HasComments
        ]
    MachinatorV3 ->
      S.fromList [
          HasStrings
        , HasVariants
        , HasLists
        , HasRecords
        , HasBools
        , HasComments
        , HasMaybe
        , HasEither
        ]


-- | Returns true if the given feature is enabled for the given version.
featureEnabled :: MachinatorVersion -> MachinatorFeature -> Bool
featureEnabled v f =
  S.member f (versionFeatures v)

-- | Succeeds iff the given feature is enabled for the given version.
featureGuard :: Alternative f => MachinatorVersion -> MachinatorFeature -> f ()
featureGuard v =
  guard . featureEnabled v


-- -----------------------------------------------------------------------------

-- | Functor that attaches a MachinatorVersion.
data Versioned a = Versioned MachinatorVersion a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
