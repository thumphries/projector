{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Runtime.Library (
    Hydrant.Html
  , Hydrant.Tag (..)
  , Hydrant.Attribute (..)
  , Hydrant.AttributeKey (..)
  , Hydrant.AttributeValue (..)
  , Hydrant.textNode
  , Hydrant.textNodeUnescaped
  , Hydrant.parentNode
  , Hydrant.voidNode
  , Hydrant.comment
  , foldHtml
  , append
  , concat
  , text
  , fmap
  , attrValue
  , blank
  , isEmpty
  , fold
  ) where


import qualified Data.Foldable as F
import           Data.Functor (fmap)
import           Data.Monoid (Monoid (..))
import qualified Hydrant
import           Projector.Html.Runtime.Prim

-- FIX Unfortunately inlining this function results in _inevitable_ GHC 7.10.2 panics.
-- Upgrading to more recent versions of GHC may help, but for now we can live with
-- the negligible runtime cost.
fold :: [[a]] -> [a]
fold =
  F.fold

-- FIX Unfortunately inlining this function results in _inevitable_ GHC 7.10.2 panics.
-- Upgrading to more recent versions of GHC may help, but for now we can live with
-- the negligible runtime cost.
foldHtml :: [Hydrant.Html] -> Hydrant.Html
foldHtml =
  F.fold

append :: Text -> Text -> Text
append =
  mappend
{-# INLINE append #-}

concat :: [Text] -> Text
concat =
  F.fold
{-# INLINE concat #-}

-- TODO this only exists until we start inlining library functions
text :: Text -> Hydrant.Html
text =
  Hydrant.textNode
{-# INLINE text #-}

-- TODO this only exists until we start inlining library functions
attrValue :: Text -> Hydrant.AttributeValue
attrValue =
  Hydrant.AttributeValue
{-# INLINE attrValue #-}

-- TODO this only exists until we start inlining library functions
blank :: Hydrant.Html
blank =
  mempty
{-# INLINE blank #-}

isEmpty :: [a] -> Bool
isEmpty =
  F.null
{-# INLINE isEmpty #-}
