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
  ) where


import           Data.Foldable (fold)
import           Data.Functor (fmap)
import           Data.Monoid (Monoid (..))
import qualified Hydrant
import           Projector.Html.Runtime.Prim

foldHtml :: [Hydrant.Html] -> Hydrant.Html
foldHtml =
  fold
{-# INLINE foldHtml #-}

append :: Text -> Text -> Text
append =
  mappend
{-# INLINE append #-}

concat :: [Text] -> Text
concat =
  fold
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
