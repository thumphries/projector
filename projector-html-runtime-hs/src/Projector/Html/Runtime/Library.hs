{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Runtime.Library (
    Hydrant.Html
  , Hydrant.Tag (..)
  , Hydrant.Attribute (..)
  , Hydrant.AttributeKey (..)
  , Hydrant.AttributeValue (..)
  , Hydrant.textNode
  , Hydrant.parentNode
  , Hydrant.voidNode
  , Hydrant.comment
  , foldHtml
  ) where


import           Data.Foldable (fold)
import qualified Hydrant

foldHtml :: [Hydrant.Html] -> Hydrant.Html
foldHtml =
  fold
{-# INLINE foldHtml #-}
