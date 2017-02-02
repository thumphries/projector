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
  , text
  , fmap
  ) where


import           Data.Foldable (fold)
import           Data.Functor (fmap)
import qualified Hydrant
import           Projector.Html.Runtime.Prim

foldHtml :: [Hydrant.Html] -> Hydrant.Html
foldHtml =
  fold
{-# INLINE foldHtml #-}

-- TODO this only exists until we start inlining library functions
text :: Text -> Hydrant.Html
text =
  Hydrant.textNode
{-# INLINE text #-}
