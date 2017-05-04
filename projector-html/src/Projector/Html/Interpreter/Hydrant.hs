{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Interpreter.Hydrant (
    toHtml
  , toText
  , toLazyText
  ) where


import qualified Data.Text.Lazy as LT

import qualified Hydrant as H

import           P

import           Projector.Html.Interpreter as I


toText :: I.Html -> Text
toText =
  H.toText . toHtml

toLazyText :: I.Html -> LT.Text
toLazyText =
  H.toLazyText . toHtml

toHtml :: I.Html -> H.Html
toHtml html =
  case html of
    Plain t ->
      H.textNode t
    Raw t ->
      H.textNodeUnescaped t
    Comment t ->
      H.comment t
    Element t as h->
      H.parentNode (H.Tag t) (fmap toAttribute as) (toHtml h)
    VoidElement t as ->
      H.voidNode (H.Tag t) (fmap toAttribute as)
    Nested hs ->
      foldMap toHtml hs

toAttribute :: I.Attribute -> H.Attribute
toAttribute (Attribute k v) =
  H.Attribute (H.AttributeKey k) (H.AttributeValue v)
