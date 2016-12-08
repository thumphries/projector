{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Runtime.Blaze (
  -- * External interface
    renderHtml
  , renderHtmlBuilder
  ) where


import           Data.ByteString.Builder (Builder)
import           Data.Foldable (foldl')
import           Data.Function  ((.))
import           Data.Functor  (fmap)
import           Data.Monoid  (mconcat)
import           Data.Text (Text)
import qualified Data.Text.Lazy as T (toStrict)

import           Projector.Html.Runtime.Prim
import           Projector.Html.Runtime.Library

import qualified Text.Blaze as B
import qualified Text.Blaze.Internal as BI
import           Text.Blaze.Renderer.Text  (renderMarkup)
import           Text.Blaze.Renderer.Utf8  (renderMarkupBuilder)


-- -----------------------------------------------------------------------------
-- External interface

renderHtml :: Html -> Text
renderHtml =
  T.toStrict . renderMarkup . htmlToMarkup

renderHtmlBuilder :: Html -> Builder
renderHtmlBuilder =
  renderMarkupBuilder . htmlToMarkup

-- -----------------------------------------------------------------------------
-- Transform
-- (This will eventually be done via rewrite rules)

htmlToMarkup :: Html -> B.Markup
htmlToMarkup (Html nodes) =
  B.toMarkup (fmap htmlNodeToMarkup nodes)

htmlNodeToMarkup :: HtmlNode -> B.Markup
htmlNodeToMarkup h =
  case h of
    Element tag attrs branches ->
      applyAttrs
        (BI.customParent (renderTag tag) (htmlToMarkup branches))
        attrs

    VoidElement tag attrs ->
      applyAttrs (BI.customLeaf (renderTag tag) True) attrs

    Comment str ->
      B.stringComment str

    Plain str ->
      B.string str

    Whitespace ->
      B.string " " -- TODO Check this

renderTag :: Tag -> B.Tag
renderTag (Tag ts) =
  B.stringTag ts

applyAttrs :: B.Markup -> [Attribute] -> B.Markup
applyAttrs h =
  foldl' (B.!) h . fmap attrToMarkup

attrToMarkup :: Attribute -> B.Attribute
attrToMarkup (Attribute (AttributeKey k) (AttributeValue v)) =
  B.customAttribute (B.stringTag k) (B.stringValue v)
