{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Hydrant (
  -- * Rendering
    toLazyText
  , toText
  , toUtf8Builder
  -- * Elements
  , Html
  , Tag (..)
  , Attribute (..)
  , unAttribute
  , AttributeKey (..)
  , AttributeValue (..)
  , textNode
  , textNodeUnescaped
  , parentNode
  , voidNode
  , doctype
  , comment
  -- * Escaping
  , Raw.escapeEntities
  ) where


import qualified Data.ByteString.Builder as BB
import           Data.Function ((.))
import           Data.Functor (Functor(..))
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Encoding as TLE

import           Projector.Hydrant.Data
import qualified Projector.Hydrant.Raw as Raw


-- | Render 'Html' as lazy 'Text'.
toLazyText :: Html -> TL.Text
toLazyText =
  TLB.toLazyText . unHtml

-- | Render 'Html' as strict 'Text'.
toText :: Html -> Text
toText =
  TL.toStrict . toLazyText

-- | Render 'Html' as a UTF8 'Bytestring' builder.
toUtf8Builder :: Html -> BB.Builder
toUtf8Builder =
  TLE.encodeUtf8Builder . toLazyText

-- | Construct a text node.
--
-- The text will be minimally escaped; see 'escapeEntities'.
textNode :: Text -> Html
textNode =
  Html . Raw.textNode
{-# INLINE textNode #-}

-- | Construct a text node.
--
-- The text will NOT be escaped. This function is unsafe.
textNodeUnescaped :: Text -> Html
textNodeUnescaped =
  Html . Raw.textNodeUnescaped
{-# INLINE textNodeUnescaped #-}

-- | Add a parent node with some 'Html' as the subtree.
--
-- - @parentNode ('Tag' "a") [] ('textNode' "foo") == "<a>foo</a>"@
parentNode :: Tag -> [Attribute] -> Html -> Html
parentNode t attrs =
  Html . Raw.parentNode (unTag t) (fmap unAttribute attrs) . unHtml
{-# INLINE parentNode #-}

-- | Add a self-closing tag with no subtree.
--
-- - @voidNode ('Tag' "img") ['Attribute' ('AttributeKey' "src") ('AttributeValue' "altavista")] == "<img src="altavista"/>"@
voidNode :: Tag -> [Attribute] -> Html
voidNode t =
  Html . Raw.voidNode (unTag t) . fmap unAttribute
{-# INLINE voidNode #-}

-- | Add a DOCTYPE.
--
-- Doctype text is not escaped. The user must ensure it satisfies their chosen HTML standard.
doctype :: Text -> Html
doctype =
  Html . Raw.doctype
{-# INLINE doctype #-}

-- | Comment text is not escaped. The user must ensure it satisfies their chosen HTML standard.
--
-- e.g. for HTML 5:
--
-- * MUST NOT contain @--@.
--
-- * MUST NOT start with @>@
--
-- * MUST NOT start with @->@
comment :: Text -> Html
comment =
  Html . Raw.comment
