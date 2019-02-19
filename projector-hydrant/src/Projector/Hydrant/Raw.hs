{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Hydrant.Raw (
  -- * Elements
    textNode
  , textNodeUnescaped
  , parentNode
  , voidNode
  , tagOpen
  , tagClose
  , doctype
  , comment
  -- * Escaping
  , escapeEntities
  ) where


import           Data.Foldable (Foldable (..))
import           Data.Functor (Functor(..))
import           Data.Function ((.))
import qualified Data.List as L
import           Data.Monoid ((<>))
import           Data.Tuple (uncurry)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TLB


textNode :: Text -> Builder
textNode =
  TLB.fromText . escapeEntities
{-# INLINEABLE textNode #-}

textNodeUnescaped :: Text -> Builder
textNodeUnescaped =
  TLB.fromText
{-# INLINEABLE textNodeUnescaped #-}

parentNode :: Text -> [(Text, Text)] -> Builder -> Builder
parentNode tag attrs b =
  tagOpen tag attrs <> b <> tagClose tag
{-# INLINEABLE parentNode #-}

voidNode :: Text -> [(Text, Text)] -> Builder
voidNode tag attrs =
     "<"
  <> fold (L.intersperse " " (TLB.fromText (escapeEntities tag) : fmap (uncurry attr) attrs))
  <> "/>"
{-# INLINEABLE voidNode #-}

tagOpen :: Text -> [(Text, Text)] -> Builder
tagOpen tag attrs =
     "<"
  <> fold (L.intersperse " " (TLB.fromText (escapeEntities tag) : fmap (uncurry attr) attrs))
  <> ">"
{-# INLINEABLE tagOpen #-}

tagClose :: Text -> Builder
tagClose t =
    "</"
  <> TLB.fromText t
  <> ">"
{-# INLINEABLE tagClose #-}

attr :: Text -> Text -> Builder
attr key val =
  TLB.fromText key <> "=\"" <> TLB.fromText (escapeEntities val) <> "\""
{-# INLINEABLE attr #-}

-- | Doctype text is not escaped. The user must ensure it satisfies their chosen HTML standard.
doctype :: Text -> Builder
doctype t =
  "<!DOCTYPE " <> TLB.fromText t <> ">"

-- | Comment text is not escaped. The user must ensure it satisfies their chosen HTML standard.
--
-- e.g. for HTML 5:
--
-- * MUST NOT contain @--@.
--
-- * MUST NOT start with @>@
--
-- * MUST NOT start with @->@
comment :: Text -> Builder
comment t =
  "<!--" <> TLB.fromText t <> "-->"
{-# INLINEABLE comment #-}

-- -----------------------------------------------------------------------------
-- Escaping

-- | Performs minimal entity escaping as follows:
--
-- > case c of
-- >   '<'  -> "&lt;"
-- >   '>'  -> "&gt;"
-- >   '&'  -> "&amp;"
-- >   '"'  -> "&quot;"
-- >   '\'' -> "&#39;"
-- >   x    -> fromString [x]
escapeEntities :: Text -> Text
escapeEntities =
    T.replace "<" "&lt;"
  . T.replace ">" "&gt;"
  . T.replace "\"" "&quot;"
  . T.replace "'" "&#39;"
  . T.replace "&" "&amp;"
{-# INLINE escapeEntities #-}
