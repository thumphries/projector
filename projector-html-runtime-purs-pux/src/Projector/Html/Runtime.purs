module Projector.Html.Runtime (
  -- * Pux HTML model
    Html
  , Attribute
  , text
  , textUnescaped
  , parent
  , void
  , blank
  , attr
  , foldHtml
  -- * Primitives
  , Maybe (..)
  , Either (..)
  , append
  , concat
  , fold
  , isEmpty
  , map
  ) where

import Control.Monad as Monad
import Data.Array as Array
import Data.Function.Uncurried (runFn3)
import Data.Functor as Functor
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.String as String
import Prelude
import Pux.Html.Attributes (attr) as Pux
import Pux.Html.Elements (Html, Attribute, element, text) as Pux

type Html ev = Array (Pux.Html ev)
type Attribute ev = Pux.Attribute ev


-- Pux 6 Html doesn't have a valid fold/concat, so we resort to this
-- awful hack: Everything must be an array.
--
-- The nested array concatenation here will lead to remarkably poor
-- performance. We just have to live with this until we can upgrade
-- to the newer version of Pux, or something else based on Smolder.
-- It may also be straightforward to use rewrite rules to install an array builder.

text :: forall ev. String -> Html ev
text =
  Array.singleton <<< Pux.text

textUnescaped :: forall ev. String -> Html ev
textUnescaped =
  Array.singleton <<< Pux.text

parent :: forall ev. String -> Array (Attribute ev) -> Array (Html ev) -> Html ev
parent name attrs =
  Array.singleton <<< runFn3 Pux.element name attrs <<< Array.concat

void :: forall ev. String -> Array (Attribute ev) -> Html ev
void name attrs =
  Array.singleton (runFn3 Pux.element name attrs [])

blank :: forall ev. Html ev
blank =
  []

attr :: forall ev. String -> String -> Attribute ev
attr =
  Pux.attr

foldHtml :: forall ev. Array (Html ev) -> Html ev
foldHtml =
  Array.concat

-- -----------------------------------------------------------------------------

append :: String -> String -> String
append =
  (<>)

concat :: Array String -> String
concat =
  String.joinWith ""

fold :: forall a. Array (Array a) -> Array a
fold =
  Monad.join

isEmpty :: forall a. Array a -> Boolean
isEmpty =
  Array.null

map :: forall f a b. Functor.Functor f => (a -> b) -> f a -> f b
map =
  Functor.map
