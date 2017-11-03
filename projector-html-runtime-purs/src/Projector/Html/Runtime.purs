module Projector.Html.Runtime (
  -- * Basic HTML model
    Html (..)
  , Attribute (..)
  , Event (..)
  , text
  , textUnescaped
  , parent
  , void
  , blank
  , attr
  , foldHtml
  -- * Primitives
  , append
  , concat
  , fold
  , isEmpty
  , map
  , module Exports
  ) where

import Data.Either (Either (..)) as Exports
import Data.Maybe (Maybe (..)) as Exports
import Control.Monad as Monad
import Data.Array as Array
import Data.Functor as Functor
import Data.String as String
import Prelude ((<>))

-- -----------------------------------------------------------------------------
-- Inefficient native HTML model

data Html ev =
    Element String (Array (Attribute ev)) (Array (Html ev))
  | VoidElement String (Array (Attribute ev))
  | Text String
  | Nested (Array (Html ev))

foldHtml :: forall ev. Array (Html ev) -> Html ev
foldHtml =
  Nested

data Attribute ev =
    Attribute String String
  | AttrEvent String (Event -> ev)

-- This is just a placeholder - if you're using this, consider reimplementing
-- the datatypes in Pux.Html.Events!
data Event =
    Event

text :: forall ev. String -> Html ev
text =
  Text

-- once again, this doesn't make much sense on the frontend
textUnescaped :: forall ev. String -> Html ev
textUnescaped =
  Text

parent :: forall ev. String -> Array (Attribute ev) -> Array (Html ev) -> Html ev
parent =
  Element

void :: forall ev. String -> Array (Attribute ev) -> Html ev
void =
  VoidElement

blank :: forall ev. Html ev
blank =
  Nested []

attr :: forall ev. String -> String -> Attribute ev
attr =
  Attribute

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
