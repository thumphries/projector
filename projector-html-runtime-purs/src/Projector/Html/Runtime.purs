module Projector.Html.Runtime (
    append
  , concat
  , fold
  , isEmpty
  , map
  ) where

import Control.Monad as Monad
import Data.Array as Array
import Data.Functor as Functor
import Data.String as String
import Prelude ((<>))

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
