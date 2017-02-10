module Projector.Html.Runtime (
    append
  , concat
  ) where

import Data.String as String
import Prelude ((<>))

append :: String -> String -> String
append =
  (<>)

concat :: Array String -> String
concat =
  String.joinWith ""
