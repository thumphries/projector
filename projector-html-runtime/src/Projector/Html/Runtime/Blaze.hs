{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Runtime.Blaze (
    renderHtml
  ) where


import           Prelude

import           Projector.Html.Runtime.Prim
import           Projector.Html.Runtime.Library

import           Text.Blaze.Html5 as H


renderHtml :: Html -> Text
renderHtml h =
  undefined
