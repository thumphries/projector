{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Syntax.Lexer.Layout (
    layout
  ) where


import           P

import           Projector.Html.Data.Position
import           Projector.Html.Syntax.Token


layout :: [Positioned Token] -> [Positioned Token]
layout =
  id
