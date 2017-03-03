{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Syntax.Lexer.Layout (
    LayoutError (..)
  , renderLayoutError
  , layout
  ) where


import           P

import           Projector.Html.Data.Position
import           Projector.Html.Syntax.Token


data LayoutError
  = LayoutError
  deriving (Eq, Ord, Show)

renderLayoutError :: LayoutError -> Text
renderLayoutError e =
  case e of
    LayoutError ->
      "LayoutError"


-- | Apply our layout rules, which comprise of
--
-- * Inserting separators and delimiters in place of indentation
-- * Discarding or collapsing whitespace we no longer care about
-- * Throwing errors when the indent is completely wrong
layout :: [Positioned Token] -> Either LayoutError [Positioned Token]
layout =
  undefined

{-

Layout rules:
1. Exprs - must continue to the right of where they began
2. Case altern
atives - likewise
3. Html - hmmmm

-}
