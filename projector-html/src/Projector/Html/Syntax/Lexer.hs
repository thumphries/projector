{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Syntax.Lexer (
    LexError (..)
  , renderLexError
  , lex
  ) where


import           P

import           Projector.Html.Data.Position
import           Projector.Html.Syntax.Token
import           Projector.Html.Syntax.Lexer.Tokenise
import           Projector.Html.Syntax.Lexer.Layout

import           System.IO (FilePath)


data LexError =
    LexTokenError TokenError
  | LexLayoutError LayoutError
  deriving (Eq, Show)

renderLexError :: LexError -> Text
renderLexError le =
  case le of
    LexTokenError te ->
      renderTokenError te
    LexLayoutError e ->
      renderLayoutError e

lex :: FilePath -> Text -> Either LexError [Positioned Token]
lex file =
  first LexLayoutError . layout <=< first LexTokenError . tokenise file
