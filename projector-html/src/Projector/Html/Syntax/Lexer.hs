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
  deriving (Eq, Show)

renderLexError :: LexError -> Text
renderLexError le =
  case le of
    LexTokenError te ->
      renderTokenError te

lex :: FilePath -> Text -> Either LexError [Positioned Token]
lex file =
  bimap LexTokenError layout . tokenise file
