{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Syntax (
    templateFromText
  , SyntaxError (..)
  , renderSyntaxError
  ) where


import           P

import           Projector.Html.Data.Position (Range)
import           Projector.Html.Data.Template (Template)
import           Projector.Html.Syntax.Lexer
import           Projector.Html.Syntax.Parser

import           System.IO (FilePath)


data SyntaxError
  = SyntaxLexError LexError
  | SyntaxParseError ParseError
  deriving (Eq, Show)

renderSyntaxError :: SyntaxError -> Text
renderSyntaxError se =
  case se of
    SyntaxLexError le ->
      renderLexError le
    SyntaxParseError pe ->
      renderParseError pe

templateFromText :: FilePath -> Text -> Either SyntaxError (Template Range)
templateFromText file =
  (first SyntaxParseError . parse) <=< (first SyntaxLexError . lex file)
