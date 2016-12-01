{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html where


import           P

import           Projector.Html.Data.Position
import           Projector.Html.Data.Template
import qualified Projector.Html.Lexer as Lexer
import qualified Projector.Html.Parser as Parser

import           System.IO  (FilePath)


data HtmlError
  = HtmlLexError Lexer.LexError
  | HtmlParseError Parser.ParseError
  deriving (Show)

parseTemplate :: FilePath -> Text -> Either HtmlError (Template Range)
parseTemplate file t =
  first HtmlLexError (Lexer.lex file t) >>=
    (first HtmlParseError . Parser.parse file)
