{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Lexer (
    LexError (..)
  , lex
  ) where


import           P

import           Projector.Html.Data.Token

import qualified Text.Megaparsec as P
import           Text.Megaparsec.Text (Parser)
-- import qualified Text.Megaparsec.Lexer as L

import           System.IO  (FilePath)


data LexError
  = LexError (P.ParseError (P.Token Text) P.Dec)
  deriving (Eq, Show)

lex :: FilePath -> Text -> Either LexError [Token]
lex file =
  first LexError . P.runParser lexer file

-- -----------------------------------------------------------------------------

lexer :: Parser [Token]
lexer =
  many token

token :: Parser Token
token =
  pure Token
