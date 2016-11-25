{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Lexer (
    LexError (..)
  , lex
  ) where


import           P

import           Projector.Html.Data.Position
import           Projector.Html.Data.Token

import qualified Text.Megaparsec as P
import           Text.Megaparsec.Text (Parser)
-- import qualified Text.Megaparsec.Lexer as L

import           System.IO  (FilePath)


data LexError
  = LexError (P.ParseError (P.Token Text) P.Dec)
  deriving (Eq, Show)

lex :: FilePath -> Text -> Either LexError [Positioned Token]
lex file =
  first LexError . P.runParser lexer file

-- -----------------------------------------------------------------------------

lexer :: Parser [Positioned Token]
lexer =
  many token

token :: Parser (Positioned Token)
token =
  withPosition (pure WhiteSpace)

-- -----------------------------------------------------------------------------

getPosition :: Parser Position
getPosition =
  fmap posPosition P.getPosition

withPosition :: Parser a -> Parser (Positioned a)
withPosition p = do
  a <- getPosition
  b <- p
  c <- getPosition
  pure (b :@ Range a c)

posPosition :: P.SourcePos -> Position
posPosition (P.SourcePos file line col) =
  Position (fromIntegral (P.unPos line)) (fromIntegral (P.unPos col)) file
