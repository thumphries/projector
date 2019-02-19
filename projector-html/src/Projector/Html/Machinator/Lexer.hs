{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Machinator.Lexer (
    lexVersioned
  , LexError (..)
  , renderLexError
  ) where


import qualified Data.Text as T

import           Projector.Html.Machinator.Data.Position
import           Projector.Html.Machinator.Data.Token
import           Projector.Html.Machinator.Data.Version

import           Projector.Core.Prelude

import           System.IO  (FilePath)

import qualified Text.Megaparsec.Lexer as ML
import qualified Text.Megaparsec as M
import           Text.Megaparsec.Text  (Parser)


data LexError
  = LexError Text
  deriving (Eq, Ord, Show)

renderLexError :: LexError -> Text
renderLexError e =
  case e of
    LexError t ->
      t

lexVersioned :: FilePath -> Text -> Either LexError (Versioned [Positioned Token])
lexVersioned file t =
  first (LexError . T.pack . M.parseErrorPretty) (M.runParser (lexVersioned' <* M.eof) file t)


-- -----------------------------------------------------------------------------


lexVersioned' :: Parser (Versioned [Positioned Token])
lexVersioned' = do
  mv <- version
  comment mv
  ts <- many (token mv)
  space
  pure (Versioned mv ts)

version :: Parser MachinatorVersion
version = do
  string "-- machinator @ v"
  v <- ML.integer
  _ <- M.newline
  space
  case versionFromNumber v of
    Just ver ->
      pure ver
    Nothing ->
      -- TODO custom error component for bad version number?
      fail ("Unknown version number " <> show v <> " - expected 1")

token :: MachinatorVersion -> Parser (Positioned Token)
token v =
  token' <* space <* comment v

token' :: Parser (Positioned Token)
token' =
  withPosition $ M.choice [
      M.try $
        string (T.unpack dataKeyword) >> M.spaceChar
          >> pure TData
    , M.try $
        string (T.unpack recordKeyword) >> M.spaceChar
          >> pure TRecord
    , string "=" *> pure TEquals
    , string "|" *> pure TChoice
    , string "(" *> pure TLParen
    , string ")" *> pure TRParen
    , string "{" *> pure TLBrace
    , string "}" *> pure TRBrace
    , string ":" *> pure TTypeSig
    , string "," *> pure TComma
    , ident
    ]

comment :: MachinatorVersion -> Parser ()
comment v =
  when (featureEnabled v HasComments) $
    void . many $ M.choice [
        ML.skipBlockCommentNested "{-" "-}"
      , ML.skipLineComment "--"
      , void M.spaceChar
      ]

ident :: Parser Token
ident = do
  s <- some M.alphaNumChar
  pure (TIdent (T.pack s))

-- -----------------------------------------------------------------------------

space :: Parser ()
space =
  many M.spaceChar *> pure ()
{-# INLINEABLE space #-}

string :: [Char] -> Parser ()
string s =
  M.string s *> pure ()
{-# INLINEABLE string #-}

getPosition :: Parser Position
getPosition =
  fmap posPosition M.getPosition
{-# INLINEABLE getPosition #-}

withPosition :: Parser a -> Parser (Positioned a)
withPosition p = do
  a <- getPosition
  b <- p
  c <- getPosition
  pure (b :@ Range a c)
{-# INLINEABLE withPosition #-}

posPosition :: M.SourcePos -> Position
posPosition (M.SourcePos file line col) =
  Position (fromIntegral (M.unPos line)) (fromIntegral (M.unPos col)) file
{-# INLINEABLE posPosition #-}
