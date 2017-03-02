{- | Initial tokeniser pass, preserving all whitespace. -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Syntax.Lexer.Tokenise (
    TokenError (..)
  , renderTokenError
  , tokenise
  ) where


import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.State (State, evalState, gets, modify')

import qualified Data.Set as S
import qualified Data.Text as T

import           P

import           Projector.Html.Data.Position
import           Projector.Html.Syntax.Token

import           Text.Megaparsec (label)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Lexer as L

import           System.IO  (FilePath)


-- -----------------------------------------------------------------------------

newtype TokenError = TokenError {
    unTokenError :: (P.ParseError Char TokenErrorComponent)
  } deriving (Eq, Show)

renderTokenError :: TokenError -> Text
renderTokenError =
  T.pack . P.parseErrorPretty . unTokenError

tokenise :: FilePath -> Text -> Either TokenError [Positioned Token]
tokenise =
  parse' template

parse' :: Parser a -> FilePath -> Text -> Either TokenError a
parse' p file =
  first TokenError . flip evalState defaultLexerState . P.runParserT (p <* P.eof) file

-- -----------------------------------------------------------------------------
-- Parser state - need a stack of modes
newtype LexerState = LexerState {
    unLexerState :: [LexerMode]
  } deriving (Eq, Ord, Show)

defaultLexerState :: LexerState
defaultLexerState =
  LexerState [HtmlMode]

peek :: Parser (Maybe LexerMode)
peek =
  lift (gets (head . unLexerState))

push :: LexerMode -> Parser ()
push mo =
  lift . modify' $ \(LexerState mos) ->  LexerState (mo : mos)

pop :: Parser ()
pop =
  lift . modify' $ \(LexerState mos) ->
    case mos of
      (_:xs) ->
        LexerState xs
      [] ->
        LexerState []

data LexerMode =
    HtmlMode
  | TagMode
  | ExprMode
  deriving (Eq, Ord, Show)

satisfyMode :: LexerMode -> Parser ()
satisfyMode m = do
  mmo <- peek
  let err = failWith (ModeMismatch m mmo)
  if (mmo == Just m) then pure () else err

-- -----------------------------------------------------------------------------
-- Megaparsec boilerplate

type Parser= P.ParsecT TokenErrorComponent Text (State LexerState)

data TokenErrorComponent =
    ParseFail [Char]
  | ParseIndentError Ordering P.Pos P.Pos
  | TagMismatch (Text, Range) (Text, Range)
  | ModeMismatch LexerMode (Maybe LexerMode)
  deriving (Eq, Ord, Show)

instance P.ErrorComponent TokenErrorComponent where
  representFail =
    ParseFail
  representIndentation =
    ParseIndentError

instance P.ShowErrorComponent TokenErrorComponent where
  showErrorComponent ec =
    case ec of
      ParseFail e ->
        "Internal parser fail:\n  " <> e <>
        "\n  (consider reporting this as a bug)"
      ParseIndentError o p1 p2 ->
        let ror =
              case o of
                LT ->
                  "less than "
                EQ ->
                  "equal to "
                GT ->
                  "greater than "
        in mconcat
             [ "Indentation error:\n  expected "
             , ror
             , show (P.unPos p1)
             , ", got "
             , show (P.unPos p2)
             ]
      TagMismatch (t1, a) (t2, b) ->
        mconcat
          [ "HTML tags are not balanced:\n  Tag '"
          , T.unpack t1
          , "' wrongly closed by '"
          , T.unpack t2 <> "' at "
          , T.unpack (renderRange (a <> b))
          ]
      ModeMismatch _ _ ->
        show ec

failWith :: TokenErrorComponent -> Parser a
failWith err =
  P.failure S.empty S.empty (S.singleton err)


-- -----------------------------------------------------------------------------

template :: Parser [Positioned Token]
template =
  push HtmlMode *> many token

token :: Parser (Positioned Token)
token =
  withPosition $
      (satisfyMode HtmlMode *> htmlToken)
  <|> (satisfyMode TagMode *> tagToken)
  <|> (satisfyMode ExprMode *> exprToken)


-- -----------------------------------------------------------------------------
-- Html mode

htmlToken :: Parser Token
htmlToken =
      whitespace
  <|> newline
  <|> tagOpen
  <|> exprStart
  <|> plainText

tagOpen :: Parser Token
tagOpen =
  char '<' *> pure TagOpen <* push TagMode

plainText :: Parser Token
plainText =
  fmap (Plain . T.pack) (some (P.try plainChar))

plainChar :: Parser Char
plainChar = do
  let breaks p =
        -- characters that begin rules at the same level
        p == '\n' || p == ' ' || p == '<' || p == '>' || p == '{' || p == '}'
  c <- P.anyChar
  if c == '\\'
    then do
      d <- optional P.anyChar
      maybe empty (\p -> if breaks p then pure p else empty) d
    else
      if breaks c then empty else pure c

exprStart :: Parser Token
exprStart =
  char '{' *> pure ExprStart <* push ExprMode


-- -----------------------------------------------------------------------------
-- Tag mode

tagToken :: Parser Token
tagToken =
      whitespace
  <|> newline
  <|> tagClose
  <|> tagSelfClose

tagClose :: Parser Token
tagClose =
  char '>' *> pure TagClose <* pop <* push HtmlMode

tagSelfClose :: Parser Token
tagSelfClose =
  string "/>" *> pure TagSelfClose <* pop


-- -----------------------------------------------------------------------------
-- Expression tokens

exprToken :: Parser Token
exprToken =
      whitespace
  <|> newline
  <|> exprEnd

exprEnd :: Parser Token
exprEnd =
  char '}' *> pure ExprEnd <* pop


-- -----------------------------------------------------------------------------
-- General tokens

whitespace :: Parser Token
whitespace =
  (Whitespace . length) <$> some (char ' ')

newline :: Parser Token
newline =
  char '\n' *> pure Newline


-- -----------------------------------------------------------------------------

char :: Char -> Parser ()
char =
  void . P.char

string :: [Char] -> Parser ()
string =
  void . P.string

getPosition :: Parser Position
getPosition =
  fmap posPosition P.getPosition
{-# INLINEABLE getPosition #-}

withPosition :: Parser a -> Parser (Positioned a)
withPosition p = do
  a <- getPosition
  b <- p
  c <- getPosition
  pure (b :@ Range a c)
{-# INLINEABLE withPosition #-}

posPosition :: P.SourcePos -> Position
posPosition (P.SourcePos file line col) =
  Position (fromIntegral (P.unPos line)) (fromIntegral (P.unPos col)) file
{-# INLINEABLE posPosition #-}
