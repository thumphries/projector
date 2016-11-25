{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Lexer.Internal (
    LexError (..)
  , renderLexError
  , lex
  , lex'
  -- * Guts
  , Parser
  , lexer
  , attr
  , caseAltPat
  , caseTokens
  , expr
  , html
  , htmlComment
  , plain
  , tagOpen
  , tagClose
  , htmlExpr
  , singleton
  , typeSig
  , typeSigs
  , lexeme
  , lexemeRN
  ) where


import           Data.Set  (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import           P

import           Projector.Html.Data.Position
import           Projector.Html.Data.Token

import           Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text (Parser)

import           System.IO  (FilePath)


data LexError
  = LexError (P.ParseError (P.Token Text) P.Dec)
  deriving (Eq, Show)

renderLexError :: LexError -> Text
renderLexError (LexError e) =
  T.pack (P.parseErrorPretty e)

lex :: FilePath -> Text -> Either LexError [Positioned Token]
lex =
  lex' lexer

lex' :: Parser a -> FilePath -> Text -> Either LexError a
lex' p file =
  first LexError . P.runParser (p <* P.eof) file

-- -----------------------------------------------------------------------------

lexer :: Parser [Positioned Token]
lexer =
  mconcat <$> sequence [
      typeSigs <?> "typesig" -- should this be optional in the AST???
    , newline *> pure []
    , html
    ]

-- -----------------------------------------------------------------------------

typeSigs :: Parser [Positioned Token]
typeSigs = do
  s <- lexeme (token TypeSigsStart)
  b <- some typeSig
  e <- lexeme (token TypeSigsEnd)
  pure $ mconcat [
      [s]
    , mconcat b
    , [e]
    ]

typeSig :: Parser [Positioned Token]
typeSig =
  some . asum $ fmap lexeme [
      typeIdent
    , token TypeLParen
    , token TypeRParen
    , token TypeSigSep
    , typeSigsSep
    ]

typeSigsSep :: Parser (Positioned Token)
typeSigsSep =
      (lexeme1 (token TypeSigsSep))
  <|> (withPosition (lexeme newline *> pure TypeSigsSep))

typeIdent :: Parser (Positioned Token)
typeIdent =
  withPosition (fmap TypeIdent ident)

-- -----------------------------------------------------------------------------

html :: Parser [Positioned Token]
html =
  fmap mconcat (many htmlNode)

htmlNode :: Parser [Positioned Token]
htmlNode =
      singleton htmlComment
  <|> tagClose
  <|> tagOpen
  <|> htmlExpr
  <|> singleton plain
  <|> singleton spacetoken

plain :: Parser (Positioned Token)
plain =
  withPosition $ do
    fmap (HtmlText . T.pack) (P.some (P.try (esc '\\')))

spacetoken :: Parser (Positioned Token)
spacetoken =
  withPosition $
    some whitespace *> pure WhiteSpace

breakers :: Set Char
breakers = S.fromList [
    '{','}', '<', '\\', ' ', '\r', '\n', '\t'
  ]

-- good lord this is going to be slow
esc :: Char -> Parser Char
esc echar = do
  c <- P.anyChar
  if c == echar
    then do
      d <- optional P.anyChar
      maybe empty (\p -> if S.member p breakers then pure p else empty) d
    else
      if S.member c breakers then empty else pure c

-- FIX tokens are wrong, three in one
htmlComment :: Parser (Positioned Token)
htmlComment =
  withPosition $ do
    string "<!--"
    t <- P.manyTill P.anyChar (string "-->")
    pure (HtmlComment (T.pack t))

tagClose :: Parser [Positioned Token]
tagClose = do
  o <- lexeme (token TagCloseOpen)
  s <- some tagIdent
  c <- token TagClose
  pure $ mconcat [[o], s, [c]]

tagOpen :: Parser [Positioned Token]
tagOpen = do
  o <- lexeme (token TagOpen)
  n <- lexeme tagIdent
  s <- many tagToken
  c <- token TagSelfClose <|> token TagClose
  pure $ mconcat [[o], [n], mconcat s, [c]]

tagToken :: Parser [Positioned Token]
tagToken =
  fmap mconcat . some . asum $ fmap lexeme [
      singleton attrName
    , singleton (token AttSep)
    , singleton attrValue
    , htmlExpr
    ]

attr :: Parser [Positioned Token]
attr = do
  n <- lexeme attrName
  s <- lexeme (token AttSep)
  v <- attrValue
  pure (n : s : v : [])

attrName :: Parser (Positioned Token)
attrName =
  withPosition (fmap AttName ident)

attrValue :: Parser (Positioned Token)
attrValue =
  withPosition $ do
    void (P.char '"')
    t <- P.someTill P.anyChar (P.char '"')
    pure (AttValueQ (T.pack t))

tagIdent :: Parser (Positioned Token)
tagIdent =
  withPosition (fmap TagIdent ident)

-- -----------------------------------------------------------------------------

htmlExpr :: Parser [Positioned Token]
htmlExpr = do
  s <- lexemeRN (token ExprStart)
  t <- lexemeRN expr
  e <- token ExprEnd
  pure $ mconcat [[s], t, [e]]

expr :: Parser [Positioned Token]
expr =
  (expr' =<< L.indentLevel) <?> "expression"

-- line-folded exprs
expr' :: P.Pos -> Parser [Positioned Token]
expr' indent =
  fmap mconcat (sepBy1' (P.choice expr'') (P.space *> indented indent *> pure []))

expr'' :: [Parser [Positioned Token]]
expr'' = [
    lexemeRN caseTokens
  , singleton exprId
  , singleton (token ExprLParen)
  , singleton (token ExprRParen)
  ]

caseTokens :: Parser [Positioned Token]
caseTokens = do
  c <- lexemeRN (token CaseStart)
  e <- lexemeRN expr
  o <- lexemeRN (token CaseOf)
  as <- sepBy1' caseAlt (singleton (lexemeRN caseSep))
  pure $ mconcat [
      [c]
    , e
    , [o]
    , mconcat as
    ]

caseAlt :: Parser [Positioned Token]
caseAlt = do
  pos <- L.indentLevel
  p <- lexemeRN caseAltPat
  s <- lexemeRN (token AltSep)
  b <- caseAltBody pos
  pure $ mconcat [
      p
    , [s]
    , b
    ]

caseAltPat :: Parser [Positioned Token]
caseAltPat =
  (some . asum $ fmap lexeme [
      patCon
    , patId
    , token PatLParen
    , token PatRParen
    ]) <?> "pattern"

-- line-folded exprs or line-folded html, indented
caseAltBody :: P.Pos -> Parser [Positioned Token]
caseAltBody indent =
      expr' indent
  <|> fmap mconcat (sepBy1' htmlNode (singleton (withPosition (some whitespace *> indented indent *> pure WhiteSpace)) <|> pure []))
  <?> "case alternative"

caseSep :: Parser (Positioned Token)
caseSep =
 (withPosition $
        (token_ CaseSep)
    <|> (newline *> many whitespace *> pure CaseSep)) <?> "case separator"

patId :: Parser (Positioned Token)
patId =
  withPosition (fmap PatId lowerIdent)

patCon :: Parser (Positioned Token)
patCon =
  withPosition (fmap PatCon upperIdent)

exprId :: Parser (Positioned Token)
exprId =
  withPosition $
    fmap ExprIdent (P.try (ident `suchThat` (not . flip S.member reserved)))

reserved :: Set Text
reserved = S.fromList [
    "case", "of"
  ]

-- -----------------------------------------------------------------------------

sepBy1' :: Parser a -> Parser a -> Parser [a]
sepBy1' p sep = do
  a <- p
  bs <- many . P.try $ do
    s <- sep
    b <- p
    pure [s,b]
  pure (a : mconcat bs)
{-# INLINEABLE sepBy1' #-}

indented :: P.Pos -> Parser ()
indented indent =
  void (L.indentGuard P.space GT indent)
{-# INLINE indented #-}

suchThat :: Parser a -> (a -> Bool) -> Parser a
suchThat g f= do
  a <- g
  guard (f a)
  pure a
{-# INLINE suchThat #-}

singleton :: Applicative f => Parser a -> Parser (f a)
singleton =
  fmap pure
{-# INLINE singleton #-}

lexeme :: Parser a -> Parser a
lexeme f =
  f <* many space
{-# INLINE lexeme #-}

lexeme1 :: Parser a -> Parser a
lexeme1 f =
  f <* some space
{-# INLINE lexeme1 #-}

lexemeRN :: Parser a -> Parser a
lexemeRN f =
  f <* P.try P.space
{-# INLINE lexemeRN #-}

newline :: Parser ()
newline =
  void P.newline
{-# INLINE newline #-}

space :: Parser ()
space =
  void (P.char ' ')
{-# INLINE space #-}

whitespace :: Parser ()
whitespace =
  (char ' ' <|> char '\n' <|> char '\t' <|> string "\r\n")
{-# INLINE whitespace #-}

lowerIdent :: Parser Text
lowerIdent = do
  a <- P.lowerChar
  s <- many P.alphaNumChar
  pure (T.pack (a:s))
{-# INLINE lowerIdent #-}

upperIdent :: Parser Text
upperIdent = do
  a <- P.upperChar
  s <- many P.alphaNumChar
  pure (T.pack (a:s))
{-# INLINE upperIdent #-}

ident :: Parser Text
ident =
  fmap T.pack (P.some P.alphaNumChar)
{-# INLINE ident #-}

token :: Token -> Parser (Positioned Token)
token =
  withPosition . token_
{-# INLINEABLE token #-}

token_ :: Token -> Parser Token
token_ t =
  pure t <* string (renderToken t)
{-# INLINEABLE token_ #-}

string :: Text -> Parser ()
string =
  void . P.string . T.unpack
{-# INLINEABLE string #-}

char :: Char -> Parser ()
char =
  void . P.char
{-# INLINEABLE char #-}

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
