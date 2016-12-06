{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Parser (
    parse
  , parse'
  , ParseError (..)
  ) where


import           Control.Comonad  (Comonad(..))

import           Data.List.NonEmpty  (NonEmpty(..))
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import           P

import           Projector.Html.Data.Position
import           Projector.Html.Data.Template
import           Projector.Html.Data.Token

import           Text.Megaparsec (label)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Lexer as L

import           System.IO  (FilePath)


-- -----------------------------------------------------------------------------

newtype ParseError = ParseError {
    unParseError :: (P.ParseError Char ParseErrorComponent)
  } deriving (Eq, Show)

parse :: FilePath -> Text -> Either ParseError (Template Range)
parse =
  parse' template

parse' :: Parser a -> FilePath -> Text -> Either ParseError a
parse' p file =
  first ParseError . P.runParser (p <* P.eof) file

-- -----------------------------------------------------------------------------

type Parser= P.Parsec ParseErrorComponent Text

data ParseErrorComponent
  = ParseFail Text
  | ParseIndentError Ordering P.Pos P.Pos
  | TagMismatch (Positioned TTag) (Positioned TTag)
  deriving (Eq, Ord, Show)

instance P.ErrorComponent ParseErrorComponent where
  representFail =
    ParseFail . T.pack
  representIndentation =
    ParseIndentError

failWith :: ParseErrorComponent -> Parser a
failWith err =
  P.failure S.empty S.empty (S.singleton err)

-- -----------------------------------------------------------------------------

template :: Parser (Template Range)
template =
  label "template" $ do
    sig <- optional typeSigs
    hs <- html
    let rang = maybe (extract hs) ((<> extract hs) . extract) sig
    pure (Template rang sig hs)

-- -----------------------------------------------------------------------------
-- type signature

typeSigs :: Parser (TTypeSig Range)
typeSigs = do
  _ :@ a <- token TypeSigsStart
  f <- typeSig
  fs <- many (P.try (typeSigsSep *> typeSig))
  _ :@ b <- token TypeSigsEnd
  pure (TTypeSig (a <> b) (f :| fs))

typeSig :: Parser (TId, TType Range)
typeSig =
  label "type signature" $ do
    t :@ _a <- typeIdent -- TODO this annotation gets dropped :(
    _ <- token TypeSigSep
    ty <- type_
    pure (TId t, ty)

type_ :: Parser (TType Range)
type_ =
  tvar -- <|> tapp <|> tlist

tvar :: Parser (TType Range)
tvar = do
  t :@ a <- typeIdent
  pure (TTVar a (TId t))

typeSigsSep :: Parser (Positioned Token)
typeSigsSep =
      (lexemeRN (token TypeSigsSep))
  <|> (withPosition (lexeme newline *> pure TypeSigsSep))

typeIdent :: Parser (Positioned Text)
typeIdent =
  label "identifier" (withPosition ident)

-- -----------------------------------------------------------------------------

html :: Parser (THtml Range)
html =
  label "html" $ do
    pos <- P.getPosition
    ns <- many htmlNode
    let r = maybe (Range (posPosition pos) (posPosition pos)) (uncurry (<>)) (listRange ns)
    pure (THtml r ns)

htmlNode :: Parser (TNode Range)
htmlNode =
  label "html node" $
        P.try semanticWS
    <|> P.try plainText
    <|> P.try element
    <|> P.try voidElement
    <|> P.try exprNode
    <|> htmlComment

semanticWS :: Parser (TNode Range)
semanticWS =
  label "whitespace" $ do
    _ :@ r <- withPosition (some whitespace)
    pure (TWhiteSpace r)

plainText :: Parser (TNode Range)
plainText =
  label "plaintext" $ do
    t :@ a <- withPosition (P.some (P.try (esc '\\')))
    pure (TPlain a (TPlainText (T.pack t)))

breakers :: Set Char
breakers = S.fromList [
    '{','}', '<', '\\', ' ', '\r', '\n', '\t'
  ]

htmlComment :: Parser (TNode Range)
htmlComment =
  label "html comment" $ do
    t :@ a <- withPosition $ do
      string "<!--"
      t <- P.manyTill P.anyChar (string "-->")
      pure (T.pack t)
    pure (TComment a (TPlainText t))

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

element :: Parser (TNode Range)
element =
  label "element" $ do
    t1 :@ a <- tagOpen
    as <- many (P.try (lexemeRN attr))
    _ <- tagClose
    hs <- html
    t2 :@ b <- closeTag
    when (t1 /= t2) (failWith (TagMismatch (t1 :@ a) (t2 :@ b)))
    pure (TElement (a <> b) t1 as hs)

voidElement :: Parser (TNode Range)
voidElement =
  label "void element" $ do
    t :@ a <- tagOpen
    as <- many (P.try (lexemeRN attr))
    b <- tagSelfClose
    pure (TVoidElement (a <> b) t as)

exprNode :: Parser (TNode Range)
exprNode =
  label "expression" $ do
    _ :@ a <- lexemeRN (token ExprStart)
    e <- expr
    _ :@ b <- token ExprEnd
    pure (TExprNode (a <> b) e)

tagOpen :: Parser (Positioned TTag)
tagOpen =
  label "tag open" $ do
    _ :@ a <- lexemeRN (token TagOpen)
    n :@ b <- lexemeRN tagIdent
    pure (TTag n :@ (a <> b))

tagClose :: Parser Range
tagClose =
  label "tag close" $ do
    _ :@ a <- token TagClose
    pure a

tagSelfClose :: Parser Range
tagSelfClose =
  label "tag self-close" $ do
    _ :@ a <- token TagSelfClose
    pure a

closeTag :: Parser (Positioned TTag)
closeTag =
  label "close tag" $ do
    _ :@ a <- lexeme (token TagCloseOpen)
    i :@ _ <- tagIdent
    b <- tagClose
    pure (TTag i :@ (a <> b))

tagIdent :: Parser (Positioned Text)
tagIdent =
  label "tag" (withPosition ident)

attr :: Parser (TAttribute Range)
attr =
  label "attribute" $ do
    n <- attrName
    attrValue n

attrName :: Parser (Positioned TAttrName)
attrName =
  label "attribute name" $ withPosition (fmap TAttrName ident)

attrValue :: Positioned TAttrName -> Parser (TAttribute Range)
attrValue (n :@ a) =
  label "attribute value" $ asum [
      do _ <- lexeme (token AttSep)
         val <- P.try attrValueString <|> P.try attrValueExpr
         pure (TAttribute (a <> extract val) n val)
    , pure (TEmptyAttribute a n)
    ]

attrValueString :: Parser (TAttrValue Range)
attrValueString =
  label "attribute value" $ do
    t :@ a <- withPosition $ do
      void (P.char '"')
      t <- P.someTill P.anyChar (P.char '"')
      pure (T.pack t)
    pure (TQuotedAttrValue a (TPlainText t))

attrValueExpr :: Parser (TAttrValue Range)
attrValueExpr =
  label "attribute value" $ do
    _ :@ a <- lexemeRN (token ExprStart)
    e <- expr
    _ :@ b <- token ExprEnd
    pure (TAttrExpr (a <> b) e)

-- -----------------------------------------------------------------------------

expr :: Parser (TExpr Range)
expr =
  label "expression" $
        P.try eapp
    <|> P.try expr'

expr' :: Parser (TExpr Range)
expr' =
  label "expression" $
        P.try ecase_
    <|> P.try eappParen
    <|> P.try evar

evar :: Parser (TExpr Range)
evar =
  label "variable" $ do
    x :@ a <- exprId
    pure (TEVar a (TId x))

ecase_ :: Parser (TExpr Range)
ecase_ =
  label "case statement" $ do
    _ :@ a <- lexemeRN (token CaseStart)
    e <- lexemeRN expr
    _ <- lexemeRN (token CaseOf)
    a1 <- alt
    as <- many (caseSep *> alt)
    let r = (a <>) (maybe (extract a1) snd (listRange as))
    pure (TECase r e (a1 :| as))

caseSep :: Parser (Positioned Token)
caseSep =
  label "case separator" . withPosition $
        (token_ CaseSep)
    <|> (newline *> many whitespace *> pure CaseSep)

eapp :: Parser (TExpr Range)
eapp =
  label "function application" $ do
    f <- expr'
    g :| gs <- some' expr'
    let r = maybe (extract f) (uncurry (<>)) (listRange (g:gs))
    pure (eappAssoc r f (g :| gs))

eappParen :: Parser (TExpr Range)
eappParen = do
  _ :@ a <- lexeme (token ExprLParen)
  f <- expr'
  g :| gs <- some' (P.try expr')
  _ :@ b <- lexeme (token ExprRParen)
  pure (eappAssoc (a <> b) f (g :| gs))

eappAssoc :: Range -> (TExpr Range) -> NonEmpty (TExpr Range) -> TExpr Range
eappAssoc r f (g :| gs) =
  case gs of
    [] -> TEApp r f g
    (h:hs) -> eappAssoc r (TEApp r f g) (h :| hs)

alt :: Parser (TAlt Range)
alt =
  label "case alternative" $ do
    _pos <- L.indentLevel -- TODO use
    p <- lexemeRN pattern
    _ <- token AltSep
    altExpr p <|> altHtml p

altExpr :: TPattern Range -> Parser (TAlt Range)
altExpr p = do
  e <- expr
  pure (TAlt (extract p <> extract e) p (TAltExpr (extract e) e))

altHtml :: TPattern Range -> Parser (TAlt Range)
altHtml p = do
  h <- html
  pure (TAlt (extract p <> extract h) p (TAltHtml (extract h) h))

pattern :: Parser (TPattern Range)
pattern =
  let
    var = do
      t :@ a <- patId
      pure (TPVar a (TId t))
    -- standalone construcotr
    con = do
      t :@ a <- patCon
      pure (TPCon a (TConstructor t) [])
    -- higher arity constructor
    conparen = do
      _ :@ a <- lexeme (token PatLParen)
      t :@ _ <- lexeme patCon
      ps <- many (lexeme pattern)
      _ :@ b <- token PatRParen
      pure (TPCon (a <> b) (TConstructor t) ps)
  in label "pattern" (P.try var <|> P.try con <|> conparen)

patId :: Parser (Positioned Text)
patId =
  withPosition lowerIdent

patCon :: Parser (Positioned Text)
patCon =
  withPosition upperIdent

exprId :: Parser (Positioned Text)
exprId =
  withPosition $
    P.try (ident `suchThat` (not . flip S.member reserved))

reserved :: Set Text
reserved = S.fromList [
    "case", "of"
  ]

-- -----------------------------------------------------------------------------

listRange :: Comonad w => [w a] -> Maybe (a, a)
listRange ls =
  case ls of
    (x:xs) ->
      pure (go x xs)
    [] ->
      empty
  where
    go x [] =
      (extract x, extract x)
    go x [y] =
      (extract x, extract y)
    go x (_:ys) =
      go x ys
{-# INLINEABLE listRange #-}

some' :: Parser a -> Parser (NonEmpty a)
some' p = do
  a <- p
  as <- many p
  pure (a :| as)
{-# INLINEABLE some' #-}

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

lexeme :: Parser a -> Parser a
lexeme f =
  f <* many space
{-# INLINE lexeme #-}

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
