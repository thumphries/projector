{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Parser (
    parse
  , parse'
  , ParseError (..)
  , renderParseError
  , voidElement
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

renderParseError :: ParseError -> Text
renderParseError =
  T.pack . P.parseErrorPretty . unParseError

parse :: FilePath -> Text -> Either ParseError (Template Range)
parse =
  parse' template

parse' :: Parser a -> FilePath -> Text -> Either ParseError a
parse' p file =
  first ParseError . P.runParser (p <* P.eof) file

-- -----------------------------------------------------------------------------

type Parser= P.Parsec ParseErrorComponent Text

data ParseErrorComponent
  = ParseFail [Char]
  | ParseIndentError Ordering P.Pos P.Pos
  | TagMismatch (TTag Range) (TTag Range)
  deriving (Eq, Ord, Show)

instance P.ErrorComponent ParseErrorComponent where
  representFail =
    ParseFail
  representIndentation =
    ParseIndentError

instance P.ShowErrorComponent ParseErrorComponent where
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
      TagMismatch (TTag a t1) (TTag b t2) ->
        mconcat
          [ "HTML tags are not balanced:\n  Tag '"
          , T.unpack t1
          , "' wrongly closed by '"
          , T.unpack t2 <> "' at "
          , T.unpack (renderRange (a <> b))
          ]

failWith :: ParseErrorComponent -> Parser a
failWith err =
  P.failure S.empty S.empty (S.singleton err)

-- -----------------------------------------------------------------------------

template :: Parser (Template Range)
template =
  label "template" $ do
    sig <- optional (P.try typeSigs)
    hs <- optional newline *> html
    let rang = maybe (extract hs) ((<> extract hs) . extract) sig
    pure (Template rang sig hs)

-- -----------------------------------------------------------------------------
-- type signature

typeSigs :: Parser (TTypeSig Range)
typeSigs = do
  _ :@ a <- lexeme (token TypeSigsStart)
  f <- lexeme typeSig
  fs <- many (P.try (typeSigsSep *> lexeme typeSig))
  _ :@ b <- token TypeSigsEnd
  pure (TTypeSig (a <> b) (f :| fs))

typeSig :: Parser (TId, TType Range)
typeSig =
  label "type signature" $ do
    t :@ _a <- lexeme typeIdent -- TODO this annotation gets dropped :(
    _ <- lexeme (token TypeSigSep)
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
        exprNode
    <|> htmlComment
    <|> plainText
    <|> semanticWS
    <|> P.try element
    <|> voidElement

semanticWS :: Parser (TNode Range)
semanticWS =
  label "whitespace" $ do
    _ :@ r <- withPosition (some (P.try whitespace))
    pure (TWhiteSpace r)

plainText :: Parser (TNode Range)
plainText =
  label "plaintext" $ do
    t :@ a <- withPosition (P.some (P.try (esc '\\')))
    pure (TPlain a (TPlainText (T.pack t)))

breakers :: Set Char
breakers = S.fromList [
    '{','}', '<', '>', '\\', ' ', '\r', '\n', '\t'
  ]

htmlComment :: Parser (TNode Range)
htmlComment =
  label "html comment" $ do
    t :@ a <- withPosition $ do
      P.try (string "<!--")
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

esc' :: (Char -> Parser Char) -> Char -> Parser Char
esc' f echar = do
  c <- P.anyChar
  if c == echar
    then do
      d <- optional P.anyChar
      maybe empty f d
    else
      pure c

elementExpr :: Parser (TExpr Range)
elementExpr =
  eOptionalParens $ do
    e <- P.choice [P.try element, voidElement]
    pure $ TENode (extract e) e

element :: Parser (TNode Range)
element =
  label "element" $ do
    ta@(TTag a t1) <- P.try (lexeme tagOpen)
    as <- many (P.try (lexemeRN attr))
    _ <- tagClose
    hs <- html
    tb@(TTag b t2) <- closeTag
    when (t1 /= t2) (failWith (TagMismatch ta tb))
    pure (TElement (a <> b) ta as hs)

voidElement :: Parser (TNode Range)
voidElement =
  label "void element" $ do
    t <- P.try (lexeme tagOpen)
    as <- many (P.try (lexemeRN attr))
    b <- P.try tagSelfClose
    pure (TVoidElement (extract t <> b) t as)

exprNode :: Parser (TNode Range)
exprNode =
  label "expression" $ do
    _ :@ a <- P.try (lexemeRN (token ExprStart))
    e <- lexemeRN expr
    _ :@ b <- token ExprEnd
    pure (TExprNode (a <> b) e)

tagOpen :: Parser (TTag Range)
tagOpen =
  label "tag open" $ do
    _ :@ a <- P.try (lexemeRN (token TagOpen <* P.notFollowedBy (P.char '/')))
    n :@ b <- lexemeRN tagIdent
    pure (TTag (a <> b) n)

tagClose :: Parser Range
tagClose =
  label "tag close" $ do
    _ :@ a <- P.try (token TagClose)
    pure a

tagSelfClose :: Parser Range
tagSelfClose =
  label "tag self-close" $ do
    _ :@ a <- P.try (token TagSelfClose)
    pure a

closeTag :: Parser (TTag Range)
closeTag =
  label "close tag" $ do
    _ :@ a <- P.try (lexeme (token TagCloseOpen))
    i :@ _ <- tagIdent
    b <- tagClose
    pure (TTag (a <> b) i)

tagIdent :: Parser (Positioned Text)
tagIdent =
  label "tag" (withPosition ident)

attr :: Parser (TAttribute Range)
attr =
  label "attribute" $ do
    n <- P.try attrName
    attrValue n

attrName :: Parser (Positioned TAttrName)
attrName =
  label "attribute name" $ withPosition (fmap TAttrName ident)

attrValue :: Positioned TAttrName -> Parser (TAttribute Range)
attrValue (n :@ a) =
  label "attribute value" $ asum [
      do _ <- P.try (lexeme (token AttSep))
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
    e <- lexemeRN expr
    _ :@ b <- token ExprEnd
    pure (TAttrExpr (a <> b) e)

-- -----------------------------------------------------------------------------

expr :: Parser (TExpr Range)
expr =
  label "expression" $
        P.try eeach
    <|> P.try eapp
    <|> P.try expr'

expr' :: Parser (TExpr Range)
expr' =
  label "expression" $ do
        P.try ecase_
    <|> P.try eeach
    <|> P.try eappParen
    <|> P.try elam
    <|> P.try evar
    <|> P.try elit
    <|> P.try elementExpr

evar :: Parser (TExpr Range)
evar =
  label "variable" . eOptionalParens $ do
    x :@ a <- exprId
    pure (TEVar a (TId x))

eeach :: Parser (TExpr Range)
eeach =
  label "each" . eOptionalParens $ do
    _ :@ a <- lexemeRN (token Each)
    f <- lexemeRN expr'
    g <- lexemeRN expr'
    pure (TEEach (a <> extract g) f g)

ecase_ :: Parser (TExpr Range)
ecase_ =
  label "case statement" . eOptionalParens $ do
    _ :@ a <- lexemeRN (token CaseStart)
    e <- lexemeRN expr
    _ <- lexemeRN (token CaseOf)
    a1 <- alt
    as <- many (lexemeRN caseSep *> alt)
    let r = (a <>) (maybe (extract a1) snd (listRange as))
    pure (TECase r e (a1 :| as))

caseSep :: Parser (Positioned Token)
caseSep =
  label "case separator" . withPosition $
        (token_ CaseSep)
    <|> (newline *> many whitespace *> pure CaseSep)

eapp :: Parser (TExpr Range)
eapp =
  label "function application" . eOptionalParens $ do
    f <- lexemeRN expr'
    g :| gs <- some' (P.try (lexemeRN expr'))
    let r = maybe (extract f) (uncurry (<>)) (listRange (g:gs))
    pure (eappAssoc r f (g :| gs))

elam :: Parser (TExpr Range)
elam =
  label "anonymous function" . eOptionalParens $ do
    _ :@ a <- lexemeRN (token LamStart)
    ids <- fmap (fmap (\(t:@_) -> TId t)) (some' (P.try (lexemeRN exprId)))
    _ :@ _ <- lexemeRN (token LamBody)
    e <- lexemeRN expr
    pure (TELam (a <> (extract e)) ids e)

eappParen :: Parser (TExpr Range)
eappParen =
  eParens $ do
    f <- lexemeRN expr'
    g :| gs <- some' (P.try (lexemeRN expr'))
    let r = maybe (extract f) (uncurry (<>)) (listRange (g : gs))
    pure (eappAssoc r f (g :| gs))

eappAssoc :: Range -> (TExpr Range) -> NonEmpty (TExpr Range) -> TExpr Range
eappAssoc r f (g :| gs) =
  case gs of
    [] -> TEApp r f g
    (h:hs) -> eappAssoc r (TEApp r f g) (h :| hs)

eParens :: Parser a -> Parser a
eParens p =
  P.between (P.try (lexemeRN (token ExprLParen))) (token ExprRParen) p

eOptionalParens :: Parser a -> Parser a
eOptionalParens p =
  P.try (eParens p) <|> p

elit :: Parser (TExpr Range)
elit =
  label "literal" $ do
    t :@ a <- withPosition . lexeme $ do
      void (P.char '"')
      let
        escF c =
          if c == 'n'then
            pure '\n'
          else if c == 'r' then
            pure '\r'
          else if c == '\\' || c == '"' then
            pure c
          else
            empty
      t <- P.try $ P.manyTill (esc' escF '\\') (P.char '"')
      pure (T.pack t)
    pure (TELit a (TLString a t))

alt :: Parser (TAlt Range)
alt =
  label "case alternative" $ do
    _pos <- L.indentLevel -- TODO use
    p <- lexemeRN pattern
    _ <- lexemeRN (token AltSep)
    lexemeRN (altExpr p)

altExpr :: TPattern Range -> Parser (TAlt Range)
altExpr p = do
  e <- expr
  pure (TAlt (extract p <> extract e) p e)

pattern :: Parser (TPattern Range)
pattern =
  let
    contop = do
      t :@ a <- lexeme patCon
      ps <- many (lexeme pattern')
      let r = maybe a ((a <>) . snd) (listRange ps)
      pure (TPCon r (TConstructor t) ps)
  in label "pattern" $ P.try contop <|> pattern'

pattern' :: Parser (TPattern Range)
pattern' =
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
      ps <- many (lexeme pattern')
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
