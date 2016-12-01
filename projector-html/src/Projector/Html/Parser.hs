{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Projector.Html.Parser (
    parse
  , ParseError (..)
  ) where


import           Data.List.NonEmpty  (NonEmpty(..))
import qualified Data.Set as S
import qualified Data.Text as T

import           P

import           Projector.Html.Data.Position
import           Projector.Html.Data.Template
import           Projector.Html.Data.Token

import           System.IO  (FilePath)

import qualified Text.Megaparsec as P


newtype ParseError = ParseError {
    unParseError :: (P.ParseError (Positioned Token) ParseErrorComponent)
  } deriving (Show)

parse :: FilePath -> [Positioned Token] -> Either ParseError (Template Range)
parse file =
  first ParseError . P.runParser (template <* P.eof) file . TokenStream

-- -----------------------------------------------------------------------------

type Parser = P.Parsec ParseErrorComponent TokenStream

newtype TokenStream = TokenStream {
    unTokenStream :: [Positioned Token]
  } deriving (Show, Eq, Ord)

instance P.Stream TokenStream where
  type Token TokenStream = Positioned Token
  {-# INLINE uncons #-}
  uncons ts =
    case unTokenStream ts of
      [] ->
        Nothing
      (x:xs) ->
        Just (x, TokenStream xs)
  {-# INLINE updatePos #-}
  updatePos _ _ _ (_ :@ (Range p1 p2)) =
    (positionPos p1, positionPos p2)

positionPos :: Position -> P.SourcePos
positionPos (Position x y f) =
  P.SourcePos f (P.unsafePos (fromIntegral x)) (P.unsafePos (fromIntegral y))
{-# INLINE positionPos #-}


data ParseErrorComponent
  = ParseRuleError Text
  | ParseIndentError Ordering P.Pos P.Pos
  | TagMismatch (Positioned Text) (Positioned Text)
  deriving (Eq, Ord, Show)

instance P.ErrorComponent ParseErrorComponent where
  representFail =
    ParseRuleError . T.pack
  representIndentation =
    ParseIndentError

failWith :: ParseErrorComponent -> Parser a
failWith err =
  P.failure S.empty S.empty (S.singleton err)

next :: Parser (Positioned Token)
next =
  P.token pure empty
{-# INLINE next #-}

-- -----------------------------------------------------------------------------

template :: Parser (Template Range)
template = do
  ts <- optional typeSigs
  hs <- html
  pure (Template undefined ts hs)

typeSigs :: Parser (TTypeSig Range)
typeSigs = do
  TypeSigsStart :@ a <- next
  f <- typeSig
  fs <- many (typeSigsSep *> typeSig)
  TypeSigsEnd :@ b <- next
  pure (TTypeSig (a <> b) (f :| fs))

typeSigsSep :: Parser ()
typeSigsSep = do
  TypeSigsSep :@ _ <- next
  pure ()

typeSig :: Parser (TId, TType Range)
typeSig = do
  TypeIdent x :@ _a <- next -- TODO this annotation gets dropped :(
  TypeSigSep :@ _ <- next
  ty <- type_
  pure (TId x, ty)

type_ :: Parser (TType Range)
type_ =
  tvar -- <|> tapp

tvar :: Parser (TType Range)
tvar = do
  TypeIdent x :@ a <- next
  pure (TTVar a (TId x))

html :: Parser (THtml Range)
html = do
  ns <- many htmlNode
  pure (THtml undefined ns)

htmlNode :: Parser (TNode Range)
htmlNode =
      whitespace
  <|> plain
  <|> exprNode
  <|> element

whitespace :: Parser (TNode Range)
whitespace = do
  WhiteSpace :@ a <- next
  pure (TWhiteSpace a)

plain :: Parser (TNode Range)
plain = do
  HtmlText t :@ a <- next
  pure (TPlain a (TPlainText t))

element :: Parser (TNode Range)
element = do
  TagOpen :@ a <- next
  TagIdent x :@ xl <- next
  as <- many attr
  TagClose :@ _ <- next
  h <- html
  TagCloseOpen :@ _ <- next
  TagIdent y :@ yl <- next
  TagClose :@ b <- next
  when (x /= y) (failWith (TagMismatch (x :@ xl) (y :@ yl)))
  pure (TElement (a <> b) (TTag x) as h)

attr :: Parser (TAttribute Range)
attr = do
  AttName x :@ a <- next
  mval <- optional $ do

  pure $ maybe (TEmptyAttribute a (TAttrName x))
    (\val -> TAttribute (a <> undefined) (TAttrName x) val)
    mval

exprNode :: Parser (TNode Range)
exprNode = do
  ExprStart :@ a <- next
  e <- expr
  ExprEnd :@ b <- next
  pure (TExprNode (a <> b) e)

expr :: Parser (TExpr Range)
expr =
  eappParen <|> eapp <|> ecase_ <|> evar

eapp :: Parser (TExpr Range)
eapp = do
  f <- expr
  g <- expr
  pure (TEApp undefined f g)

eappParen :: Parser (TExpr Range)
eappParen = do
  ExprLParen :@ a <- next
  f <- expr
  g <- expr
  ExprLParen :@ b <- next
  pure (TEApp (a <> b) f g)

evar :: Parser (TExpr Range)
evar = do
  ExprIdent x :@ a <- next
  pure (TEVar a (TId x))

ecase_ :: Parser (TExpr Range)
ecase_ = do
  CaseStart :@ a <- next
  e <- expr
  CaseOf :@ _ <- next
  a <- alt
  as <- many (caseSep *> alt)
  pure (TECase (a <> undefined) e (a :| as))

caseSep :: Parser ()
caseSep = do
  CaseSep :@ _ <- next
  pure ()

alt :: Parser (TAlt Range)
alt = do
  p <- pattern
  AltSep :@ _ <- next
  altExpr p <|> altHtml p

altExpr :: TPattern Range -> Parser (TAlt Range)
altExpr p = do
  e <- expr
  pure (TAlt undefined p (TAltExpr undefined e))

altHtml :: TPattern Range -> Parser (TAlt Range)
altHtml p = do
  h <- html
  pure (TAlt undefined p (TAltHtml undefined h))

pattern :: Parser (TPattern Range)
pattern =
  let
    var = do
      PatId x :@ a <- next
      pure (TPVar a (TId x))
    -- standalone constructor
    con = do
      PatCon x :@ a <- next
      pure (TPCon a (TConstructor x) [])
    -- higher arity constructor
    conparen = do
      PatLParen :@ a <- next
      PatCon x :@ _ <- next
      ps <- many pattern
      PatRParen :@ b <- next
      pure (TPCon (a <> b) (TConstructor x) ps)
    -- TODO should probably allow top level cons without parens
  in var <|> con <|> conparen
