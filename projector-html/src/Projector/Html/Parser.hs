{-# LANGUAGE LambdaCase #-}
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
  } deriving (Eq, Show)

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

instance Show a => P.ShowToken (Positioned a) where
  showTokens = show

instance P.ShowErrorComponent ParseErrorComponent where
  showErrorComponent = show

data ParseErrorComponent
  = ParseFail Text
  | ParseIndentError Ordering P.Pos P.Pos
  | TagMismatch (Positioned Text) (Positioned Text)
  deriving (Eq, Ord, Show)

instance P.ErrorComponent ParseErrorComponent where
  representFail =
    ParseFail . T.pack
  representIndentation =
    ParseIndentError

failWith :: ParseErrorComponent -> Parser a
failWith err =
  P.failure S.empty S.empty (S.singleton err)

satisfy_ :: Maybe Token -> (Token -> Bool) -> Parser (Positioned Token)
satisfy_ mrep p =
  P.token
    (\pt ->
       if p (extractPositioned pt)
         then pure pt
         else Left
                ( S.singleton (P.Tokens (pt :| []))
                , maybe
                    mempty
                    (S.singleton . P.Tokens . (:| []) . (:@ mempty))
                    mrep
                , mempty))
    empty
{-# INLINE satisfy_ #-}

satisfy :: (Token -> Bool) -> Parser (Positioned Token)
satisfy =
  satisfy_ empty
{-# INLINE satisfy #-}

expect :: Token -> Parser (Positioned Token)
expect t =
  P.token
    (\pt ->
       if t == extractPositioned pt
         then pure pt
         else Left
                ( S.singleton (P.Tokens (pt :| []))
                , S.singleton (P.Tokens (t :@ mempty :| []))
                , mempty))
    empty
{-# INLINE expect #-}

token :: Token -> Parser Range
token =
  fmap range . expect
{-# INLINE token #-}


-- -----------------------------------------------------------------------------

template :: Parser (Template Range)
template = do
  ts <- optional (P.try typeSigs)
  hs <- html
  pure (Template undefined ts hs)

typeSigs :: Parser (TTypeSig Range)
typeSigs = do
  a <- token TypeSigsStart
  f <- typeSig
  fs <- many (P.try (token TypeSigsSep *> typeSig))
  b <- token TypeSigsEnd
  pure (TTypeSig (a <> b) (f :| fs))

typeSig :: Parser (TId, TType Range)
typeSig = P.dbg "typeSig" $ do
  (x, _a) <- typeIdent -- TODO this annotation gets dropped :(
  _ <- token TypeSigSep
  ty <- type_
  pure (x, ty)

typeIdent :: Parser (TId, Range)
typeIdent = do
  TypeIdent x :@ a <- satisfy
    (\case
       TypeIdent _ ->
         True
       _ ->
         False)
  pure (TId x, a)

type_ :: Parser (TType Range)
type_ =
  tvar -- <|> tapp

tvar :: Parser (TType Range)
tvar = do
  (x, a) <- typeIdent
  pure (TTVar a x)

html :: Parser (THtml Range)
html = P.dbg "html" $ do
  ns <- many htmlNode
  pure (THtml undefined ns)

htmlNode :: Parser (TNode Range)
htmlNode =
  P.dbg "htmlNode" . asum . fmap P.try $ [
    whitespace
  , plain
  , exprNode
  , voidelement
  , element
  , comment
  ]

whitespace :: Parser (TNode Range)
whitespace = do
  a <- token WhiteSpace
  pure (TWhiteSpace a)

plain :: Parser (TNode Range)
plain = do
  HtmlText t :@ a <- satisfy (\case HtmlText _ -> True; _ -> False)
  pure (TPlain a (TPlainText t))

comment :: Parser (TNode Range)
comment = do
  HtmlComment t :@ a <- satisfy (\case HtmlComment _ -> True; _ -> False)
  pure (TComment a (TPlainText t))

voidelement :: Parser (TNode Range)
voidelement = do
  a <- token TagOpen
  TagIdent x :@ _ <- tagIdent
  as <- many attr
  b <- token TagSelfClose
  pure (TVoidElement (a <> b) (TTag x) as)

element :: Parser (TNode Range)
element = do
  a <- token TagOpen
  TagIdent x :@ xl <- tagIdent
  as <- many attr
  _ <- token TagClose
  h <- html
  _ <- token TagCloseOpen
  TagIdent y :@ yl <- tagIdent
  b <- token TagClose
  when (x /= y) (failWith (TagMismatch (x :@ xl) (y :@ yl)))
  pure (TElement (a <> b) (TTag x) as h)

tagIdent :: Parser (Positioned Token)
tagIdent =
  satisfy (\case TagIdent _ -> True; _ -> False)

attr :: Parser (TAttribute Range)
attr = do
  AttName x :@ a <- satisfy (\case AttName _ -> True; _ -> False)
  mval <- optional attrval
  pure $ maybe (TEmptyAttribute a (TAttrName x))
    (\val -> TAttribute (a <> undefined) (TAttrName x) val)
    mval

attrval :: Parser (TAttrValue Range)
attrval =
  let
    qval = do
      _ <- token AttSep
      AttValueQ t :@ a <- satisfy (\case AttValueQ _ -> True; _ -> False)
      pure (TQuotedAttrValue a (TPlainText t))
    vale = do
      _ <- token AttSep
      a <- token ExprStart
      e <- expr
      b <- token ExprEnd
      pure (TAttrExpr (a <> b) e)
  in qval <|> vale

exprNode :: Parser (TNode Range)
exprNode = P.dbg "exprNode" $ do
  a <- token ExprStart
  e <- expr
  b <- token ExprEnd
  pure (TExprNode (a <> b) e)

expr :: Parser (TExpr Range)
expr =
  P.dbg "expr" $
  asum $ fmap P.try [
      ecase_
    , evar
    ]

eapp :: Parser (TExpr Range)
eapp = P.dbg "eapp" $ do
  f <- expr
  g <- expr
  pure (TEApp undefined f g)

eappParen :: Parser (TExpr Range)
eappParen = P.dbg "eappParen" $ do
  a <- token ExprLParen
  f <- expr
  g <- expr
  b <- token ExprRParen
  pure (TEApp (a <> b) f g)

evar :: Parser (TExpr Range)
evar = P.dbg "evar" $ do
  ExprIdent x :@ a <- satisfy (\case ExprIdent _ -> True; _ -> False)
  pure (TEVar a (TId x))

ecase_ :: Parser (TExpr Range)
ecase_ = P.dbg "ecase" $ do
  a <- token CaseStart
  e <- expr
  _ <- token CaseOf
  a1 <- alt
  as <- many (caseSep *> alt)
  pure (TECase (a <> undefined) e (a1 :| as))

caseSep :: Parser ()
caseSep = do
  _ <- token CaseSep
  pure ()

alt :: Parser (TAlt Range)
alt = do
  p <- pattern
  _ <- token AltSep
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
      PatId x :@ a <- satisfy (\case PatId _ -> True; _ -> False)
      pure (TPVar a (TId x))
    -- standalone constructor
    con = do
      PatCon x :@ a <- satisfy (\case PatCon _ -> True; _ -> False)
      pure (TPCon a (TConstructor x) [])
    -- higher arity constructor
    conparen = do
      a <- token PatLParen
      PatCon x :@ _ <- satisfy (\case PatCon _ -> True; _ -> False)
      ps <- many pattern
      b <- token PatRParen
      pure (TPCon (a <> b) (TConstructor x) ps)
    -- TODO should probably allow top level cons without parens
  in var <|> con <|> conparen
