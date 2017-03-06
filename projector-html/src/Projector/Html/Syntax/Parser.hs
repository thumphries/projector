{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Projector.Html.Syntax.Parser (
    ParseError (..)
  , renderParseError
  , parse
  ) where


import           Control.Comonad

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T

import           P

import           Projector.Html.Data.Position
import           Projector.Html.Data.Template
import           Projector.Html.Syntax.Token

import qualified Text.Earley as E

import Text.Show.Pretty


data ParseError =
    ParseError Text
  deriving (Eq, Ord, Show)

renderParseError :: ParseError -> Text
renderParseError pe =
  case pe of
    ParseError t ->
      "Parse error: " <> t

parse :: [Positioned Token] -> Either ParseError (Template Range)
parse toks =
  let (results, report) = E.fullParses (E.parser template) toks
  in case results of
       [x] ->
         pure x
       [] ->
         -- TODO extract location
         -- TODO pretty-print report
         Left (ParseError (T.pack (ppShow report)))
       _ ->
         Left . ParseError . T.unlines $ [
             T.pack (ppShow report)
           , "(grammar ambiguity - " <> renderIntegral (length results) <> " parses)"
           , T.pack (ppShow results)
           ]

-- -----------------------------------------------------------------------------

type Rule r = E.Prod r Text (Positioned Token)
type Grammar r a = E.Grammar r (Rule r a)

template :: Grammar r (Template Range)
template = mdo
  expr' <- expr
  html' <- html expr'
  E.rule $
    (\thtml -> Template (extract thtml) Nothing thtml)
      <$> html'

-- -----------------------------------------------------------------------------

html :: Rule r (TExpr Range) -> Grammar r (THtml Range)
html expr' = mdo
  node1 <- E.rule (htmlNode expr' html1)
  html1 <- E.rule $
    (\(a, es) -> (THtml a (toList es)))
      <$> (someNodes (node1 <|> htmlLeafNode))
  pure html1

someNodes :: Rule r (TNode Range) -> Rule r (Range, NonEmpty (TNode Range))
someNodes node' =
  (\nss -> (someRange nss, nss))
    <$> some' node'

htmlNode :: Rule r (TExpr Range) -> Rule r (THtml Range) -> Rule r (TNode Range)
htmlNode expr' html' =
      htmlVoidElement expr'
  <|> htmlElement expr' html'
  <|> htmlComment
  <|> htmlExpr expr'

htmlLeafNode :: Rule r (TNode Range)
htmlLeafNode =
      htmlPlain
  <|> htmlWhitespace

htmlPlain :: Rule r (TNode Range)
htmlPlain =
  (\ne -> TPlain (foldMap extractPosition ne) (TPlainText (foldMap extractPositioned ne)))
    <$> some' htmlText

htmlWhitespace :: Rule r (TNode Range)
htmlWhitespace =
  -- TODO this is bug-for-bug, it's safe to merge these into plaintext now
  E.terminal $ \case
    Whitespace _ :@ a ->
      pure (TWhiteSpace a)
    Newline :@ a ->
      pure (TWhiteSpace a)
    _ ->
      empty

htmlExpr :: Rule r (TExpr Range) -> Rule r (TNode Range)
htmlExpr expr' =
  (\a e b -> TExprNode (a <> b) e)
    <$> token ExprStart
    <*> expr'
    <*> token ExprEnd

htmlElement :: Rule r (TExpr Range) -> Rule r (THtml Range) -> Rule r (TNode Range)
htmlElement expr' html' =
  -- FIX FIX FIX FIX closetag needs to be recorded and checked
  (\a (tag :@ ta) attrs b subt close ->
    TElement (a <> extract close) (TTag ta tag) attrs (fromMaybe (THtml b []) subt))
    <$> token TagOpen
    <*> htmlTagIdent
    <*> many (htmlAttribute expr')
    <*> token TagClose
    <*> optional html'
    <*> htmlTagClose

htmlTagClose :: Rule r (TTag Range)
htmlTagClose =
  (\a (tag :@ _) b -> TTag (a <> b) tag)
    <$> token TagCloseOpen
    <*> htmlTagIdent
    <*> token TagClose

htmlVoidElement :: Rule r (TExpr Range) -> Rule r (TNode Range)
htmlVoidElement expr' =
  (\a (tag :@ ta) attrs b -> TVoidElement (a <> b) (TTag ta tag) attrs)
    <$> token TagOpen
    <*> htmlTagIdent
    <*> many (htmlAttribute expr')
    <*> token TagSelfClose

htmlComment :: Rule r (TNode Range)
htmlComment =
  (\a (t :@ _) _ b -> TComment (a <> b) (TPlainText t))
    <$> token TagCommentStart
    <*> htmlCommentText
    <*> token TagCommentEnd
    <*> token TagClose

htmlAttribute :: Rule r (TExpr Range) -> Rule r (TAttribute Range)
htmlAttribute expr' =
      htmlAttributeKV expr'
  <|> htmlAttributeEmpty

htmlAttributeKV :: Rule r (TExpr Range) -> Rule r (TAttribute Range)
htmlAttributeKV expr' =
  (\(t :@ a) _ val -> TAttribute (a <> extract val) (TAttrName t) val)
    <$> htmlTagIdent
    <*> token TagEquals
    <*> htmlAttributeValue expr'

htmlAttributeValue :: Rule r (TExpr Range) -> Rule r (TAttrValue Range)
htmlAttributeValue expr' =
      htmlAttributeValueQuoted expr'
  <|> htmlAttributeValueExpr expr'

htmlAttributeValueQuoted :: Rule r (TExpr Range) -> Rule r (TAttrValue Range)
htmlAttributeValueQuoted expr' =
  (\str -> TQuotedAttrValue (extract str) str)
    <$> interpolatedString expr'

htmlAttributeValueExpr :: Rule r (TExpr Range) -> Rule r (TAttrValue Range)
htmlAttributeValueExpr expr' =
  (\a e b -> TAttrExpr (a <> b) e)
    <$> token ExprStart
    <*> expr'
    <*> token ExprEnd

htmlAttributeEmpty :: Rule r (TAttribute Range)
htmlAttributeEmpty =
  (\(t :@ a) -> TEmptyAttribute a (TAttrName t))
    <$> htmlTagIdent

htmlTagIdent :: Rule r (Positioned Text)
htmlTagIdent =
  E.terminal $ \case
    TagIdent t :@ a ->
      pure (t :@ a)
    _ ->
      empty

htmlText :: Rule r (Positioned Text)
htmlText =
  E.terminal $ \case
    Plain t :@ a ->
      pure (t :@ a)
    -- TODO match Whitespace and Newline
    _ ->
      empty

htmlCommentText :: Rule r (Positioned Text)
htmlCommentText =
  E.terminal $ \case
    TagCommentChunk t :@ a ->
      pure (t :@ a)
    _ ->
      empty


-- -----------------------------------------------------------------------------

expr :: Grammar r (TExpr Range)
expr = mdo
  expr3 <- E.rule $
        exprApp expr3 expr2
    <|> exprCase expr3 pat1
    <|> expr2
  expr2 <- E.rule $
        exprParens expr3
    <|> exprList expr3
    <|> exprString expr3
    <|> exprVar
    <|> expr1
  expr1 <- E.rule $
        exprLam expr3
  pat1 <- pattern
  pure expr1

exprParens :: Rule r (TExpr Range) -> Rule r (TExpr Range)
exprParens =
  delimited ExprLParen ExprRParen (\a b -> setTExprAnnotation (a <> b))

exprApp :: Rule r (TExpr Range) -> Rule r (TExpr Range) -> Rule r (TExpr Range)
exprApp expr' expr'' =
  (\e1 e2 -> TEApp (extract e1 <> extract e2) e1 e2)
    <$> expr'
    <*> expr''

exprLam :: Rule r (TExpr Range) -> Rule r (TExpr Range)
exprLam expr' =
  (\a xs _ e -> TELam (a <> extract e) (fmap TId xs) e)
    <$> token ExprLamStart
    <*> some' (fmap extractPositioned exprVarId)
    <*> token ExprArrow
    <*> expr'

exprVar :: Rule r (TExpr Range)
exprVar =
  E.terminal $ \case
    ExprVarId t :@ a ->
      pure (TEVar a (TId t))
    ExprConId t :@ a ->
      pure (TEVar a (TId t))
    _ ->
      empty

exprList :: Rule r (TExpr Range) -> Rule r (TExpr Range)
exprList expr' =
  (\a es b -> TEList (a <> b) es)
    <$> token ExprListStart
    <*> sepBy expr' (token ExprListSep)
    <*> token ExprListEnd

exprCase :: Rule r (TExpr Range) -> Rule r (TPattern Range) -> Rule r (TExpr Range)
exprCase expr' pat' =
  (\a e _ alts -> TECase (a <> someRange alts) e alts)
    <$> token ExprCaseStart
    <*> expr'
    <*> token ExprCaseOf
    <*> exprAlts expr' pat'

exprAlts :: Rule r (TExpr Range) -> Rule r (TPattern Range) -> Rule r (NonEmpty (TAlt Range))
exprAlts expr' pat' =
  sepBy1 (exprAlt expr' pat') (token ExprCaseSep)

exprAlt :: Rule r (TExpr Range) -> Rule r (TPattern Range) -> Rule r (TAlt Range)
exprAlt expr' pat' =
  (\p _ e -> TAlt (extract p <> extract e) p e)
    <$> pat'
    <*> token ExprArrow
    <*> expr'

exprString :: Rule r (TExpr Range) -> Rule r (TExpr Range)
exprString expr' =
  (\str -> TEString (extract str) str)
    <$> interpolatedString expr'

exprVarId :: Rule r (Positioned Text)
exprVarId =
  E.terminal $ \case
    ExprVarId t :@ a ->
      pure (t :@ a)
    _ ->
      empty

interpolatedString :: Rule r (TExpr Range) -> Rule r (TIString Range)
interpolatedString expr' =
  (\a ss b -> TIString (a <> b) ss)
    <$> token StringStart
    <*> many (stringChunk <|> exprChunk expr')
    <*> token StringEnd

stringChunk :: Rule r (TIChunk Range)
stringChunk =
  E.terminal $ \case
    StringChunk t :@ a ->
      pure (TStringChunk a t)
    _ ->
      empty

exprChunk :: Rule r (TExpr Range) -> Rule r (TIChunk Range)
exprChunk expr' =
  (\a e b -> TExprChunk (a <> b) e)
    <$> token ExprStart
    <*> expr'
    <*> token ExprEnd

-- -----------------------------------------------------------------------------

pattern :: Grammar r (TPattern Range)
pattern = mdo
  pat1 <- E.rule $
        patParen pat1
    <|> patCon pat1
    <|> patVar
  pure pat1

patParen :: Rule r (TPattern Range) -> Rule r (TPattern Range)
patParen =
  delimited ExprLParen ExprRParen (\a b -> setTPatAnnotation (a <> b))

patVar :: Rule r (TPattern Range)
patVar =
  E.terminal $ \case
    ExprVarId t :@ a ->
      pure (TPVar a (TId t))
    _ ->
      empty

patCon :: Rule r (TPattern Range) -> Rule r (TPattern Range)
patCon pat' =
  (\(c :@ a) ps -> TPCon (fold (a : fmap extract ps)) (TConstructor c) ps)
    <$> patConId
    <*> many pat'

patConId :: Rule r (Positioned Text)
patConId =
  E.terminal $ \case
    ExprConId t :@ a ->
      pure (t :@ a)
    _ ->
      empty


-- -----------------------------------------------------------------------------

sepBy1 :: Alternative f => f a -> f sep -> f (NonEmpty a)
sepBy1 f sep =
  (:|)
    <$> f
    <*> many (sep *> f)

sepBy :: Alternative f => f a -> f sep -> f [a]
sepBy f sep =
  (toList <$> sepBy1 f sep) <|> pure []

someRange :: (Comonad w, Monoid a) => NonEmpty (w a) -> a
someRange ws =
  uncurry (<>) (someRange' ws)

someRange' :: Comonad w => NonEmpty (w a) -> (a, a)
someRange' (l :| ls) =
  case ls of
    (x:xs) ->
      go (extract l) (extract x) xs
    [] ->
      (extract l, extract l)
  where
    go a b [] = (a, b)
    go a _ (x:xs) = go a (extract x) xs

some' :: Alternative f => f a -> f (NonEmpty a)
some' f =
  (:|) <$> f <*> many f

delimited :: Token -> Token -> (Range -> Range -> a -> b) -> Rule r a -> Rule r b
delimited start end apply thing =
  (\a c b -> apply a b c)
    <$> token start
    <*> thing
    <*> token end

satisfy :: (Token -> Bool) -> Rule r (Positioned Token)
satisfy p =
  E.satisfy $ \(t :@ _) -> p t

token :: Token -> Rule r Range
token =
  fmap extractPosition . satisfy . (==)
