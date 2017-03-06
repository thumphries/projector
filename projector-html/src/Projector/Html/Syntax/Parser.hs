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
           ]

-- -----------------------------------------------------------------------------

type Rule r = E.Prod r Text (Positioned Token)
type Grammar r a = E.Grammar r (Rule r a)

template :: Grammar r (Template Range)
template = mdo
  expr' <- expr
  html' <- E.rule (html expr')
  E.rule $
        (\thtml -> Template (extract thtml) Nothing thtml)
    <$> (html' <* optional (token Newline))


-- -----------------------------------------------------------------------------

html :: Rule r (TExpr Range) -> Rule r (THtml Range)
html expr' =
  (\(a, es) -> THtml a es)
    <$> fmap (\e -> (extract e, [e])) (htmlExpr expr')

htmlExpr :: Rule r (TExpr Range) -> Rule r (TNode Range)
htmlExpr expr' =
  (\a e b -> TExprNode (a <> b) e)
    <$> token ExprStart
    <*> expr'
    <*> token ExprEnd

-- -----------------------------------------------------------------------------

expr :: Grammar r (TExpr Range)
expr = mdo
  expr' <- E.rule $
        exprApp expr' expr''
    <|> expr''
  expr'' <- E.rule $
        exprParens expr'
    <|> exprLam expr'
--    <|> exprPrj expr''
    <|> exprVar
  pure expr'

exprParens :: Rule r (TExpr Range) -> Rule r (TExpr Range)
exprParens =
  delimited ExprLParen ExprRParen (\a b -> setTExprAnnotation (a <> b))

exprApp :: Rule r (TExpr Range) -> Rule r (TExpr Range) -> Rule r (TExpr Range)
exprApp expr' expr'' =
  (\e1 e2 -> TEApp (extract e1 <> extract e2) e1 e2)
    <$> expr'
    <*> expr''
{-
exprPrj :: Rule r (TExpr Range) -> Rule r (TExpr Range)
exprPrj expr' =
  (\e _ (f :@ b) -> TEPrj (extract e <> b) e f)
    <$> expr'
    <*> token ExprDot
    <*> exprVarId
-}

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

exprVarId :: Rule r (Positioned Text)
exprVarId =
  E.terminal $ \case
    ExprVarId t :@ a ->
      pure (t :@ a)
    _ ->
      empty

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
