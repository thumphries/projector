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

import qualified Data.Text as T

import           P

import           Projector.Html.Data.Position
-- import           Projector.Html.Data.Template
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

parse :: [Positioned Token] -> Either ParseError (Expr Range)
parse toks =
  let (results, report) = E.fullParses (E.parser template) toks
  in case results of
       [] ->
         Left (ParseError (T.pack (ppShow report)))
       (x:_) ->
         pure x

-- -----------------------------------------------------------------------------

data Expr a =
    EApp a (Expr a) (Expr a)
  | ELam a Text (Expr a)
  | EPrj a (Expr a) Text
  | EVar a Text
  deriving (Eq, Ord, Show)

extractAnnotation :: Expr a -> a
extractAnnotation e =
  case e of
    EApp a _ _ ->
      a
    ELam a _ _ ->
      a
    EPrj a _ _ ->
      a
    EVar a _ ->
      a

setAnnotation :: a -> Expr a -> Expr a
setAnnotation a e =
  case e of
    EApp _ b c ->
      EApp a b c
    ELam _ b c ->
      ELam a b c
    EPrj _ b c ->
      EPrj a b c
    EVar _ b ->
      EVar a b

-- -----------------------------------------------------------------------------

type Rule r = E.Prod r Text (Positioned Token)
type Grammar r a = E.Grammar r (Rule r a)

template :: Grammar r (Expr Range)
template = mdo
  expr' <- expr
  html' <- E.rule (html expr')
  pure (html' <* optional (token Newline))

-- -----------------------------------------------------------------------------

html :: Rule r (Expr Range) -> Rule r (Expr Range)
html expr' =
  token ExprStart *> expr' <* token ExprEnd

-- -----------------------------------------------------------------------------

expr :: Grammar r (Expr Range)
expr = mdo
  expr' <- E.rule $
        exprParens expr'
    <|> exprApp expr'
    <|> exprLam expr'
    <|> exprPrj expr'
    <|> exprVar
  pure expr'

exprParens :: Rule r (Expr Range) -> Rule r (Expr Range)
exprParens =
  delimited ExprLParen ExprRParen (\a b -> setAnnotation (a <> b))

exprApp :: Rule r (Expr Range) -> Rule r (Expr Range)
exprApp expr' =
  (\e1 e2 -> EApp (extractAnnotation e1 <> extractAnnotation e2) e1 e2)
    <$> expr'
    <*> expr'

exprPrj :: Rule r (Expr Range) -> Rule r (Expr Range)
exprPrj expr' =
  (\e _ (f :@ b) -> EPrj (extractAnnotation e <> b) e f)
    <$> expr'
    <*> token ExprDot
    <*> exprVarId

exprLam :: Rule r (Expr Range) -> Rule r (Expr Range)
exprLam expr' =
  (\a x _ e -> ELam (a <> extractAnnotation e) x e)
    <$> token ExprLamStart
    <*> fmap extractPositioned exprVarId
    <*> token ExprArrow
    <*> expr'

exprVar :: Rule r (Expr Range)
exprVar =
  E.terminal $ \case
    ExprVarId t :@ a ->
      pure (EVar a t)
    ExprConId t :@ a ->
      pure (EVar a t)
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
