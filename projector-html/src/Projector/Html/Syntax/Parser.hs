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

import           Text.Earley ((<?>))
import qualified Text.Earley as E


data ParseError =
    EndOfInput [Text]
  | Unexpected Range [Text] Token
  | AmbiguousParse Int [Template Range]
  deriving (Eq, Ord, Show)

renderParseError :: ParseError -> Text
renderParseError pe =
  case pe of
    EndOfInput expect ->
      T.unlines [
          "Parse error:"
        , "  " <> "Unexpected end of input"
        , "  " <> renderExpected expect
        ]
    Unexpected loc expect got ->
      T.unlines [
          renderRange loc <> ": Parse error:"
        , "  " <> "Unexpected '" <> renderToken got <> "' (" <> T.pack (show got) <> ")"
        , "  " <> renderExpected expect
        ]
    AmbiguousParse x _ts ->
      T.unlines [
          "Parse error:"
        , "  " <> "BUG: Grammar ambiguity (" <> renderIntegral x <> " parses)"
        ]

renderExpected :: [Text] -> Text
renderExpected [] =
  T.empty
renderExpected expects =
  "Expected one of: [" <> T.intercalate ", " expects <> "]"

parse :: [Positioned Token] -> Either ParseError (Template Range)
parse toks =
  let (results, report) = E.fullParses (E.parser template) toks
  in case results of
       [x] ->
         pure x
       [] ->
         case head (E.unconsumed report) of
           Just (x :@ a) ->
             Left (Unexpected a (E.expected report) x)
           Nothing ->
             Left (EndOfInput (E.expected report))
       xs ->
         -- TODO would be helpful to diff these
         Left (AmbiguousParse (length xs) xs)

-- -----------------------------------------------------------------------------

type Rule r = E.Prod r Text (Positioned Token)
type Grammar r a = E.Grammar r (Rule r a)

template :: Grammar r (Template Range)
template = mdo
  tsig' <- typeSignatures
  expr' <- expr html'
  node' <- E.rule (htmlNode expr' html')
  html' <- html node'
  E.rule (template' tsig' expr')

template' :: Rule r (TTypeSig Range) -> Rule r (TExpr Range) -> Rule r (Template Range)
template' tsig' expr' =
  (\tsig thtml -> Template (extract thtml) tsig thtml)
    <$> optional tsig'
    <*> expr'
    <?> "template"


-- -----------------------------------------------------------------------------

typeSignatures :: Grammar r (TTypeSig Range)
typeSignatures = mdo
  sigs <- E.rule (typeSigN type2 sig)
  sig <- E.rule (typeSig1 type2)
  type2 <- E.rule $
        typeApp type2 type1
    <|> type1
  type1 <- E.rule (typeSigType type2)
  E.rule (typeSig sigs)

typeSig :: Rule r (TTypeSig Range) -> Rule r (TTypeSig Range)
typeSig sigs' =
  delimited TypeSigStart TypeSigEnd (\a b tsig -> setTTypeSigAnnotation (a <> b) tsig) sigs'

typeSig1 :: Rule r (TType Range) -> Rule r (TId, TType Range)
typeSig1 type' =
  (\a _ b _ -> (a, b))
    <$> fmap (TId . extractPositioned) sigIdent
    <*> token TypeSig
    <*> type'
    <*> token TypeSigSep

typeSigN :: Rule r (TType Range) -> Rule r (TId, TType Range) -> Rule r (TTypeSig Range)
typeSigN type' sig' =
  (\ss ty -> TTypeSig (foldMap (extract . snd) ss) ss ty)
    <$> many sig'
    <*> type'

typeSigType :: Rule r (TType Range) -> Rule r (TType Range)
typeSigType type' =
      typeParens type'
  <|> typeVar

typeApp :: Rule r (TType Range) -> Rule r (TType Range) -> Rule r (TType Range)
typeApp type1 type2 =
  (\t1 t2 -> TTApp (extract t1 <> extract t2) t1 t2)
    <$> type1
    <*> type2

typeVar :: Rule r (TType Range)
typeVar =
  fmap (\(t :@ a) -> TTVar a (TId t)) sigIdent

typeParens :: Rule r (TType Range) -> Rule r (TType Range)
typeParens =
  delimited TypeLParen TypeRParen (\a b ty -> setTTypeAnnotation (a <> b) ty)

sigIdent :: Rule r (Positioned Text)
sigIdent =
  E.terminal $ \case
    TypeIdent t :@ a ->
      pure (t :@ a)
    _ ->
      empty

-- -----------------------------------------------------------------------------

html :: Rule r (TNode Range) -> Grammar r (THtml Range)
html node' = mdo
  html1 <- E.rule $
        someHtml node'
    <?> "HTML"
  pure html1

someHtml :: Rule r (TNode Range) -> Rule r (THtml Range)
someHtml node' =
  (\nss -> THtml (someRange nss) (toList nss))
    <$> some' (node' <|> htmlPlain <|> htmlPreserveWhitespace)

htmlNode :: Rule r (TExpr Range) -> Rule r (THtml Range) -> Rule r (TNode Range)
htmlNode expr' html' =
      htmlVoidElement expr'
  <|> htmlElement expr' html'
  <|> htmlComment
  <|> htmlExpr expr'
  <|> htmlWS html'
  <|> htmlTextExpr expr'

htmlPlain :: Rule r (TNode Range)
htmlPlain =
  (\ne -> TPlain (extractPosition ne) (TPlainText (extractPositioned ne)))
    <$> htmlText

htmlPreserveWhitespace :: Rule r (TNode Range)
htmlPreserveWhitespace =
  E.terminal $ \case
    Whitespace x :@ a ->
      pure (TWhiteSpace a x)
    Newline :@ a ->
      pure (TNewline a)
    _ ->
      empty

htmlWS :: Rule r (THtml Range) -> Rule r (TNode Range)
htmlWS html' =
  (\a (THtml _ nodes) b -> THtmlWS (a <> b) nodes)
    <$> token ExprStartWS
    <*> html'
    <*> token ExprEndWS

htmlExpr :: Rule r (TExpr Range) -> Rule r (TNode Range)
htmlExpr expr' =
  (\a e b -> TExprNode (a <> b) e)
    <$> token ExprStart
    <*> expr'
    <*> token ExprEnd
    <?> "expression"

htmlTextExpr :: Rule r (TExpr Range) -> Rule r (TNode Range)
htmlTextExpr expr' =
  (\a e b -> TTextExprNode (a <> b) e)
    <$> token TextExprStart
    <*> expr'
    <*> token TextExprEnd
    <?> "text expression"

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
    <?> "element"

htmlTagClose :: Rule r (TTag Range)
htmlTagClose =
  (\a (tag :@ _) b -> TTag (a <> b) tag)
    <$> token TagCloseOpen
    <*> htmlTagIdent
    <*> token TagClose
    <?> "tag close"

htmlVoidElement :: Rule r (TExpr Range) -> Rule r (TNode Range)
htmlVoidElement expr' =
  (\a (tag :@ ta) attrs b -> TVoidElement (a <> b) (TTag ta tag) attrs)
    <$> token TagOpen
    <*> htmlTagIdent
    <*> many (htmlAttribute expr')
    <*> token TagSelfClose
    <?> "void element"

htmlComment :: Rule r (TNode Range)
htmlComment =
  (\a (t :@ _) _ b -> TComment (a <> b) (TPlainText t))
    <$> token TagCommentStart
    <*> htmlCommentText
    <*> token TagCommentEnd
    <*> token TagClose
    <?> "HTML comment"

htmlAttribute :: Rule r (TExpr Range) -> Rule r (TAttribute Range)
htmlAttribute expr' =
      htmlAttributeKV expr'
  <|> htmlAttributeEmpty
  <|> htmlAttributeExpr expr'
  <?> "attribute"

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
  <?> "attribute value"

htmlAttributeExpr :: Rule r (TExpr Range) -> Rule r (TAttribute Range)
htmlAttributeExpr expr' =
  (\a e b -> TAttributeExpr (a <> b) e)
    <$> token ExprStart
    <*> expr'
    <*> token ExprEnd

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

expr :: Rule r (THtml Range) -> Grammar r (TExpr Range)
expr html' = mdo
  expr4 <- E.rule $
        exprHtml html'
    <|> expr3
  expr3 <- E.rule $
        exprApp expr3 expr2
    <|> exprEach expr3 expr2
    <|> expr2
  expr2 <- E.rule $
        exprPrj expr2
    <|> expr1
  expr1 <- E.rule $
        exprLam expr4
    <|> exprCase expr4 pat1
    <|> exprList expr4
    <|> exprString expr4
    <|> exprVar
    <|> exprHole
    <|> exprParens expr4
  pat1 <- pattern
  pure expr4

exprParens :: Rule r (TExpr Range) -> Rule r (TExpr Range)
exprParens =
  delimited ExprLParen ExprRParen (\a b -> setTExprAnnotation (a <> b))

exprHtml :: Rule r (THtml Range) -> Rule r (TExpr Range)
exprHtml html' =
  (\n -> TENode (extract n) n)
    <$> html'
    <?> "HTML"

exprApp :: Rule r (TExpr Range) -> Rule r (TExpr Range) -> Rule r (TExpr Range)
exprApp expr' expr'' =
  (\e1 e2 -> TEApp (extract e1 <> extract e2) e1 e2)
    <$> expr'
    <*> expr''

exprEach :: Rule r (TExpr Range) -> Rule r (TExpr Range) -> Rule r (TExpr Range)
exprEach expr' expr'' =
  (\a e1 e2 -> TEEach (a <> extract e2) e1 e2)
    <$> token ExprEach
    <*> expr'
    <*> expr''

exprLam :: Rule r (TExpr Range) -> Rule r (TExpr Range)
exprLam expr' =
  (\a xs _ e -> TELam (a <> extract e) (fmap TId xs) e)
    <$> token ExprLamStart
    <*> some' (fmap extractPositioned exprVarId)
    <*> token ExprArrow
    <*> expr'
    <?> "lambda"

exprPrj :: Rule r (TExpr Range) -> Rule r (TExpr Range)
exprPrj expr' =
  (\e _ (fn :@ b) -> TEPrj (extract e <> b) e (TId fn))
    <$> expr'
    <*> token ExprDot
    <*> exprVarId

exprVar :: Rule r (TExpr Range)
exprVar =
  E.terminal
    (\case
       ExprVarId t :@ a ->
         pure (TEVar a (TId t))
       ExprConId t :@ a ->
         pure (TEVar a (TId t))
       _ ->
         empty)
    <?> "variable"

exprHole :: Rule r (TExpr Range)
exprHole =
  TEHole
    <$> token ExprHole
    <?> "hole"

exprList :: Rule r (TExpr Range) -> Rule r (TExpr Range)
exprList expr' =
  (\a es b -> TEList (a <> b) es)
    <$> token ExprListStart
    <*> sepBy expr' (token ExprListSep)
    <*> token ExprListEnd
    <?> "list"

exprCase :: Rule r (TExpr Range) -> Rule r (TPattern Range) -> Rule r (TExpr Range)
exprCase expr' pat' =
  (\a e _ alts -> TECase (a <> someRange alts) e alts)
    <$> token ExprCaseStart
    <*> expr'
    <*> token ExprCaseOf
    <*> exprAlts expr' pat'
    <?> "case"

exprAlts :: Rule r (TExpr Range) -> Rule r (TPattern Range) -> Rule r (NonEmpty (TAlt Range))
exprAlts expr' pat' =
  some' (exprAlt expr' pat')

exprAlt :: Rule r (TExpr Range) -> Rule r (TPattern Range) -> Rule r (TAlt Range)
exprAlt expr' pat' =
  (\p _ e b -> TAlt (extract p <> b) p e)
    <$> pat'
    <*> token ExprArrow
    <*> expr'
    <*> token ExprCaseSep

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
    <*> many (stringChunk <|> textExprChunk expr')
    <*> token StringEnd

stringChunk :: Rule r (TIChunk Range)
stringChunk =
  E.terminal $ \case
    StringChunk t :@ a ->
      pure (TStringChunk a t)
    _ ->
      empty

textExprChunk :: Rule r (TExpr Range) -> Rule r (TIChunk Range)
textExprChunk expr' =
  (\a e b -> TExprChunk (a <> b) e)
    <$> token TextExprStart
    <*> expr'
    <*> token TextExprEnd

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
