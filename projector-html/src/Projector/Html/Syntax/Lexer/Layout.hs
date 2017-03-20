{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Syntax.Lexer.Layout (
    layout
  , isBalanced
  ) where


import qualified Data.List as L

import           P

import           Projector.Html.Data.Position
import           Projector.Html.Syntax.Token


-- | Eliminate all expression whitespace in favour of explicit groupings.
-- Minimise HTML whitespace as far as possible.
layout :: [Positioned Token] -> [Positioned Token]
layout =
  applyLayout'' []

-- -----------------------------------------------------------------------------

data Scope =
    Cases -- expecting block indent, ) or }
  | CaseAlt -- expecting case separator
  | Paren -- explicit parentheses
  | Brace -- explicit braces { }
  | Block -- an implicit scope for which we inject parens
  | Indent -- an implicit scope we ignore for indent tracking purposes
  | Html -- an implicit scope for html closed only by brace, eof, indent
  deriving (Eq, Ord, Show)

newtype IndentLevel = IndentLevel {
    _unIndentLevel :: Int
  } deriving (Eq, Ord, Show)


-- -----------------------------------------------------------------------------

-- peek, decide to enter sig mode or html mode.
-- take care of leading indentation when it exists
applyLayout'' :: [IndentLevel] -> [Positioned Token] -> [Positioned Token]
applyLayout'' il xs@(TypeSigStart :@ a : _) =
  applyLayout' a [TypeSigMode, ExprMode] il [] xs
applyLayout'' il xs@(_ :@ a : _) =
  applyLayout' a [ExprMode] il [] xs
applyLayout'' _ [] =
  []


applyLayout' :: Range -> [LexerMode] -> [IndentLevel] -> [Scope] -> [Positioned Token] -> [Positioned Token]


--
-- type signatures
--

-- Drop out of signature mode on sig end, with optional newline
applyLayout' _ (TypeSigMode : ms) il ss (end@(TypeSigEnd :@ _) : Newline :@ z : xs) =
  end : applyLayout' z ms il ss xs
applyLayout' _ (TypeSigMode : ms) il ss (end@(TypeSigEnd :@ z) : xs) =
  end : applyLayout' z ms il ss xs

-- Drop whitespace and newlines in the type signature
applyLayout' _ mms@(TypeSigMode : _) il ss (Whitespace _ :@ z : xs) =
  applyLayout' z mms il ss xs
applyLayout' _ mms@(TypeSigMode : _) il ss (Newline :@ z : xs) =
  applyLayout' z mms il ss xs


--
-- html mode
--

-- Drop into expr mode on left brace
applyLayout' _ mms@(HtmlMode : _) il ss (est@(ExprStart :@ z) : xs) =
  est : applyLayout' z (ExprMode : mms) il (Brace : ss) xs

-- Drop into tag open mode on tagopen
applyLayout' _ mms@(HtmlMode : _) il ss (top@(TagOpen :@ z) : xs) =
  top : applyLayout' z (TagOpenMode : mms) il ss xs

-- Drop into tag close mode on tag close
applyLayout' _ (HtmlMode : ms) il ss (tcl@(TagCloseOpen :@ z) : xs) =
  tcl : applyLayout' z (TagCloseMode : ms) il ss xs

-- Close scopes and pop layout on expr end
applyLayout' _ (HtmlMode : ExprMode : ms) il ss (ExprEnd :@ a : xs) =
  closeEmThen a Brace ss il $ \ils sss toks ->
    toks <> applyLayout' a ms ils sss xs

-- Close scopes and pop layout on right paren
applyLayout' _ (HtmlMode : ms) il ss (ExprRParen :@ a : xs) =
  closeEmThen a Paren ss il $ \ils sss toks ->
    toks <> applyLayout' a ms ils sss xs

-- Track indent/dedent
applyLayout' _ ms@(HtmlMode : _) il ss (n@(Newline :@ _) : (Whitespace x :@ b) : xs) =
  n : newline ms il ss b x xs
applyLayout' _ ms@(HtmlMode : _) il ss (n1@(Newline :@ a) : n2@(Newline :@ _) : xs) =
  n1 : applyLayout' a ms il ss (n2:xs)
applyLayout' _ ms@(HtmlMode : _) il ss (n@(Newline :@ _) : xs@(_ :@ b : _)) =
  n : newline ms il ss b 0 xs


--
-- tag open mode
--

-- Drop into html mode on tagclose
applyLayout' _ (TagOpenMode : ms) il ss (tcl@(TagClose :@ z) : xs) =
  tcl : applyLayout' z (HtmlMode : ms) il ss xs

-- drop into expr mode on tagopen
applyLayout' _ mms@(TagOpenMode : _) il ss (est@(ExprStart :@ z) : xs) =
  est : applyLayout' z (ExprMode : mms) il (Brace : ss) xs

-- Pop mode on tagselfclose
applyLayout' _ (TagOpenMode : ms) il ss (tsc@(TagSelfClose :@ z) : xs) =
  tsc : applyLayout' z ms il ss xs

-- Drop whitespace, newlines
applyLayout' _ mms@(TagOpenMode : _) il ss (Whitespace _ :@ z : xs) =
  applyLayout' z mms il ss xs
applyLayout' _ mms@(TagOpenMode : _) il ss (Newline :@ z : xs) =
  applyLayout' z mms il ss xs

--
-- tag close mode
--

-- Pop mode on tag close
applyLayout' _ (TagCloseMode : ms) il ss (tcl@(TagClose :@ z) : xs) =
  tcl : applyLayout' z ms il ss xs

-- Drop whitespace, newlines
applyLayout' _ mms@(TagCloseMode : _) il ss (Whitespace _ :@ z : xs) =
  applyLayout' z mms il ss xs
applyLayout' _ mms@(TagCloseMode : _) il ss (Newline :@ z : xs) =
  applyLayout' z mms il ss xs

--
-- html comment mode
--

-- Pop mode on tag close
applyLayout' _ (HtmlCommentMode : ms) il ss (tce@(TagCommentEnd :@ _) : tc@(TagClose :@ z) : xs) =
  tce : tc : applyLayout' z ms il ss xs

--
-- string mode
--

-- Pop mode on string end
applyLayout' _ (StringMode : ms) il ss (ese@(StringEnd :@ a) : xs) =
  ese : applyLayout' a ms il ss xs

-- Push expr mode on exprstart
applyLayout' _ mms@(StringMode : _) il ss (est@(ExprStart :@ z) : xs) =
  est : applyLayout' z (ExprMode : mms) il (Brace : ss) xs

--
-- expr mode
--

-- Drop into tag open mode on tagopen
applyLayout' _ mms@(ExprMode : _) il ss (top@(TagOpen :@ a) : xs) =
  ExprLParen :@ a : top : applyLayout' a (TagOpenMode : HtmlMode : mms) il (Html : ss) xs

-- Likewise for TagCommentStart
applyLayout' _ mms@(ExprMode : _) il ss (tcs@(TagCommentStart :@ a) : xs) =
  ExprLParen :@ a : tcs : applyLayout' a (HtmlCommentMode : HtmlMode : mms) il (Html : ss) xs

-- Enter string mode on string start
applyLayout' _ mms@(ExprMode : _) il ss (ess@(StringStart :@ a) : xs) =
  ess : applyLayout' a (StringMode : mms) il ss xs

-- Pop mode on expr end
applyLayout' _ (ExprMode : ms) il ss ((ExprEnd :@ a) : xs) =
  closeEmThen a Brace ss il $ \ils sss toks ->
    toks <> applyLayout' a ms ils sss xs

-- Nested expr mode
applyLayout' _ mms@(ExprMode : _) il ss (est@(ExprStart :@ z) : xs) =
  ExprLParen :@ z : est : applyLayout' z (ExprMode : HtmlMode : mms) il (Brace : Html : ss) xs

-- Enter block scope on lambda
applyLayout' _ mms@(ExprMode : _) il ss (est@(ExprLamStart :@ a) : xs) =
  ExprLParen :@ a : est : applyLayout' a mms il (Block : ss) xs

-- Enter Cases block scope on case start
applyLayout' _ mms@(ExprMode : _) il ss (est@(ExprCaseStart :@ a) : xs) =
  ExprLParen :@ a : est : applyLayout' a mms il (Cases : ss) xs

-- Enter pattern mode on case of
applyLayout' _ mms@(ExprMode : _) il ss (cof@(ExprCaseOf :@ z) : xs) =
  cof : applyLayout' z (ExprPatternMode : mms) il ss xs

-- Enter block scope on arrow
applyLayout' _ mms@(ExprMode : _) il ss (arr@(ExprArrow :@ a) : xs) =
  arr : ExprLParen :@ a : applyLayout' a mms il (Block : ss) xs

-- close block scope on casesep, push pattern mode
applyLayout' _ mms@(ExprMode : _) il ss (est@(ExprCaseSep :@ a) : xs) =
  closeEmThen a CaseAlt ss il $ \ils sss toks ->
    toks <> (est : applyLayout' a (ExprPatternMode : mms) ils sss xs)

-- enter paren scopes on left paren
applyLayout' _ mms@(ExprMode : _) il ss (elp@(ExprLParen :@ z) : xs) =
  elp : applyLayout' z mms il (Paren : ss) xs

-- close scopes on right paren
applyLayout' _ mms@(ExprMode : _) il ss ((ExprRParen :@ a) : xs) =
  closeEmThen a Paren ss il $ \ils sss toks ->
    toks <> applyLayout' a mms ils sss xs

-- Track indent/dedent
applyLayout' _ ms@(ExprMode : _) il ss ((Newline :@ _) : (Whitespace x :@ b) : xs) =
  newline ms il ss b x xs
applyLayout' _ ms@(ExprMode : _) il ss (Newline :@ a : n2@(Newline :@ _) : xs) =
  applyLayout' a ms il ss (n2:xs)
applyLayout' _ ms@(ExprMode : _) il ss ((Newline :@ _) : xs@(_ :@ b : _)) =
  newline ms il ss b 0 xs

-- Drop whitespace and newlines
applyLayout' _ mms@(ExprMode : _) il ss (Whitespace _ :@ z : xs) =
  applyLayout' z mms il ss xs
applyLayout' _ mms@(ExprMode : _) il ss (Newline :@ z : xs) =
  applyLayout' z mms il ss xs


--
-- expr pat mode
--

-- open case scope and pop mode on arrow
applyLayout' _ (ExprPatternMode : ms) il ss (arr@(ExprArrow :@ a) : xs) =
  arr : ExprLParen :@ a : applyLayout' a ms il (CaseAlt : ss) xs

-- Track indent/dedent
applyLayout' _ ms@(ExprPatternMode : _) il ss ((Newline :@ _) : (Whitespace x :@ b) : xs) =
  newline ms il ss b x xs
applyLayout' _ ms@(ExprPatternMode : _) il ss (n1@(Newline :@ a) : n2@(Newline :@ _) : xs) =
  n1 : applyLayout' a ms il ss (n2:xs)
applyLayout' _ ms@(ExprPatternMode : _) il ss ((Newline :@ _) : xs@(_ :@ b : _)) =
  newline ms il ss b 0 xs

-- Handle left and right parens
applyLayout' _ ms@(ExprPatternMode : _) il ss (elp@(ExprLParen :@ z) : xs) =
  elp : applyLayout' z ms il (Paren : ss) xs
applyLayout' _ mms@(ExprPatternMode : _) il ss ((ExprRParen :@ a) : xs) =
  closeEmThen a Paren ss il $ \ils sss toks ->
    toks <> applyLayout' a mms ils sss xs

-- pop mode on expr brace
applyLayout' _ (ExprPatternMode : ms) il ss ((ExprEnd :@ a) : xs) =
  closeEmThen a Brace ss il $ \ils sss toks ->
    toks <> applyLayout' a ms ils sss xs

-- drop whitespace and newlines
applyLayout' _ mms@(ExprPatternMode : _) il ss (Whitespace _ :@ z : xs) =
  applyLayout' z mms il ss xs
applyLayout' _ mms@(ExprPatternMode : _) il ss (Newline :@ z : xs) =
  applyLayout' z mms il ss xs


--
-- General
--

-- Drop expr comments
applyLayout' _ ms il ss (ExprCommentStart :@ z : xs) =
  applyLayout' z ms il ss xs
applyLayout' _ ms il ss (ExprCommentChunk _ :@ z : xs) =
  applyLayout' z ms il ss xs
applyLayout' _ ms il ss (ExprCommentEnd :@ z : xs) =
  applyLayout' z ms il ss xs


-- Drop trailing newline and close implicit scopes at EOF
applyLayout' _ _ il ss (Newline :@ a : []) =
  fmap (:@ a) (closeImplicitScopes ss il)

-- Pass over any ignored tokens
applyLayout' _ ms il ss (x:xs) =
  x : applyLayout' (extractPosition x) ms il ss xs

-- lose implicit scopes at EOF
applyLayout' a _ il ss [] =
  fmap (:@ a) (closeImplicitScopes ss il)

closeEmThen ::
     Range
  -> Scope
  -> [Scope]
  -> [IndentLevel]
  -> ([IndentLevel] -> [Scope] -> [Positioned Token] -> [Positioned Token])
  -> [Positioned Token]
closeEmThen a s ss il k =
  maybe
    (k il ss [])
    (\(toks, ils, sss) -> k ils sss (fmap (:@ a) toks))
    (closeScopes s ss il)


-- -----------------------------------------------------------------------------


-- | Given a new indent level, insert indent or dedent tokens
-- accordingly, then continue with applyLayout.
newline :: [LexerMode] -> [IndentLevel] -> [Scope] -> Range -> Int -> [Positioned Token] -> [Positioned Token]

newline ms iis@(IndentLevel i : _) ss a x xs
  | i == x =
    -- same level
    -- this is reason enough to close a case
    -- trace (show ms) $
    case ss of
      CaseAlt : _ ->
        closeEmThen a Indent ss iis $ \iiis sss toks ->
          toks <> applyLayout' a (ExprPatternMode : ms) iiis sss xs
      _ ->
        applyLayout' a ms iis ss xs

  | i > x =
    -- indent decreased
    -- close an indent scope
    -- trace (show ms) $
    maybe
      (applyLayout' a ms iis ss xs)
      (\(toks, iiis, sss) -> fmap (:@ a) toks <> newline ms iiis sss a x xs)
      (closeScopes Indent ss iis)

  | otherwise {- i < x -} =
    -- indent increased
    -- open an indent scope
    applyLayout' a ms (IndentLevel x : iis) (Indent : ss) xs

newline ms [] ss a x xs
  | x == 0 =
    -- initial unindented
    applyLayout' a ms [] ss xs

  | otherwise =
    -- initially indented
    applyLayout' a ms [IndentLevel x] (Indent : ss) xs


-- -----------------------------------------------------------------------------

closeScopes :: Scope -> [Scope] -> [IndentLevel] -> Maybe ([Token], [IndentLevel], [Scope])
closeScopes s =
  --trace (show s) .
  closeScopes' (closeScope s)

closeScopes' :: (Scope -> ScopeClose) -> [Scope] -> [IndentLevel] -> Maybe ([Token], [IndentLevel], [Scope])
closeScopes' fs sss ils =
  --trace ("closes " <> show result <> " from " <> show sss <> "(" <> show ils <> ")")  $
  result
  where
    result = splits (go (fmap fs sss))
    splits :: [[Token]] -> Maybe ([Token], [IndentLevel], [Scope])
    splits ts = if null ts then Nothing else Just (fold ts, ilevels ts, L.drop (length ts) sss)
    ilevels ts = L.drop (length (L.filter (== Indent) (L.take (length ts) sss))) ils
    go [] = []
    go (Stop : _) = []
    go (CloseAndStop t : _) = [t]
    go (Continue : cs) = [] : go cs
    go (CloseAndContinue t : cs) = t : go cs

closeImplicitScopes :: [Scope] -> [IndentLevel] -> [Token]
closeImplicitScopes ss ils =
  fromMaybe [] $ (\(ts, _, _) -> ts) <$> closeScopes' (\case
    Indent -> Continue
    Block -> CloseAndContinue [ExprRParen]
    Cases -> CloseAndContinue [ExprRParen]
    CaseAlt -> CloseAndContinue [ExprRParen, ExprCaseSep]
    Html -> CloseAndContinue [ExprRParen]
    Brace -> Stop
    Paren -> Stop)
    ss
    ils

data ScopeClose =
    CloseAndStop [Token]
  | CloseAndContinue [Token]
  | Continue
  | Stop
  deriving (Eq, Ord, Show)

-- | Which scopes can close which.
closeScope :: Scope -> Scope -> ScopeClose

-- brace scopes are closed by braces
closeScope Brace Brace = CloseAndStop [ExprEnd]
-- braces close blocks
closeScope Brace Block = CloseAndContinue [ExprRParen]
-- braces close case statements
closeScope Brace Cases = CloseAndContinue [ExprRParen]
-- braces close case alts
closeScope Brace CaseAlt = CloseAndContinue [ExprRParen, ExprCaseSep]
-- braces close soft indent
closeScope Brace Indent = Continue
-- braces close html
closeScope Brace Html = CloseAndContinue [ExprRParen]
-- braces can't close explicit parens
closeScope Brace Paren = Stop

-- parens close other parens
closeScope Paren Paren = CloseAndStop [ExprRParen]
-- parens can close blocks
closeScope Paren Block = CloseAndContinue [ExprRParen]
-- parens can clsoe soft indent
closeScope Paren Indent = Continue
closeScope Paren Cases = CloseAndContinue [ExprRParen]
closeScope Paren CaseAlt = CloseAndContinue [ExprRParen, ExprCaseSep]
closeScope Paren Html = CloseAndContinue [ExprRParen]
-- parens cant close braces or cases
closeScope Paren Brace = Stop


closeScope CaseAlt CaseAlt = CloseAndStop [ExprRParen]
closeScope CaseAlt Block = CloseAndContinue [ExprRParen]
closeScope CaseAlt Indent = Continue
closeScope CaseAlt Html = CloseAndContinue [ExprRParen]
-- ; can't close cases, parens or braces
closeScope CaseAlt Cases = Stop
closeScope CaseAlt Paren = Stop
closeScope CaseAlt Brace = Stop


-- soft indent closes itself
closeScope Indent Indent = CloseAndStop []
-- soft indent can close block scopes
closeScope Indent Block = CloseAndContinue [ExprRParen]
-- soft indent can inject case separators
closeScope Indent Cases = CloseAndContinue [ExprRParen]
closeScope Indent CaseAlt = CloseAndStop [ExprRParen, ExprCaseSep]
-- soft indent can't close braces or parens
closeScope Indent Brace = Stop
closeScope Indent Paren = Stop
closeScope Indent Html = CloseAndContinue [ExprRParen]

-- block scopes are only closed by indentation, they don't appear on lhs
closeScope Block _ = Stop

-- cases are only closed by parens/indent, they don't appear on lhs
closeScope Cases _ = Stop

-- html does not appear on lhs
closeScope Html _ = Stop

-- Maybe this should be run as validation?
isBalanced :: [Token] -> Bool
isBalanced toks =
  go [] toks
  where
    go (ExprLParen : xs) (ExprRParen : ts) =
      go xs ts
    go _ (ExprRParen : _) =
      False
    go xs (ExprLParen : ts) =
      go (ExprLParen : xs) ts
    go (ExprStart : xs) (ExprEnd : ts) =
      go xs ts
    go _ (ExprEnd : _) =
      False
    go xs (ExprStart : ts) =
      go (ExprStart : xs) ts
    go xs (_:ts) =
      go xs ts
    go [] [] =
      True
    go __ [] =
      False
