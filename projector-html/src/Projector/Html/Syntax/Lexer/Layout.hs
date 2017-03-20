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
  deriving (Eq, Ord, Show)

newtype IndentLevel = IndentLevel {
    _unIndentLevel :: Int
  } deriving (Eq, Ord, Show)


-- -----------------------------------------------------------------------------

-- peek, decide to enter sig mode or html mode.
-- take care of leading indentation when it exists
applyLayout'' :: [IndentLevel] -> [Positioned Token] -> [Positioned Token]
applyLayout'' il xs@(TypeSigStart :@ _ : _) =
  applyLayout' [TypeSigMode, HtmlMode] il [] xs
applyLayout'' il xs =
  applyLayout' [HtmlMode] il [] xs


applyLayout' :: [LexerMode] -> [IndentLevel] -> [Scope] -> [Positioned Token] -> [Positioned Token]


--
-- type signatures
--

-- Drop out of signature mode on sig end, with optional newline
applyLayout' (TypeSigMode : ms) il ss (end@(TypeSigEnd :@ _) : Newline :@ _ : xs) =
  end : applyLayout' ms il ss xs
applyLayout' (TypeSigMode : ms) il ss (end@(TypeSigEnd :@ _) : xs) =
  end : applyLayout' ms il ss xs

-- Drop whitespace and newlines in the type signature
applyLayout' mms@(TypeSigMode : _) il ss (Whitespace _ :@ _ : xs) =
  applyLayout' mms il ss xs
applyLayout' mms@(TypeSigMode : _) il ss (Newline :@ _ : xs) =
  applyLayout' mms il ss xs


--
-- html mode
--

-- Drop into expr mode on left brace
applyLayout' mms@(HtmlMode : _) il ss (est@(ExprStart :@ _) : xs) =
  est : applyLayout' (ExprMode : mms) il (Brace : ss) xs

-- Drop into tag open mode on tagopen
applyLayout' mms@(HtmlMode : _) il ss (top@(TagOpen :@ _) : xs) =
  top : applyLayout' (TagOpenMode : mms) il ss xs

-- Drop into tag close mode on tag close
applyLayout' (HtmlMode : ms) il ss (tcl@(TagCloseOpen :@ _) : xs) =
  tcl : applyLayout' (TagCloseMode : ms) il ss xs


--
-- tag open mode
--

-- Drop into html mode on tagclose
applyLayout' (TagOpenMode : ms) il ss (tcl@(TagClose :@ _) : xs) =
  tcl : applyLayout' (HtmlMode : ms) il ss xs

-- drop into expr mode on tagopen
applyLayout' mms@(TagOpenMode : _) il ss (est@(ExprStart :@ _) : xs) =
  est : applyLayout' (ExprMode : mms) il (Brace : ss) xs

-- Pop mode on tagselfclose
applyLayout' (TagOpenMode : ms) il ss (tsc@(TagSelfClose :@ _) : xs) =
  tsc : applyLayout' ms il ss xs

-- Drop whitespace, newlines
applyLayout' mms@(TagOpenMode : _) il ss (Whitespace _ :@ _ : xs) =
  applyLayout' mms il ss xs
applyLayout' mms@(TagOpenMode : _) il ss (Newline :@ _ : xs) =
  applyLayout' mms il ss xs

--
-- tag close mode
--

-- Pop mode on tag close
applyLayout' (TagCloseMode : ms) il ss (tcl@(TagClose :@ _) : xs) =
  tcl : applyLayout' ms il ss xs

-- Drop whitespace, newlines
applyLayout' mms@(TagCloseMode : _) il ss (Whitespace _ :@ _ : xs) =
  applyLayout' mms il ss xs
applyLayout' mms@(TagCloseMode : _) il ss (Newline :@ _ : xs) =
  applyLayout' mms il ss xs


--
-- expr mode
--

-- Drop into tag open mode on tagopen
applyLayout' mms@(ExprMode : _) il ss (top@(TagOpen :@ _) : xs) =
  top : applyLayout' (TagOpenMode : mms) il ss xs

-- Pop mode on expr end
applyLayout' (ExprMode : ms) il ss ((ExprEnd :@ a) : xs) =
  let (toks, sss) = first (fmap (:@ a)) (closeScopes Brace ss) in
  toks <> applyLayout' ms il sss xs

-- Nested expr mode
applyLayout' mms@(ExprMode : _) il ss (est@(ExprStart :@ _) : xs) =
  est : applyLayout' (ExprMode : mms) il (Brace : ss) xs

-- Enter block scope on lambda
applyLayout' mms@(ExprMode : _) il ss (est@(ExprLamStart :@ a) : xs) =
  ExprLParen :@ a : est : applyLayout' mms il (Block : ss) xs

-- Enter Cases block scope on case start
applyLayout' mms@(ExprMode : _) il ss (est@(ExprCaseStart :@ a) : xs) =
  ExprLParen :@ a : est : applyLayout' mms il (Cases : ss) xs

-- Enter pattern mode on case of
applyLayout' mms@(ExprMode : _) il ss (cof@(ExprCaseOf :@ _) : xs) =
  cof : applyLayout' (ExprPatternMode : mms) il ss xs

-- Enter block scope on arrow
applyLayout' mms@(ExprMode : _) il ss (arr@(ExprArrow :@ a) : xs) =
  arr : ExprLParen :@ a : applyLayout' mms il (Block : ss) xs

-- close block scope on casesep, push pattern mode
applyLayout' mms@(ExprMode : _) il ss (est@(ExprCaseSep :@ a) : xs) =
  let (toks, sss) = first (fmap (:@ a)) (closeScopes CaseAlt ss) in
  toks <> (est : applyLayout' (ExprPatternMode : mms) il sss xs)

-- enter paren scopes on left paren
applyLayout' mms@(ExprMode : _) il ss (elp@(ExprLParen :@ _) : xs) =
  elp : applyLayout' mms il (Paren : ss) xs

-- close scopes on right paren
applyLayout' mms@(ExprMode : _) il ss ((ExprRParen :@ a) : xs) =
  let (toks, sss) = first (fmap (:@ a)) (closeScopes Paren ss) in
  toks <> applyLayout' mms il sss xs

-- Track indent/dedent
applyLayout' ms@(ExprMode : _) il ss ((Newline :@ _) : (Whitespace x :@ b) : xs) =
  newline ms il ss b x xs
applyLayout' ms@(ExprMode : _) il ss ((Newline :@ _) : xs@(_ :@ b : _)) =
  newline ms il ss b 0 xs

-- Drop whitespace and newlines
applyLayout' mms@(ExprMode : _) il ss (Whitespace _ :@ _ : xs) =
  applyLayout' mms il ss xs
applyLayout' mms@(ExprMode : _) il ss (Newline :@ _ : xs) =
  applyLayout' mms il ss xs


--
-- expr pat mode
--

-- open case scope and pop mode on arrow
applyLayout' (ExprPatternMode : ms) il ss (arr@(ExprArrow :@ a) : xs) =
  arr : ExprLParen :@ a : applyLayout' ms il (CaseAlt : ss) xs

-- Track indent/dedent
applyLayout' ms@(ExprPatternMode : _) il ss ((Newline :@ _) : (Whitespace x :@ b) : xs) =
  newline ms il ss b x xs
applyLayout' ms@(ExprPatternMode : _) il ss ((Newline :@ _) : xs@(_ :@ b : _)) =
  newline ms il ss b 0 xs

-- Handle left and right parens
applyLayout' ms@(ExprPatternMode : _) il ss (elp@(ExprLParen :@ _) : xs) =
  elp : applyLayout' ms il (Paren : ss) xs
applyLayout' mms@(ExprPatternMode : _) il ss ((ExprRParen :@ a) : xs) =
  let (toks, sss) = first (fmap (:@ a)) (closeScopes Paren ss) in
  toks <> applyLayout' mms il sss xs

-- pop mode on expr brace
applyLayout' (ExprPatternMode : ms) il ss ((ExprEnd :@ a) : xs) =
  let (toks, sss) = first (fmap (:@ a)) (closeScopes Brace ss) in
  toks <> applyLayout' ms il sss xs

-- drop whitespace and newlines
applyLayout' mms@(ExprPatternMode : _) il ss (Whitespace _ :@ _ : xs) =
  applyLayout' mms il ss xs
applyLayout' mms@(ExprPatternMode : _) il ss (Newline :@ _ : xs) =
  applyLayout' mms il ss xs


--
-- General
--

-- Drop expr comments
applyLayout' ms il ss (ExprCommentStart :@ _ : xs) =
  applyLayout' ms il ss xs
applyLayout' ms il ss (ExprCommentChunk _ :@ _ : xs) =
  applyLayout' ms il ss xs
applyLayout' ms il ss (ExprCommentEnd :@ _ : xs) =
  applyLayout' ms il ss xs


-- Drop trailing newline
applyLayout' _ _ _ (Newline :@ _ : []) =
  []

-- Pass over any ignored tokens
applyLayout' ms il ss (x:xs) =
  x : applyLayout' ms il ss xs

applyLayout' _ _ _ [] =
  []

-- -----------------------------------------------------------------------------


-- | Given a new indent level, insert indent or dedent tokens
-- accordingly, then continue with applyLayout.
newline :: [LexerMode] -> [IndentLevel] -> [Scope] -> Range -> Int -> [Positioned Token] -> [Positioned Token]

newline ms iis@(IndentLevel i : is) ss a x xs
  | i == x =
    -- same level
    -- this is reason enough to close a case
    case ss of
      CaseAlt : _ ->
        let (toks, sss) = first (fmap (:@ a)) (closeScopes Indent ss) in
        toks <> applyLayout' (ExprPatternMode : ms) iis sss xs
      _ ->
        applyLayout' ms iis ss xs

  | i > x =
    -- indent decreased
    -- close an indent scope
    let (toks, sss) = first (fmap (:@ a)) (closeScopes Indent ss) in
    toks <> newline ms is sss a x xs

  | otherwise {- i < x -} =
    -- indent increased
    -- open an indent scope
    applyLayout' ms (IndentLevel x : iis) (Indent : ss) xs

newline ms [] ss _a x xs
  | x == 0 =
    -- initial unindented
    applyLayout' ms [] ss xs

  | otherwise =
    -- initially indented
    applyLayout' ms [IndentLevel x] (Indent : ss) xs


-- -----------------------------------------------------------------------------

closeScopes :: Scope -> [Scope] -> ([Token], [Scope])
closeScopes s sss =
  -- trace (show s <> " closes " <> show result <> " from " <> show sss)  $
  result
  where
    result = splits (go (fmap (closeScope s) sss))
    splits :: [[Token]] -> ([Token], [Scope])
    splits ts = (fold ts, L.drop (length ts) sss)
    go [] = []
    go (Stop : _) = []
    go (CloseAndStop t : _) = [t]
    go (Continue : cs) = [] : go cs
    go (CloseAndContinue t : cs) = t : go cs


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
-- parens cant close braces or cases
closeScope Paren Brace = Stop


closeScope CaseAlt CaseAlt = CloseAndStop [ExprRParen]
closeScope CaseAlt Block = CloseAndContinue [ExprRParen]
closeScope CaseAlt Indent = Continue
-- ; can't close cases, parens or braces
closeScope CaseAlt Cases = Stop
closeScope CaseAlt Paren = Stop
closeScope CaseAlt Brace = Stop

-- soft indent closes itself
closeScope Indent Indent = CloseAndStop []
-- soft indent can close block scopes
closeScope Indent Block = CloseAndStop [ExprRParen]
-- soft indent can inject case separators
closeScope Indent Cases = CloseAndStop [ExprRParen]
closeScope Indent CaseAlt = CloseAndStop [ExprRParen, ExprCaseSep]
-- soft indent can't close braces or parens
closeScope Indent Brace = Stop
closeScope Indent Paren = Stop

-- block scopes are only closed by indentation, they don't appear on lhs
closeScope Block _ = Stop

-- cases are only closed by parens/indent, they don't appear on lhs
closeScope Cases _ = Stop

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
