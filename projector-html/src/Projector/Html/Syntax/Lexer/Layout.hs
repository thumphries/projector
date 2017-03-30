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
  | LineBrace -- whitespace braces {| |}
  | Block -- an implicit scope for which we inject parens
  | Indent -- an implicit scope we ignore for indent tracking purposes
  | Html -- an implicit scope for html closed only by brace, eof, indent
  deriving (Eq, Ord, Show)

newtype IndentLevel = IndentLevel {
    _unIndentLevel :: Int
  } deriving (Eq, Ord, Show)

data WSMode =
    PreserveWS
  | CollapseWS
  deriving (Eq, Ord, Show)

wsf :: [WSMode] -> Positioned Token -> [Positioned Token]
wsf (PreserveWS : _) t = [t]
wsf (CollapseWS : _) _ = []
wsf [] t = [t]

wnl :: Range -> [WSMode] -> Positioned Token
wnl a (CollapseWS : _) = Whitespace 1 :@ a
wnl a _ = Newline :@ a

-- -----------------------------------------------------------------------------

-- peek, decide to enter sig mode or html mode.
-- take care of leading indentation when it exists
applyLayout'' :: [IndentLevel] -> [Positioned Token] -> [Positioned Token]
applyLayout'' il xs@(TypeSigStart :@ a : _) =
  applyLayout' a [TypeSigMode, ExprMode] [CollapseWS] il [] xs
applyLayout'' il xs@(_ :@ a : _) =
  applyLayout' a [ExprMode] [CollapseWS] il [] xs
applyLayout'' _ [] =
  []


applyLayout' :: Range -> [LexerMode] -> [WSMode] -> [IndentLevel] -> [Scope] -> [Positioned Token] -> [Positioned Token]


--
-- type signatures
--

-- Drop out of signature mode on sig end, with optional newline
applyLayout' _ (TypeSigMode : ms) ws il ss (end@(TypeSigEnd :@ _) : Newline :@ z : xs) =
  end : applyLayout' z ms ws il ss xs
applyLayout' _ (TypeSigMode : ms) ws il ss (end@(TypeSigEnd :@ z) : xs) =
  end : applyLayout' z ms ws il ss xs

-- Drop whitespace and newlines in the type signature
applyLayout' _ mms@(TypeSigMode : _) ws il ss (Whitespace _ :@ z : xs) =
  applyLayout' z mms ws il ss xs
applyLayout' _ mms@(TypeSigMode : _) ws il ss (Newline :@ z : xs) =
  applyLayout' z mms ws il ss xs


--
-- whitespace sensitivity
--

-- Push whitespace mode and go to HTMLMode on {|
applyLayout' _ mms@(ExprMode : _) ws il ss (est@(ExprStartWS :@ z) : xs) =
  ExprLParen :@ z : est : applyLayout' z (HtmlMode : HtmlMode : mms) (PreserveWS : ws) il (LineBrace : Html : ss) xs
applyLayout' _ mms@(HtmlMode : _) ws il ss (est@(ExprStartWS :@ z) : xs) =
  est : applyLayout' z (HtmlMode : mms) (PreserveWS : ws) il (LineBrace : ss) xs
applyLayout' _ ms ws il ss (est@(ExprStartWS :@ z) : xs) =
  est : applyLayout' z (HtmlMode : ms) (PreserveWS : ws) il (LineBrace : ss) xs

-- Pop whitespace mode and close scopes on |} on |}
applyLayout' _ (HtmlMode : ms) (PreserveWS : ws) il ss ((ExprEndWS :@ z) : xs) =
  closeEmThen z LineBrace ss il $ \ils sss toks ->
    toks <> applyLayout' z ms ws ils sss xs
applyLayout' _ ms (PreserveWS : ws) il ss ((ExprEndWS :@ z) : xs) =
  closeEmThen z LineBrace ss il $ \ils sss toks ->
    toks <> applyLayout' z ms ws ils sss xs

--
-- html mode
--

-- Drop into expr mode on left brace
applyLayout' _ mms@(HtmlMode : _) ws il ss (est@(ExprStart :@ z) : xs) =
  est : applyLayout' z (ExprMode : mms) ws il (Brace : ss) xs

-- Drop into tag open mode on tagopen
applyLayout' _ mms@(HtmlMode : _) ws il ss (top@(TagOpen :@ z) : xs) =
  top : applyLayout' z (TagOpenMode : mms) ws il ss xs

-- Drop into tag close mode on tag close
applyLayout' _ (HtmlMode : ms) ws il ss (tcl@(TagCloseOpen :@ z) : xs) =
  tcl : applyLayout' z (TagCloseMode : ms) ws il ss xs

-- Close scopes and pop layout on expr end
applyLayout' _ (HtmlMode : ExprMode : ms) ws il ss (ExprEnd :@ a : xs) =
  closeEmThen a Brace ss il $ \ils sss toks ->
    toks <> applyLayout' a ms ws ils sss xs

applyLayout' _ (HtmlMode : ms) ws il ss (ExprEnd :@ a : xs) =
  closeEmThen a Brace ss il $ \ils sss toks ->
    toks <> applyLayout' a ms ws ils sss xs


-- Close scopes and pop layout on right paren
applyLayout' _ (HtmlMode : ms) ws il ss (ExprRParen :@ a : xs) =
  closeEmThen a Paren ss il $ \ils sss toks ->
    toks <> applyLayout' a ms ws ils sss xs

-- Track indent/dedent
applyLayout' _ ms@(HtmlMode : _) ws il ss (Newline :@ z : w@(Whitespace x :@ b) : xs) =
  wnl z ws : wsf ws w <> newline ms ws il ss b x xs
applyLayout' _ ms@(HtmlMode : _) ws il ss (Newline :@ a : n2@(Newline :@ _) : xs) =
  wnl a ws : applyLayout' a ms ws il ss (n2:xs)
applyLayout' _ ms@(HtmlMode : _) ws il ss (Newline :@ a : xs@(_ :@ b : _)) =
  wnl a ws : newline ms ws il ss b 0 xs

-- Collapse whitespace when appropriate
applyLayout' _ ms@(HtmlMode : _) ws@(CollapseWS : _) il ss (Whitespace _ :@ b : xs) =
  Whitespace 1 :@ b : applyLayout' b ms ws il ss xs


--
-- tag open mode
--

-- Drop into html mode on tagclose
applyLayout' _ (TagOpenMode : ms) ws il ss (tcl@(TagClose :@ z) : xs) =
  tcl : applyLayout' z (HtmlMode : ms) ws il ss xs

-- drop into expr mode on tagopen
applyLayout' _ mms@(TagOpenMode : _) ws il ss (est@(ExprStart :@ z) : xs) =
  est : applyLayout' z (ExprMode : mms) ws il (Brace : ss) xs

-- Pop mode on tagselfclose
applyLayout' _ (TagOpenMode : ms) ws il ss (tsc@(TagSelfClose :@ z) : xs) =
  tsc : applyLayout' z ms ws il ss xs

-- Drop whitespace, newlines
applyLayout' _ mms@(TagOpenMode : _) ws il ss (Whitespace _ :@ z : xs) =
  applyLayout' z mms ws il ss xs
applyLayout' _ mms@(TagOpenMode : _) ws il ss (Newline :@ z : xs) =
  applyLayout' z mms ws il ss xs

--
-- tag close mode
--

-- Pop mode on tag close
applyLayout' _ (TagCloseMode : ms) ws il ss (tcl@(TagClose :@ z) : xs) =
  tcl : applyLayout' z ms ws il ss xs

-- Drop whitespace, newlines
applyLayout' _ mms@(TagCloseMode : _) ws il ss (Whitespace _ :@ z : xs) =
  applyLayout' z mms ws il ss xs
applyLayout' _ mms@(TagCloseMode : _) ws il ss (Newline :@ z : xs) =
  applyLayout' z mms ws il ss xs

--
-- html comment mode
--

-- Pop mode on tag close
applyLayout' _ (HtmlCommentMode : ms) ws il ss (tce@(TagCommentEnd :@ _) : tc@(TagClose :@ z) : xs) =
  tce : tc : applyLayout' z ms ws il ss xs

--
-- string mode
--

-- Pop mode on string end
applyLayout' _ (StringMode : ms) ws il ss (ese@(StringEnd :@ a) : xs) =
  ese : applyLayout' a ms ws il ss xs

-- Push expr mode on exprstart
applyLayout' _ mms@(StringMode : _) ws il ss (est@(ExprStart :@ z) : xs) =
  est : applyLayout' z (ExprMode : mms) ws il (Brace : ss) xs

--
-- expr mode
--

-- Drop into tag open mode on tagopen
applyLayout' _ mms@(ExprMode : _) ws il ss (top@(TagOpen :@ a) : xs) =
  ExprLParen :@ a : top : applyLayout' a (TagOpenMode : HtmlMode : mms) ws il (Html : ss) xs

-- Likewise for TagCommentStart
applyLayout' _ mms@(ExprMode : _) ws il ss (tcs@(TagCommentStart :@ a) : xs) =
  ExprLParen :@ a : tcs : applyLayout' a (HtmlCommentMode : HtmlMode : mms) ws il (Html : ss) xs

-- Enter string mode on string start
applyLayout' _ mms@(ExprMode : _) ws il ss (ess@(StringStart :@ a) : xs) =
  ess : applyLayout' a (StringMode : mms) ws il ss xs

-- Pop mode on expr end
applyLayout' _ (ExprMode : ms) ws il ss ((ExprEnd :@ a) : xs) =
  closeEmThen a Brace ss il $ \ils sss toks ->
    toks <> applyLayout' a ms ws ils sss xs

-- Nested expr mode
applyLayout' _ mms@(ExprMode : _) ws il ss (est@(ExprStart :@ z) : xs) =
  ExprLParen :@ z : est : applyLayout' z (ExprMode : HtmlMode : mms) ws il (Brace : Html : ss) xs

-- Enter block scope on lambda
applyLayout' _ mms@(ExprMode : _) ws il ss (est@(ExprLamStart :@ a) : xs) =
  ExprLParen :@ a : est : applyLayout' a mms ws il (Block : ss) xs

-- Enter Cases block scope on case start
applyLayout' _ mms@(ExprMode : _) ws il ss (est@(ExprCaseStart :@ a) : xs) =
  ExprLParen :@ a : est : applyLayout' a mms ws il (Cases : ss) xs

-- Enter pattern mode on case of
applyLayout' _ mms@(ExprMode : _) ws il ss (cof@(ExprCaseOf :@ z) : xs) =
  cof : applyLayout' z (ExprPatternMode : mms) ws il ss xs

-- Enter block scope on arrow
applyLayout' _ mms@(ExprMode : _) ws il ss (arr@(ExprArrow :@ a) : xs) =
  arr : ExprLParen :@ a : applyLayout' a mms ws il (Block : ss) xs

-- close block scope on casesep, push pattern mode
applyLayout' _ mms@(ExprMode : _) ws il ss (est@(ExprCaseSep :@ a) : xs) =
  closeEmThen a CaseAlt ss il $ \ils sss toks ->
    toks <> (est : applyLayout' a (ExprPatternMode : mms) ws ils sss xs)

-- enter paren scopes on left paren
applyLayout' _ mms@(ExprMode : _) ws il ss (elp@(ExprLParen :@ z) : xs) =
  elp : applyLayout' z mms ws il (Paren : ss) xs

-- close scopes on right paren
applyLayout' _ mms@(ExprMode : _) ws il ss ((ExprRParen :@ a) : xs) =
  closeEmThen a Paren ss il $ \ils sss toks ->
    toks <> applyLayout' a mms ws ils sss xs

-- Track indent/dedent
applyLayout' _ ms@(ExprMode : _) ws il ss ((Newline :@ _) : (Whitespace x :@ b) : xs) =
  newline ms ws il ss b x xs
applyLayout' _ ms@(ExprMode : _) ws il ss (Newline :@ a : n2@(Newline :@ _) : xs) =
  applyLayout' a ms ws il ss (n2:xs)
applyLayout' _ ms@(ExprMode : _) ws il ss ((Newline :@ _) : xs@(_ :@ b : _)) =
  newline ms ws il ss b 0 xs

-- Drop whitespace and newlines
applyLayout' _ mms@(ExprMode : _) ws il ss (Whitespace _ :@ z : xs) =
  applyLayout' z mms ws il ss xs
applyLayout' _ mms@(ExprMode : _) ws il ss (Newline :@ z : xs) =
  applyLayout' z mms ws il ss xs


--
-- expr pat mode
--

-- open case scope and pop mode on arrow
applyLayout' _ (ExprPatternMode : ms) ws il ss (arr@(ExprArrow :@ a) : xs) =
  arr : ExprLParen :@ a : applyLayout' a ms ws il (CaseAlt : ss) xs

-- Track indent/dedent
applyLayout' _ ms@(ExprPatternMode : _) ws il ss ((Newline :@ _) : (Whitespace x :@ b) : xs) =
  newline ms ws il ss b x xs
applyLayout' _ ms@(ExprPatternMode : _) ws il ss (n1@(Newline :@ a) : n2@(Newline :@ _) : xs) =
  n1 : applyLayout' a ms ws il ss (n2:xs)
applyLayout' _ ms@(ExprPatternMode : _) ws il ss ((Newline :@ _) : xs@(_ :@ b : _)) =
  newline ms ws il ss b 0 xs

-- Handle left and right parens
applyLayout' _ ms@(ExprPatternMode : _) ws il ss (elp@(ExprLParen :@ z) : xs) =
  elp : applyLayout' z ms ws il (Paren : ss) xs
applyLayout' _ mms@(ExprPatternMode : _) ws il ss ((ExprRParen :@ a) : xs) =
  closeEmThen a Paren ss il $ \ils sss toks ->
    toks <> applyLayout' a mms ws ils sss xs

-- pop mode on expr brace
applyLayout' _ (ExprPatternMode : ms) ws il ss ((ExprEnd :@ a) : xs) =
  closeEmThen a Brace ss il $ \ils sss toks ->
    toks <> applyLayout' a ms ws ils sss xs

-- drop whitespace and newlines
applyLayout' _ mms@(ExprPatternMode : _) ws il ss (Whitespace _ :@ z : xs) =
  applyLayout' z mms ws il ss xs
applyLayout' _ mms@(ExprPatternMode : _) ws il ss (Newline :@ z : xs) =
  applyLayout' z mms ws il ss xs


--
-- General
--

-- Drop expr comments
applyLayout' _ ms ws il ss (ExprCommentStart :@ z : xs) =
  applyLayout' z ms ws il ss xs
applyLayout' _ ms ws il ss (ExprCommentChunk _ :@ z : xs) =
  applyLayout' z ms ws il ss xs
applyLayout' _ ms ws il ss (ExprCommentEnd :@ z : xs) =
  applyLayout' z ms ws il ss xs


-- Drop trailing newline and close implicit scopes at EOF
applyLayout' _ _ _ il ss (Newline :@ a : []) =
  fmap (:@ a) (closeImplicitScopes ss il)

-- Pass over any ignored tokens
applyLayout' _ ms ws il ss (x:xs) =
  x : applyLayout' (extractPosition x) ms ws il ss xs

-- lose implicit scopes at EOF
applyLayout' a _ _ il ss [] =
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
newline :: [LexerMode] -> [WSMode] -> [IndentLevel] -> [Scope] -> Range -> Int -> [Positioned Token] -> [Positioned Token]

newline ms ws iis@(IndentLevel i : _) ss a x xs
  | i == x =
    -- same level
    -- this is reason enough to close a case
    -- trace (show ms) $
    case ss of
      CaseAlt : _ ->
        closeEmThen a Indent ss iis $ \iiis sss toks ->
          toks <> applyLayout' a (ExprPatternMode : ms) ws iiis sss xs
      _ ->
        applyLayout' a ms ws iis ss xs

  | i > x =
    -- indent decreased
    -- close an indent scope
    -- trace (show ms) $
    maybe
      (applyLayout' a ms ws iis ss xs)
      (\(toks, iiis, sss) -> fmap (:@ a) toks <> newline ms ws iiis sss a x xs)
      (closeScopes Indent ss iis)

  | otherwise {- i < x -} =
    -- indent increased
    -- open an indent scope
    applyLayout' a ms ws (IndentLevel x : iis) (Indent : ss) xs

newline ms ws [] ss a x xs
  | x == 0 =
    -- initial unindented
    applyLayout' a ms ws [] ss xs

  | otherwise =
    -- initially indented
    applyLayout' a ms ws [IndentLevel x] (Indent : ss) xs


-- -----------------------------------------------------------------------------

closeScopes :: Scope -> [Scope] -> [IndentLevel] -> Maybe ([Token], [IndentLevel], [Scope])
closeScopes s =
--  trace (show s) .
  closeScopes' (closeScope s)

closeScopes' :: (Scope -> ScopeClose) -> [Scope] -> [IndentLevel] -> Maybe ([Token], [IndentLevel], [Scope])
closeScopes' fs sss ils =
--  trace ("closes " <> show result <> " from " <> show sss <> "(" <> show ils <> ")")  $
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
    LineBrace -> Stop
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
closeScope Brace LineBrace = Stop

closeScope LineBrace LineBrace = CloseAndStop [ExprEndWS]
closeScope LineBrace Block = CloseAndContinue [ExprRParen]
closeScope LineBrace Cases = CloseAndContinue [ExprRParen]
closeScope LineBrace CaseAlt = CloseAndContinue [ExprRParen, ExprCaseSep]
closeScope LineBrace Indent = Continue
closeScope LineBrace Html = CloseAndContinue [ExprRParen]
closeScope LineBrace Brace = Stop
closeScope LineBrace Paren = Stop

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
closeScope Paren LineBrace = Stop


closeScope CaseAlt CaseAlt = CloseAndStop [ExprRParen]
closeScope CaseAlt Block = CloseAndContinue [ExprRParen]
closeScope CaseAlt Indent = Continue
closeScope CaseAlt Html = CloseAndContinue [ExprRParen]
-- ; can't close cases, parens or braces
closeScope CaseAlt Cases = Stop
closeScope CaseAlt Paren = Stop
closeScope CaseAlt Brace = Stop
closeScope CaseAlt LineBrace = Stop


-- soft indent closes itself
closeScope Indent Indent = CloseAndStop []
-- soft indent can close block scopes
closeScope Indent Block = CloseAndContinue [ExprRParen]
-- soft indent can inject case separators
closeScope Indent Cases = CloseAndContinue [ExprRParen]
closeScope Indent CaseAlt = CloseAndStop [ExprRParen, ExprCaseSep]
-- soft indent can't close braces or parens
closeScope Indent Brace = Stop
closeScope Indent LineBrace = Stop
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
