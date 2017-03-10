{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Syntax.Lexer.Layout (
    layout
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
    CaseScope -- expecting case separator
  | ParenScope -- explicit parentheses
  | BraceScope -- explicit braces { }
  | BlockScope Int -- an implicit scope
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
-- FIX not sure about this case at this point
applyLayout'' il (Whitespace x :@ b : xs) =
  newline [HtmlMode] il [] b x xs
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

-- Drop whitespace, indent, dedent in the type signature
applyLayout' mms@(TypeSigMode : _) il ss (Whitespace _ :@ _ : xs) =
  applyLayout' mms il ss xs
applyLayout' mms@(TypeSigMode : _) il ss (Indent _ :@ _ : xs) =
  applyLayout' mms il ss xs
applyLayout' mms@(TypeSigMode : _) il ss (Dedent :@ _ : xs) =
  applyLayout' mms il ss xs

-- Separators can be injected on newline where needed
applyLayout' mms@(TypeSigMode : _) il ss (Newline :@ a : xs) =
  TypeSigSep :@ a : applyLayout' mms il ss xs

-- ... but they're not needed when they're explicit:
applyLayout' mms@(TypeSigMode : _) il ss (sep@(TypeSigSep :@ _) : Newline :@ _ : xs) =
  sep : applyLayout' mms il ss xs
applyLayout' mms@(TypeSigMode : _) il ss (sep@(TypeSigSep :@ _) : Whitespace _ :@ _ : Newline :@ _ : xs) =
  sep : applyLayout' mms il ss xs


--
-- html mode
--

-- Drop into expr mode on left brace
applyLayout' mms@(HtmlMode : ms) il ss (est@(ExprStart :@ _) : xs) =
  est : applyLayout' (ExprMode : mms) il ss xs

-- Drop into tag open mode on tagopen
applyLayout' mms@(HtmlMode : ms) il ss (top@(TagOpen :@ _) : xs) =
  top : applyLayout' (TagOpenMode : mms) il ss xs

-- Drop into tag close mode on tag close
applyLayout' (HtmlMode : ms) il ss (tcl@(TagClose :@ _) : xs) =
  tcl : applyLayout' (TagCloseMode : ms) il ss xs


--
-- tag open mode
--

-- Drop into html mode on tagclose
applyLayout' (TagOpenMode : ms) il ss (tcl@(TagClose :@ _) : xs) =
  tcl : applyLayout' (HtmlMode : ms) il ss xs

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
applyLayout' mms@(ExprMode : _) il ss (top@(TagOpen :@ a) : xs) =
  top : applyLayout' (TagOpenMode : mms) il ss xs

-- Pop mode on expr end
applyLayout' mms@(ExprMode : ms) il ss (est@(ExprEnd :@ a) : xs) =
  est : applyLayout' ms il ss xs

-- Nested expr mode
applyLayout' mms@(ExprMode : _) il ss (est@(ExprStart :@ a) : xs) =
  est : applyLayout' (ExprMode : mms) il ss xs

-- Track indent/dedent
applyLayout' ms@(ExprMode : _) il ss (n@(Newline :@ _) : (Whitespace x :@ b) : xs) =
  newline ms il ss b x xs
applyLayout' ms@(ExprMode : _) il ss (n@(Newline :@ _) : xs@(_ :@ b : _)) =
  newline ms il ss b 0 xs


-- Drop whitespace and newlines
applyLayout' mms@(ExprMode : ms) il ss (Whitespace _ :@ _ : xs) =
  applyLayout' mms il ss xs
applyLayout' mms@(ExprMode : ms) il ss (Newline :@ _ : xs) =
  applyLayout' mms il ss xs


-- Drop trailing newline
applyLayout' _ _ _ (Newline :@ _ : []) =
  []

--
-- Pass over any ignored tokens
--
applyLayout' ms il ss (x:xs) =
  x : applyLayout' ms il ss xs

applyLayout' _ _ _ [] =
  []

-- -----------------------------------------------------------------------------


-- | Given a new indent level, insert indent or dedent tokens
-- accordingly, then continue with applyLayout.
newline :: [LexerMode] -> [IndentLevel] -> [Scope] -> Range -> Int -> [Positioned Token] -> [Positioned Token]

newline ms@(ExprMode : _) iis@(IndentLevel i : is) ss a x xs
  | i == x =
    applyLayout' ms iis ss xs

  | i > x =
    (Dedent :@ a) : newline ms is ss a x xs

  | otherwise {- i < x -} =
    (Indent (x - i) :@ a) : applyLayout' ms (IndentLevel x : iis) ss xs

newline ms iis@(IndentLevel i : is) ss a x xs
  | i == x =
    applyLayout' ms iis ss xs

  | i > x =
    newline ms is ss a x xs

  | otherwise {- i < x -} =
    applyLayout' ms (IndentLevel x : iis) ss xs

newline ms@(ExprMode : _) [] ss a x xs
  | x == 0 =
    applyLayout' ms [] ss xs

  | otherwise =
    (Indent x :@ a) : applyLayout' ms [IndentLevel x] ss xs

newline ms [] ss a x xs
  | x == 0 =
    applyLayout' ms [] ss xs

  | otherwise =
    applyLayout' ms [IndentLevel x] ss xs



-- -----------------------------------------------------------------------------

{-

-- | Close scopes
closeScopes :: Range -> Scope -> [LexerMode] -> [Scope] -> [Positioned Token] -> [Positioned Token]
closeScopes a s ms ss xs =
  fmap (:@ a) toks <> applyLayout' ms sss xs
  where
    (toks, sss) = closeScopes' s ss

closeScopes' :: Scope -> [Scope] -> ([Token], [Scope])
closeScopes' s ss =
  go ([], []) ss
  where
    go acc [] = acc
    go (ts, sss) (x:xs) =
      case closeScope s x of
        Just t ->
          go (t <> ts, sss) xs
        Nothing ->
          (ts, (x:xs))


closeScope :: Scope -> Scope -> Maybe [Token]

closeScope BraceScope BraceScope = Just [ExprEnd]
closeScope BraceScope (BlockScope _) = Just [ExprRParen]
closeScope BraceScope CaseScope = Just []
closeScope BraceScope ParenScope = Nothing

closeScope ParenScope ParenScope = Just [ExprRParen]
closeScope ParenScope (BlockScope _) = Just [ExprRParen]
closeScope ParenScope _ = Nothing

closeScope (BlockScope _) (BlockScope _) = Just [ExprRParen]
closeScope (BlockScope _) CaseScope = Just []
closeScope (BlockScope _) _ = Nothing

closeScope CaseScope CaseScope = Just []
closeScope CaseScope (BlockScope _) = Just [ExprRParen]
closeScope CaseScope _ = Nothing


-}
