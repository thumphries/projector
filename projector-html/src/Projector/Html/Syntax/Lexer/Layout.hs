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
  applyLayout [] []

data Scope =
    CaseScope -- expecting case separator
  | ParenScope -- explicit parentheses
  | BraceScope -- explicit braces { }
  | BlockScope Int -- an implicit scope
  deriving (Eq, Ord, Show)

data Mode =
    HtmlMode
  | TagOpenMode
  | TagCloseMode
  | ExprMode
  | TypeSigMode
  deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------

-- | Apply offside rule, remove redundant whitespace, etc.
applyLayout :: [Mode] -> [Scope] -> [Positioned Token] -> [Positioned Token]

--
-- start
--

-- Peek, decide to enter sig mode or html mode
applyLayout [] ss xs@(TypeSigStart :@ _ : _) =
  applyLayout [TypeSigMode, HtmlMode] ss xs
applyLayout [] ss xs =
  applyLayout [HtmlMode] ss xs

--
-- type signatures
--

-- Drop out of signature mode on sig end, with optional newline
applyLayout (TypeSigMode : ms) ss (end@(TypeSigEnd :@ _) : Newline :@ _ : Dedent :@ _ : xs) =
  end : applyLayout ms ss xs
applyLayout (TypeSigMode : ms) ss (end@(TypeSigEnd :@ _) : Newline :@ _ : xs) =
  end : applyLayout ms ss xs
applyLayout (TypeSigMode : ms) ss (end@(TypeSigEnd :@ _) : xs) =
  end : applyLayout ms ss xs

-- Drop whitespace, indent, dedent in the type signature
applyLayout mms@(TypeSigMode : _) ss (Whitespace _ :@ _ : xs) =
  applyLayout mms ss xs
applyLayout mms@(TypeSigMode : _) ss (Indent _ :@ _ : xs) =
  applyLayout mms ss xs
applyLayout mms@(TypeSigMode : _) ss (Dedent :@ _ : xs) =
  applyLayout mms ss xs

-- Separators can be injected on newline where needed
applyLayout mms@(TypeSigMode : _) ss (Newline :@ a : xs) =
  TypeSigSep :@ a : applyLayout mms ss xs

-- ... but they're not needed when they're explicit:
applyLayout mms@(TypeSigMode : _) ss (sep@(TypeSigSep :@ _) : Newline :@ _ : xs) =
  sep : applyLayout mms ss xs
applyLayout mms@(TypeSigMode : _) ss (sep@(TypeSigSep :@ _) : Whitespace _ :@ _ : Newline :@ _ : xs) =
  sep : applyLayout mms ss xs


--
-- html mode
--

-- Drop into expr mode on left brace
applyLayout mms@(HtmlMode : ms) ss (est@(ExprStart :@ _) : xs) =
  est : applyLayout (ExprMode : mms) (BraceScope : ss) xs

-- Drop into tag open mode on tagopen
applyLayout mms@(HtmlMode : ms) ss (top@(TagOpen :@ _) : xs) =
  top : applyLayout (TagOpenMode : mms) ss xs

-- Drop into tag close mode on tag close
applyLayout (HtmlMode : ms) ss (tcl@(TagClose :@ _) : xs) =
  tcl : applyLayout (TagCloseMode : ms) ss xs


--
-- tag open mode
--

-- Drop into html mode on tagclose
applyLayout (TagOpenMode : ms) ss (tcl@(TagClose :@ _) : xs) =
  tcl : applyLayout (HtmlMode : ms) ss xs

-- Pop mode on tagselfclose
applyLayout (TagOpenMode : ms) ss (tsc@(TagSelfClose :@ _) : xs) =
  tsc : applyLayout ms ss xs

-- Drop whitespace, newlines
applyLayout mms@(TagOpenMode : _) ss (Whitespace _ :@ _ : xs) =
  applyLayout mms ss xs
applyLayout mms@(TagOpenMode : _) ss (Newline :@ _ : xs) =
  applyLayout mms ss xs

--
-- tag close mode
--

-- Pop mode on tag close
applyLayout (TagCloseMode : ms) ss (tcl@(TagClose :@ _) : xs) =
  tcl : applyLayout ms ss xs

-- Drop whitespace, newlines
applyLayout mms@(TagCloseMode : _) ss (Whitespace _ :@ _ : xs) =
  applyLayout mms ss xs
applyLayout mms@(TagCloseMode : _) ss (Newline :@ _ : xs) =
  applyLayout mms ss xs


--
-- expr mode
--

-- Drop into tag open mode on tagopen
applyLayout mms@(ExprMode : _) ss (top@(TagOpen :@ a) : xs) =
  top : applyLayout (TagOpenMode : mms) ss xs

-- Close relevant implicit scopes on right brace
applyLayout mms@(ExprMode : ms) ss (est@(ExprEnd :@ a) : xs) =
  closeScopes a BraceScope ms ss xs

-- Drop whitespace and newlines
applyLayout mms@(ExprMode : ms) ss (Whitespace _ :@ _ : xs) =
  applyLayout mms ss xs
applyLayout mms@(ExprMode : ms) ss (Newline :@ _ : xs) =
  applyLayout mms ss xs

-- Drop trailing newline
applyLayout _ _ (Newline :@ _ : []) =
  []

--
-- Pass over any ignored tokens
--
applyLayout ms ss (x:xs) =
  x : applyLayout ms ss xs

applyLayout _ _ [] =
  []

-- -----------------------------------------------------------------------------

-- | Close scopes
closeScopes :: Range -> Scope -> [Mode] -> [Scope] -> [Positioned Token] -> [Positioned Token]
closeScopes a s ms ss xs =
  fmap (:@ a) toks <> applyLayout ms sss xs
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

