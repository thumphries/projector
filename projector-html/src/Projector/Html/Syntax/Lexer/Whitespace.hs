{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Syntax.Lexer.Whitespace (
    deindent
  ) where


import           P

import           Projector.Html.Data.Position
import           Projector.Html.Syntax.Token


-- | Eliminates unimportant leading whitespace, creating indent/dedent tokens.
deindent :: [Positioned Token] -> [Positioned Token]
deindent =
  deindent'' []

newtype IndentLevel = IndentLevel {
    _unIndentLevel :: Int
  } deriving (Eq, Ord, Show)


-- -----------------------------------------------------------------------------

-- peek, decide to enter sig mode or html mode.
-- take care of leading indentation when it exists
deindent'' :: [IndentLevel] -> [Positioned Token] -> [Positioned Token]
deindent'' il xs@(TypeSigStart :@ _ : _) =
  deindent' [TypeSigMode, HtmlMode] il xs
deindent'' il (Whitespace x :@ b : xs) =
  newline [HtmlMode] il b x xs
deindent'' il xs =
  deindent' [HtmlMode] il xs


deindent' :: [LexerMode] -> [IndentLevel] -> [Positioned Token] -> [Positioned Token]



--
-- type signatures
--

-- Drop out of signature mode on sig end, with optional newline
deindent' (TypeSigMode : ms) il (end@(TypeSigEnd :@ _) : Newline :@ _ : xs) =
  end : deindent' ms il xs
deindent' (TypeSigMode : ms) il (end@(TypeSigEnd :@ _) : xs) =
  end : deindent' ms il xs

-- Drop whitespace, indent, dedent in the type signature
deindent' mms@(TypeSigMode : _) il (Whitespace _ :@ _ : xs) =
  deindent' mms il xs
deindent' mms@(TypeSigMode : _) il (Indent _ :@ _ : xs) =
  deindent' mms il xs
deindent' mms@(TypeSigMode : _) il (Dedent :@ _ : xs) =
  deindent' mms il xs

-- Separators can be injected on newline where needed
deindent' mms@(TypeSigMode : _) il (Newline :@ a : xs) =
  TypeSigSep :@ a : deindent' mms il xs

-- ... but they're not needed when they're explicit:
deindent' mms@(TypeSigMode : _) il (sep@(TypeSigSep :@ _) : Newline :@ _ : xs) =
  sep : deindent' mms il xs
deindent' mms@(TypeSigMode : _) il (sep@(TypeSigSep :@ _) : Whitespace _ :@ _ : Newline :@ _ : xs) =
  sep : deindent' mms il xs


--
-- html mode
--

-- Drop into expr mode on left brace
deindent' mms@(HtmlMode : ms) il (est@(ExprStart :@ _) : xs) =
  est : deindent' (ExprMode : mms) il xs

-- Drop into tag open mode on tagopen
deindent' mms@(HtmlMode : ms) il (top@(TagOpen :@ _) : xs) =
  top : deindent' (TagOpenMode : mms) il xs

-- Drop into tag close mode on tag close
deindent' (HtmlMode : ms) il (tcl@(TagClose :@ _) : xs) =
  tcl : deindent' (TagCloseMode : ms) il xs

-- Track indent levels
deindent' ms@(HtmlMode : _) il (n@(Newline :@ _) : w@(Whitespace x :@ b) : xs) =
  n : w : newline ms il b x xs
deindent' ms@(HtmlMode : _) il (n@(Newline :@ _) : xs@(_ :@ b : _)) =
  n : newline ms il b 0 xs


--
-- tag open mode
--

-- Drop into html mode on tagclose
deindent' (TagOpenMode : ms) il (tcl@(TagClose :@ _) : xs) =
  tcl : deindent' (HtmlMode : ms) il xs

-- Pop mode on tagselfclose
deindent' (TagOpenMode : ms) il (tsc@(TagSelfClose :@ _) : xs) =
  tsc : deindent' ms il xs

-- Drop whitespace, newlines
deindent' mms@(TagOpenMode : _) il (Whitespace _ :@ _ : xs) =
  deindent' mms il xs
deindent' mms@(TagOpenMode : _) il (Newline :@ _ : xs) =
  deindent' mms il xs

--
-- tag close mode
--

-- Pop mode on tag close
deindent' (TagCloseMode : ms) il (tcl@(TagClose :@ _) : xs) =
  tcl : deindent' ms il xs

-- Drop whitespace, newlines
deindent' mms@(TagCloseMode : _) il (Whitespace _ :@ _ : xs) =
  deindent' mms il xs
deindent' mms@(TagCloseMode : _) il (Newline :@ _ : xs) =
  deindent' mms il xs


--
-- expr mode
--

-- Drop into tag open mode on tagopen
deindent' mms@(ExprMode : _) il (top@(TagOpen :@ a) : xs) =
  top : deindent' (TagOpenMode : mms) il xs

-- Pop mode on expr end
deindent' mms@(ExprMode : ms) il (est@(ExprEnd :@ a) : xs) =
  est : deindent' ms il xs

-- Nested expr mode
deindent' mms@(ExprMode : _) il (est@(ExprStart :@ a) : xs) =
  est : deindent' (ExprMode : mms) il xs

-- Track indent/dedent
deindent' ms@(ExprMode : _) il (n@(Newline :@ _) : (Whitespace x :@ b) : xs) =
  newline ms il b x xs
deindent' ms@(ExprMode : _) il (n@(Newline :@ _) : xs@(_ :@ b : _)) =
  newline ms il b 0 xs


-- Drop whitespace and newlines
deindent' mms@(ExprMode : ms) il (Whitespace _ :@ _ : xs) =
  deindent' mms il xs
deindent' mms@(ExprMode : ms) il (Newline :@ _ : xs) =
  deindent' mms il xs


-- Drop trailing newline
deindent' _ _ (Newline :@ _ : []) =
  []

--
-- Pass over any ignored tokens
--
deindent' ms il (x:xs) =
  x : deindent' ms il xs

deindent' _ _ [] =
  []





-- -----------------------------------------------------------------------------


-- | Given a new indent level, insert indent or dedent tokens
-- accordingly, then continue with deindent.
newline :: [LexerMode] -> [IndentLevel] -> Range -> Int -> [Positioned Token] -> [Positioned Token]

newline ms@(ExprMode : _) iis@(IndentLevel i : is) a x xs
  | i == x =
    deindent' ms iis xs

  | i > x =
    (Dedent :@ a) : newline ms is a x xs

  | otherwise {- i < x -} =
    (Indent (x - i) :@ a) : deindent' ms (IndentLevel x : iis) xs

newline ms iis@(IndentLevel i : is) a x xs
  | i == x =
    deindent' ms iis xs

  | i > x =
    newline ms is a x xs

  | otherwise {- i < x -} =
    deindent' ms (IndentLevel x : iis) xs

newline ms@(ExprMode : _) [] a x xs
  | x == 0 =
    deindent' ms [] xs

  | otherwise =
    (Indent x :@ a) : deindent' ms [IndentLevel x] xs

newline ms [] a x xs
  | x == 0 =
    deindent' ms [] xs

  | otherwise =
    deindent' ms [IndentLevel x] xs
