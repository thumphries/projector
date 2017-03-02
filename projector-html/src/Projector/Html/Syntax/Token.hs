{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Syntax.Token (
    Token (..)
  ) where


import           P


data Token =
  -- Type signatures
  -- HTML mode
    TagOpen          -- <
  | TagClose         -- >
  | TagCloseOpen     -- </
  | TagSelfClose     -- />
  | TagComment       -- <!-- foo -->
  | TagIdent Text    -- a, href, true
  | TagEquals        -- =
  | Plain Text       -- hello world

  -- Expr mode
  | ExprLParen       -- (
  | ExprRParen       -- )
  | ExprListStart    -- [
  | ExprListSep      -- ,
  | ExprListEnd      -- ]
  | ExprCaseStart    -- case
  | ExprCaseOf       -- of
  | ExprArrow        -- ->
  | ExprCaseSep      -- ;
  | ExprIdent Text   -- id, Maybe
  | ExprLamStart     -- \
  | ExprComment Text -- {- foo -}

  -- General ambiguous
  | Whitespace Int   -- "   "
  | Newline          -- \n
  | ExprStart        -- {
  | ExprEnd          -- }
  | StringDelimiter  -- "
  | StringChunk Text -- foo
  deriving (Eq, Ord, Show)
