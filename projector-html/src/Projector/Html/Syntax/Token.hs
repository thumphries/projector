{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Syntax.Token (
    Token (..)
  ) where


import           P


data Token =
  -- Type signatures (OLD FORMAT)
    TypeSigStart          -- \
  | TypeSigSep            -- ;
  | TypeSigEnd            -- ->
  | TypeSig               -- :
  | TypeIdent Text        -- foo | Foo
  | TypeLParen            -- (
  | TypeRParen            -- )
  -- HTML mode
  | TagOpen               -- <
  | TagClose              -- >
  | TagCloseOpen          -- </
  | TagSelfClose          -- />
  | TagCommentStart       -- <!--
  | TagCommentChunk Text  -- foo
  | TagCommentEnd         -- --
  | TagIdent Text         -- a, href, true
  | TagEquals             -- =
  | Plain Text            -- hello
  | ExprStart             -- {

  -- Expr mode
  | ExprLParen            -- (
  | ExprRParen            -- )
  | ExprListStart         -- [
  | ExprListSep           -- ,
  | ExprListEnd           -- ]
  | ExprCaseStart         -- case
  | ExprCaseOf            -- of
  | ExprArrow             -- ->
  | ExprCaseSep           -- ;
  | ExprConId Text        -- Maybe
  | ExprVarId Text        -- id
  | ExprLamStart          -- \
  | ExprDot               -- .
  | ExprCommentStart      -- {-
  | ExprCommentChunk Text -- foo
  | ExprCommentEnd        -- -}
  | ExprEnd               -- }

  -- General ambiguous
  | Whitespace Int        -- "   "
  | Newline               -- \n
  | StringStart           -- "
  | StringChunk Text      -- foo
  | StringEnd             -- "
  deriving (Eq, Ord, Show)
