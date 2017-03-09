{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Syntax.Token (
    Token (..)
  , renderToken
  , LexerMode (..)
  ) where


import qualified Data.Text as T

import           P


data LexerMode =
    HtmlMode
  | HtmlCommentMode
  | TagOpenMode
  | TagCloseMode
  | ExprMode
  | ExprPatternMode
  | ExprCommentMode
  | StringMode
  | TypeSigMode
  deriving (Eq, Ord, Show)

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
  -- TODO remove this
  | ExprEach              -- each

  -- General ambiguous
  | Whitespace Int        -- "   "
  | Newline               -- \n
  | StringStart           -- "
  | StringChunk Text      -- foo
  | StringEnd             -- "
  deriving (Eq, Ord, Show)

renderToken :: Token -> Text
renderToken tok =
  case tok of
    -- Type signatures (OLD FORMAT)
    TypeSigStart          -> "\\"
    TypeSigSep            -> ";"
    TypeSigEnd            -> "->"
    TypeSig               -> ":"
    TypeIdent t           -> t
    TypeLParen            -> "("
    TypeRParen            -> ")"

    -- HTML mode
    TagOpen               -> "<"
    TagClose              -> ">"
    TagCloseOpen          -> "</"
    TagSelfClose          -> "/>"
    TagCommentStart       -> "<!--"
    TagCommentChunk t     -> t
    TagCommentEnd         -> "--"
    TagIdent t            -> t
    TagEquals             -> "="
    Plain t               -> t
    ExprStart             -> "{"

    -- Expr mode
    ExprLParen            -> "("
    ExprRParen            -> ")"
    ExprListStart         -> "["
    ExprListSep           -> ","
    ExprListEnd           -> "]"
    ExprCaseStart         -> "case"
    ExprCaseOf            -> "of"
    ExprArrow             -> "->"
    ExprCaseSep           -> ";"
    ExprConId t           -> t
    ExprVarId t           -> t
    ExprLamStart          -> "\\"
    ExprDot               -> "."
    ExprCommentStart      -> "{-"
    ExprCommentChunk t    -> t
    ExprCommentEnd        -> "-}"
    ExprEnd               -> "}"
    ExprEach              -> "each"

    -- General ambiguous
    Whitespace x          -> T.replicate x " "
    Newline               -> "\n"
    StringStart           -> "\""
    StringChunk t         -> t
    StringEnd             -> "\""
