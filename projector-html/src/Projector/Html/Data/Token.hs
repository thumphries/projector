-- These are the tokens used by the old pretty-printer. These need to go soon!
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Data.Token (
    Token (..)
  , renderToken
  ) where


import qualified Data.Text as T

import P


data Token
  -- Type sigs
  = TypeSigsStart   -- \
  | TypeSigsSep     -- [->]
  | TypeSigsEnd     -- =
  | TypeSigSep      -- :
  | TypeIdent Text  -- foo | Foo
  | TypeLParen      -- (
  | TypeRParen      -- )
  | TypeLSquare     -- [
  | TypeRSquare     -- ]
  -- Html
  | TagIdent Text   -- img
  | TagOpen         -- <
  | TagCloseOpen    -- </
  | TagClose        -- >
  | TagSelfClose    -- />
  -- Attributes
  | AttName Text    -- id
  | AttSep          -- =
  | AttValueQ Text  -- "true"
  | AttValue Text   -- true
  -- Misc
  | WhiteSpace Int  -- [ \t\r\n]+
  | HtmlComment Text -- <!-- foo -->
  | HtmlText Text   -- Hello!
  | StringStart     -- "
  | StringEnd       -- "
  | StringChunk Text -- foo
  -- Exprs
  | ExprStart       -- {
  | ExprEnd         -- }
  | ExprStartWS     -- {|
  | ExprEndWS       -- |}
  | ExprIdent Text  -- foo
  | ExprLParen      -- (
  | ExprRParen      -- )
  | CaseStart       -- case
  | CaseOf          -- of
  | CaseSep         -- ;
  | AltSep          -- ->
  | LamStart        -- \
  | LamBody         -- ->
  | Each            -- "each"
  | ListStart       -- [
  | ListEnd         -- ]
  | ListSep         -- ,
  | ExprDot         -- .
  | ExprHole        -- _
  -- Patterns
  | PatCon Text     -- Just
  | PatId Text      -- x
  | PatLParen       -- (
  | PatRParen       -- )
  deriving (Eq, Ord, Show)

renderToken :: Token -> Text
renderToken tok =
  case tok of
    TypeSigsStart   -> "\\"
    TypeSigsSep     -> "->"
    TypeSigsEnd     -> "="
    TypeSigSep      -> ":"
    TypeIdent t     -> t
    TypeLParen      -> "("
    TypeRParen      -> ")"
    TypeLSquare     -> "["
    TypeRSquare     -> "]"

    TagIdent t      -> t
    TagOpen         -> "<"
    TagCloseOpen    -> "</"
    TagClose        -> ">"
    TagSelfClose    -> "/>"

    AttName t       -> t
    AttSep          -> "="
    AttValueQ t     -> "\"" <> escape t <> "\""
    AttValue t      -> t

    WhiteSpace x    -> T.replicate x " "
    HtmlComment t   -> "<!--" <> t <> "-->" -- TODO this should be three tokens
    HtmlText t      -> escape t
    StringStart     -> "\""
    StringEnd       -> "\""
    StringChunk t   -> escapeLiteral t

    ExprStart       -> "{"
    ExprEnd         -> "}"
    ExprStartWS     -> "{|"
    ExprEndWS       -> "|}"
    ExprIdent t     -> t
    ExprLParen      -> "("
    ExprRParen      -> ")"
    CaseStart       -> "case"
    CaseOf          -> "of"
    CaseSep         -> ";"
    AltSep          -> "->"
    LamStart        -> "\\"
    LamBody         -> "->"
    Each            -> "each"
    ListStart       -> "["
    ListEnd         -> "]"
    ListSep         -> ","
    ExprDot         -> "."
    ExprHole        -> "_"

    PatCon t        -> t
    PatId t         -> t
    PatLParen       -> "("
    PatRParen       -> ")"

escape :: Text -> Text
escape =
    T.replace "{" "\\{"
  . T.replace "}" "\\}"
  . T.replace "<" "\\<"
  . T.replace "-->" "\\-\\->"
  . T.replace ">" "\\>"
  . T.replace "\r" "\\\r"
  . T.replace "\n" "\\\n"
  . T.replace "\\" "\\\\"

escapeLiteral :: Text -> Text
escapeLiteral =
    T.replace "\"" "\\\""
  . T.replace "\n" "\\n"
  . T.replace "\r" "\\r"
  . T.replace "\\" "\\\\"
  . T.replace "{" "\\{"
  . T.replace "}" "\\}"
