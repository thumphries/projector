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
  | TypeSigsSep     -- [;\n]
  | TypeSigsEnd     -- ->
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
  | WhiteSpace      -- [ \t\r\n]+
  | HtmlComment Text -- <!-- foo -->
  | HtmlText Text   -- Hello!
  -- Exprs
  | ExprStart       -- {
  | ExprEnd         -- }
  | ExprIdent Text  -- foo
  | ExprLParen      -- (
  | ExprRParen      -- )
  | CaseStart       -- case
  | CaseOf          -- of
  | CaseSep         -- ;
  | AltSep          -- ->
  | LamStart        -- \
  | LamBody         -- ->
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
    TypeSigsSep     -> ";"
    TypeSigsEnd     -> "->"
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

    WhiteSpace      -> " "
    HtmlComment t   -> "<!--" <> t <> "-->" -- TODO this should be three tokens
    HtmlText t      -> escape t

    ExprStart       -> "{"
    ExprEnd         -> "}"
    ExprIdent t     -> t
    ExprLParen      -> "("
    ExprRParen      -> ")"
    CaseStart       -> "case"
    CaseOf          -> "of"
    CaseSep         -> ";"
    AltSep          -> "->"
    LamStart        -> "\\"
    LamBody         -> "->"

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
