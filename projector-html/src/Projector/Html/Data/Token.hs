{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Data.Token (
    Token (..)
  , renderToken
  ) where


import P


data Token
  -- * Type sigs
  = TypeSigsStart   -- \
  | TypeSigsSep     -- [;\n]
  | TypeSigsEnd     -- ->
  | TypeSigSep      -- :
  | TypeIdent Text  -- foo | Foo
  | TypeLParen      -- (
  | TypeRParen      -- )
  -- * Html
  | TagIdent Text   -- img
  | TagOpen         -- <
  | TagCloseOpen    -- </
  | TagClose        -- >
  | TagSelfClose    -- />
  -- ** Attributes
  | AttName Text    -- id
  | AttSep          -- =
  | AttValueQ Text  -- "true"
  | AttValue Text   -- true
  -- ** Misc
  | WhiteSpace      -- [ \t\r\n]+
  | HtmlComment Text -- <!-- foo -->
  | HtmlText Text   -- Hello!
  -- * Exprs
  | ExprStart       -- {
  | ExprEnd         -- }
  | ExprIdent Text  -- foo
  | ExprLParen      -- (
  | ExprRParen      -- )
  | CaseStart       -- case
  | CaseOf          -- of
  | CaseSep         -- ;
  | AltSep          -- ->
  -- ** Patterns
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

    TagIdent t      -> t
    TagOpen         -> "<"
    TagCloseOpen    -> "</"
    TagClose        -> ">"
    TagSelfClose    -> "/>"

    AttName t       -> t
    AttSep          -> "="
    AttValueQ t     -> "\"" <> t <> "\"" -- TODO escape \" etc
    AttValue t      -> t

    WhiteSpace      -> " "
    HtmlComment t   -> "<!--" <> t <> "-->"
    HtmlText t      -> t

    ExprStart       -> "{"
    ExprEnd         -> "}"
    ExprIdent t     -> t
    ExprLParen      -> "("
    ExprRParen      -> ")"
    CaseStart       -> "case"
    CaseOf          -> "of"
    CaseSep         -> ";"
    AltSep          -> "->"

    PatCon t        -> t
    PatId t         -> t
    PatLParen       -> "("
    PatRParen       -> ")"
