{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Lexer where


import           Data.String.QQ (s)

import           Disorder.Core
import           Disorder.Jack

import           P

import           Projector.Html.Data.Position
import           Projector.Html.Data.Token
import           Projector.Html.Lexer.Internal


-- -----------------------------------------------------------------------------
-- regression unit tests

lexProp :: (Eq a, Show a) => Parser [Positioned a] -> Text -> [a] -> Property
lexProp p i o =
  bimap renderLexError (fmap extractPositioned) (lex' p "Test.Projector.Html.Arbitrary" i) === pure o


prop_typeSigs_inline =
  once $
  lexProp
    typeSigs
    "\\ foo : Foo; bar : Foo Bar ->"
    [ TypeSigsStart
    , TypeIdent "foo"
    , TypeSigSep
    , TypeIdent "Foo"
    , TypeSigsSep
    , TypeIdent "bar"
    , TypeSigSep
    , TypeIdent "Foo"
    , TypeIdent "Bar"
    , TypeSigsEnd
    ]

prop_typeSigs_newline =
  once $
  lexProp
    typeSigs
    "\\foo: Foo\nbar:Maybe Foo\nbaz : Maybe (Maybe Foo) ->"
    [ TypeSigsStart
    , TypeIdent "foo"
    , TypeSigSep
    , TypeIdent "Foo"
    , TypeSigsSep
    , TypeIdent "bar"
    , TypeSigSep
    , TypeIdent "Maybe"
    , TypeIdent "Foo"
    , TypeSigsSep
    , TypeIdent "baz"
    , TypeSigSep
    , TypeIdent "Maybe"
    , TypeLParen
    , TypeIdent "Maybe"
    , TypeIdent "Foo"
    , TypeRParen
    , TypeSigsEnd
    ]

prop_typeSig =
  once $
  lexProp
    typeSig
    "foo123: Maybe (Foo Bar)"
    [ TypeIdent "foo123"
    , TypeSigSep
    , TypeIdent "Maybe"
    , TypeLParen
    , TypeIdent "Foo"
    , TypeIdent "Bar"
    , TypeRParen
    ]

prop_htmlComment =
  once $ lexProp html "<!-- hello world-->" [HtmlComment " hello world"]

prop_attr =
  once $
  lexProp attr "enabled=\"true\"" [AttName "enabled", AttSep, AttValueQ "true"]

prop_tagclose =
  once $ lexProp html "</a>" [TagCloseOpen, TagIdent "a", TagClose]

prop_tagopen =
  once $
  lexProp
    html
    "<a href=\"http://google.com\" enabled=\"true\"   >"
    [ TagOpen
    , TagIdent "a"
    , AttName "href"
    , AttSep
    , AttValueQ "http://google.com"
    , AttName "enabled"
    , AttSep
    , AttValueQ "true"
    , TagClose
    ]

prop_tagselfclose =
  once $
  lexProp
    html
    "<img src=\"http://google.com\" />"
    [ TagOpen
    , TagIdent "img"
    , AttName "src"
    , AttSep
    , AttValueQ "http://google.com"
    , TagSelfClose
    ]

prop_htmlExpr_id =
  once $ lexProp htmlExpr "{ id }" [ExprStart, ExprIdent "id", ExprEnd]

prop_htmlExpr_app =
  once $
  lexProp
    html
    "{\n      fix not\n        foo}"
    [ExprStart, ExprIdent "fix", ExprIdent "not", ExprIdent "foo", ExprEnd]

-- bad indentation on an expression
prop_expr_indent =
  neg . once $
  lexProp
    expr
    "  foo bar\n baz"
    [ExprIdent "foo", ExprIdent "bar", ExprIdent "baz"]

prop_caseAltPat =
  once $
  lexProp
    caseAltPat
    "x y (Foo bar (Baz bap))"
    [ PatId "x"
    , PatId "y"
    , PatLParen
    , PatCon "Foo"
    , PatId "bar"
    , PatLParen
    , PatCon "Baz"
    , PatId "bap"
    , PatRParen
    , PatRParen
    ]

prop_expr_case =
  once $
  lexProp
    expr
    "case foo (bar baz) of Foo x -> y; Bar z x spectrum -> p  z x spectrum"
    [ CaseStart
    , ExprIdent "foo"
    , ExprLParen
    , ExprIdent "bar"
    , ExprIdent "baz"
    , ExprRParen
    , CaseOf
    , PatCon "Foo"
    , PatId "x"
    , AltSep
    , ExprIdent "y"
    , CaseSep
    , PatCon "Bar"
    , PatId "z"
    , PatId "x"
    , PatId "spectrum"
    , AltSep
    , ExprIdent "p"
    , ExprIdent "z"
    , ExprIdent "x"
    , ExprIdent "spectrum"
    ]

prop_expr_case_indent =
  once $
  lexProp expr
  [s|case foo of
  bar -> baz
  cuil ->
    <html>Hamburger</html>
  beef ->
    bil bof pel|]
  [ CaseStart
  , ExprIdent "foo"
  , CaseOf
  , PatId "bar"
  , AltSep
  , ExprIdent "baz"
  , CaseSep
  , PatId "cuil"
  , AltSep
  , TagOpen
  , TagIdent "html"
  , TagClose
  , HtmlText "Hamburger"
  , TagCloseOpen
  , TagIdent "html"
  , TagClose
  , CaseSep
  , PatId "beef"
  , AltSep
  , ExprIdent "bil"
  , ExprIdent "bof"
  , ExprIdent "pel"
  ]

prop_html_plain =
  once $
  lexProp
    html
    "<html>Hello, world!</html>"
    [ TagOpen
    , TagIdent "html"
    , TagClose
    , HtmlText "Hello,"
    , WhiteSpace
    , HtmlText "world!"
    , TagCloseOpen
    , TagIdent "html"
    , TagClose
    ]

prop_plain =
  once $
  lexProp
    html
    "Hello, world!"
    [HtmlText "Hello,", WhiteSpace, HtmlText "world!"]

prop_plain_unesc =
  neg . once $
  lexProp html "Hello, worl\\" [HtmlText "Hello,", WhiteSpace, HtmlText "worl\\"]

prop_plain_esc =
  once $
  lexProp
    html
    "Hello, worl\\\\"
    [HtmlText "Hello,", WhiteSpace, HtmlText "worl\\"]

prop_expr_unesc =
  once $
  lexProp
    html
    "Hi, {name}!"
    [ HtmlText "Hi,"
    , WhiteSpace
    , ExprStart
    , ExprIdent "name"
    , ExprEnd
    , HtmlText "!"
    ]

prop_expr_esc =
  once $
  lexProp
    html
    "Hi, \\{name\\}!"
    [HtmlText "Hi,", WhiteSpace, HtmlText "{name}!"]

prop_html_case_indent =
  once $ lexProp html
    [s|
  { case foo of
      bar ->
        text goes here
      baz ->
        <html>Hello world!</html>
        <a href="google.com">
          Google
        </a>
      quux ->
        grault
      corge -> <blink>{corge}&nbsp;&nbsp;</blink>
  }|]
  [ WhiteSpace
  , ExprStart
  , CaseStart
  , ExprIdent "foo"
  , CaseOf
  , PatId "bar"
  , AltSep
  , ExprIdent "text"
  , ExprIdent "goes"
  , ExprIdent "here"
  , CaseSep
  , PatId "baz"
  , AltSep
  , TagOpen
  , TagIdent "html"
  , TagClose
  , HtmlText "Hello"
  , WhiteSpace
  , HtmlText "world!"
  , TagCloseOpen
  , TagIdent "html"
  , TagClose
  , WhiteSpace
  , TagOpen
  , TagIdent "a"
  , AttName "href"
  , AttSep
  , AttValueQ "google.com"
  , TagClose
  , WhiteSpace
  , HtmlText "Google"
  , WhiteSpace
  , TagCloseOpen
  , TagIdent "a"
  , TagClose
  , CaseSep
  , PatId "quux"
  , AltSep
  , ExprIdent "grault"
  , CaseSep
  , PatId "corge"
  , AltSep
  , TagOpen
  , TagIdent "blink"
  , TagClose
  , ExprStart
  , ExprIdent "corge"
  , ExprEnd
  , HtmlText "&nbsp;&nbsp;"
  , TagCloseOpen
  , TagIdent "blink"
  , TagClose
  , ExprEnd
  ]

prop_html_empty =
  once $ lexProp html "" []

return []
tests = $disorderCheckEnvAll TestRunNormal
