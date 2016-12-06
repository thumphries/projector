{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Parser where


import           Data.List.NonEmpty  (NonEmpty(..))
import           Data.String.QQ (s)

import           Disorder.Core
import           Disorder.Jack

import           P

import           Projector.Html.Data.Template
import           Projector.Html.Parser



-- -----------------------------------------------------------------------------
-- regression unit tests

parseProp :: Text -> Template () -> Property
parseProp t p =
  fmap (fmap (const ())) (parse "Test.Projector.Html.Parser" t) === pure p

emptyTemplate :: Template ()
emptyTemplate =
  Template () Nothing (THtml () [])

prop_parse_empty =
  once $ parseProp "" emptyTemplate

prop_parse_typeSigs_inline =
  once $ parseProp "\\ foo : Foo; bar : Bar ->" emptyTemplate

prop_typeSigs_newline =
  once $
  parseProp
    "\\foo: Foo\nbar:Maybe Foo\nbaz : Maybe (Maybe Foo) ->"
    emptyTemplate

prop_plain =
  once $
  parseProp
    "hello world"
    (Template
       ()
       Nothing
       (THtml
          ()
          [ TPlain () TPlainText {unTPlainText = "hello"}
          , TWhiteSpace ()
          , TPlain () TPlainText {unTPlainText = "world"}
          ]))

prop_htmlComment =
  once $ parseProp "<!-- hello world-->" emptyTemplate

prop_htmlExpr_app =
  once $ parseProp "{\n      fix not\n        foo}" emptyTemplate

prop_htmlExpr_id =
  once $ parseProp "{ id }" emptyTemplate

prop_htmlExpr_case =
  once $
  parseProp
    "{ case foo (bar baz) of Foo x -> y; Bar z x spectrum -> p  z x spectrum }"
    emptyTemplate

prop_parse_foo =
  once $ parseProp
    "\\foo : Bar ->\n hello! {foo}"
    (Template
       ()
       (Just (TTypeSig () ((TId "foo", TTVar () (TId "Bar")) :| [])))
       (THtml
          ()
          [ TWhiteSpace ()
          , TPlain () (TPlainText "hello!")
          , TWhiteSpace ()
          , TExprNode () (TEVar () TId {unTId = "foo"})
          ]))

prop_parse_tag =
  once $
  parseProp
    "\\foo : Bar ->\n <a href={foo} enabled>Google</a>"
    (Template
       ()
       (Just
          (TTypeSig
             ()
             ((TId {unTId = "foo"}, TTVar () TId {unTId = "Bar"}) :| [])))
       (THtml
          ()
          [ TWhiteSpace ()
          , TElement
              ()
              TTag {unTTag = "a"}
              [ TAttribute
                  ()
                  TAttrName {unTAttrName = "href"}
                  (TAttrExpr () (TEVar () TId {unTId = "foo"}))
              , TEmptyAttribute () TAttrName {unTAttrName = "enabled"}
              ]
              (THtml () [TPlain () TPlainText {unTPlainText = "Google"}])
          ]))

prop_parse_foobar =
  once $ parseProp "\\foo : Bar; bar : Baz->\n{foo bar (baz quux bil) corge}" emptyTemplate

{-prop_parse_unbalanced =
  once $ parseProp "\\foo : Bar ->\n<a><img src=\"google\"/> no close</foo>" emptyTemplate -}

prop_parse_case =
  once $
  parseProp
    "\\foo : Bar ->\n { case foo of\n        Bar -> <blink>marquee</blink> }\n<marquee></marquee>"
    (Template
       ()
       (Just
          (TTypeSig
             ()
             ((TId {unTId = "foo"}, TTVar () TId {unTId = "Bar"}) :| [])))
       (THtml
          ()
          [ TWhiteSpace ()
          , TExprNode
              ()
              (TECase
                 ()
                 (TEVar () TId {unTId = "foo"})
                 (TAlt
                    ()
                    (TPCon () TConstructor {unTConstructor = "Bar"} [])
                    (TAltHtml
                       ()
                       (THtml
                          ()
                          [ TElement
                              ()
                              TTag {unTTag = "blink"}
                              []
                              (THtml
                                 ()
                                 [ TPlain
                                     ()
                                     TPlainText {unTPlainText = "marquee"}
                                 ])
                          ])) :|
                  []))
          , TWhiteSpace ()
          , TElement () TTag {unTTag = "marquee"} [] (THtml () [])
          ]))


return []
tests = $disorderCheckEnvAll TestRunNormal
