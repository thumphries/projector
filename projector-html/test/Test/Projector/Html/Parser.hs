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
import           Projector.Html (parseTemplate)



-- -----------------------------------------------------------------------------
-- regression unit tests

parseProp :: Text -> Template () -> Property
parseProp t p =
  fmap (fmap (const ())) (parseTemplate "Test.Projector.Html.Parser" t) === pure p

emptyTemplate :: Template ()
emptyTemplate =
  Template () Nothing (THtml () [])

prop_parse_empty =
  parseProp "" emptyTemplate

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
  once $ parseProp "\\foo : Bar ->\n<a href={foo} enabled>Google</a>" emptyTemplate

prop_parse_unbalanced =
  once $ parseProp "\\foo : Bar ->\n<a><img src=\"google\"/> no close" emptyTemplate

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
