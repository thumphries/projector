{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Elaborator where


import qualified Data.Text as T

import           Disorder.Core
import           Disorder.Jack

import           P

import qualified Projector.Html.Core as HC
import qualified Projector.Html.Core.Elaborator as HE
import           Projector.Html.Data.Template  (Template)
import           Projector.Html.Parser.QQ  (template)


-- Input parses AND typechecks, else dump core
elabProp :: Show a => Template a -> Property
elabProp ast =
  let core = HE.elaborate ast
      ety = HC.typeCheck core
  in either (\e -> counterexample (show e) (property False)) (const (property True)) ety

tshow :: Show a => a -> Text
tshow =
  T.pack . show

-- -----------------------------------------------------------------------------
-- unit / regression tests for now, generators are hard

prop_hello =
  once (elabProp [template|Hello, world!|])

prop_foo_element =
  once (elabProp [template|\foo : HtmlNode ->
      <blink>{ foo }</blink>
  |])

prop_foo_element_neg =
  once . neg $ elabProp [template|\foo : HtmlNode ->
    <blink enabled={ foo }>Bad attribute!</blink>
  |]

prop_foo_bar =
  once $ elabProp [template|\ foo : HtmlNode
  bar : AttributeValue ->
<marquee wild={ bar }>{ foo }! {foo} !</marquee>|]


return []
tests = $disorderCheckEnvAll TestRunNormal
