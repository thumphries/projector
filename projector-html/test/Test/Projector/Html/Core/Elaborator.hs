{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Core.Elaborator where


import qualified Data.Text as T

import           Disorder.Core
import           Disorder.Jack

import           P

import           Projector.Html
import           Projector.Html.Syntax.QQ  (template)


elabProp :: Template Range -> Property
elabProp ast =
  either
    (\e -> counterexample (show e) (property False))
    (const (property True))
    (checkTemplate mempty ast)

tshow :: Show a => a -> Text
tshow =
  T.pack . show

-- -----------------------------------------------------------------------------
-- unit / regression tests for now, generators are hard

prop_hello =
  once (elabProp [template|<h1>Hello, world!</h1>|])

prop_hello_text =
  once (elabProp [template|\ x : String -> Html ={{ x }}|])

prop_foo_element =
  once (elabProp [template|\foo : Html -> Html =
      <blink>{ foo }</blink>
  |])

prop_foo_element_neg =
  once . neg $ elabProp [template|\foo : Html -> Html =
    <blink enabled={ foo }>Bad attribute!</blink>
  |]

prop_foo_bar =
  once $ elabProp [template|\ foo : Html
  -> bar : AttributeValue
  -> Html =
<marquee wild={ bar }>{ foo }! {foo} !</marquee>|]

prop_foo_literal =
  once $ elabProp [template| { text "test" }|]

prop_each_list =
  once $ elabProp [template| { each [text "test", text "me"] \x -> x } |]

return []
tests = $disorderCheckEnvAll TestRunNormal
