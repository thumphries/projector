{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.Projector.Html.Core.Elaborator where


import qualified Data.Text as T

import           Hedgehog

import           Projector.Core.Prelude

import           Projector.Html
import           Projector.Html.Syntax.QQ  (template)

import           System.IO (IO)


elabProp :: Template Projector.Html.Range -> PropertyT IO ()
elabProp ast =
  case checkTemplate mempty ast of
    Left e -> do
      annotateShow e
      failure
    Right _ ->
      success

elabPropNeg :: Template Projector.Html.Range -> PropertyT IO ()
elabPropNeg ast =
  case checkTemplate mempty ast of
    Left _ ->
      success
    Right v -> do
      annotateShow v
      failure

tshow :: Show a => a -> Text
tshow =
  T.pack . show

-- -----------------------------------------------------------------------------
-- unit / regression tests for now, generators are hard
prop_hello :: Property
prop_hello =
  once (elabProp [template|<h1>Hello, world!</h1>|])

prop_hello_text :: Property
prop_hello_text =
  once (elabProp [template|\ x : String -> Html ={{ x }}|])

prop_foo_element :: Property
prop_foo_element =
  once (elabProp [template|\foo : Html -> Html =
      <blink>{ foo }</blink>
  |])

prop_foo_element_neg :: Property
prop_foo_element_neg =
  once $ elabPropNeg [template|\foo : Html -> Html =
    <blink enabled={ foo }>Bad attribute!</blink>
  |]


prop_foo_bar :: Property
prop_foo_bar =
  once $ elabProp [template|\ foo : Html
  -> bar : AttributeValue
  -> Html =
<marquee wild={ bar }>{ foo }! {foo} !</marquee>|]

prop_foo_literal :: Property
prop_foo_literal =
  once $ elabProp [template| { text "test" }|]

prop_each_list :: Property
prop_each_list =
  once $ elabProp [template| { each [text "test", text "me"] \x -> x } |]

once :: PropertyT IO () -> Property
once =
  withTests 1 . property

tests :: IO Bool
tests =
  checkParallel $$(discover)
