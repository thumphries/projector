{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Elaborator where


import           Data.String.QQ (s)
import qualified Data.Text as T

import           Disorder.Core
import           Disorder.Either (testEither)
import           Disorder.Jack

import           P

import qualified Projector.Html as Html
import qualified Projector.Html.Core as HC
import qualified Projector.Html.Core.Elaborator as HE
import qualified Projector.Html.Parser as HP


-- Input parses AND typechecks, else dump core
elabProp :: Text -> Property
elabProp t =
  testEither tshow $ do
    ast <- first Html.HtmlParseError (HP.parse "Test.Projector.Html.Elaborator" t)
    let core = HE.elaborate ast
        ety = HC.typeCheck core
    pure $ either (\e -> counterexample (show e) (property False)) (const (property True)) ety

tshow :: Show a => a -> Text
tshow =
  T.pack . show

-- -----------------------------------------------------------------------------
-- unit / regression tests for now, generators are hard

prop_hello =
  once (elabProp [s|Hello, world!|])

prop_foo_element =
  once (elabProp [s|\foo : HtmlNode ->
      <blink>{ foo }</blink>
  |])

prop_foo_element_neg =
  once . neg $ elabProp [s|\foo : HtmlNode ->
    <blink enabled={ foo }>Bad attribute!</blink>
  |]

return []
tests = $disorderCheckEnvAll TestRunNormal
