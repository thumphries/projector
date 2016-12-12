{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html (
    HtmlError (..)
  , renderHtmlError
  , parseTemplate
  , checkTemplate
  -- * re-exports
  , Template
  , Range
  , HtmlType
  , HtmlExpr
  ) where


import           P

import qualified Projector.Html.Core as HC
import           Projector.Html.Core  (CoreError(..), HtmlType, HtmlExpr)
import           Projector.Html.Data.Position  (Range)
import           Projector.Html.Data.Template  (Template)
import           Projector.Html.Parser (ParseError (..), renderParseError, parse)

import           System.IO  (FilePath)


data HtmlError
  = HtmlParseError ParseError
  | HtmlCoreError (CoreError Range)
  deriving (Eq, Show)

renderHtmlError :: HtmlError -> Text
renderHtmlError he =
  case he of
    HtmlParseError e ->
      renderParseError e
    HtmlCoreError e ->
      HC.renderCoreErrorRange e

parseTemplate :: FilePath -> Text -> Either HtmlError (Template Range)
parseTemplate f =
  first HtmlParseError . parse f

checkTemplate :: Template Range -> Either HtmlError (HtmlType, HtmlExpr Range)
checkTemplate =
  first HtmlCoreError . HC.templateToCore
