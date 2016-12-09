{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html where


import           P

import           Projector.Html.Core  (CoreError(..), templateToCore, HtmlType, HtmlExpr)
import           Projector.Html.Data.Position  (Range)
import           Projector.Html.Parser (ParseError (..), parse)

import           System.IO  (FilePath)


data HtmlError
  = HtmlParseError ParseError
  | HtmlCoreError (CoreError Range)
  deriving (Eq, Show)

thing :: FilePath -> Text -> Either HtmlError (HtmlType, HtmlExpr Range)
thing file t =
  first HtmlParseError (parse file t) >>= first HtmlCoreError . templateToCore
