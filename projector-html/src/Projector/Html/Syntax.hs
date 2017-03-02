{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Syntax (
    templateFromText
  , SyntaxError (..)
  , renderSyntaxError
  ) where


import           P

import           Projector.Html.Data.Position (Range)
import           Projector.Html.Data.Template (Template)

import           System.IO (FilePath)


data SyntaxError
  = SyntaxParseError
  deriving (Eq, Ord, Show)

renderSyntaxError :: SyntaxError -> Text
renderSyntaxError =
  undefined

templateFromText :: FilePath -> Text -> Either SyntaxError (Template Range)
templateFromText =
  undefined
