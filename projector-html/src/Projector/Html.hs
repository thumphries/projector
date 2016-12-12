{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html (
    HtmlError (..)
  , renderHtmlError
  , thing
  , interact
  ) where


import qualified Data.Text.IO as T

import           P

import qualified Projector.Core as Core
import qualified Projector.Html.Core as HC
import           Projector.Html.Core  (CoreError(..), HtmlType, HtmlExpr)
import           Projector.Html.Data.Position  (Range)
import           Projector.Html.Parser (ParseError (..), renderParseError, parse)

import           System.IO  (FilePath, IO, stderr)


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

thing :: FilePath -> Text -> Either HtmlError (HtmlType, HtmlExpr Range)
thing file t =
  first HtmlParseError (parse file t) >>= first HtmlCoreError . HC.templateToCore

-- shunt to repl executable soon
interact :: FilePath -> Text -> IO ()
interact file t =
  ecase (thing file t) (T.hPutStrLn stderr . renderHtmlError) $ \(ty, core) -> do
    T.putStrLn (Core.ppType ty)
    T.putStrLn (Core.ppExpr core)
