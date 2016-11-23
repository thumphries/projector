{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Pretty (
    ppTemplate
  ) where


import           Data.List.NonEmpty  (NonEmpty(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as T (toStrict)

import           P

import           Projector.Html.Data.Template
import           Projector.Html.Data.Token

import           Text.PrettyPrint.Leijen.Text (Doc, (<$$>))
import qualified Text.PrettyPrint.Leijen.Text as WL


ppTemplate :: Template a -> Text
ppTemplate =
  T.toStrict . WL.displayT . WL.renderPretty 0.4 100 . ppTokens . templateTokens

templateTokens :: Template a -> [Token]
templateTokens =
  undefined

ppTokens :: [Token] -> Doc
ppTokens =
  foldl' (WL.<>) WL.empty . fmap ppToken

ppToken :: Token -> Doc
ppToken t =
  case t of
    TypeSigsStart ->
      WL.char '\\'
    TypeSigsSep ->
      WL.line
    TypeSigsEnd ->
      WL.text " ->" WL.<> WL.line
    TypeSigSep ->
      WL.text " : "
    TypeIdent t ->
      WL.text t
    _ ->
      undefined
