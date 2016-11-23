{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Pretty (
    ppTemplate
  ) where


import           Data.List.NonEmpty  (NonEmpty(..))
import qualified Data.Text as T

import           P

import           Projector.Html.Data.Template

import           Text.PrettyPrint.Leijen (Doc, (<$$>))
import qualified Text.PrettyPrint.Leijen as WL


ppTemplate :: Template a -> Text
ppTemplate =
  T.pack . ($ []) . WL.displayS . WL.renderPretty 0.4 100 . ppTemplate'

ppTemplate' :: Template a -> Doc
ppTemplate' (Template _ mts html) =
       maybe WL.empty ppTypeSig mts
  <$$> ppHtml html

ppTypeSig :: TTypeSig a -> Doc
ppTypeSig (TTypeSig _ sigs) =
  WL.char '\\' <> ppTypeSigs sigs <> WL.text " ->"

ppTypeSigs :: 

ppHtml :: THtml a -> Doc
ppHtml =
  undefined
