{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Data.Annotation (
    Annotation (..)
  , annotateTemplate
  , renderAnnotation
  ) where


import           P

import           Projector.Core
import           Projector.Html.Data.Position
import           Projector.Html.Data.Template


data Annotation
  -- TODO we want to swap SourceAnnotation for something a little more specific
  = SourceAnnotation Range
  | LibraryFunction Name
  deriving (Eq, Ord, Show)

annotateTemplate :: Template Range -> Template Annotation
annotateTemplate temp =
  -- TODO more specific
  fmap SourceAnnotation temp

renderAnnotation :: Annotation -> Text
renderAnnotation ann =
  case ann of
    SourceAnnotation r ->
      renderRange r
    LibraryFunction (Name n) ->
      "In the standard library function '" <> n <> "'"
