{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Data.Annotation (
    Annotation (..)
  , SrcAnnotation
  , annotateTemplate
  , renderAnnotation
  ) where


import           P

import           Projector.Core
import           Projector.Html.Data.Position
import           Projector.Html.Data.Template


data Annotation a
  -- TODO we want to swap SourceAnnotation for something a little more specific
  = SourceAnnotation a
  | LibraryFunction Name
  | DataConstructor Constructor TypeName
  deriving (Eq, Ord, Show)

type SrcAnnotation = Annotation Range

annotateTemplate :: Template Range -> Template (Annotation Range)
annotateTemplate temp =
  -- TODO more specific
  fmap SourceAnnotation temp

renderAnnotation :: (a -> Text) -> Annotation a -> Text
renderAnnotation f ann =
  case ann of
    SourceAnnotation r ->
      f r
    LibraryFunction (Name n) ->
      "In the standard library function '" <> n <> "'"
    DataConstructor (Constructor c) (TypeName tn) ->
      "In the data constructor '" <> c <> "' for type '" <> tn <> "'"
