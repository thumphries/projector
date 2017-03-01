{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Data.Annotation (
    Annotation (..)
  , SrcAnnotation
  , renderAnnotation
  ) where


import           P

import           Projector.Core
import           Projector.Html.Data.Position


data Annotation a
  -- TODO we want to swap SourceAnnotation for something a little more specific
  = SourceAnnotation a
  | LibraryFunction Name
  | DataConstructor Constructor TypeName
  | TypeSignature a
  | Variable Name a
  | CaseExpression a
  | InlineFunction Name a
  | FunctionApplication a
  | HtmlExpression a
  | PatternVar Name a
  | PatternCon Name a
  | StringLiteral a
  | HtmlBlock a
  | AttributeExpression a
  | ListLiteral a
  deriving (Eq, Ord, Show)

type SrcAnnotation = Annotation Range

renderAnnotation :: (a -> Text) -> Annotation a -> Text
renderAnnotation f ann =
  case ann of
    SourceAnnotation r ->
      f r
    LibraryFunction (Name n) ->
      "In the standard library function '" <> n <> "'"
    DataConstructor (Constructor c) (TypeName tn) ->
      "In the data constructor '" <> c <> "' for type '" <> tn <> "'"
    TypeSignature r ->
      f r <> " in a type signature"
    Variable (Name n) r ->
      f r <> " in the variable '" <> n <> "'"
    CaseExpression r ->
      f r <> " in a case expression"
    InlineFunction (Name n) r ->
      f r <> " in the inline function binding '" <> n <> "'"
    FunctionApplication r ->
      f r <> " in a function application"
    HtmlExpression r ->
      f r <> " in a HTML expression"
    HtmlBlock r ->
      f r <> " in a HTML block"
    PatternVar (Name n) r ->
      f r <> " in the pattern binding '" <> n <> "'"
    PatternCon (Name n) r ->
      f r <> " in the constructor pattern '" <> n <> "'"
    StringLiteral r ->
      f r <> " in a string literal"
    AttributeExpression r ->
      f r <> " in an attribute expression"
    ListLiteral r ->
      f r <> " in a list literal"
