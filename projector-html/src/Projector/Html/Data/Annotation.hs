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
  = EmptyAnnotation
  | SourceAnnotation a
  | LibraryFunction Name
  | DataConstructor Constructor TypeName
  | RecordConstructor TypeName
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
  | RecordProjection a FieldName
  | TypedHole a
  | Constant Name
  deriving (Eq, Ord, Show)

type SrcAnnotation = Annotation Range

renderAnnotation :: (a -> Text) -> Annotation a -> Text
renderAnnotation f ann =
  case ann of
    EmptyAnnotation ->
      "(no source information)"
    SourceAnnotation r ->
      f r
    LibraryFunction (Name n) ->
      "In the standard library function '" <> n <> "'"
    DataConstructor (Constructor c) (TypeName tn) ->
      "In the data constructor '" <> c <> "' for type '" <> tn <> "'"
    RecordConstructor (TypeName tn) ->
      "In the constructor for record '" <> tn <> "'"
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
    RecordProjection r (FieldName fn) ->
      f r <> " in a record projection '" <> fn <> "'"
    TypedHole r ->
      f r <> " in a hole '_'"
    Constant (Name n) ->
      "In a constant '" <> n <> "'"
