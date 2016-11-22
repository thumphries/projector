{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Runtime.Library (
    Html (..)
  , Tag (..)
  , Attribute (..)
  , AttributeKey (..)
  , AttributeValue (..)
  ) where


import           Data.String  (String)


-- This needs to be kept in sync with Projector.Html.Core.Library.
data Attribute = Attribute !AttributeKey !AttributeValue
data AttributeKey = AttributeKey !String
data AttributeValue = AttributeValue !String
data Html
    = Element !Tag !([Attribute]) !([Html])
    | VoidElement !Tag !([Attribute])
    | Comment !String
    | Plain !String
data Tag = Tag !String
