{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Machinator.Data.Token (
    Token (..)
  , recordKeyword
  , dataKeyword
  ) where


import           Projector.Core.Prelude


data Token
  = TData
  | TIdent Text
  | TEquals
  | TChoice
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TComma
  | TTypeSig
  | TRecord
  deriving (Eq, Ord, Show)


recordKeyword :: Text
recordKeyword =
  "record"

dataKeyword :: Text
dataKeyword =
  "data"
