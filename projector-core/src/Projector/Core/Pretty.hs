{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.Pretty (
    ppType
  , ppExpr
  , ppExprUntyped
  ) where


import qualified Data.Text as T

import           P

import           Projector.Core.Syntax (Expr (..), Name (..))
import           Projector.Core.Type (Type (..), Ground (..))


ppType :: Ground l => Type l -> Text
ppType t =
  case t of
    TLit g ->
      ppGroundType g

    TArrow a b ->
      "(" <> ppType a <> " -> " <> ppType b <> ")"

ppExpr :: Ground l => Expr l -> Text
ppExpr =
  ppExpr' True

ppExprUntyped :: Ground l => Expr l -> Text
ppExprUntyped =
  ppExpr' False

ppExpr' :: Ground l => Bool -> Expr l -> Text
ppExpr' types e =
  case e of
    EVar (Name n) ->
      n

    ELit b ->
      ppGroundValue b

    EApp f g ->
      let ff = ppExpr' types f
          gg = ppExpr' types g
      in parenMay ff <> " " <> parenMay gg

    ELam (Name n) t f ->
      "\\" <> n <> typeMay t <> ". " <> ppExpr' types f
  where typeMay t = if types then " : " <> ppType t else T.empty

hasSpace :: Text -> Bool
hasSpace =
  isJust . T.find (== ' ')

parenMay :: Text -> Text
parenMay t =
  if hasSpace t then "(" <> t <> ")" else t
