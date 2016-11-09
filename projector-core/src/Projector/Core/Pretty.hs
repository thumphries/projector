{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.Pretty (
    ppType
  , ppTypeInfo
  , ppExpr
  , ppExprUntyped
  ) where


import qualified Data.Text as T

import           P

import           Projector.Core.Syntax (Expr (..), Name (..), Pattern (..))
import           Projector.Core.Type (Type (..), Ground (..), Constructor (..), TypeName (..))


ppType :: Ground l => Type l -> Text
ppType =
  ppType' False

ppTypeInfo :: Ground l => Type l -> Text
ppTypeInfo =
  ppType' True

ppType' :: Ground l => Bool -> Type l -> Text
ppType' verbose t =
  case t of
    TLit g ->
      ppGroundType g

    TVar (TypeName ty) ->
      ty

    TArrow a b ->
      "(" <> ppType a <> " -> " <> ppType b <> ")"

    TVariant (TypeName ty) cts ->
      ty <> if verbose then " = " <> ppConstructors cts else T.empty

ppConstructors :: Ground l => [(Constructor, [Type l])] -> Text
ppConstructors =
  T.intercalate " | " . fmap (\(Constructor n, rts) -> T.unwords (n : fmap ppType rts))

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

    ECon (Constructor c) _ es ->
      c <> " " <> T.unwords (fmap (parenMay . ppExpr' types) es)

    ECase f bs ->
      "case " <> ppExpr' types f <> " of " <>
        T.intercalate "; " (fmap (\(p, g) -> ppPattern p <> " -> " <> ppExpr' types g) bs)

  where typeMay t = if types then " : " <> ppType t else T.empty

ppPattern :: Pattern -> Text
ppPattern p =
  case p of
    PVar (Name n) ->
      n

    PCon (Constructor c) ps ->
      c <> " " <> T.unwords (fmap (parenMay . ppPattern) ps)

hasSpace :: Text -> Bool
hasSpace =
  isJust . T.find (== ' ')

parenMay :: Text -> Text
parenMay t =
  if hasSpace t then "(" <> t <> ")" else t
