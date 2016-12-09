{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.Pretty (
    ppTypeError
  , ppTypeErrorDecorated
  , ppType
  , ppTypeInfo
  , ppExpr
  , ppExprUntyped
  ) where


import           Data.Functor.Identity (Identity, runIdentity)
import qualified Data.Text as T

import           P

import           Projector.Core.Check  (TypeError(..))
import           Projector.Core.Syntax (Expr (..), Name (..), Pattern (..))
import           Projector.Core.Type

import           Text.PrettyPrint.Annotated.Leijen  (Doc)
import qualified Text.PrettyPrint.Annotated.Leijen as WL

-- -----------------------------------------------------------------------------

ppTypeError :: Ground l => TypeError l a -> Text
ppTypeError =
  prettyUndecorated . ppTypeError'

ppTypeErrorDecorated :: Ground l => (a -> Text) -> (a -> Text) -> TypeError l a -> Text
ppTypeErrorDecorated start end =
  prettyDecorated start end . ppTypeError'

ppTypeError' :: Ground l => TypeError l a -> Doc a
ppTypeError' err =
  case err of
    Mismatch t1 t2 a ->
      WL.annotate a $
      text
        (T.unwords
           ["Type mismatch! Expected", ppType t1, ", but found", ppType t2])
    CouldNotUnify ts
    --WL.annotate a $
     ->
      text
        (T.unwords
           ("Type mismatch! Expected these to be equal:" : fmap ppType ts))
    ExpectedArrow t1 t2 a ->
      WL.annotate a $
      text
        (T.unwords
           [ "Type mismatch! Expected a function from"
           , ppType t1
           , ", but found"
           , ppType t2
           ])
    FreeVariable (Name n) a ->
      WL.annotate a $
      text (T.unwords ["Unknown variable! '", n, "' is not in scope"])
    FreeTypeVariable (TypeName tn) a ->
      WL.annotate a $
      text (T.unwords ["Unknown type! '", tn, "' has not been declared"])
    BadConstructorName (Constructor c) (TypeName tn) d a ->
      case d of
        DVariant cts ->
          WL.annotate a $
          text
            (T.unwords
               [ "Invalid constructor! '"
               , c
               , "' is not a constructor for"
               , tn
               , ". Perhaps you meant one of:"
               , T.unwords (fmap (unConName . fst) cts)
               ])
    BadConstructorArity (Constructor c) (DVariant cts) i a ->
      WL.annotate a $
      text
        (T.unwords
           [ "Constructor"
           , c
           , "expects"
           , renderIntegral (length cts)
           , "arguments, but received"
           , renderIntegral i
           ])
    BadPattern t p ->
      text
        (T.unwords ["Invalid pattern '", ppPattern p, "' for type", ppType t])
    NonExhaustiveCase _e ty a ->
      WL.annotate a $
      text
        (T.unwords
           [ "Pattern matches are non-exhaustive for expression of type"
           , ppType ty
           ])

-- -----------------------------------------------------------------------------

ppType :: Ground l => Type l -> Text
ppType =
  ppType' mempty False

ppTypeInfo :: Ground l => TypeDecls l -> Type l -> Text
ppTypeInfo ctx =
  ppType' ctx True

ppType' :: Ground l => TypeDecls l -> Bool -> Type l -> Text
ppType' ctx verbose t =
  case t of
    TLit g ->
      ppGroundType g

    TVar tn@(TypeName ty) ->
      let mty = lookupType tn ctx
      in ty <> case (verbose, mty) of
           (True, Just (DVariant cts)) ->
             " = " <> ppConstructors cts
           (False, _) ->
             T.empty
           (_, Nothing) ->
             T.empty

    TArrow a b ->
      "(" <> ppType a <> " -> " <> ppType b <> ")"

    TList ty ->
      "[" <> ppType ty <> "]"

ppConstructors :: Ground l => [(Constructor, [Type l])] -> Text
ppConstructors =
  T.intercalate " | " . fmap (\(Constructor n, rts) -> T.unwords (n : fmap ppType rts))

-- -----------------------------------------------------------------------------

ppExpr :: Ground l => Expr l a -> Text
ppExpr =
  ppExpr' True

ppExprUntyped :: Ground l => Expr l a -> Text
ppExprUntyped =
  ppExpr' False

ppExpr' :: Ground l => Bool -> Expr l a -> Text
ppExpr' types e =
  case e of
    EVar _ (Name n) ->
      n

    ELit _ b ->
      ppGroundValue b

    EApp _ f g ->
      let ff = ppExpr' types f
          gg = ppExpr' types g
      in parenMay ff <> " " <> parenMay gg

    ELam _ (Name n) t f ->
      "\\" <> n <> typeMay t <> ". " <> ppExpr' types f

    ECon _ (Constructor c) _ es ->
      c <> " " <> T.unwords (fmap (parenMay . ppExpr' types) es)

    ECase _ f bs ->
      "case " <> ppExpr' types f <> " of " <>
        T.intercalate "; " (fmap (\(p, g) -> ppPattern p <> " -> " <> ppExpr' types g) bs)

    EList _ _ es ->
      "[" <> T.intercalate ", " (fmap (ppExpr' types) es) <> "]"

    EForeign _ (Name n) ty ->
      parenMay (n <> "#" <> typeMay ty)

  where typeMay t = if types then " : " <> ppType t else T.empty

ppPattern :: Pattern a -> Text
ppPattern p =
  case p of
    PVar _ (Name n) ->
      n

    PCon _ (Constructor c) ps ->
      c <> " " <> T.unwords (fmap (parenMay . ppPattern) ps)

hasSpace :: Text -> Bool
hasSpace =
  isJust . T.find (== ' ')

parenMay :: Text -> Text
parenMay t =
  if hasSpace t then "(" <> t <> ")" else t

-- -----------------------------------------------------------------------------

text :: Text -> Doc a
text =
  WL.string . T.unpack

pretty :: Doc a -> WL.SimpleDoc a
pretty =
  WL.renderPretty 0.4 100

prettyDecorated :: (a -> Text) -> (a -> Text) -> Doc a -> Text
prettyDecorated start end =
  runIdentity . WL.displayDecoratedA str (pure . start) (pure . end) . pretty
  where
    str :: [Char] -> Identity Text
    str = pure . T.pack

prettyUndecorated :: Doc a -> Text
prettyUndecorated =
  prettyDecorated (const mempty) (const mempty)
