{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.Pretty (
    ppTypeError
  , ppTypeErrorDecorated
  , ppType
  , ppTypeInfo
  , ppExpr
  , ppExprDecorated
  , ppExprUntyped
  , ppExprUntypedDecorated
  ) where


import           Data.Functor.Identity (Identity, runIdentity)
import qualified Data.Text as T

import           P

import           Projector.Core.Check  (TypeError(..))
import           Projector.Core.Syntax (Expr (..), Name (..), Pattern (..))
import           Projector.Core.Type

import           Text.PrettyPrint.Annotated.Leijen  (Doc, (<+>), (</>), (<$$>))
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
        (mconcat
           ["Type mismatch! Expected '", ppType t1, "', but found '", ppType t2, "'"])
    CouldNotUnify ts a ->
      WL.annotate a . WL.hang 2 $
      (text "Type mismatch! Expected these types to be equal:" <$$>
       (WL.vcat (fmap (\(ty, aa) -> WL.annotate aa (text (ppType ty))) ts)))
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
      text (mconcat ["Unknown variable! '", n, "' is not in scope"])
    FreeTypeVariable (TypeName tn) a ->
      WL.annotate a $
      text (mconcat ["Unknown type! '", tn, "' has not been declared"])
    BadConstructorName (Constructor c) (TypeName tn) d a ->
      case d of
        DVariant cts ->
          WL.annotate a $
          text
            (mconcat
               [ "Invalid constructor! '"
               , c
               , "' is not a constructor for"
               , tn
               , ". Perhaps you meant one of:"
               , T.intercalate ", " (fmap (unConName . fst) cts)
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
    BadPatternArity (Constructor c) ty nexp ngot a ->
      WL.annotate a $
      WL.hang
        2
        (((text "Invalid pattern for type") <+> text (ppType ty) <+> (text ":")) <$$>
         (text "Constructor" <+>
          (WL.squotes (text c)) <+>
          text
            (T.unwords
               [ "expects"
               , renderIntegral nexp
               , "arguments, but got"
               , renderIntegral ngot
               ])))
    BadPatternConstructor (Constructor c) ty a ->
      WL.annotate a $
      WL.hang
        2
        ((text "Invalid pattern for type" <+> text (ppType ty) <+> text ":") <$$>
         (text "Constructor" <+>
          (WL.squotes (text c)) <+>
          text "is not a constructor for type" <+> text (ppType ty)))
    NonExhaustiveCase _e ty a ->
      WL.annotate a $
      text
        (T.unwords
           [ "Pattern matches are non-exhaustive for an expression of type"
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
  prettyUndecorated . ppExpr' True

ppExprUntyped :: Ground l => Expr l a -> Text
ppExprUntyped =
  prettyUndecorated . ppExpr' False

ppExprDecorated :: Ground l => (a -> Text) -> (a -> Text) -> Expr l a -> Text
ppExprDecorated start end =
  prettyDecorated start end . ppExpr' True

ppExprUntypedDecorated :: Ground l => (a -> Text) -> (a -> Text) -> Expr l a -> Text
ppExprUntypedDecorated start end =
  prettyDecorated start end . ppExpr' False

ppExpr' :: Ground l => Bool -> Expr l a -> Doc a
ppExpr' types e =
  case e of
    EVar a (Name n) ->
      WL.annotate a (text n)

    ELit a b ->
      WL.annotate a (text (ppGroundValue b))

    EApp a f g ->
      let ff = ppExpr' types f
          gg = ppExpr' types g
      in WL.annotate a (WL.hang 2 (WL.parens (ff </> gg)))

    ELam a (Name n) t f ->
      WL.annotate a $
        WL.hang 2
              ((text ("\\" <> n <> typeMay t <> "."))
          <$$> (ppExpr' types f))

    ECon a (Constructor c) _ es ->
      WL.annotate a $
        WL.nest 2
          (WL.parens (text c <+> WL.fillSep (fmap (ppExpr' types) es)))

    ECase a f bs ->
      WL.annotate a $
        WL.hang 2
              ((WL.hang 2 (text "case" <+> ppExpr' types f <+> text "of"))
          </> (foldr (\(p, g) doc ->
                     (WL.hang 2 ((text (ppPattern p) <+> text "->") </> ppExpr' types g))
                 </> (doc WL.<> text ";")) WL.empty bs))

    EList a _ es ->
      WL.annotate a $ WL.hang 2 (WL.list (fmap (ppExpr' types) es))

    EForeign a (Name n) ty ->
      WL.annotate a $ WL.parens (text n WL.<> text "#" WL.<> text (typeMay ty))
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
