{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.Pretty (
    ppTypeError
  , ppTypeErrorDecorated
  , ppWarning
  , ppWarningDecorated
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
import           Projector.Core.Warn

import           Text.PrettyPrint.Annotated.Leijen  (Doc, (<+>), (</>))
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
    UnificationError (t1, a) (t2, b) ->
      WL.hang 2
        (text "Type error. The differing types are:"
          WL.<$$> annNL a (text (ppType t1))
          WL.<$$> annNL b (text (ppType t2)))
    FreeVariable (Name n) a ->
      WL.annotate a . WL.hang 2 $
        WL.empty WL.<$$> text ("Not in scope: '" <> n <> "'")
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
               , T.intercalate ", " (fmap (unConstructor . fst) cts)
               ])
        DRecord _ ->
          WL.annotate a $
          text
            (mconcat
               [ "Invalid constructor! '"
               , c
               , "' is not a constructor for"
               , tn
               , ". Perhaps you meant: "
               , tn
               ])
    BadConstructorArity (Constructor c) i j a ->
      WL.annotate a $
      text
        (T.unwords
           [ "Constructor"
           , c
           , "expects"
           , renderIntegral i
           , "arguments, but received"
           , renderIntegral j
           ])
    BadPatternArity (Constructor c) ty nexp ngot a ->
      WL.annotate a $
      WL.hang
        2
        (((text "Invalid pattern for type") <+> text (ppType ty) <+> (text ":")) WL.<$$>
         (text "Constructor" <+>
          (WL.squotes (text c)) <+>
          text
            (T.unwords
               [ "expects"
               , renderIntegral nexp
               , "arguments, but got"
               , renderIntegral ngot
               ])))
    BadPatternConstructor (Constructor c) a ->
      WL.annotate a (text "Unknown constructor: " WL.<> WL.squotes (text c))
    MissingRecordField (TypeName tn) (FieldName fn) (ty, _b) a ->
      WL.annotate a . WL.hang 2 $
        text "Missing record field for type " WL.<> WL.squotes (text tn) WL.<> text ":"
          WL.<$$> WL.squotes (text fn) <+> text ":" <+> text (ppType ty)
    ExtraRecordField (TypeName tn) (FieldName fn) (ty, _b) a ->
      WL.annotate a . WL.hang 2 $ text "Extraneous record field for type " WL.<> WL.squotes (text tn) WL.<> text ":"
        WL.<$$> WL.squotes (text fn) <+> text ":" <+> text (ppType ty)
    DuplicateRecordFields (TypeName tn) fns a ->
      WL.annotate a $
        WL.hang 2
          (text "Duplicate record fields for type " WL.<> WL.squotes (text tn) WL.<> text ":"
          WL.<$$> (WL.hcat (WL.punctuate WL.comma (fmap (text . unFieldName) fns))))
    UndeclaredType (TypeName n) a ->
      WL.annotate a (text "Undeclared type: " WL.<> WL.squotes (text n))
    InferenceError a ->
      -- TODO this error is really awful
      WL.annotate a (text "Could not infer a monotype for some expression.")
    RecordInferenceError fts a ->
      WL.annotate a $
        WL.hang 2
          ((text "Could not infer the full type for the record with fields")
           WL.<$$> (WL.semiBraces (fmap (\(FieldName fn, ty) -> text fn WL.<> text " : " WL.<> text (ppType ty)) fts)))
    InfiniteType (t1, a) (t2, b) ->
      WL.hang 2
        (text "Type error (occurs check) - cannot construct the infinite type!"
          WL.<$$> annNL a (text (ppType t1))
          WL.<$$> annNL b (text (ppType t2)))
    TypeHole (ty, _b) a ->
      WL.annotate a . WL.hang 2 $
        text "Found hole with type:"
          WL.<$$> text (ppType ty)
    Annotated a te ->
      WL.annotate a $ ppTypeError' te


annNL :: a -> Doc a -> Doc a
annNL a val =
  WL.hang 2 ((WL.annotate a (WL.empty)) WL.<$$> val)

-- -----------------------------------------------------------------------------

ppWarning :: Ground l => Warning l a -> Text
ppWarning =
  prettyUndecorated . ppWarning'

ppWarningDecorated :: Ground l => (a -> Text) -> (a -> Text) -> Warning l a -> Text
ppWarningDecorated start end =
  prettyDecorated start end . ppWarning'

ppWarning' :: Ground l => Warning l a -> Doc a
ppWarning' w =
  case w of
    ShadowedName a (Name n) ->
      -- this message is probably not the most intuitive thing
      WL.annotate a (text ("This binding for '" <> n <> "' shadows an existing definition."))
    InexhaustiveCase a cs ->
      WL.annotate a (text ("Patterns not matched: " <> T.intercalate ", " (fmap unConstructor cs)))
    Invariant t ->
      text t

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
    Type (TLitF g) ->
      ppGroundType g

    Type (TVarF tn@(TypeName ty)) ->
      let mty = lookupType tn ctx
      in ty <> case (verbose, mty) of
           (True, Just (DVariant cts)) ->
             " = " <> ppConstructors cts
           (True, Just (DRecord fts)) ->
             " = " <> ppRecordFields fts
           (False, _) ->
             T.empty
           (_, Nothing) ->
             T.empty

    Type (TArrowF a b) ->
      "(" <> ppType a <> " -> " <> ppType b <> ")"

    Type (TListF ty) ->
      "[" <> ppType ty <> "]"

    Type (TForallF as b) ->
      "forall " <> T.intercalate " " (fmap unTypeName as) <> ". " <> ppType b

ppConstructors :: Ground l => [(Constructor, [Type l])] -> Text
ppConstructors =
  T.intercalate " | " . fmap (\(Constructor n, rts) -> T.unwords (n : fmap ppType rts))

ppRecordFields :: Ground l => [(FieldName, Type l)] -> Text
ppRecordFields =
  T.intercalate " , " . fmap (\(FieldName fn, ty) -> T.unwords [fn, ":", ppType ty])

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

    ELam a (Name n) mt f ->
      WL.annotate a $
        WL.hang 2
              ((text ("\\" <> n <> maybe mempty typeMay mt <> "."))
          WL.<$$> (ppExpr' types f))

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

    ERec a (TypeName tn) fes ->
      WL.annotate a . WL.hang 2 . WL.parens $
        text tn <+> WL.encloseSep WL.lbrace WL.rbrace WL.comma
          (fmap (\(FieldName fn, fe) -> text fn <+> text "=" <+> ppExpr' types fe) fes)

    EPrj a f fn ->
      WL.annotate a . WL.hang 2 $ ppExpr' types f WL.<> text ("." <> unFieldName fn)

    EList a es ->
      WL.annotate a $ WL.hang 2 (WL.list (fmap (ppExpr' types) es))

    EMap a f g ->
      let ff = ppExpr' types f
          gg = ppExpr' types g
      in WL.annotate a (WL.hang 2 (WL.parens ((text "map#") </> ff </> gg)))

    EForeign a (Name n) ty ->
      WL.annotate a $ WL.parens (text n WL.<> text "#" WL.<> text (typeMay ty))

    EHole a ->
      WL.annotate a (text "_")
  where typeMay t = if types then " : " <> ppType t else T.empty

ppPattern :: Pattern a -> Text
ppPattern p =
  case p of
    PVar _ (Name n) ->
      n

    PCon _ (Constructor c) ps ->
      c <> " " <> T.unwords (fmap (parenMay . ppPattern) ps)

    PWildcard _ ->
      "_"

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
