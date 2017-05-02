{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Backend.Purescript (
    purescriptBackend
  ---
  , renderModule
  , renderExpr
  , predicates
  , PurescriptError
  , renderPurescriptError
  ) where


import           Data.Functor.Identity  (Identity, runIdentity)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           P

import           Projector.Core

import           Projector.Html.Core
import           Projector.Html.Data.Backend hiding (Backend (..))
import qualified Projector.Html.Data.Backend as BE
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim

import           System.IO  (FilePath)

import           Text.PrettyPrint.Annotated.Leijen  (Doc, (<+>), (</>))
import qualified Text.PrettyPrint.Annotated.Leijen as WL


purescriptBackend :: BE.Backend a PurescriptError
purescriptBackend =
  BE.Backend {
      BE.renderModule = renderModule
    , BE.renderExpr = renderExpr
    , BE.predicates = predicates
    }

-- -----------------------------------------------------------------------------

data PurescriptError
  = RecordTypeInvariant
  | TypeHolePresent
  deriving (Eq, Ord, Show)

renderPurescriptError :: PurescriptError -> Text
renderPurescriptError e =
  case e of
    RecordTypeInvariant ->
      "BUG: Invariant failure - expected a record type, but found something else."
    TypeHolePresent ->
      "BUG: Type hole was present for code generation. Should have been a type error."

predicates :: [Predicate PurescriptError]
predicates = [
  ]

-- -----------------------------------------------------------------------------

renderModule :: ModuleName -> Module HtmlType PrimT (HtmlType, a) -> Either PurescriptError (FilePath, Text)
renderModule mn@(ModuleName n) m = do
  let modName = T.unwords ["module", n, "where"]
      imports = (htmlRuntime, OpenImport) : (M.toList (moduleImports m))
      importText = fmap (uncurry genImport) imports
  decls <- fmap (fmap prettyUndecorated) (genModule m)
  pure (genFileName mn, T.unlines $ mconcat [
      [modName]
    , importText
    , decls
    ])

renderExpr :: Name -> HtmlExpr (HtmlType, a) -> Either PurescriptError Text
renderExpr n =
  fmap prettyUndecorated . genExpDec n

genModule :: Module HtmlType PrimT (HtmlType, a) -> Either PurescriptError [Doc (HtmlType, a)]
genModule (Module ts _ es) = do
  let tdecs = genTypeDecs ts
  decs <- for (M.toList es) $ \(n, ModuleExpr ty e) -> do
    d <- genExpDec n e
    pure [genTypeSig n ty, d]
  pure (tdecs <> fold decs)

genImport :: ModuleName -> Imports -> Text
genImport (ModuleName n) imports =
  case imports of
    OpenImport ->
      "import " <> n
    OnlyImport funs ->
      "import " <> n <> " (" <> T.intercalate ", " (fmap unName funs) <> ")"
    ImportQualified ->
      "import qualified " <> n

genFileName :: ModuleName -> FilePath
genFileName (ModuleName n) =
  T.unpack (T.replace "." "/" n) <> ".purs"

htmlRuntime :: ModuleName
htmlRuntime =
  ModuleName "Projector.Html.Runtime"


-- -----------------------------------------------------------------------------

genTypeDecs :: HtmlDecls -> [Doc a]
genTypeDecs =
  fmap (uncurry genTypeDec) . M.toList . unTypeDecls

genTypeDec :: TypeName -> HtmlDecl -> Doc a
genTypeDec (TypeName n) ty =
  case ty of
    DVariant cts ->
      WL.hang 2
        (text "data" <+> text n WL.<$$> text "="
          WL.<> (foldl'
                  (<+>)
                  WL.empty
                  (WL.punctuate (WL.linebreak WL.<> text "|") (fmap (uncurry genCon) cts))))
    DRecord fts ->
      WL.vcat [
        -- newtype
          WL.hang 2
            (text "newtype" <+> text n <+> text "=" <+> text n <+> WL.lbrace
              WL.<$$> WL.vcat (WL.punctuate WL.comma (with fts $ \(FieldName fn, ft) -> text fn <+> text "::" <+> genType ft))
              WL.<$$> WL.rbrace)
        ]

genCon :: Constructor -> [HtmlType] -> Doc a
genCon (Constructor c) ts =
  WL.hang 2 (text c WL.<> foldl' (<+>) WL.empty (fmap genType ts))

genType :: HtmlType -> Doc a
genType ty =
  case ty of
    Type (TLitF l) ->
      text (ppGroundType l)

    Type (TVarF (TypeName n)) ->
      text n

    Type (TArrowF t1 t2) ->
      WL.parens (genType t1 <+> text "->" <+> genType t2)

    Type (TListF t)->
      WL.parens (text "Array" <+> genType t)

    Type (TForallF ts t1) ->
      WL.parens (text "forall" <+> text (T.unwords $ fmap unTypeName ts) WL.<> text "." <+> genType t1)

genTypeSig :: Name -> HtmlType -> Doc a
genTypeSig (Name n) ty =
  WL.hang 2 (text n <+> "::" <+> genType ty)

genExpDec :: Name -> HtmlExpr (HtmlType, a) -> Either PurescriptError (Doc (HtmlType, a))
genExpDec (Name n) expr = do
  e <- genExp expr
  pure (WL.hang 2 (text n <+> text "=" WL.<$$> e))

genExp :: HtmlExpr (HtmlType, a) -> Either PurescriptError (Doc (HtmlType, a))
genExp expr =
  case expr of
    ELit a v ->
      pure (WL.annotate a (genLit v))

    EVar a (Name x) ->
      pure (WL.annotate a (text x))

    ELam a (Name n) _ body -> do
      body' <- genExp body
      pure (WL.annotate a (WL.hang 2 (WL.parens (text ("\\" <> n <> " -> ") WL.<$$> body'))))

    EApp a fun arg -> do
      fun' <- genExp fun
      arg' <- genExp arg
      pure (WL.annotate a (WL.hang 2 (WL.parens (fun' </> arg'))))

    ECon a (Constructor c) _ es -> do
      es' <- traverse genExp es
      pure (WL.annotate a (WL.nest 2 (WL.parens (text c <+> WL.fillSep es'))))

    ECase a f bs -> do
      f' <- genExp f
      fmap
        (WL.annotate a . WL.hang 2 . WL.parens . (text "case" <+> f' <+> text "of" WL.<$$>))
        (foldrM
          (\(p, g) doc -> do
            mat <- genMatch p g
            pure (WL.hang 2 mat WL.<$$> doc))
          WL.empty
          bs)

    ERec a (TypeName tn) fes -> do
      fes' <- traverse (uncurry fieldInst) fes
      pure (WL.annotate a . WL.hang 2 . WL.parens $
        text tn <+> WL.encloseSep WL.lbrace WL.rbrace WL.comma fes')

    EPrj a e fn ->
      WL.annotate a <$> genRecordPrj e fn

    EList a es -> do
      es' <- traverse genExp es
      pure (WL.annotate a (WL.hang 2 (WL.list es')))

    EMap a f g ->
      genExp (EApp a (EApp a (EVar a (Name "map")) f) g)

    EForeign a (Name n) _ ->
      pure (WL.annotate a (text n))

    EHole _ ->
      Left TypeHolePresent

fieldInst :: FieldName -> HtmlExpr (HtmlType, a) -> Either PurescriptError (Doc (HtmlType, a))
fieldInst (FieldName fn) expr = do
  expr' <- genExp expr
  pure (text (fn <> ":") <+> expr')

-- This is the only type-directed part of purescript codegen.
-- Could potentially get rid of this with purescript-newtype unwrap.
genRecordPrj :: HtmlExpr (HtmlType, a) -> FieldName -> Either PurescriptError (Doc (HtmlType, a))
genRecordPrj e (FieldName fn) =
  case extractAnnotation e of
    (TVar (TypeName recName), _) -> do
      e' <- genExp e
      pure (WL.parens (text "case" <+> e' <+> text "of" <+> text recName <+> text "x -> x")
        WL.<> (text ("." <> fn)))
    (_, _) ->
      Left RecordTypeInvariant

genMatch :: Pattern (HtmlType, a) -> HtmlExpr (HtmlType, a) -> Either PurescriptError (Doc (HtmlType, a))
genMatch p e = do
  e' <- genExp e
  pure (WL.hang 2 ((genPat p WL.<> text " ->") WL.<$$> e'))

genPat :: Pattern a -> Doc a
genPat p =
  case p of
    PVar a (Name n) ->
      WL.annotate a (text n)
    PCon a (Constructor n) ps ->
      WL.annotate a (WL.parens (text n <+> WL.hsep (fmap genPat ps)))

genLit :: Value PrimT -> Doc a
genLit v =
  case v of
    VString x ->
      WL.dquotes (text x)

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
