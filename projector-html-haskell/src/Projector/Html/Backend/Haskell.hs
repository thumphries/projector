{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Projector.Html.Backend.Haskell (
    haskellBackend
  ---
  , renderModule
  , renderExpr
  , predicates
  , HaskellError (..)
  , renderHaskellError
  -- * guts
  , genModule
  , genTypeDecs
  , genTypeDec
  , genExpDec
  , genType
  , genExp
  , genMatch
  , genPat
  , genLit
  ) where


import qualified Data.Char as Char
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import qualified Language.Haskell.TH as TH

import           P

import           Projector.Core

import           Projector.Html.Core
import           Projector.Html.Core.Library
import           Projector.Html.Data.Backend hiding (Backend(..))
import qualified Projector.Html.Data.Backend as BE
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim
import           Projector.Html.Backend.Haskell.Prim
import           Projector.Html.Backend.Haskell.Rewrite

import           System.IO (FilePath)

import           X.Language.Haskell.TH.Syntax


haskellBackend :: BE.Backend a HaskellError
haskellBackend =
  BE.Backend {
      BE.renderModule = renderModule
    , BE.renderExpr = renderExpr
    , BE.predicates = predicates
    }

-- -----------------------------------------------------------------------------

data HaskellError
  = HtmlCase -- TODO should really return decorated parameterised error here
  | RecordTypeInvariant
  | TypeHolePresent
  deriving (Eq, Ord, Show)

renderHaskellError :: HaskellError -> Text
renderHaskellError e =
  case e of
    HtmlCase ->
      "Don't case on Html, pal!"
    RecordTypeInvariant ->
      "BUG: Invariant failure - expected a record type, but found something else."
    TypeHolePresent ->
      "BUG: Type hole was present for code generation. Should have been a type error."

predicates :: [Predicate HaskellError]
predicates = [
    PatPredicate $ \case
      PCon _ c _ ->
        if S.member c htmlConstructors
          then PredError HtmlCase
          else PredOk
      _ ->
        PredOk
  ]

-- | The set of constructors used for the Html library type.
htmlConstructors :: Set Constructor
htmlConstructors =
  fold [
      sumCons dHtml
    , sumCons dTag
    , sumCons dAttribute
    , sumCons dAttributeKey
    , sumCons dAttributeValue
    ]
  where
  sumCons x =
    case x of
      DVariant _ps cts ->
        S.fromList (fmap fst cts)
      DRecord _ps _ ->
        mempty




-- -----------------------------------------------------------------------------

renderModule :: HtmlDecls -> ModuleName -> Module HtmlType PrimT (HtmlType, a) -> Either HaskellError (FilePath, Text)
renderModule _decls mn@(ModuleName n) m = do
  let pragmas = [
          "{-# LANGUAGE NoImplicitPrelude #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
        -- FIX This is because of each introducing a lambda with a constant variable "x"
        -- We should be writing the tree to stop shadowing in the output
        , "{-# OPTIONS_GHC -fno-warn-name-shadowing #-}"
        ]
      (_mn', m') = second toHaskellModule (rewriteModule mn m)
      modName = T.unwords ["module", n, "where"]
      importText = fmap (uncurry genImport) imports
      imports =
        (htmlRuntime, ImportQualified)
          : M.toList (moduleImports m')
  decls <- fmap (fmap (T.pack . TH.pprint)) (genModule m')
  pure (genFileName mn, T.unlines $ mconcat [
       pragmas
     , [modName]
     , importText
     , decls
     ])

renderExpr :: HtmlDecls -> Name -> HtmlExpr (HtmlType, a) -> Either HaskellError Text
renderExpr _decls n =
  fmap (T.pack . TH.pprint) . genExpDec n . toHaskellExpr . rewriteExpr

genImport :: ModuleName -> Imports -> Text
genImport (ModuleName n) imports =
  case imports of
    OpenImport ->
      "import " <> n
    OnlyImport funs ->
      "import " <> n <> " (" <> T.intercalate ", " (fmap unName funs) <> ")"
    ImportQualified ->
      "import qualified " <> n
    ImportQualifiedAs (ModuleName mn) ->
      "import qualified " <> n <> " as " <> mn

genModule :: HaskellModule (HtmlType, a) -> Either HaskellError [TH.Dec]
genModule (Module ts _ es) = do
  let tdecs = genTypeDecs ts
  decs <- for (M.toList es) $ \(n, ModuleExpr ty e) -> do
    d <- genExpDec n e
    pure [genTypeSig n ty, d]
  pure (tdecs <> fold decs)

genFileName :: ModuleName -> FilePath
genFileName (ModuleName n) =
  T.unpack (T.replace "." "/" n) <> ".hs"

htmlRuntime :: ModuleName
htmlRuntime =
  ModuleName "Projector.Html.Runtime"

-- -----------------------------------------------------------------------------
-- | Type declarations.
--
-- This should be done via Machinator eventually.
-- They shouldn't even be a field in 'Module'.
-- We only use this for testing: we generate a lot of datatypes for our expressions.

genTypeDecs :: HaskellDecls -> [TH.Dec]
genTypeDecs =
  fmap (uncurry genTypeDec) . M.toList . unTypeDecls

genTypeDec :: TypeName -> HaskellDecl -> TH.Dec
genTypeDec (TypeName n) ty =
  case ty of
    DVariant ps cts ->
      data_ (mkName_ n) (fmap (mkName_ . unTypeName) ps) (fmap (uncurry genCon) cts)
    DRecord ps fts ->
      data_ (mkName_ n) (fmap (mkName_ . unTypeName) ps) [recordCon (Constructor n) fts]

-- | Constructor declarations.
genCon :: Constructor -> [HaskellType] -> TH.Con
genCon (Constructor n) ts =
  normalC_' (mkName_ n) (fmap genType ts)

-- | Record declarations.
recordCon :: Constructor -> [(FieldName, HaskellType)] -> TH.Con
recordCon c@(Constructor n) fts =
  recC_' (mkName_ n) (fmap (uncurry (genRecordField c)) fts)

genRecordField :: Constructor -> FieldName -> HaskellType -> (TH.Name, TH.Type)
genRecordField c fn ft =
  (fieldNameHeuristic c fn, genType ft)

-- | Decide on a field name.
-- TODO this should probably be customisable.
-- TODO this has to line up with machinator, dedupe needed
fieldNameHeuristic :: Constructor -> FieldName -> TH.Name
fieldNameHeuristic (Constructor c) (FieldName n) =
  mkName_ (T.singleton (Char.toLower (T.head c)) <> T.drop 1 c <> (T.toTitle n))

-- | Types.
genType :: HaskellType -> TH.Type
genType (Type ty) =
  case ty of
    TLitF l ->
      conT (mkName_ (ppGroundType l))

    TVarF (TypeName n) ->
      conT (mkName_ n)

    TArrowF t1 t2 ->
      arrowT_ (genType t1) (genType t2)

    TAppF t1 t2 ->
      appT (genType t1) (genType t2)

    TListF t ->
      listT_ (genType t)

    TForallF _ts t1 ->
      -- Abuse implicit foralls
      genType t1

-- -----------------------------------------------------------------------------

-- | Expression declarations.
genExpDec :: Name -> HaskellExpr (HtmlType, a) -> Either HaskellError TH.Dec
genExpDec (Name n) expr =
  val_ (varP (mkName_ n)) <$> genExp expr

genTypeSig :: Name -> HaskellType -> TH.Dec
genTypeSig (Name n) ty =
  sig (mkName_ n) (genType ty)


-- | Expressions.
genExp :: HaskellExpr (HtmlType, a) -> Either HaskellError TH.Exp
genExp expr =
  case expr of
    ELit _ v ->
      pure (litE (genLit v))

    EVar _ (Name x) ->
      pure (varE (mkName_ x))

    ELam _ (Name n) _ body ->
      lamE [varP (mkName_ n)] <$> genExp body

    EApp _ fun arg ->
      appE <$> genExp fun <*> genExp arg

    ECon _ (Constructor c) _ es ->
      applyE (conE (mkName_ c)) <$> traverse genExp es

    ECase _ e pats ->
      caseE <$> genExp e <*> traverse (uncurry genMatch) pats

    ERec _ (TypeName tn) fes -> do
      recConE (mkName_ tn) <$> traverse (\(fn, fe) -> (fieldNameHeuristic (Constructor tn) fn,) <$> genExp fe) fes

    EPrj _ e fn ->
      genRecordPrj e fn

    EList _ es ->
      listE <$> traverse genExp es

    EForeign _ (Name x) _ ->
      pure (varE (mkName_ x))

    EMap _ f g -> do
      f' <- genExp f
      g' <- genExp g
      pure (applyE (varE (mkName_ "Projector.Html.Runtime.fmap")) [f', g'])

    EHole _ ->
      Left TypeHolePresent

-- | Compile a Projector record projection to Haskell.
--
-- This is type-directed, unlike everything else in this compiler!
-- We need to look at the type of the expr, and use it to apply 'fieldNameHeuristic'.
--
-- This is partial, but the failing branch can only occur if the
-- typechecker doesn't work.
genRecordPrj :: HaskellExpr (HtmlType, a) -> FieldName -> Either HaskellError TH.Exp
genRecordPrj e fn =
  case extractAnnotation e of
    (TVar (TypeName recName), _) ->
      appE (varE (fieldNameHeuristic (Constructor recName) fn)) <$> genExp e

    (_, _) ->
      Left RecordTypeInvariant

-- | Case alternatives.
genMatch :: Pattern (HtmlType, a) -> HaskellExpr (HtmlType, a) -> Either HaskellError TH.Match
genMatch p e =
  match_ (genPat p) <$> genExp e

-- | Patterns.
genPat :: Pattern (HtmlType, a) -> TH.Pat
genPat p = case p of
  PVar _ (Name n) ->
    varP (mkName_ n)

  PCon _ (Constructor n) ps ->
    conP (mkName_ n) (fmap genPat ps)

  PWildcard _ ->
    varP (mkName_ "_")

-- | Literals.
genLit :: Value HaskellPrimT -> TH.Lit
genLit v =
  case v of
    HTextV x ->
      stringL_ x
