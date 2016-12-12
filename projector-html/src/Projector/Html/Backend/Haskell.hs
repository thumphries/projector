{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Projector.Html.Backend.Haskell (
    ModuleName (..)
  , renderModule
  , renderExpr
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


import qualified Data.Map.Strict as M
import qualified Data.Text as T

import qualified Language.Haskell.TH as TH

import           P

import           Projector.Core
import           Projector.Html.Backend.Data
import           Projector.Html.Backend.Haskell.TH
import           Projector.Html.Core.Prim as Prim

import           System.IO (FilePath)


-- -----------------------------------------------------------------------------

renderModule :: ModuleName -> Module a -> (FilePath, Text)
renderModule mn@(ModuleName n) m =
  let pragmas = [
          "{-# LANGUAGE NoImplicitPrelude #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        ]
      modName = T.unwords ["module", n, "where"]
      imports = fmap (uncurry genImport) (M.toList (moduleImports m))
      decls = fmap (T.pack . TH.pprint) (genModule m)

  in (genFileName mn, T.unlines $ mconcat [
         pragmas
       , [modName]
       , imports
       , decls
       ])

renderExpr :: Name -> HtmlExpr a -> Text
renderExpr n =
  T.pack . TH.pprint . genExpDec n

genImport :: ModuleName -> Imports -> Text
genImport (ModuleName n) imports =
  T.unwords [
      "import"
    , n
    , case imports of
        OpenImport ->
          T.empty
        OnlyImport quals ->
          "(" <> T.intercalate ", " (fmap unName quals) <> ")"
    ]

genModule :: Module a -> [TH.Dec]
genModule (Module ts _ es) =
     genTypeDecs ts
  <> (mconcat . with (M.toList es) $ \(n, (ty, e)) ->
       [genTypeSig n ty, genExpDec n e])

genFileName :: ModuleName -> FilePath
genFileName (ModuleName n) =
  T.unpack (T.replace "." "/" n)

-- -----------------------------------------------------------------------------

genTypeDecs :: HtmlDecls -> [TH.Dec]
genTypeDecs =
  fmap (uncurry genTypeDec) . M.toList . unTypeDecls

-- | Type declarations.
--
-- This should be done via Machinator eventually.
genTypeDec :: TypeName -> HtmlDecl -> TH.Dec
genTypeDec (TypeName n) ty =
  case ty of
    DVariant cts ->
      data_ (mkName_ n) [] (fmap (uncurry genCon) cts)

-- | Expression declarations.
genExpDec :: Name -> HtmlExpr a -> TH.Dec
genExpDec (Name n) expr =
  val_ (varP (mkName_ n)) (genExp expr)

genTypeSig :: Name -> HtmlType -> TH.Dec
genTypeSig (Name n) ty =
  sig (mkName_ n) (genType ty)

-- | Constructor declarations.
genCon :: Constructor -> [HtmlType] -> TH.Con
genCon (Constructor n) ts =
  normalC_' (mkName_ n) (fmap genType ts)

-- | Types.
genType :: HtmlType -> TH.Type
genType ty =
  case ty of
    TLit l ->
      conT (mkName_ (ppGroundType l))

    TVar (TypeName n) ->
      conT (mkName_ n)

    TArrow t1 t2 ->
      arrowT_ (genType t1) (genType t2)

    TList t ->
      listT_ (genType t)

-- | Expressions.
genExp :: HtmlExpr a -> TH.Exp
genExp expr =
  case expr of
    ELit _ v ->
      litE (genLit v)

    EVar _ (Name x) ->
      varE (mkName_ x)

    ELam _ (Name n) _ body ->
      lamE [varP (mkName_ n)] (genExp body)

    EApp _ fun arg ->
      appE (genExp fun) (genExp arg)

    ECon _ (Constructor c) _ es ->
      applyE (conE (mkName_ c)) (fmap genExp es)

    ECase _ e pats ->
      caseE (genExp e) (fmap (uncurry genMatch) pats)

    EList _ _ es ->
      listE (fmap genExp es)

    EForeign _ (Name x) _ ->
      varE (mkName_ x)

-- | Case alternatives.
genMatch :: Pattern a -> HtmlExpr a -> TH.Match
genMatch p e =
  match_ (genPat p) (genExp e)

-- | Patterns.
genPat :: Pattern a -> TH.Pat
genPat p = case p of
  PVar _ (Name n) ->
    varP (mkName_ n)

  PCon _ (Constructor n) ps ->
    conP (mkName_ n) (fmap genPat ps)

-- | Literals.
genLit :: Value PrimT -> TH.Lit
genLit v =
  case v of
    VString x ->
      stringL_ x
