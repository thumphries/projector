{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Projector.Html.Backend.Haskell (
    ModuleName (..)
  , renderModule
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
import           Projector.Html.Backend.Haskell.TH
import           Projector.Html.Core.Prim as Prim


-- -----------------------------------------------------------------------------

newtype ModuleName = ModuleName { unModuleName :: Text }
  deriving (Eq, Ord, Show)

renderModule :: ModuleName -> [TH.Dec] -> Text
renderModule (ModuleName n) ds =
  let pragmas = [
          "{-# LANGUAGE NoImplicitPrelude #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        ]
      modName = T.unwords ["module", n, "where"]
      imports = [
          "import Data.String (String)"
        ]
      prims = fmap (T.pack . TH.pprint) (genTypeDecs Prim.types)
      decls = fmap (T.pack . TH.pprint) ds

  in T.unlines $ mconcat [
         pragmas
       , [modName]
       , imports
       , prims
       , decls
       ]

genModule :: HtmlDecls -> [(Name, HtmlType, HtmlExpr)] -> [TH.Dec]
genModule env exprs =
     genTypeDecs env
  <> (mconcat . with exprs $ \(n, ty, e) ->
       [genTypeSig n ty, genExpDec n e])

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
genExpDec :: Name -> HtmlExpr -> TH.Dec
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
genExp :: HtmlExpr -> TH.Exp
genExp expr =
  case expr of
    ELit v ->
      litE (genLit v)

    EVar (Name x) ->
      varE (mkName_ x)

    ELam (Name n) _ body ->
      lamE [varP (mkName_ n)] (genExp body)

    EApp fun arg ->
      appE (genExp fun) (genExp arg)

    ECon (Constructor c) _ es ->
      applyE (conE (mkName_ c)) (fmap genExp es)

    ECase e pats ->
      caseE (genExp e) (fmap (uncurry genMatch) pats)

    EList _ es ->
      listE (fmap genExp es)

    EForeign (Name x) _ ->
      varE (mkName_ x)

-- | Case alternatives.
genMatch :: Pattern -> HtmlExpr -> TH.Match
genMatch p e =
  match_ (genPat p) (genExp e)

-- | Patterns.
genPat :: Pattern -> TH.Pat
genPat p = case p of
  PVar (Name n) ->
    varP (mkName_ n)

  PCon (Constructor n) ps ->
    conP (mkName_ n) (fmap genPat ps)

-- | Literals.
genLit :: Value PrimT -> TH.Lit
genLit v =
  case v of
    VString x ->
      stringL_ x
