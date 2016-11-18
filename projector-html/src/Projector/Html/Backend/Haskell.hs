{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Projector.Html.Backend.Haskell (
    genTypeDecs
  , genTypeDec
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
import           Projector.Html.Core.Prim


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
      data_ (mkName_ n) [] (fmap (uncurry genCon) cts) []

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

-- | Expressions.
genExp :: HtmlExpr -> TH.Exp
genExp expr =
  case expr of
    ELit v ->
      litE (genLit v)

    EVar x ->
      varE (mkName_ (unName x))

    ELam n _ body ->
      lamE [varP (mkName_ n)] (genExp body)

    EApp fun arg ->
      appE (genExp fun) (genExp arg)

    ECon (Constructor c) _ es ->
      applyE (conE (mkName_ c)) (fmap genExp es)

    ECase e pats ->
      caseE (genExp e) (fmap (uncurry genMatch pats))


-- | Case alternatives.
genMatch :: Pattern -> HtmlExpr -> TH.Match
genMatch p e =
  match_ (genPat p) (genExp e) []

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
