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


import qualified Data.Text as T

import           Language.Haskell.TH
import qualified Language.Haskell.TH as TH

import           P

import           Projector.Core
import           Projector.Html.Core.Prim


genTypeDecs :: [(TypeName, HtmlType)] -> [TH.Dec]
genTypeDecs =
  catMaybes . fmap (uncurry genTypeDec)

-- | Type declarations.
-- This should be done via Machinator.
genTypeDec :: TypeName -> HtmlType -> Maybe TH.Dec
genTypeDec (TypeName n) ty =
  case ty of
    TLit _ ->
      Nothing

    TVar _ ->
      Nothing

    TArrow _ _ ->
      Nothing

    TVariant _ cts ->
      Just (DataD [] (mkName' n) [] (fmap (uncurry genCon) cts) [])

-- | Constructor declarations.
genCon :: Constructor -> [HtmlType] -> TH.Con
genCon (Constructor n) ts =
  NormalC (mkName' n) (fmap ((IsStrict,) . genType) ts)

-- | Types.
genType :: HtmlType -> TH.Type
genType ty =
  case ty of
    TLit l ->
      ConT (mkName' (ppGroundType l))

    TVar (TypeName n) ->
      ConT (mkName' n)

    TArrow t1 t2 ->
      AppT (AppT ArrowT (genType t1)) (genType t2)

    TVariant (TypeName n) _ ->
      ConT (mkName' n)

-- | Expressions.
genExp :: HtmlExpr -> TH.Exp
genExp expr =
  case expr of
    ELit v ->
      LitE (genLit v)

    EVar x ->
      VarE (mkName' (unName x))

    ELam n _ body ->
      LamE [(VarP (mkName' (unName n)))] (genExp body)

    EApp fun arg ->
      AppE (genExp fun) (genExp arg)

    ECon (Constructor c) _ es ->
      foldl' (\x -> AppE x . genExp) (ConE (mkName' c)) es

    ECase e pats ->
      CaseE (genExp e) (fmap (uncurry genMatch) pats)

-- | Case alternatives.
genMatch :: Pattern -> HtmlExpr -> TH.Match
genMatch p e =
  Match (genPat p) (NormalB (genExp e)) []

-- | Patterns.
genPat :: Pattern -> TH.Pat
genPat p = case p of
  PVar n ->
    VarP (mkName' (unName n))

  PCon (Constructor n) ps ->
    ConP (mkName' n) (fmap genPat ps)

-- | Literals.
genLit :: Value PrimT -> TH.Lit
genLit v =
  case v of
    VString x ->
      StringL (T.unpack x)


mkName' :: Text -> TH.Name
mkName' =
  mkName . T.unpack
