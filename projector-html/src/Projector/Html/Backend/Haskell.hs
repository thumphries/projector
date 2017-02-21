{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Projector.Html.Backend.Haskell (
    renderModule
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
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim
import           Projector.Html.Backend.Haskell.Prim
import           Projector.Html.Backend.Haskell.Rewrite
import           Projector.Html.Backend.Haskell.TH

import           System.IO (FilePath)


-- -----------------------------------------------------------------------------

data HaskellError
  = HtmlCase -- TODO should really return decorated parameterised error here
  deriving (Eq, Ord, Show)

renderHaskellError :: HaskellError -> Text
renderHaskellError e =
  case e of
    HtmlCase ->
      "Don't case on Html or HtmlNode, pal!"

predicates :: [Predicate a HaskellError]
predicates = [
    PatPredicate $ \case
      PCon _ c _ ->
        if S.member c htmlConstructors || S.member c htmlNodeConstructors
          then PredError HtmlCase
          else PredOk
      _ ->
        PredOk
  ]

htmlConstructors :: Set Constructor
htmlConstructors =
  case dHtml of
    DVariant cts ->
      S.fromList (fmap fst cts)

htmlNodeConstructors :: Set Constructor
htmlNodeConstructors =
  case dHtmlNode of
    DVariant cts ->
      S.fromList (fmap fst cts)

-- -----------------------------------------------------------------------------

renderModule :: ModuleName -> Module HtmlType PrimT a -> (FilePath, Text)
renderModule mn@(ModuleName n) m =
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

      imports = fmap (uncurry genImport) (M.toList (moduleImports m'))
      decls = fmap (T.pack . TH.pprint) (genModule m')

  in (genFileName mn, T.unlines $ mconcat [
         pragmas
       , [modName]
       , imports
       , decls
       ])

renderExpr :: Name -> HtmlExpr a -> Text
renderExpr n =
  T.pack . TH.pprint . genExpDec n . toHaskellExpr . rewriteExpr

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

genModule :: HaskellModule a -> [TH.Dec]
genModule (Module ts _ es) =
     genTypeDecs ts
  <> (mconcat . with (M.toList es) $ \(n, (ty, e)) ->
       [genTypeSig n ty, genExpDec n e])

genFileName :: ModuleName -> FilePath
genFileName (ModuleName n) =
  T.unpack (T.replace "." "/" n) <> ".hs"

-- -----------------------------------------------------------------------------
-- | Type declarations.
--
-- This should be done via Machinator eventually.
-- They shouldn't even be a field in 'Module'.

genTypeDecs :: HaskellDecls -> [TH.Dec]
genTypeDecs =
  fmap (uncurry genTypeDec) . M.toList . unTypeDecls

genTypeDec :: TypeName -> HaskellDecl -> TH.Dec
genTypeDec (TypeName n) ty =
  case ty of
    DVariant cts ->
      data_ (mkName_ n) [] (fmap (uncurry genCon) cts)

-- | Constructor declarations.
genCon :: Constructor -> [HaskellType] -> TH.Con
genCon (Constructor n) ts =
  normalC_' (mkName_ n) (fmap genType ts)

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

    TListF t ->
      listT_ (genType t)

-- -----------------------------------------------------------------------------

-- | Expression declarations.
genExpDec :: Name -> HaskellExpr a -> TH.Dec
genExpDec (Name n) expr =
  val_ (varP (mkName_ n)) (genExp expr)

genTypeSig :: Name -> HaskellType -> TH.Dec
genTypeSig (Name n) ty =
  sig (mkName_ n) (genType ty)


-- | Expressions.
genExp :: HaskellExpr a -> TH.Exp
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

    EList _ es ->
      listE (fmap genExp es)

    EForeign _ (Name x) _ ->
      varE (mkName_ x)

    EMap _ f g ->
      applyE (varE (mkName_ "fmap")) [genExp f, genExp g]

-- | Case alternatives.
genMatch :: Pattern a -> HaskellExpr a -> TH.Match
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
genLit :: Value HaskellPrimT -> TH.Lit
genLit v =
  case v of
    HTextV x ->
      stringL_ x
