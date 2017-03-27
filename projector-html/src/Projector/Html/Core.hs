{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Projector.Html.Core (
    CoreError (..)
  , renderCoreError
  , renderCoreErrorAnnotation
  , renderCoreWarning
  , renderCoreWarningAnnotation
  , templateToCore
  , typeCheck
  , typeCheckAll
  , typeCheckIncremental
  , htmlTypes
  , libraryExprs
  -- * Various type aliases
  , HtmlType
  , HtmlDecl
  , HtmlDecls
  , HtmlExpr
  , HtmlLit
  , constructorFunctions
  , constructorFunctionExprs
  , constructorFunctionTypes
  , mkCon
  ) where


import           Data.Char (ord, chr)
import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           P

import qualified Projector.Core as PC
import qualified Projector.Core.Pretty as PCP
import qualified Projector.Html.Core.Elaborator as Elab
import qualified Projector.Html.Core.Library as Library
import           Projector.Html.Data.Annotation
import           Projector.Html.Data.Prim
import qualified Projector.Html.Core.Prim as Prim
import           Projector.Html.Data.Template (Template)


data CoreError a
  = HtmlTypeError [PC.TypeError PrimT a]
  | HtmlElabError (Elab.ElaboratorError a)
  deriving (Eq, Show, Ord)

renderCoreError :: (a -> Text) -> (a -> Text) -> CoreError a -> Text
renderCoreError start end err =
  case err of
    HtmlTypeError tes ->
      T.unlines (fmap (PCP.ppTypeErrorDecorated start end) tes)
    HtmlElabError e ->
      Elab.renderElaboratorError start e

renderCoreErrorAnnotation :: (a -> Text) -> CoreError (Annotation a) -> Text
renderCoreErrorAnnotation f =
  renderCoreError (\r -> (renderAnnotation f r <> ": ")) (const mempty)

renderCoreWarning :: (a -> Text) -> (a -> Text) -> HtmlWarning a -> Text
renderCoreWarning start end =
  PCP.ppWarningDecorated start end

renderCoreWarningAnnotation :: (a -> Text) -> HtmlWarning (Annotation a) -> Text
renderCoreWarningAnnotation f =
  renderCoreWarning (\r -> (renderAnnotation f r <> ": ")) (const mempty)

templateToCore :: Template a -> Either (CoreError (Annotation a)) (HtmlType, HtmlExpr (HtmlType, Annotation a))
templateToCore t = do
  ast <- first HtmlElabError (Elab.elaborate t)
  typeTree ast

typeCheck :: HtmlExpr a -> Either (CoreError a) HtmlType
typeCheck =
  fmap fst . typeTree

typeTree :: HtmlExpr a -> Either (CoreError a) (HtmlType, HtmlExpr (HtmlType, a))
typeTree =
   first HtmlTypeError . fmap (\e -> (extractType e, e)) . PC.typeTree htmlTypes

typeCheckAll ::
     HtmlDecls
  -> Map PC.Name (HtmlExpr a)
  -> Either (CoreError a) (Map PC.Name (HtmlType, HtmlExpr (HtmlType, a)))
typeCheckAll typs =
  first HtmlTypeError . fmap (fmap (\e -> (extractType e, e))) . PC.typeCheckAll (typs <> htmlTypes)

typeCheckIncremental ::
     HtmlDecls
  -> Map PC.Name (HtmlType, a)
  -> Map PC.Name (HtmlExpr a)
  -> Either (CoreError a) (Map PC.Name (HtmlType, HtmlExpr (HtmlType, a)))
typeCheckIncremental typs known =
  first HtmlTypeError . fmap (fmap (\e -> (extractType e, e))) . PC.typeCheckIncremental (typs <> htmlTypes) known

htmlTypes :: HtmlDecls
htmlTypes =
  Prim.types <> Library.types

libraryExprs :: Map PC.Name (HtmlType, Annotation a)
libraryExprs =
  fmap (fmap (snd . PC.extractAnnotation)) Library.exprs

extractType :: HtmlExpr (HtmlType, a) -> HtmlType
extractType =
  fst . PC.extractAnnotation

-- Produce regular curried functions for each constructor.
constructorFunctions :: PC.TypeDecls a -> Map PC.Name (PC.Type a, PC.Expr a (PC.Type a, Annotation b))
constructorFunctions (PC.TypeDecls m) =
  M.fromList $ M.toList m >>= \(tn, decl) ->
    case decl of
      PC.DVariant cts ->
        with cts $ \(c@(PC.Constructor cn), ts) ->
          (PC.Name cn, (foldr PC.TArrow (PC.TVar tn) ts, mkCon (DataConstructor c tn) c tn ts))
      PC.DRecord fts ->
        [(PC.Name (PC.unTypeName tn), (foldr PC.TArrow (PC.TVar tn) (fmap snd fts), mkRec (RecordConstructor tn) tn fts))]

mkCon :: a -> PC.Constructor -> PC.TypeName -> [PC.Type l] -> PC.Expr l (PC.Type l, a)
mkCon a c tn ts =
  let vars = fmap intVar [1..(length ts)] in
  foldr
    (\(name, ty) expr -> PC.ELam (PC.TArrow ty (fst (PC.extractAnnotation expr)), a) name (Just ty) expr)
    (PC.ECon (PC.TVar tn, a) c tn (L.zipWith (\v t -> PC.EVar (t, a) v) vars ts))
    (L.zip vars ts)

mkRec :: a -> PC.TypeName -> [(PC.FieldName, PC.Type l)] -> PC.Expr l (PC.Type l, a)
mkRec a tn fts =
  let vars = fmap intVar [1..(length fts)] in
  foldr
    (\(name, ty) expr -> PC.ELam (PC.TArrow ty (fst (PC.extractAnnotation expr)), a) name (Just ty) expr)
    (PC.ERec (PC.TVar tn, a) tn (L.zipWith (\(fn, t) v -> (fn, PC.EVar (t, a) v)) fts vars))
    (L.zip vars (fmap snd fts))

-- produce a, z, a1, z26 style names from integers
intVar :: Int -> PC.Name
intVar x =
  let letter j = chr (ord 'a' + j)
  in case (x `mod` 26, x `div` 26) of
    (i, 0) ->
      PC.Name (T.pack [letter i])
    (m, n) ->
      PC.Name (T.pack [letter m] <> renderIntegral n)

constructorFunctionExprs :: PC.TypeDecls l -> Map PC.Name (PC.Expr l (PC.Type l, Annotation b))
constructorFunctionExprs =
  fmap snd . constructorFunctions

constructorFunctionTypes :: PC.TypeDecls a -> Map PC.Name (PC.Type a, Annotation b)
constructorFunctionTypes =
  fmap (fmap (snd . PC.extractAnnotation)) . constructorFunctions
