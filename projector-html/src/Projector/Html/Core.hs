{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Projector.Html.Core (
    CoreError (..)
  , renderCoreError
  , renderCoreErrorRange
  , templateToCore
  , typeCheck
  , typeCheckAll
  , typeCheckIncremental
  , htmlTypes
  -- * Various type aliases
  , HtmlType
  , HtmlDecl
  , HtmlDecls
  , HtmlExpr
  , HtmlLit
  ) where


import           Data.Map.Strict (Map)
import qualified Data.Text as T

import           P

import qualified Projector.Core as PC
import qualified Projector.Core.Pretty as PCP
import qualified Projector.Html.Core.Elaborator as Elab
import qualified Projector.Html.Core.Library as Library
import           Projector.Html.Data.Prim
import qualified Projector.Html.Core.Prim as Prim
import           Projector.Html.Data.Position (Range, renderRange)
import           Projector.Html.Data.Template (Template)


data CoreError a
  = HtmlTypeError [PC.TypeError PrimT a]
  deriving (Eq, Show, Ord)

renderCoreError :: (a -> Text) -> (a -> Text) -> CoreError a -> Text
renderCoreError start end err =
  case err of
    HtmlTypeError tes ->
      T.unlines (fmap (PCP.ppTypeErrorDecorated start end) tes)

renderCoreErrorRange :: CoreError Range -> Text
renderCoreErrorRange =
  renderCoreError (\r -> (renderRange r <> ": ")) (const mempty)

templateToCore :: Template a -> Either (CoreError a) (HtmlType, HtmlExpr (HtmlType, a))
templateToCore =
  typeTree . Elab.elaborate

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

extractType :: HtmlExpr (HtmlType, a) -> HtmlType
extractType =
  fst . PC.extractAnnotation
