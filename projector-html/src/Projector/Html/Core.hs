{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Projector.Html.Core (
    CoreError (..)
  , renderCoreError
  , renderCoreErrorRange
  , templateToCore
  , typeCheck
  , htmlTypes
  -- * Various type aliases
  , Prim.HtmlType
  , Prim.HtmlDecl
  , Prim.HtmlDecls
  , Prim.HtmlExpr
  , Prim.HtmlLit
  ) where


import qualified Data.Text as T

import           P

import qualified Projector.Core as PC
import qualified Projector.Core.Pretty as PCP
import qualified Projector.Core.Simplify as Simp
import qualified Projector.Html.Core.Elaborator as Elab
import qualified Projector.Html.Core.Library as Library
import           Projector.Html.Core.Prim
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

templateToCore :: Template a -> Either (CoreError a) (HtmlType, HtmlExpr a)
templateToCore t =
  let core = Elab.elaborate t
  in fmap (, Simp.nf core) (typeCheck core)

typeCheck :: HtmlExpr a -> Either (CoreError a) HtmlType
typeCheck =
  first HtmlTypeError . PC.typeCheck htmlTypes

htmlTypes :: HtmlDecls
htmlTypes =
  Prim.types <> Library.types
