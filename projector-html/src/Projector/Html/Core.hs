{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Projector.Html.Core (
    CoreError (..)
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


import           P

import qualified Projector.Core as PC
import qualified Projector.Core.Simplify as Simp
import qualified Projector.Html.Core.Elaborator as Elab
import qualified Projector.Html.Core.Library as Library
import           Projector.Html.Core.Prim
import qualified Projector.Html.Core.Prim as Prim
import           Projector.Html.Data.Template (Template)


data CoreError a
  = HtmlTypeError [PC.TypeError PrimT a]
  deriving (Eq, Show, Ord)

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
