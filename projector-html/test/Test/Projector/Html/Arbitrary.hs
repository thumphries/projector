{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Arbitrary where


import           Disorder.Corpus
import           Disorder.Jack

import           P

import           Projector.Html.Core.Prim (HtmlDecls, HtmlType, HtmlExpr, HtmlLit)
import qualified Projector.Html.Core.Prim as Prim
import qualified Projector.Html.Core.Library as Lib

import           Test.Projector.Core.Arbitrary


genHtmlTypeDecls :: Jack HtmlDecls
genHtmlTypeDecls =
  let htmlTypes =
        Prim.types <> Lib.types
  in genTypeDecls htmlTypes genTypeName genConstructor genHtmlLitT

genWellTypedHtmlExpr :: HtmlDecls -> Jack (HtmlType, HtmlExpr ())
genWellTypedHtmlExpr ctx = do
  ty <- genHtmlType ctx
  ex <- genWellTypedExpr ctx ty (genHtmlType ctx) genWellTypedHtmlLit
  pure (ty, ex)

genHtmlType :: HtmlDecls -> Jack HtmlType
genHtmlType ctx =
  genTypeFromContext ctx genHtmlLitT

genWellTypedHtmlLit :: Prim.PrimT -> Jack HtmlLit
genWellTypedHtmlLit t =
  case t of
    Prim.TString ->
      Prim.VString <$> elements boats

genHtmlLitT :: Jack Prim.PrimT
genHtmlLitT =
  elements [
      Prim.TString
    ]
