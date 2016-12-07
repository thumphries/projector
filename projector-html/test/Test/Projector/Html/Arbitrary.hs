{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Arbitrary where


import           Data.List.NonEmpty  (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import           Data.Text.Arbitrary ()

import           Disorder.Corpus
import           Disorder.Jack

import           P

import           Projector.Html.Data.Template
import           Projector.Html.Core.Prim (HtmlDecls, HtmlType, HtmlExpr, HtmlLit)
import qualified Projector.Html.Core.Prim as Prim
import qualified Projector.Html.Core.Library as Lib

import           Test.Projector.Core.Arbitrary
import           Test.QuickCheck.Jack hiding (listOf1)


-- -----------------------------------------------------------------------------
-- Generating core types and expressions

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
  arbitraryBoundedEnum


-- -----------------------------------------------------------------------------
-- Generating untyped templates

genTemplate :: Jack (Template ())
genTemplate =
  Template ()
    <$> genTemplateTypeSig
    <*> genHtml

genTemplateTypeSig :: Jack (Maybe (TTypeSig ()))
genTemplateTypeSig = do
  k <- chooseInt (0, 20)
  case k of
    0 ->
      pure Nothing
    _ ->
      fmap (pure . TTypeSig ()) $ do
        listOf1
          ((,) <$> (TId <$> elements muppets) <*>
           ((TTVar () . TId . T.toTitle) <$> elements waters))

genHtml :: Jack (THtml ())
genHtml =
  THtml () <$>
  listOf (oneOf [genElement, genVoidElement, genComment, genPlain, genHtmlExpr])

genElement :: Jack (TNode ())
genElement =
  TElement ()
    <$> genTag
    <*> listOf genAttribute
    <*> genHtml

genVoidElement :: Jack (TNode ())
genVoidElement =
  TVoidElement ()
    <$> genTag
    <*> listOf genAttribute

genComment :: Jack (TNode ())
genComment =
  TComment () <$> genPlainText

genPlain :: Jack (TNode ())
genPlain =
  TPlain () <$> genPlainText

genPlainText :: Jack TPlainText
genPlainText =
  fmap (TPlainText . escape) (arbitrary `suchThat` (/= T.empty))

escape :: Text -> Text
escape =
    T.replace "{" "\\{"
  . T.replace "}" "\\}"
  . T.replace "<" "\\<"
  . T.replace "-->" "\\-\\->"
  . T.replace ">" "\\>"
  . T.replace " " "a"
  . T.replace "\n" "b"
  . T.replace "\t" "c"

genHtmlExpr :: Jack (TNode ())
genHtmlExpr =
  TExprNode () <$> genTemplateExpr

genAttribute :: Jack (TAttribute ())
genAttribute =
  oneOf [
      TEmptyAttribute () <$> genAttributeName
    , TAttribute () <$> genAttributeName <*> genAttributeValue
    ]

genAttributeName :: Jack TAttrName
genAttributeName =
  TAttrName <$> oneOf [
      pure "enabled"
    , pure "display"
    , pure "style"
    , pure "src"
    , pure "href"
    , elements boats
    ]

-- TODO size here would help
genAttributeValue :: Jack (TAttrValue ())
genAttributeValue =
  oneOf [
      TQuotedAttrValue () <$> genAttrValueText
--    , TUnquotedAttrValue () <$> genAttrValueText -- TODO REMOVE FROM AST
    , TAttrExpr () <$> genTemplateExpr
    ]

genAttrValueText :: Jack TPlainText
genAttrValueText =
  TPlainText <$> oneOf [
      pure "true"
    , pure "false"
    , pure "0"
    , elements muppets
    -- TODO arbitrary JS would be nice
    ]

genTag :: Jack TTag
genTag =
  TTag <$> elements [
      "a"
    , "html"
    , "span"
    , "p"
    , "h1"
    , "blink"
    , "marquee"
    ]

genVoidTag :: Jack TTag
genVoidTag =
  elements [
      TTag "img"
    ]

genTemplateExpr :: Jack (TExpr ())
genTemplateExpr =
  let nonrec = [
          TEVar () <$> (TId <$> elements muppets)
        ]
      recc = [
          TEApp () <$> oneOf nonrec <*> oneOf nonrec
        , TECase () <$> oneOf nonrec <*> genTemplateAlts
        ]
  in oneOfRec nonrec recc

genTemplateAlts :: Jack (NonEmpty (TAlt ()))
genTemplateAlts = do
  k <- chooseInt (1, 10)
  (:|) <$> genTemplateAlt <*> vectorOf k genTemplateAlt

genTemplateAlt :: Jack (TAlt ())
genTemplateAlt =
  TAlt () <$> genTemplatePattern <*> genTemplateAltBody

genTemplateAltBody :: Jack (TAltBody ())
genTemplateAltBody =
  oneOf [
      TAltExpr () <$> genTemplateExpr
    , TAltHtml () <$> (THtml () . NE.toList <$> listOf1 (oneOf [genElement, genVoidElement])) -- need to fix AST
    ]

genTemplatePattern :: Jack (TPattern ())
genTemplatePattern =
  let nonrec = [
          TPVar () <$> (TId <$> elements waters)
        ]
      recc = [
          TPCon () <$> ((TConstructor . T.toTitle) <$> elements muppets)
                   <*> listOf genTemplatePattern
        ]
  in oneOfRec nonrec recc
