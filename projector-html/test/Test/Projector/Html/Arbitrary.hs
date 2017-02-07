{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Arbitrary where


import           Data.Generics.Aliases
import           Data.Generics.Schemes
import           Data.List.NonEmpty  (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Text.Arbitrary ()

import           Disorder.Corpus
import           Disorder.Jack

import           P

import           Projector.Core
import           Projector.Html.Data.Backend
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim as Prim
import           Projector.Html.Data.Template
import           Projector.Html.Core (constructorFunctions, htmlTypes)

import           Test.Projector.Core.Arbitrary
import           Test.QuickCheck.Jack hiding (listOf1)


-- -----------------------------------------------------------------------------
-- Generating core types and expressions

genHtmlTypeDecls :: Jack HtmlDecls
genHtmlTypeDecls =
  genTypeDecls htmlTypes genTypeName genConstructor genHtmlLitT

genWellTypedHtmlExpr :: HtmlDecls -> Jack (HtmlType, HtmlExpr ())
genWellTypedHtmlExpr ctx = do
  ty <- genHtmlType ctx
  ex <- genWellTypedExpr ctx ty (genHtmlType ctx) genWellTypedHtmlLit
  pure (ty, ex)

genWellTypedHtmlModule :: Int -> HtmlDecls -> Jack (Module HtmlType PrimT ())
genWellTypedHtmlModule n decls =
  let ourDecls = subtractTypes decls htmlTypes in
  Module
    <$> pure ourDecls
    <*> pure (M.fromList [(htmlRuntime, OpenImport)])
    <*> genWellTypedLetrec n (decls <> htmlTypes) (fst <$> constructorFunctions ourDecls) (genHtmlType decls) genWellTypedHtmlLit

genHtmlType :: HtmlDecls -> Jack HtmlType
genHtmlType ctx =
  genTypeFromContext ctx genHtmlLitT

genWellTypedHtmlLit :: PrimT -> Jack HtmlLit
genWellTypedHtmlLit t =
  case t of
    Prim.TString ->
      Prim.VString <$> elements boats

genHtmlLitT :: Jack PrimT
genHtmlLitT =
  arbitraryBoundedEnum


-- -----------------------------------------------------------------------------
-- Generating untyped templates

genTemplate :: Jack (Template ())
genTemplate =
  sized $ \k -> do
    j <- chooseInt (0, k)
    Template () <$> genTemplateTypeSig <*>
      fmap (everywhere (mkT mergePlain)) (genHtml j)

genTemplateTypeSig :: Jack (Maybe (TTypeSig ()))
genTemplateTypeSig = do
  k <- chooseInt (0, 20)
  case k of
    0 ->
      pure Nothing
    _ ->
      fmap (pure . TTypeSig () . NE.fromList) $ do
        listOfN 1 k
          ((,) <$> (TId <$> elements muppets) <*>
           ((TTVar () . TId . T.toTitle) <$> elements waters))

genHtml :: Int -> Jack (THtml ())
genHtml k =
  let j = k `div` 2 in
  THtml () <$>
  listOfN 0 k (oneOf [genElement j, genVoidElement j, genComment, genPlain, genHtmlExpr j])

genElement :: Int -> Jack (TNode ())
genElement k =
  let j = k `div` 2 in
  TElement ()
    <$> genTag
    <*> listOfN 0 j (genAttribute (j `div` 2))
    <*> genHtml j

genVoidElement :: Int -> Jack (TNode ())
genVoidElement k =
  TVoidElement ()
    <$> genTag
    <*> listOfN 0 k (genAttribute (k `div` 2))

genComment :: Jack (TNode ())
genComment =
  TComment () <$> genPlainText

genPlain :: Jack (TNode ())
genPlain =
  TPlain () <$> genPlainText

genPlainText :: Jack TPlainText
genPlainText =
  fmap (TPlainText . mangle) (arbitrary `suchThat` (/= T.empty))

mangle :: Text -> Text
mangle =
    T.replace " " "a"
  . T.replace "\n" "b"
  . T.replace "\t" "c"
  . T.replace "\\" "\\\\"

genHtmlExpr :: Int -> Jack (TNode ())
genHtmlExpr k =
  TExprNode () <$> genTemplateExpr k

genAttribute :: Int -> Jack (TAttribute ())
genAttribute k =
  oneOf [
      TEmptyAttribute () <$> genAttributeName
    , TAttribute () <$> genAttributeName <*> genAttributeValue k
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

genAttributeValue :: Int -> Jack (TAttrValue ())
genAttributeValue k =
  if k <= 2
    then TQuotedAttrValue () <$> genAttrValueText
    else TAttrExpr () <$> genTemplateExpr k

genAttrValueText :: Jack TPlainText
genAttrValueText =
  TPlainText <$> oneOf [
      pure "true"
    , pure "false"
    , pure "0"
    , elements muppets
    -- TODO arbitrary JS would be nice
    ]

genTag :: Jack (TTag ())
genTag =
  TTag () <$> elements [
      "a"
    , "html"
    , "span"
    , "p"
    , "h1"
    , "blink"
    , "marquee"
    ]

genVoidTag :: Jack (TTag ())
genVoidTag =
  elements [
      TTag () "img"
    ]

genTemplateExpr :: Int -> Jack (TExpr ())
genTemplateExpr k =
  let j = k `div` 2
      nonrec = [
          TEVar () <$> (TId <$> elements muppets)
        , TELit () <$> genLit
        ]
      recc = [
          TEApp () <$> genTemplateExpr j <*> genTemplateExpr j
        , TECase () <$> genTemplateExpr j <*> genTemplateAlts j
        , TELam () <$> (listOf1 (TId <$> elements simpsons)) <*> genTemplateExpr j
        , TEEach () <$> genTemplateExpr j <*> genTemplateExpr j
        ]
  in if k <= 2 then oneOf nonrec else oneOf recc

genLit :: Jack (TLit ())
genLit =
  oneof [
      TLString () <$> arbitrary
    ]

genTemplateAlts :: Int -> Jack (NonEmpty (TAlt ()))
genTemplateAlts j = do
  k <- chooseInt (1, 10)
  (:|) <$> genTemplateAlt j <*> vectorOf k (genTemplateAlt (j `div` k))

genTemplateAlt :: Int -> Jack (TAlt ())
genTemplateAlt k =
  let j = k `div` 2 in
  TAlt () <$> genTemplatePattern j <*> genTemplateExpr j

genTemplatePattern :: Int -> Jack (TPattern ())
genTemplatePattern k =
  let j = k `div` 2
      nonrec = [
          TPVar () <$> (TId <$> elements waters)
        ]
      recc = [
          TPCon () <$> ((TConstructor . T.toTitle) <$> elements muppets)
                   <*> listOfN 0 k (genTemplatePattern j)
        ]
  in if k <= 2 then oneOf nonrec else oneOf recc

mergePlain :: THtml () -> THtml ()
mergePlain (THtml a nodes) =
  THtml a (go nodes)
  where
    go ((TPlain _ (TPlainText b)) : (TPlain _ (TPlainText c)) : xs) = go (TPlain () (TPlainText (b <> c)) : xs)
    go (x:xs) = x : go xs
    go [] = []
