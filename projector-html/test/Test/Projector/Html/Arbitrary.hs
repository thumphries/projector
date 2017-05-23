{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Arbitrary where


import           Data.Generics.Aliases
import           Data.Generics.Schemes
import qualified Data.List as L
import           Data.List.NonEmpty  (NonEmpty(..))
import qualified Data.Text as T
import           Data.Text.Arbitrary ()

import           Disorder.Corpus
import           Disorder.Jack

import           P

import           Projector.Core
import           Projector.Html
import           Projector.Html.Data.Annotation
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
  genTypeDecls htmlTypes genHtmlLitT

genWellTypedHtmlExpr :: HtmlDecls -> Jack (HtmlType, HtmlExpr ())
genWellTypedHtmlExpr ctx = do
  ty <- genHtmlType ctx
  ex <- genWellTypedExpr ctx ty (genHtmlType ctx) genWellTypedHtmlLit
  pure (ty, ex)

genWellTypedHtmlModule :: Int -> HtmlDecls -> Jack (Module HtmlType PrimT (HtmlType, SrcAnnotation))
genWellTypedHtmlModule n decls = do
  let ourDecls = subtractTypes decls htmlTypes
  modl <- Module
    <$> pure ourDecls
    <*> pure mempty
    <*> fmap (fmap (\(ty, ex) -> ModuleExpr (Just ty) ex))
          (genWellTypedLetrec
            n
            (decls <> htmlTypes)
            (fst <$> constructorFunctions ourDecls)
            (genHtmlType decls)
            genWellTypedHtmlLit)
  either
    (\e -> (fail ("invariant: module was not well-typed!\n" <> show e)))
    pure
    (checkModule ourDecls mempty (fmap (const EmptyAnnotation) modl))

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
    j <- chooseInt (1, k+1)
    Template () <$> genTemplateTypeSig <*> genTemplateExpr j

genTemplateTypeSig :: Jack (Maybe (TTypeSig ()))
genTemplateTypeSig = do
  k <- chooseInt (0, 20)
  case k of
    0 ->
      pure Nothing
    _ ->
      fmap Just . TTypeSig ()
        <$> listOfN 0 k ((,) <$> (TId <$> elements muppets) <*> genTVar)
        <*> genTVar

genTVar :: Jack (TType ())
genTVar =
  (TTVar () . TId . T.toTitle) <$> elements waters

genHtml :: Int -> Jack (THtml ())
genHtml k =
  let j = k `div` 2
  in THtml () . everywhere (mkT mergePlain) <$>
     listOfN
       1
       (k + 1)
       (oneOf
          [ genElement j
          , genVoidElement j
          , genComment
          , genHtmlExpr j
          , genHtmlWS j
          , genHtmlTextExpr j
          ])

genHtmlWS :: Int -> Jack (TNode ())
genHtmlWS k =
  (\(THtml () nodes) -> THtmlWS () nodes)
    <$> genHtml k

genElement :: Int -> Jack (TNode ())
genElement k =
  let j = k `div` 2 in
  reshrink
    (\node ->
      case node of
        TElement () _tag _attrs (THtml _ nodes) ->
          nodes
        _ ->
          [node])
    (TElement ()
      <$> genTag
      <*> listOfN 0 j (genAttribute (j `div` 2))
      <*> genHtml (j `div` 2))

genVoidElement :: Int -> Jack (TNode ())
genVoidElement k =
  TVoidElement ()
    <$> genTag
    <*> listOfN 0 k (genAttribute (k `div` 2))

genComment :: Jack (TNode ())
genComment =
  TComment () <$> genCommentText

genPlain :: Jack (TNode ())
genPlain =
  TPlain () <$> genPlainText

genCommentText :: Jack TPlainText
genCommentText =
  TPlainText . T.replace "-" "a" . mangle <$> (arbitrary `suchThat` (/= T.empty))

genPlainText :: Jack TPlainText
genPlainText =
  fmap (TPlainText . mangle) (arbitrary `suchThat` (/= T.empty))

mangle :: Text -> Text
mangle =
    T.replace " " "a"
  . T.replace "\r" "d"
  . T.replace "\n" "b"
  . T.replace "\t" "c"
  . T.replace "\\" "\\\\"
  . T.replace "(" "e"
  . T.replace ")" "f"

genHtmlExpr :: Int -> Jack (TNode ())
genHtmlExpr k =
  TExprNode () <$> genTemplateExpr k

genHtmlTextExpr :: Int -> Jack (TNode ())
genHtmlTextExpr k =
  TTextExprNode () <$> genTemplateExpr k

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
    then TQuotedAttrValue () <$> genInterpolatedString k
    else TAttrExpr () <$> genTemplateExpr k

genInterpolatedString :: Int -> Jack (TIString ())
genInterpolatedString k = do
  n <- chooseInt (0, k)
  (everywhere (mkT mergeTIChunk) . TIString ()) <$>
    listOfN 0 n
      (oneOf [genStringChunk, fmap (TExprChunk ()) (genTemplateExpr (k - n))])

genStringChunk :: Jack (TIChunk ())
genStringChunk =
  TStringChunk () <$> oneOf [
      pure "true"
    , pure "false"
    , pure "0"
    , elements muppets
    -- TODO arbitrary JS would be nice
    ]

mergeTIChunk :: TIString () -> TIString ()
mergeTIChunk (TIString () chunks) =
  TIString () (go chunks)
  where
    go [] = []
    go (TStringChunk _ a : TStringChunk _ b : xs) = go (TStringChunk () (a <> b) : xs)
    go (x:xs) = x : go xs

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
          TEVar () <$> genTId
        , pure (TEHole ())
        ]
      recc = [
          TEApp () <$> genTemplateExpr j <*> genTemplateExpr j
        , TECase () <$> genTemplateExpr j <*> genTemplateAlts j
        , TELam () <$> (listOf1 (TId <$> elements simpsons)) <*> genTemplateExpr j
        , TEString () <$> genInterpolatedString j
        , TEEach () <$> genTemplateExpr j <*> genTemplateExpr j
        , TEList () <$> listOfN 0 (j `div` 2) (genTemplateExpr (j `div` 2))
        , TEPrj () <$> genTemplateExpr j <*> genField
        , TENode () <$> genHtml j
        ]
  in if k <= 2 then oneOf nonrec else oneOf recc

genTId :: Jack TId
genTId =
  TId <$> oneOf [
      elements muppets
    , elements (L.zipWith (\a b -> a <> "/" <> b) cooking muppets)
    ]

genTemplateAlts :: Int -> Jack (NonEmpty (TAlt ()))
genTemplateAlts j = do
  k <- chooseInt (1, 10)
  (:|) <$> genTemplateAlt j <*> vectorOf k (genTemplateAlt (j `div` k))

genTemplateAlt :: Int -> Jack (TAlt ())
genTemplateAlt k =
  let j = k `div` 2 in
  TAlt () <$> genTemplatePattern j <*> genTemplateExpr j

genField :: Jack TId
genField =
  TId <$> elements (waters <> boats <> muppets)

genTemplatePattern :: Int -> Jack (TPattern ())
genTemplatePattern k =
  let j = k `div` 2
      nonrec = [
          TPVar () <$> (TId <$> elements waters)
        , pure (TPWildcard ())
        ]
      recc = [
          TPCon () <$> ((TConstructor . T.toTitle) <$> elements muppets)
                   <*> listOfN 0 k (genTemplatePattern j)
        ]
  in if k <= 2 then oneOf nonrec else oneOf recc

mergePlain :: THtml () -> THtml ()
mergePlain html =
  case html of
    (THtml a nodes) ->
      THtml a (mergePlain' nodes)

mergePlain' :: [TNode ()] -> [TNode ()]
mergePlain' =
  go
  where
    go ((TPlain _ (TPlainText b)) : (TPlain _ (TPlainText c)) : xs) = go (TPlain () (TPlainText (b <> c)) : xs)
    go (x:xs) = x : go xs
    go [] = []
