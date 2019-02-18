{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Gen where


import           Data.Generics.Aliases
import           Data.Generics.Schemes
import qualified Data.List as L
import           Data.List.NonEmpty  (NonEmpty(..))
import           Data.String (IsString (..))

import qualified Data.Text as T

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Projector.Core.Prelude

import           Projector.Core
import           Projector.Html
import           Projector.Html.Data.Annotation
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim as Prim
import           Projector.Html.Data.Template
import           Projector.Html.Core (constructorFunctions, htmlTypes)

import           Test.Projector.Core.Gen


-- -----------------------------------------------------------------------------
-- Generating core types and expressions

genHtmlTypeDecls :: Gen HtmlDecls
genHtmlTypeDecls =
  genTypeDecls htmlTypes genHtmlLitT

genWellTypedHtmlExpr :: HtmlDecls -> Gen (HtmlType, HtmlExpr ())
genWellTypedHtmlExpr ctx = do
  ty <- genHtmlType ctx
  ex <- genWellTypedExpr ctx ty (genHtmlType ctx) genWellTypedHtmlLit
  pure (ty, ex)

genWellTypedHtmlModule :: Int -> HtmlDecls -> Gen (Module HtmlType PrimT (HtmlType, SrcAnnotation))
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

genHtmlType :: HtmlDecls -> Gen HtmlType
genHtmlType ctx =
  genTypeFromContext ctx genHtmlLitT

genWellTypedHtmlLit :: PrimT -> Gen HtmlLit
genWellTypedHtmlLit t =
  case t of
    Prim.TString ->
      Prim.VString <$> Gen.element boats

genHtmlLitT :: Gen PrimT
genHtmlLitT =
  Gen.enumBounded

-- -----------------------------------------------------------------------------
-- Generating untyped templates

genTemplate :: Gen (Template ())
genTemplate =
  Gen.sized $ \k -> do
    j <- Gen.int (Range.linear 1 (fromIntegral k + 1))
    Template () <$> genTemplateTypeSig <*> genTemplateExpr j

genTemplateTypeSig :: Gen (Maybe (TTypeSig ()))
genTemplateTypeSig = do
  k <- Gen.int (Range.linear 0 20)
  case k of
    0 ->
      pure Nothing
    _ ->
      fmap Just . TTypeSig ()
        <$> Gen.list (Range.linear 0 k) ((,) <$> (TId <$> Gen.element muppets) <*> genTVar)
        <*> genTVar

genTVar :: Gen (TType ())
genTVar =
  (TTVar () . TId . T.toTitle) <$> Gen.element waters

genHtml :: Int -> Gen (THtml ())
genHtml k =
  let j = k `div` 2
  in THtml () . everywhere (mkT mergePlain) <$>
     Gen.list
       (Range.linear 1 (k + 1))
       (Gen.choice
          [ genElement j
          , genVoidElement j
          , genComment
          , genHtmlExpr j
          , genHtmlWS j
          , genHtmlTextExpr j
          ])

genHtmlWS :: Int -> Gen (TNode ())
genHtmlWS k =
  (\(THtml () nodes) -> THtmlWS () nodes)
    <$> genHtml k

genElement :: Int -> Gen (TNode ())
genElement k =
  let j = k `div` 2 in
  Gen.shrink
    (\node ->
      case node of
        TElement () _tag _attrs (THtml _ nodes) ->
          nodes
        _ ->
          [node])
    (TElement ()
      <$> genTag
      <*> Gen.list (Range.linear 0 j) (genAttribute (j `div` 2))
      <*> genHtml (j `div` 2))

genVoidElement :: Int -> Gen (TNode ())
genVoidElement k =
  TVoidElement ()
    <$> genTag
    <*> Gen.list (Range.linear 0 k) (genAttribute (k `div` 2))

genComment :: Gen (TNode ())
genComment =
  TComment () <$> genCommentText

genPlain :: Gen (TNode ())
genPlain =
  TPlain () <$> genPlainText

genCommentText :: Gen TPlainText
genCommentText =
  TPlainText . T.replace "-" "a" . mangle <$> (Gen.text (Range.linear 1 25) Gen.unicode)

genPlainText :: Gen TPlainText
genPlainText =
  fmap (TPlainText . mangle) (Gen.text (Range.linear 1 25) Gen.unicode)

mangle :: Text -> Text
mangle =
    T.replace " " "a"
  . T.replace "\r" "d"
  . T.replace "\n" "b"
  . T.replace "\t" "c"
  . T.replace "\\" "\\\\"
  . T.replace "(" "e"
  . T.replace ")" "f"

genHtmlExpr :: Int -> Gen (TNode ())
genHtmlExpr k =
  TExprNode () <$> genTemplateExpr k

genHtmlTextExpr :: Int -> Gen (TNode ())
genHtmlTextExpr k =
  TTextExprNode () <$> genTemplateExpr k

genAttribute :: Int -> Gen (TAttribute ())
genAttribute k =
  Gen.choice [
      TEmptyAttribute () <$> genAttributeName
    , TAttribute () <$> genAttributeName <*> genAttributeValue k
    ]

genAttributeName :: Gen TAttrName
genAttributeName =
  TAttrName <$> Gen.choice [
      pure "enabled"
    , pure "display"
    , pure "style"
    , pure "src"
    , pure "href"
    , Gen.element boats
    ]

genAttributeValue :: Int -> Gen (TAttrValue ())
genAttributeValue k =
  if k <= 2
    then TQuotedAttrValue () <$> genInterpolatedString k
    else TAttrExpr () <$> genTemplateExpr k

genInterpolatedString :: Int -> Gen (TIString ())
genInterpolatedString k = do
  n <- Gen.int (Range.linear 0 k)
  (everywhere (mkT mergeTIChunk) . TIString ()) <$>
    Gen.list (Range.linear 0 n)
      (Gen.choice [genStringChunk, fmap (TExprChunk ()) (genTemplateExpr (k - n))])

genStringChunk :: Gen (TIChunk ())
genStringChunk =
  TStringChunk () <$> Gen.choice [
      pure "true"
    , pure "false"
    , pure "0"
    , Gen.element muppets
    -- TODO arbitrary JS would be nice
    ]

mergeTIChunk :: TIString () -> TIString ()
mergeTIChunk (TIString () chunks) =
  TIString () (go chunks)
  where
    go [] = []
    go (TStringChunk _ a : TStringChunk _ b : xs) = go (TStringChunk () (a <> b) : xs)
    go (x:xs) = x : go xs

genTag :: Gen (TTag ())
genTag =
  TTag () <$> Gen.element [
      "a"
    , "html"
    , "span"
    , "p"
    , "h1"
    , "blink"
    , "marquee"
    ]

genVoidTag :: Gen (TTag ())
genVoidTag =
  Gen.element [
      TTag () "img"
    ]

genTemplateExpr :: Int -> Gen (TExpr ())
genTemplateExpr k =
  let j = k `div` 2
      nonrec = [
          TEVar () <$> genTId
        , pure (TEHole ())
        ]
      recc = [
          TEApp () <$> genTemplateExpr j <*> genTemplateExpr j
        , TECase () <$> genTemplateExpr j <*> genTemplateAlts j
        , TELam () <$> (Gen.nonEmpty (Range.linear 1 10) (TId <$> Gen.element simpsons)) <*> genTemplateExpr j
        , TEString () <$> genInterpolatedString j
        , TEEach () <$> genTemplateExpr j <*> genTemplateExpr j
        , TEList () <$> Gen.list (Range.linear 0 (j `div` 2)) (genTemplateExpr (j `div` 2))
        , TEPrj () <$> genTemplateExpr j <*> genField
        , TENode () <$> genHtml j
        ]
  in if k <= 2 then Gen.choice nonrec else Gen.choice recc

genTId :: Gen TId
genTId =
  TId <$> Gen.choice [
      Gen.element muppets
    , Gen.element (L.zipWith (\a b -> a <> "/" <> b) cooking muppets)
    ]

genTemplateAlts :: Int -> Gen (NonEmpty (TAlt ()))
genTemplateAlts j = do
  k <- Gen.int (Range.linear 1 10)
  (:|) <$> genTemplateAlt j <*> Gen.list (Range.singleton k) (genTemplateAlt (j `div` k))

genTemplateAlt :: Int -> Gen (TAlt ())
genTemplateAlt k =
  let j = k `div` 2 in
  TAlt () <$> genTemplatePattern j <*> genTemplateExpr j

genField :: Gen TId
genField =
  TId <$> Gen.element (waters <> boats <> muppets)

genTemplatePattern :: Int -> Gen (TPattern ())
genTemplatePattern k =
  let j = k `div` 2
      nonrec = [
          TPVar () <$> (TId <$> Gen.element waters)
        , pure (TPWildcard ())
        ]
      recc = [
          TPCon () <$> ((TConstructor . T.toTitle) <$> Gen.element muppets)
                   <*> Gen.list (Range.linear 0 k) (genTemplatePattern j)
        ]
  in if k <= 2 then Gen.choice nonrec else Gen.choice recc

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

boats :: IsString a => [a]
boats = [
    "barge"
  , "battleship"
  , "canoe"
  , "catamaran"
  , "dinghy"
  , "ferry"
  , "gondola"
  , "jetski"
  , "kayak"
  , "longship"
  , "motorboat"
  , "pontoon"
  , "powerboat"
  , "rowboat"
  , "ship"
  , "steamboat"
  , "tanker"
  , "trawler"
  , "tugboat"
  , "yacht"
  ]
