{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Projector.Hydrant where

import           Data.Foldable (fold)
import           Data.Monoid ((<>))
import           Data.Text (Text)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Projector.Hydrant as Hydrant

import           System.IO (IO)

import           Test.Projector.Hydrant.Gen

import qualified Text.HTML.TagSoup as TS


prop_tagsoup_equiv :: Property
prop_tagsoup_equiv =
  property $ do
    t <- forAll genTagTree
    TS.parseTags (Hydrant.toText (tagTreeHtml t)) === treeToSoup t


treeToSoup :: TagTree -> [TS.Tag Text]
treeToSoup tt =
  case tt of
    TagNode t attrs sub ->
      fold [
          [TS.TagOpen (Hydrant.unTag t) (fmap Hydrant.unAttribute attrs)]
        , foldMap treeToSoup sub
        , [TS.TagClose (Hydrant.unTag t)]
        ]
    TagVoidNode t attrs -> [
        TS.TagOpen (Hydrant.unTag t) (fmap Hydrant.unAttribute attrs)
      , TS.TagClose (Hydrant.unTag t)
      ]
    TagText t ->
      [TS.TagText t]
    Doctype t ->
      -- tagsoup handles doctypes very poorly, almost arbitrarily
      TS.parseTags ("<!DOCTYPE " <> t <> ">")
    Comment t ->
      [TS.TagComment t]

tests :: IO Bool
tests =
  checkParallel $$(discover)
