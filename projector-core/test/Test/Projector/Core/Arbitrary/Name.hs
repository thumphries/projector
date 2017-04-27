{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Projector.Core.Arbitrary.Name (
    genName
  , genConstructor
  , genTypeName
  , genIdent
  ) where


import qualified Data.Text as T

import           Disorder.Corpus

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P

import           Projector.Core.Syntax
import           Projector.Core.Type


genName :: Monad m => Gen m Name
genName =
  Name <$> genIdent 12

genIdent :: Monad m => Int -> Gen m Text
genIdent x =
  Gen.choice [
      Gen.element waters
    , Gen.text (Range.singleton x) Gen.lower
    ]

genConstructor :: Monad m => Gen m Constructor
genConstructor =
  fmap (Constructor . T.toTitle) (genIdent 8)

genTypeName :: Monad m => Gen m TypeName
genTypeName =
  fmap (TypeName . T.toTitle) (genIdent 8)
