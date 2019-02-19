{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Machinator.Gen where


import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NE
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import           Disorder.Corpus
import           Disorder.Jack

import           Projector.Html.Machinator.Data.Definition
import           Projector.Html.Machinator.Data.Version
import           Projector.Html.Machinator.Data.Token

import           Projector.Core.Prelude


-- TODO this should probably branch on version at relevant points,
-- instead of having separate implementations per version

genDefinitionFilesV1 :: Jack [Versioned DefinitionFile]
genDefinitionFilesV1 =
  sized $ \n -> do
    k <- chooseInt (1, 10)
    fns <- genFree k genName mempty
    fmap (\(res,_,_) -> res) (foldM
        (\(res, kt, kc) fn -> do
          (defs, kt', kc') <- genDefinitionFileV1' (n `div` k) kt kc
          pure (Versioned MachinatorV1 (DefinitionFile fn defs):res, kt', kc'))
        (mempty, mempty, mempty)
        (fmap (T.unpack . unName) (toList fns)))

genDefinitionFileV2 :: Jack (Versioned DefinitionFile)
genDefinitionFileV2 =
  -- V2 is the same as V1 with comments, which doesn't change the parser
  genDefinitionFileV1

genDefinitionFileV1 :: Jack (Versioned DefinitionFile)
genDefinitionFileV1 =
  sized $ \n ->
    fmap
      (Versioned MachinatorV1 . DefinitionFile "Test.Projector.Html.Machinator.Arbitrary") $ do
        (def, _, _) <- genDefinitionFileV1' n mempty mempty
        pure def

genDefinitionFileV1' :: Int -> Set Name -> Set Name -> Jack ([Definition], Set Name, Set Name)
genDefinitionFileV1' n kts kcs = do
  tns <- genFree n genName kts
  let kts' = kts <> tns
  fmap (\(res, kc) -> (res, kts', kc))
    (foldM
      (\(res, kc) name -> do
        (d, kc') <- genDefinitionV1' (n `div` 2) name kts' kc
        pure (d:res, kc'))
      (mempty, kcs)
      (toList tns))

genDefinitionV1' :: Int -> Name -> Set Name -> Set Name -> Jack (Definition, Set Name)
genDefinitionV1' k n knownTypes knownCons =
  oneOf [
      do (v, cons) <- genVariantV1 k (S.insert n knownTypes) knownCons
         pure (Definition n v, knownCons <> cons)
    , do v <- genRecordV1 k (S.insert n knownTypes)
         pure (Definition n v, S.insert n knownCons)
    ]

genVariantV1 :: Int -> Set Name -> Set Name -> Jack (DataType, Set Name)
genVariantV1 k knownTypes knownCons = do
  k' <- chooseInt (1, k+1)
  ns <- genFree k' genName (knownTypes <> knownCons)
  cts <- for (toList ns) $ \n -> fmap (n,) (listOfN 0 10 (genTypeV1 knownTypes))
  pure (Variant (NE.fromList cts), ns <> knownCons)

genRecordV1 :: Int -> Set Name -> Jack DataType
genRecordV1 k knownTypes = do
  k' <- chooseInt (0, k)
  fns <- toList <$> genFree k' genFieldName mempty
  fts <- for fns $ \fn -> (fn,) <$> genTypeV1 knownTypes
  pure (Record fts)

genTypeV1 :: Set Name -> Jack Type
genTypeV1 knownTypes =
  let genVar = fmap Variable (elements (toList knownTypes))
      genGround = fmap GroundT genGroundTypeV1
      genList = fmap ListT (genTypeV1 knownTypes)
  in if S.null knownTypes then genGround else oneOf [genVar, genGround, genList]

genGroundTypeV1 :: Jack Ground
genGroundTypeV1 =
  elements [
      StringT
    ]

genName :: Jack Name
genName =
  fmap (Name . T.toTitle) $ oneOf [
      elements waters
    , T.pack <$> vectorOf 8 (arbitrary `suchThat` Char.isAsciiLower)
    ]

genFree :: Ord k => Int -> Jack k -> Set k -> Jack (Set k)
genFree k gen known =
  go k gen known mempty
  where
    go 0 _ _ r = pure r
    go j g s r = do
      e <- g `suchThat` (`S.notMember` s)
      go (j-1) g (S.insert e s) (S.insert e r)

genFieldName :: Jack Name
genFieldName =
  oneOf [
      fmap Name (elements boats)
    , fmap Name (elements waters)
    , fmap Name (elements [dataKeyword, recordKeyword])
    , (Name . T.pack) <$> vectorOf 8 (arbitrary `suchThat` Char.isAsciiLower)
    ]
