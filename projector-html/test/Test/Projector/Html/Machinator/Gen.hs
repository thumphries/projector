{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Test.Projector.Html.Machinator.Gen where

import qualified Data.List.NonEmpty as NE
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.String (IsString (..))
import qualified Data.Text as T

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Projector.Html.Machinator.Data.Definition
import           Projector.Html.Machinator.Data.Version
import           Projector.Html.Machinator.Data.Token

import           Projector.Core.Prelude


-- TODO this should probably branch on version at relevant points,
-- instead of having separate implementations per version

genDefinitionFilesV1 :: Gen [Versioned DefinitionFile]
genDefinitionFilesV1 =
  Gen.sized $ \n -> do
    k <- Gen.int (Range.linear 1 10)
    fns <- genFree k genName mempty
    fmap (\(res,_,_) -> res) (foldM
        (\(res, kt, kc) fn -> do
          (defs, kt', kc') <- genDefinitionFileV1' (fromIntegral n `div` k) kt kc
          pure (Versioned MachinatorV1 (DefinitionFile fn defs):res, kt', kc'))
        (mempty, mempty, mempty)
        (fmap (T.unpack . unName) (toList fns)))

genDefinitionFileV2 :: Gen (Versioned DefinitionFile)
genDefinitionFileV2 =
  -- V2 is the same as V1 with comments, which doesn't change the parser
  genDefinitionFileV1

genDefinitionFileV1 :: Gen (Versioned DefinitionFile)
genDefinitionFileV1 =
  Gen.sized $ \n ->
    fmap
      (Versioned MachinatorV1 . DefinitionFile "Test.Projector.Html.Machinator.Arbitrary") $ do
        (def, _, _) <- genDefinitionFileV1' (fromIntegral n) mempty mempty
        pure def

genDefinitionFileV1' :: Int -> Set Name -> Set Name -> Gen ([Definition], Set Name, Set Name)
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

genDefinitionV1' :: Int -> Name -> Set Name -> Set Name -> Gen (Definition, Set Name)
genDefinitionV1' k n knownTypes knownCons =
  Gen.choice [
      do (v, cons) <- genVariantV1 k (S.insert n knownTypes) knownCons
         pure (Definition n v, knownCons <> cons)
    , do v <- genRecordV1 k (S.insert n knownTypes)
         pure (Definition n v, S.insert n knownCons)
    ]

genVariantV1 :: Int -> Set Name -> Set Name -> Gen (DataType, Set Name)
genVariantV1 k knownTypes knownCons = do
  k' <- Gen.int (Range.linear 1 (k+1))
  ns <- genFree k' genName (knownTypes <> knownCons)
  cts <- for (toList ns) $ \n -> fmap (n,) (Gen.list (Range.linear 0 10) (genTypeV1 knownTypes))
  pure (Variant (NE.fromList cts), ns <> knownCons)

genRecordV1 :: Int -> Set Name -> Gen DataType
genRecordV1 k knownTypes = do
  k' <- Gen.int (Range.linear 0 k)
  fns <- toList <$> genFree k' genFieldName mempty
  fts <- for fns $ \fn -> (fn,) <$> genTypeV1 knownTypes
  pure (Record fts)

genTypeV1 :: Set Name -> Gen Type
genTypeV1 knownTypes =
  let genVar = fmap Variable (Gen.element (toList knownTypes))
      genGround = fmap GroundT genGroundTypeV1
      genList = fmap ListT (genTypeV1 knownTypes)
  in if S.null knownTypes then genGround else Gen.choice [genVar, genGround, genList]

genGroundTypeV1 :: Gen Ground
genGroundTypeV1 =
  Gen.element [
      StringT
    ]

genName :: Gen Name
genName =
  fmap (Name . T.toTitle) $ Gen.choice [
      Gen.element waters
    , Gen.text (Range.singleton 8) Gen.lower
    ]

genFree :: Ord k => Int -> Gen k -> Set k -> Gen (Set k)
genFree k gen known =
  go k gen known mempty
  where
    go 0 _ _ r = pure r
    go j g s r = do
      e <- Gen.filter (`S.notMember` s) g
      go (j-1) g (S.insert e s) (S.insert e r)

genFieldName :: Gen Name
genFieldName =
  Gen.choice [
      fmap Name (Gen.element boats)
    , fmap Name (Gen.element waters)
    , fmap Name (Gen.element [dataKeyword, recordKeyword])
    , Name <$> Gen.text (Range.singleton 8) Gen.lower
    ]


waters :: IsString a => [a]
waters = [
    "basin"
  , "bay"
  , "billabong"
  , "canal"
  , "channel"
  , "creek"
  , "estuary"
  , "fjord"
  , "harbour"
  , "lake"
  , "loch"
  , "marsh"
  , "ocean"
  , "pond"
  , "puddle"
  , "reservoir"
  , "river"
  , "sea"
  , "slough"
  , "sound"
  , "spring"
  , "stream"
  , "swamp"
  , "wetland"
  ]

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
