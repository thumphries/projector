{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Projector.Html.ModuleGraph where

import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Projector.Core.Prelude

import           Projector.Core.Syntax
import           Projector.Html.Core
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim
import           Projector.Html.ModuleGraph

import           Test.Projector.Core.Gen (genName)

import           System.IO (IO)

prop_cycles :: Property
prop_cycles =
  property $ do
    me <- forAll genCyclicExprs
    detectCycles (buildModuleGraph (deriveImports (buildSingletonModules me))) /== pure ()

prop_no_cycles :: Property
prop_no_cycles =
  property $ do
    me <- forAll genAcyclicExprs
    detectCycles (buildModuleGraph (deriveImports (buildSingletonModules me))) === pure ()

prop_dependencies :: Property
prop_dependencies =
  property $ do
    (me, order) <- forAll genAcyclicSet
    dependencyOrder (buildDependencyGraph (buildModuleGraph (deriveImports (buildSingletonModules me))))
      ===
        fmap (ModuleName . unName) order

-- generate a set of exprs where all branches in the dependency forest have cycles
genCyclicExprs :: Gen (Map Name (HtmlExpr ()))
genCyclicExprs =
  Gen.sized $ \n -> do
    k <- Gen.int (Range.linear 2 (fromIntegral n + 2))
    names <- vectorOfUnique k genName
    -- take partitions of at least two
    sets <- partition 2 names
    pure (fold (fmap genCycle sets))

-- generate a set of independent dependency forests, all of which have no cycles
genAcyclicExprs :: Gen (Map Name (HtmlExpr ()))
genAcyclicExprs =
  Gen.sized $ \n -> do
    k <- Gen.int (Range.linear 0 (fromIntegral n))
    names <- vectorOfUnique k genName
    sets <- partition 1 names
    pure (fold (fmap genAcycle sets))

-- like above but only one unbroken dependency tree, not a forest
-- (this makes it easier to reason about the precise dep order)
genAcyclicSet :: Gen (Map Name (HtmlExpr ()), [Name])
genAcyclicSet =
  Gen.sized $ \n -> do
    k <- Gen.int (Range.linear 0 (fromIntegral n))
    names <- vectorOfUnique k genName
    pure (genAcycle names, names)

vectorOfUnique :: Ord a => Int -> Gen a -> Gen [a]
vectorOfUnique k gen =
  go k gen mempty
  where
    go 0 _ set = pure (toList set)
    go n g set = do
      a <- Gen.filter (\b -> not (S.member b set)) g
      go (n-1) g (S.insert a set)

partition :: Int -> [a] -> Gen [[a]]
partition _ [] = pure []
partition i xs =
  Gen.sized $ \n -> do
    k <- Gen.int (Range.linear i (fromIntegral n + i))
    let (y, ys) = L.splitAt k xs
    fmap (y:) (partition i ys)

buildSingletonModules :: Map Name (HtmlExpr ()) -> Map ModuleName (Module () PrimT ())
buildSingletonModules mapp =
  M.mapKeys (ModuleName . unName) . flip M.mapWithKey mapp $ \n e ->
    Module {
        moduleTypes = mempty
      , moduleImports = mempty
      , moduleExprs = M.singleton n (ModuleExpr () e)
      }

genCycle :: [Name] -> Map Name (HtmlExpr ())
genCycle names = do
  case names of
    (x:y:xs) ->
      M.fromList ((x, var y) : genem x y xs)
    _ ->
      mempty -- shouldn't happen
  where
    genem x y [] = [(y, var x)]
    genem x y (z:zs) =
      (y, var z) : genem x z zs

genAcycle :: [Name] -> Map Name (HtmlExpr ())
genAcycle names =
  case names of
    [] ->
      mempty
    (x:xs) ->
      M.fromList ((x, lit (VString "foo")) : (genem x xs))
  where
    genem _ [] =
      []
    genem x (y:ys) =
      (y, var x) : genem y ys


prop_unit_simple_cycle :: Property
prop_unit_simple_cycle =
  once $ assert (isLeft (detectCycles (buildModuleGraph (deriveImports testModuleSet))))

testModuleSet :: Map ModuleName (Module () PrimT ())
testModuleSet =
  M.fromList [
      (ModuleName "Module.Foo", Module {
          moduleTypes = mempty
        , moduleImports = mempty
        , moduleExprs = M.fromList [
              (Name "foo", (ModuleExpr () $ var_ "bar"))
            , (Name "bar", (ModuleExpr () $ var_ "baz"))
            , (Name "bort", (ModuleExpr () $ var_ "quux"))
            ]
        })
    , (ModuleName "Module.Baz", Module {
          moduleTypes = mempty
        , moduleImports = mempty
        , moduleExprs = M.fromList [
              (Name "baz", (ModuleExpr () $ lit (VString "hello")))
            ]
        })
    , (ModuleName "Module.Quux", Module {
          moduleTypes = mempty
        , moduleImports = mempty
        , moduleExprs = M.fromList [
            (Name "quux", (ModuleExpr () $ var_ "baz"))
          ]
        })
    , (ModuleName "Module.Independent", mempty)
    , (ModuleName "Subtree.Root.Two", Module {
          moduleTypes = mempty
        , moduleImports = mempty
        , moduleExprs = M.fromList [
            (Name "bil", (ModuleExpr () $ var_ "pib"))
          , (Name "mun", (ModuleExpr () $ lit (VString "mun")))
          , (Name "pib", (ModuleExpr () $ var_ "mun"))
          , (Name "wol", (ModuleExpr () $ var_ "heck"))
          ]
        })
    , (ModuleName "Hapless.Tepid.Module", Module {
          moduleTypes = mempty
        , moduleImports = mempty
        , moduleExprs = M.fromList [
            (Name "heck", (ModuleExpr () $ var_ "bil"))
          ]
        })
    ]


once :: PropertyT IO () -> Property
once =
  withTests 1 . property

tests :: IO Bool
tests =
  checkParallel $$(discover)
