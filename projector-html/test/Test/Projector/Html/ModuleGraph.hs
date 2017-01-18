{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.ModuleGraph where
import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           Disorder.Core
import           Disorder.Jack

import           P

import           Projector.Core.Syntax
import           Projector.Html.Backend
import           Projector.Html.Core
import           Projector.Html.Core.Prim
import           Projector.Html.ModuleGraph

import           Test.Projector.Core.Arbitrary (genName)


prop_cycles =
  neg . gamble genCyclicExprs $ \me ->
    detectCycles (buildModuleGraph (deriveImports (buildSingletonModules me))) === pure ()

prop_no_cycles =
  gamble genAcyclicExprs $ \me ->
    detectCycles (buildModuleGraph (deriveImports (buildSingletonModules me))) === pure ()

prop_dependencies =
  gamble genAcyclicSet $ \(me, order) ->
    dependencyOrder (buildDependencyGraph (buildModuleGraph (deriveImports (buildSingletonModules me))))
    ===
    fmap (ModuleName . unName) order

-- generate a set of exprs where all branches in the dependency forest have cycles
genCyclicExprs :: Jack (Map Name (HtmlExpr ()))
genCyclicExprs =
  sized $ \n -> do
    k <- chooseInt (2, n + 2)
    names <- (L.nub <$> vectorOf k genName) `suchThat` ((== k) . L.length)
    -- take partitions of at least two
    sets <- partition 2 names
    pure (fold (fmap genCycle sets))

-- generate a set of independent dependency forests, all of which have no cycles
genAcyclicExprs :: Jack (Map Name (HtmlExpr ()))
genAcyclicExprs =
  sized $ \n -> do
    k <- chooseInt (0, n)
    names <- (L.nub <$> vectorOf k genName) `suchThat` ((== k) . L.length)
    sets <- partition 1 names
    pure (fold (fmap genAcycle sets))

-- like above but only one unbroken dependency tree, not a forest
-- (this makes it easier to reason about the precise dep order)
genAcyclicSet :: Jack (Map Name (HtmlExpr ()), [Name])
genAcyclicSet =
  sized $ \n -> do
    k <- chooseInt (0, n)
    names <- (L.nub <$> vectorOf k genName) `suchThat` ((== k) . L.length)
    pure (genAcycle names, names)


partition :: Int -> [a] -> Jack [[a]]
partition _ [] = pure []
partition i xs =
  sized $ \n -> do
    k <- chooseInt (i, n+i)
    let (y, ys) = L.splitAt k xs
    fmap (y:) (partition i ys)

buildSingletonModules :: Map Name (HtmlExpr ()) -> Map ModuleName (Module () ())
buildSingletonModules mapp =
  M.mapKeys (ModuleName . unName) . flip M.mapWithKey mapp $ \n e ->
    Module {
        moduleTypes = mempty
      , moduleImports = mempty
      , moduleExprs = M.singleton n ((), e)
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


prop_unit_simple_cycle =
  once (isLeft (detectCycles (buildModuleGraph testModuleSet)))

testModuleSet :: Map ModuleName (Module () ())
testModuleSet =
  M.fromList [
      (ModuleName "Module.Foo", Module {
          moduleTypes = mempty
        , moduleImports = mempty
        , moduleExprs = M.fromList [
              (Name "foo", ((), var_ "bar"))
            , (Name "bar", ((), var_ "baz"))
            , (Name "bort", ((), var_ "quux"))
            ]
        })
    , (ModuleName "Module.Baz", Module {
          moduleTypes = mempty
        , moduleImports = mempty
        , moduleExprs = M.fromList [
              (Name "baz", ((), lit (VString "hello")))
            ]
        })
    , (ModuleName "Module.Quux", Module {
          moduleTypes = mempty
        , moduleImports = mempty
        , moduleExprs = M.fromList [
            (Name "quux", ((), var_ "baz"))
          ]
        })
    , (ModuleName "Module.Independent", mempty)
    , (ModuleName "Subtree.Root.Two", Module {
          moduleTypes = mempty
        , moduleImports = mempty
        , moduleExprs = M.fromList [
            (Name "bil", ((), var_ "pib"))
          , (Name "mun", ((), lit (VString "mun")))
          , (Name "pib", ((), var_ "mun"))
          , (Name "wol", ((), var_ "heck"))
          ]
        })
    , (ModuleName "Hapless.Tepid.Module", Module {
          moduleTypes = mempty
        , moduleImports = mempty
        , moduleExprs = M.fromList [
            (Name "heck", ((), var_ "bil"))
          ]
        })
    ]


return []
tests = $disorderCheckEnvAll TestRunNormal
