{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.ModuleGraph where
import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           Disorder.Core hiding (vectorOfUnique)
import           Disorder.Jack

import           P

import           Projector.Core.Syntax
import           Projector.Html.Core
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim
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
    k <- chooseInt (2, n+2)
    names <- vectorOfUnique k genName
    -- take partitions of at least two
    sets <- partition 2 names
    pure (fold (fmap genCycle sets))

-- generate a set of independent dependency forests, all of which have no cycles
genAcyclicExprs :: Jack (Map Name (HtmlExpr ()))
genAcyclicExprs =
  sized $ \n -> do
    k <- chooseInt (0, n)
    names <- vectorOfUnique k genName
    sets <- partition 1 names
    pure (fold (fmap genAcycle sets))

-- like above but only one unbroken dependency tree, not a forest
-- (this makes it easier to reason about the precise dep order)
genAcyclicSet :: Jack (Map Name (HtmlExpr ()), [Name])
genAcyclicSet =
  sized $ \n -> do
    k <- chooseInt (0, n)
    names <- vectorOfUnique k genName
    pure (genAcycle names, names)

vectorOfUnique :: Ord a => Int -> Jack a -> Jack [a]
vectorOfUnique k gen =
  go k gen mempty
  where
    go 0 _ set = pure (toList set)
    go n g set = do
      a <- g `suchThat` (\b -> not (S.member b set))
      go (n-1) g (S.insert a set)

partition :: Int -> [a] -> Jack [[a]]
partition _ [] = pure []
partition i xs =
  sized $ \n -> do
    k <- chooseInt (i, n+i)
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


prop_unit_simple_cycle =
  once (isLeft (detectCycles (buildModuleGraph (deriveImports testModuleSet))))

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


return []
tests = $disorderCheckEnvAll TestRunNormal
