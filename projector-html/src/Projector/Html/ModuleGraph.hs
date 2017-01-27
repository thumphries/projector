{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Projector.Html.ModuleGraph (
    ModuleGraph (..)
  , buildModuleGraph
  , DependencyGraph (..)
  , buildDependencyGraph
  , deriveImports
  , dependencyOrder
  , rebuildOrder
  , detectCycles
  , GraphError (..)
  , renderGraphError
  ) where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Graph as G
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Tree as Tree

import           P

import           Projector.Core
import           Projector.Html.Data.Module


data GraphError
  = GraphCycle [[ModuleName]]
  deriving (Eq, Ord, Show)

-- | The call graph.
newtype ModuleGraph = ModuleGraph {
    unModuleGraph :: Map ModuleName (Set ModuleName)
  } deriving (Eq, Ord, Show)

-- | An inverted ModuleGraph.
newtype DependencyGraph = DependencyGraph {
    unDependencyGraph :: Map ModuleName (Set ModuleName)
  } deriving (Eq, Ord, Show)


-- | Figure out the complete set of imports for a set of modules.
-- Since we have globally-unique names (i.e. our modules are a
-- compilation detail), we can figure these out automatically.
deriveImports :: Map ModuleName (Module b l a) -> Map ModuleName (Module b l a)
deriveImports mods =
  let modfrees :: Map ModuleName (Set Name)
      modfrees = fmap moduleFree mods

      modbinds :: Map ModuleName (Set Name)
      modbinds = fmap moduleBound mods

      inverted :: Map Name ModuleName
      inverted = M.foldMapWithKey (\k vs -> foldl' (\acc v -> M.insert v k acc) mempty vs) modbinds

      mg :: Map ModuleName (Set ModuleName)
      mg = with modfrees $ \frees ->
             S.fromList (catMaybes (with (toList frees) (flip M.lookup inverted)))
  in flip M.mapWithKey mods $ \k m@(Module typs imps exps) ->
       mcase (M.lookup k mg) m $ \newimps ->
         Module typs (imps <> (M.fromList (fmap (,OpenImport) (toList newimps)))) exps


-- | Construct the module graph for some set of modules.
buildModuleGraph :: Map ModuleName (Module b l a) -> ModuleGraph
buildModuleGraph mods =
  ModuleGraph (with mods (\(Module _typs imps _exps) -> S.fromList (M.keys imps)))

-- | Construct the dependency graph from the call graph.
buildDependencyGraph :: ModuleGraph -> DependencyGraph
buildDependencyGraph (ModuleGraph calls) =
  DependencyGraph
    (M.foldlWithKey
       (\acc i call ->
          foldl' (\acc' c -> M.insertWith (<>) c (S.singleton i) acc') acc call)
       (fmap (const S.empty) calls)
       calls)

-- | The order in which we need to typecheck / compile this set of modules.
-- The result is only correct if the graph forms a DAG.
-- TODO This should probably return a forest, would be nice to parallelise.
-- TODO we also probably want a function to do reachability -> rebuild subtree
dependencyOrder :: DependencyGraph -> [ModuleName]
dependencyOrder (DependencyGraph deps) =
  let (g, lv, _) = G.graphFromEdges (fmap (\(i, depends) -> (i, i, toList depends)) (M.toList deps))
  in fmap (\x -> case lv x of (a,_,_) -> a) (G.topSort g)

-- | Given a list of dirty/changed modules, figure out which dependent
-- mods also need to be rebuilt, and in what order.
rebuildOrder :: DependencyGraph -> [ModuleName] -> [ModuleName]
rebuildOrder dg@(DependencyGraph deps) dirty =
  let (g, lv, vl) = G.graphFromEdges (fmap (\(i, depends) -> (i, i, toList depends)) (M.toList deps))
      dirty' = S.map (\x -> case lv x of (a,_,_) -> a)
                 (foldMap S.fromList (fmap (maybe mempty (G.reachable g) . vl) dirty))
  in filter (flip S.member dirty') (dependencyOrder dg)

-- | Report an error if the call graph does not form a DAG.
-- This does not return an error for free variables or reflexive edges.
detectCycles :: ModuleGraph -> Either GraphError ()
detectCycles cg =
  let (g, lv, _) =
          G.graphFromEdges
        . fmap (\(n, es) -> (n, n, S.toList es))
        . M.toList
        $ unModuleGraph cg
      sccs = G.scc g
      -- for each cycle, take a representative path for error reporting.
      path n = Tree.rootLabel n : case Tree.subForest n of [] -> []; (x:_) -> path x
      labelled = fmap ((\(a, _, _) -> a) . lv)
  in case filter (not . null . Tree.subForest) sccs of
       [] ->
         pure ()
       xs ->
         Left (GraphCycle (fmap (labelled . path) xs))

renderGraphError :: GraphError -> Text
renderGraphError ce =
  case ce of
    GraphCycle cycles ->
      T.intercalate "\n\n" (fmap ppCycle cycles)

ppCycle :: [ModuleName] -> Text
ppCycle cycle =
  case cycle of
    [] ->
      mempty
    (x:xs) ->
      mconcat (
          "A cycle was detected in the module graph:\n"
        : "  Module " <> renderName x <> "\n"
        : with xs (\y -> " reaches " <> renderName y <> "\n")
        <> [" reaches " <> renderName x <> ", forming a cycle."]
        )
  where
    renderName (ModuleName z) = "'" <> z <> "'"
