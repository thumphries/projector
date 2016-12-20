{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.CallGraph (
    CallGraph (..)
  , buildCallGraph
  , detectCycles
  , CycleError (..)
  ) where


import qualified Data.Graph as G
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Tree as T

import           P

import           Projector.Core.Syntax


data CycleError
  = CycleError [[Name]]
  deriving (Eq, Ord, Show)

data CallGraph = CallGraph {
    unCallGraph :: Map Name (Set Name)
  } deriving (Eq, Ord, Show)

buildCallGraph :: Map Name (Expr l a) -> CallGraph
buildCallGraph =
  CallGraph . fmap gatherFree

-- | Report an error if the call graph does not form a DAG.
-- This does not return an error for free variables or reflexive edges.
detectCycles :: CallGraph -> Either CycleError ()
detectCycles cg =
  let (g, lv, _) =
          G.graphFromEdges
        . fmap (\(n, es) -> (n, n, S.toList es))
        . M.toList
        $ unCallGraph cg
      sccs = G.scc g
      -- for each cycle, take the first, simplest path for error reporting.
      path n = T.rootLabel n : case T.subForest n of [] -> []; (x:_) -> path x
      labelled = fmap ((\(a, _, _) -> a) . lv)
  in case filter (not . null . T.subForest) sccs of
       [] ->
         pure ()
       xs ->
         trace (show sccs) $
         Left (CycleError (fmap (labelled . path) xs))
