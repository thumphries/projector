{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.CallGraph (
    CallGraph (..)
  , buildCallGraph
  , hasCycles
  ) where


import qualified Data.Graph as G
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Tree as T

import           P

import           Projector.Core.Syntax


data CallGraph = CallGraph {
    callGraph :: G.Graph
  , lookupByVertex :: G.Vertex -> (Name, Name, [Name])
  , lookupByName :: Name -> Maybe G.Vertex
  }

buildCallGraph :: Map Name (Expr l a) -> CallGraph
buildCallGraph m =
  let (g, lv, ln) =
          G.graphFromEdges
        . fmap (\(n, es) -> (n, n, S.toList es))
        . M.toList
        $ fmap gatherFree m
  in CallGraph g lv ln

hasCycles :: CallGraph -> Bool
hasCycles cgr =
  not (all (null . T.subForest) (G.scc (callGraph cgr)))
