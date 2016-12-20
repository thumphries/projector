{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.CallGraph (
    CallGraph (..)
  , buildCallGraph
  ) where


import           Data.Map.Strict (Map)
import           Data.Set (Set)

import           P

import           Projector.Core.Syntax


newtype CallGraph = CallGraph {
    unCallGraph :: Map Name (Set Name)
  } deriving (Eq, Show, Monoid)

buildCallGraph :: Map Name (Expr l a) -> CallGraph
buildCallGraph =
  CallGraph . fmap gatherFree
