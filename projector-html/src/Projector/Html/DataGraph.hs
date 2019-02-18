{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Projector.Html.DataGraph (
    DataGraph (..)
  , buildFileGraph
  ) where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S

import           Projector.Core (TypeName (..))
import           Projector.Core.Type (free)
import           Projector.Html.Data.Prim

import           P

import           System.IO (FilePath)

newtype DataGraph =
  DataGraph {
      dataGraph :: Map FilePath (Set FilePath)
    } deriving (Eq, Ord, Show)

-- | Figures out the file graph
-- i.e. for each file, which other files does it depend on?
buildFileGraph :: [(FilePath, Map TypeName HtmlDecl)] -> DataGraph
buildFileGraph fs =
  let
    binds :: Map FilePath [TypeName]
    binds =
      M.fromList . with fs $ \(fp, defs) ->
        (fp, M.keys defs)

    uses :: Map FilePath [TypeName]
    uses =
      M.fromList . with fs $ \(fp, defs) ->
        (fp, M.elems defs >>= S.toList . free)

    inverted :: Map TypeName FilePath
    inverted =
      M.foldMapWithKey (\k ns -> foldl' (\acc v -> M.insertWith (<>) v k acc) mempty ns) binds

    fg :: Map FilePath (Set FilePath)
    fg =
      M.fromList . with fs $ \(fp, _) ->
        (fp,) (maybe mempty (S.fromList . filter (/= fp) . catMaybes . fmap (flip M.lookup inverted)) (M.lookup fp uses))
  in DataGraph fg
