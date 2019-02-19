{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Projector.Html.Machinator.Graph (
    buildFileGraph
  ) where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S

import           Projector.Html.Machinator.Data.Definition

import           Projector.Core.Prelude

import           System.IO (FilePath)


-- | Figures out the file graph
-- i.e. for each file, which other files does it depend on?
buildFileGraph :: [DefinitionFile] -> DefinitionFileGraph
buildFileGraph fs =
  let
    binds :: Map FilePath [Name]
    binds =
      M.fromList . with fs $ \(DefinitionFile fp defs) ->
        (fp, fmap defName defs)

    uses :: Map FilePath [Name]
    uses =
      M.fromList . with fs $ \(DefinitionFile fp defs) ->
        (fp, toList (fold (fmap (free . defType) defs)))

    inverted :: Map Name FilePath
    inverted =
      M.foldMapWithKey (\k ns -> foldl' (\acc v -> M.insertWith (<>) v k acc) mempty ns) binds

    fg :: Map FilePath (Set FilePath)
    fg =
      M.fromList . with fs $ \(DefinitionFile fp _) ->
        (fp,) (maybe mempty (S.fromList . filter (/= fp) . catMaybes . fmap (flip M.lookup inverted)) (M.lookup fp uses))
  in DefinitionFileGraph fg
