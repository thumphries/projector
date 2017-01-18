{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html (
    HtmlError (..)
  , renderHtmlError
  , parseTemplate
  , checkTemplate
  , checkModule
  , checkModules
  , codeGenModule
  -- * Templates
  , Template
  , Range
  , HtmlType
  , HtmlExpr
  ) where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           P

import qualified Projector.Core as PC
import qualified Projector.Html.Backend as HB
import qualified Projector.Html.Core as HC
import           Projector.Html.Core  (CoreError(..))
import           Projector.Html.Data.Position  (Range)
import           Projector.Html.Data.Prim
import           Projector.Html.Data.Template  (Template)
import           Projector.Html.ModuleGraph
import           Projector.Html.Parser (ParseError (..), renderParseError, parse)

import           System.IO  (FilePath)


data HtmlError
  = HtmlParseError ParseError
  | HtmlCoreError (CoreError Range)
  deriving (Eq, Show)

renderHtmlError :: HtmlError -> Text
renderHtmlError he =
  case he of
    HtmlParseError e ->
      renderParseError e
    HtmlCoreError e ->
      HC.renderCoreErrorRange e

parseTemplate :: FilePath -> Text -> Either HtmlError (Template Range)
parseTemplate f =
  first HtmlParseError . parse f

checkTemplate :: Template Range -> Either HtmlError (HtmlType, HtmlExpr (HtmlType, Range))
checkTemplate =
  first HtmlCoreError . HC.templateToCore

checkModule ::
     HtmlDecls
  -> HB.Module () Range
  -> Either HtmlError (HB.Module HtmlType (HtmlType, Range))
checkModule decls (HB.Module typs imps exps) = do
  exps' <- first HtmlCoreError (HC.typeCheckAll (decls <> typs) (fmap snd exps))
  pure (HB.Module typs imps exps')

-- | Figure out the dependency order of a set of modules, then
-- typecheck them all in that order.
checkModules ::
     HtmlDecls
  -> Map HB.ModuleName (HB.Module () Range)
  -> Either HtmlError (Map HB.ModuleName (HB.Module HtmlType (HtmlType, Range)))
checkModules decls exprs =
  first HtmlCoreError (fmap fst (foldM fun (mempty, mempty) deps))
  where
    deps = dependencyOrder (buildDependencyGraph (buildModuleGraph exprs))
    --
    fun (res, acc) n =
      maybe (pure (res, acc)) (\mo -> doit n mo res acc) (M.lookup n exprs)
    --
    doit n (HB.Module types imports exps) res acc = do
      exps' <- HC.typeCheckIncremental decls acc (fmap snd exps)
      let result = HB.Module types imports exps'
          newacc = M.union (fmap (PC.extractAnnotation . snd) exps') acc
      pure (M.insert n result res, newacc)

codeGenModule ::
     HB.BackendT
  -> HB.ModuleName
  -> HB.Module HtmlType (HtmlType, Range)
  -> (FilePath, Text)
codeGenModule backend =
  HB.renderModule (HB.getBackend backend)
