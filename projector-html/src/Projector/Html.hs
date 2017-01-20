{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html (
    HtmlError (..)
  , renderHtmlError
  -- * Builds
  , Build (..)
  , runBuild
  , RawTemplates (..)
  , BuildArtefacts (..)
  , ModulePrefix (..)
  , ModuleGraph (..)
  -- * Useful template and module utils
  , parseTemplate
  , checkTemplate
  , checkTemplateIncremental
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
import qualified Projector.Html.Core.Elaborator as Elab
import qualified Projector.Html.Data.Backend as HB
import qualified Projector.Html.Data.Module as HB
import           Projector.Html.Data.Position  (Range)
import           Projector.Html.Data.Prim
import           Projector.Html.Data.Template  (Template)
import           Projector.Html.ModuleGraph
import           Projector.Html.Parser (ParseError (..), renderParseError, parse)

import           System.IO  (FilePath)


-- -----------------------------------------------------------------------------
-- Top-level errors

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

-- -----------------------------------------------------------------------------
-- Interfaces for doing things with templates

parseTemplate :: FilePath -> Text -> Either HtmlError (Template Range)
parseTemplate f =
  first HtmlParseError . parse f

checkTemplate :: Template Range -> Either HtmlError (HtmlType, HtmlExpr (HtmlType, Range))
checkTemplate =
  first HtmlCoreError . HC.templateToCore

checkTemplateIncremental ::
     Map Text (HtmlType, Range)
  -> Template Range
  -> Either HtmlError (HtmlType, HtmlExpr (HtmlType, Range))
checkTemplateIncremental known ast =
    first HtmlCoreError
  . (>>= (maybe (Left (HC.HtmlTypeError [])) pure . M.lookup (PC.Name "it")))
  . HC.typeCheckIncremental mempty (M.mapKeys PC.Name known)
  . M.singleton (PC.Name "it")
  $ Elab.elaborate ast

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

-- -----------------------------------------------------------------------------
-- Build interface, i.e. things an end user should use

data Build = Build {
    buildBackend :: Maybe BackendT
  , buildModulePrefix :: ModulePrefix
  , buildOutputPath :: FilePath
  } deriving (Eq, Ord, Show)

newtype ModulePrefix = ModulePrefix {
    unModulePrefix :: ModuleName
  } deriving (Eq, Ord, Show)

newtype RawTemplates = RawTemplates {
    unRawTemplates :: [(FilePath, Text)]
  } deriving (Eq, Ord, Show)

newtype BuildArtefacts = BuildArtefacts {
    unBuildArtefacts :: [(FilePath, Text)]
  } deriving (Eq, Ord, Show)


-- | Run a complete build from start to finish, with no caching of artefacts.
--
-- TODO pass in datatypes too.
-- TODO return partial results when we bomb out. 'These'-esque datatype.
runBuild :: Build -> RawTemplates -> Either HtmlError BuildArtefacts
runBuild =
  undefined
