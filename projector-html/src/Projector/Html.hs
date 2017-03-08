{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Projector.Html (
    HtmlError (..)
  , renderHtmlError
  -- * Builds
  , Build (..)
  , runBuild
  , runBuildIncremental
  , RawTemplates (..)
  , BuildArtefacts (..)
  , DataModuleName (..)
  , UserDataTypes (..)
  , ModuleNamer (..)
  , ModuleGraph (..)
  , HB.ModuleName (..)
  -- * Useful template and module utils
  , checkExpr
  , checkExprIncremental
  , parseTemplate
  , checkTemplate
  , checkTemplateIncremental
  , checkModule
  , checkModules
  , codeGen
  , codeGenModule
  , validateModules
  , warnModules
  -- * Templates
  , Template
  , Range
  , HtmlType
  , HtmlModules
  , HtmlExpr
  , moduleNamerSimple
  ) where


import qualified Data.Char as Char
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

import           P

import qualified Projector.Core as PC
import qualified Projector.Html.Backend as HB
import qualified Projector.Html.Core as HC
import           Projector.Html.Core  (CoreError(..))
import qualified Projector.Html.Core.Elaborator as Elab
import           Projector.Html.Data.Annotation
import qualified Projector.Html.Data.Backend as HB
import qualified Projector.Html.Data.Module as HB
import           Projector.Html.Data.Position  (Range, renderRange)
import           Projector.Html.Data.Prim
import           Projector.Html.Data.Template  (Template)
import           Projector.Html.ModuleGraph
import           Projector.Html.Parser (ParseError (..), renderParseError, parse)

import qualified System.FilePath.Posix as FilePath
import           System.IO  (FilePath)

import           X.Control.Monad.Trans.Either (sequenceEither)


-- -----------------------------------------------------------------------------
-- Top-level errors

data HtmlError
  = HtmlParseError ParseError
  | HtmlCoreError (CoreError SrcAnnotation)
  | HtmlCoreWarning (HtmlWarning SrcAnnotation)
  | HtmlModuleGraphError GraphError
  deriving (Eq, Show)

renderHtmlError :: HtmlError -> Text
renderHtmlError he =
  case he of
    HtmlParseError e ->
      renderParseError e
    HtmlCoreError e ->
      HC.renderCoreErrorAnnotation renderRange e
    HtmlCoreWarning e ->
      HC.renderCoreWarningAnnotation renderRange e
    HtmlModuleGraphError e ->
      renderGraphError e

-- -----------------------------------------------------------------------------
-- Interfaces for doing things with templates

parseTemplate :: FilePath -> Text -> Either HtmlError (Template Range)
parseTemplate f =
  first HtmlParseError . parse f

checkTemplate ::
     HtmlDecls
  -> Template Range
  -> Either HtmlError (HtmlType, HtmlExpr (HtmlType, SrcAnnotation))
checkTemplate decls =
  checkTemplateIncremental decls mempty

checkTemplateIncremental ::
     HtmlDecls
  -> Map Text (HtmlType, SrcAnnotation)
  -> Template Range
  -> Either HtmlError (HtmlType, HtmlExpr (HtmlType, SrcAnnotation))
checkTemplateIncremental decls known =
  checkExprIncremental decls known . Elab.elaborate

checkExpr ::
     HtmlDecls
  -> HtmlExpr SrcAnnotation
  -> Either HtmlError (HtmlType, HtmlExpr (HtmlType, SrcAnnotation))
checkExpr decls =
  checkExprIncremental decls mempty

checkExprIncremental ::
     HtmlDecls
  -> Map Text (HtmlType, SrcAnnotation)
  -> HtmlExpr SrcAnnotation
  -> Either HtmlError (HtmlType, HtmlExpr (HtmlType, SrcAnnotation))
checkExprIncremental decls known =
    first HtmlCoreError
  . (>>= (maybe (Left (HC.HtmlTypeError [])) pure . M.lookup (PC.Name "it")))
  . HC.typeCheckIncremental decls (HC.constructorFunctionTypes decls <> libraryExprs <> (M.mapKeys PC.Name known))
  . M.singleton (PC.Name "it")

checkModule ::
     HtmlDecls
  -> HB.Module a PrimT SrcAnnotation
  -> Either HtmlError (HB.Module HtmlType PrimT (HtmlType, SrcAnnotation))
checkModule decls (HB.Module typs imps exps) = do
  exps' <- first HtmlCoreError (HC.typeCheckIncremental (decls <> typs) (HC.constructorFunctionTypes decls) (fmap snd exps))
  pure (HB.Module typs imps exps')

-- | Figure out the dependency order of a set of modules, then
-- typecheck them all in that order.
checkModules ::
     HtmlDecls
  -> Map PC.Name (HtmlType, SrcAnnotation)
  -> Map HB.ModuleName (HB.Module a PrimT SrcAnnotation)
  -> Either HtmlError (Map HB.ModuleName (HB.Module HtmlType PrimT (HtmlType, SrcAnnotation)))
checkModules decls known exprs =
  -- FIX Check for duplicate function names
  first HtmlCoreError (fmap fst (foldM fun (mempty, HC.constructorFunctionTypes decls <> known) deps))
  where
    deps = dependencyOrder (buildDependencyGraph (buildModuleGraph exprs))
    --
    fun (res, acc) n =
      maybe (pure (res, acc)) (\mo -> doit n mo res acc) (M.lookup n exprs)
    --
    doit n (HB.Module types imports exps) res acc = do
      exps' <- HC.typeCheckIncremental (decls <> types) acc (fmap snd exps)
      let result = HB.Module types imports exps'
          newacc = M.union (fmap (PC.extractAnnotation . snd) exps') acc
      pure (M.insert n result res, newacc)

codeGen :: HB.Backend SrcAnnotation e -> BuildArtefacts -> Either [e] [(FilePath, Text)]
codeGen backend (BuildArtefacts checked) = do
  validateModules backend checked
  -- TODO this can be a lazy stream
  fmap M.elems $ first pure (M.traverseWithKey (codeGenModule backend) checked)

codeGenModule ::
     HB.Backend a e
  -> HB.ModuleName
  -> HB.Module HtmlType PrimT (HtmlType, a)
  -> Either e (FilePath, Text)
codeGenModule =
  HB.renderModule

-- -----------------------------------------------------------------------------
-- Build interface, i.e. things an end user should use

data Build = Build {
    buildModuleNamer :: ModuleNamer -- ^ A customisable way to name modules
  , buildDataModules :: [DataModuleName] -- ^ The modules containing user datatypes.
  }

-- | A set of templates that have been read from disk, but not yet
-- parsed or checked.
--
-- The generated module name will be derived from the 'FilePath'
-- argument. Any irrelevant common prefixes should be stripped before
-- construction.
newtype RawTemplates = RawTemplates {
    unRawTemplates :: [(FilePath, Text)]
  } deriving (Eq, Ord, Show)

data BuildArtefacts = BuildArtefacts {
    buildArtefactsHtmlModules :: HtmlModules
  } deriving (Eq, Show)

type HtmlModules = Map HB.ModuleName (HB.Module HtmlType PrimT (HtmlType, SrcAnnotation))

newtype DataModuleName = DataModuleName {
    unDataModuleName :: HB.ModuleName
  } deriving (Eq, Ord, Show)

newtype UserDataTypes = UserDataTypes {
    unUserDataTypes :: HtmlDecls
  } deriving (Eq, Ord, Show, Monoid)

data ModuleNamer = ModuleNamer {
    pathToModuleName :: FilePath -> HB.ModuleName
  , filePathToExprName  :: FilePath -> PC.Name
  }

-- | Run a complete build from start to finish, with no caching of artefacts.
--
-- TODO return partial results when we bomb out. 'These'-esque datatype.
runBuild :: Build -> UserDataTypes -> RawTemplates -> Either [HtmlError] BuildArtefacts
runBuild b udt rts =
  runBuildIncremental b udt mempty rts

runBuildIncremental :: Build -> UserDataTypes -> HtmlModules -> RawTemplates -> Either [HtmlError] BuildArtefacts
runBuildIncremental (Build mnr mdm) (UserDataTypes decls) hms rts = do
  -- Build the module map
  (mg, mmap) <- smush mdm mnr hms rts
  -- Check it for cycles
  (_ :: ()) <- first (pure . HtmlModuleGraphError) (detectCycles mg)
  -- Check all modules (this can be a lazy stream)
  -- TODO the Map forces all of this at once, remove
  fmap BuildArtefacts $ first pure (checkModules decls (libraryExprs <> HB.extractModuleBindings hms) mmap)

-- TODO hmm is this a compilation detail we should hide in HC?
libraryExprs :: Map PC.Name (HtmlType, SrcAnnotation)
libraryExprs =
  M.mapWithKey (\n (ty,_e) -> (ty, LibraryFunction n)) HC.libraryExprs

-- | Run a set of backend-specific predicates.
validateModules :: HB.Backend a e -> Map HB.ModuleName (HB.Module HtmlType PrimT b) -> Either [e] ()
validateModules backend mods =
  fmap (const ()) (sequenceEither (with mods (HB.checkModule backend)))

-- | Look for anything we can warn about.
warnModules :: HtmlDecls -> HtmlModules -> Either [HtmlError] ()
warnModules decls mods =
  let binds = S.fromList (M.keys (HB.extractModuleBindings mods))
      exprs = fmap (fmap snd) (HB.extractModuleExprs mods)
      shadowing = void (sequenceEither (fmap (first (fmap HtmlCoreWarning) . PC.warnShadowing binds) exprs))
      exhaustiv = void (sequenceEither (fmap (first (fmap HtmlCoreWarning) . PC.warnExhaustivity decls) exprs))
  in shadowing *> exhaustiv

-- | Produce the initial module map from a set of template inputs.
-- Note that:
-- * we do one template per module right now
-- * the expression names are derived from the filepath
-- * the module name is also derived from the filepath
-- * we expect all user datatypes to be exported from a single module
smush ::
     [DataModuleName]
  -> ModuleNamer
  -> HtmlModules
  -> RawTemplates
  -> Either
       [HtmlError]
       (ModuleGraph, Map HB.ModuleName (HB.Module () PrimT SrcAnnotation))
smush mdm mnr hms (RawTemplates templates) = do
  let
    known = fmap (M.keysSet . HB.moduleExprs) hms
  mmap <- fmap (deriveImportsIncremental known . M.fromList) . sequenceEither . with templates $ \(fp, body) -> do
    ast <- first (:[]) (parseTemplate fp body)
    let core = Elab.elaborate ast
        modn = pathToModuleName mnr fp
        expn = filePathToExprName mnr fp
    pure (modn, HB.Module {
        HB.moduleTypes = mempty
      , HB.moduleImports = M.fromList $
          (HB.htmlRuntime, HB.OpenImport) : fmap (\(DataModuleName dm) -> (dm, HB.OpenImport)) mdm
      , HB.moduleExprs = M.singleton expn ((), core)
      })
  pure (buildModuleGraph mmap, mmap)

-- | Provide a default naming scheme for modules and function names
moduleNamerSimple :: Maybe HB.ModuleName -> ModuleNamer
moduleNamerSimple prefix =
  ModuleNamer (filePathToModuleNameSimple prefix) filePathToExprNameSimple

-- | Derive a module name from the relative 'FilePath'.
--
-- @
-- λ> filePathToModuleNameSimple Nothing "./path_to/my/favourite_Template_place.hs"
-- ModuleName {unModuleName = "PathTo.My.FavouriteTemplatePlace"}
-- @
filePathToModuleNameSimple :: Maybe HB.ModuleName -> FilePath -> HB.ModuleName
filePathToModuleNameSimple prefix fp =
  let
    n = HB.ModuleName . T.pack . goUpper . FilePath.dropExtension $ fp
  in
    maybe n (flip HB.moduleNameAppend n) prefix
  where
    goUpper [] = []
    goUpper (x:xs)
      | Char.isAlphaNum x = Char.toUpper x : go xs
      | otherwise = goUpper xs
    go [] = []
    go (x:xs)
      | x == '/' = '.' : goUpper xs
      | Char.isAlphaNum x = x : go xs
      | otherwise = goUpper xs

-- | Derive an expression name from the relative 'FilePath'.
--
-- @
-- λ> 'filePathToExprName' "path/to/foo_bar_baz_bapGilPoilk.hsbc"
-- Name {unName = "fooBarBazBapGilPoilk"}
-- @
filePathToExprNameSimple :: FilePath -> PC.Name
filePathToExprNameSimple =
  PC.Name . T.pack . goLower . FilePath.dropExtension
  where
    go [] = []
    go (x:xs)
      | Char.isAlphaNum x = x : go xs
      | otherwise = goUpper xs
    goUpper [] = []
    goUpper (x:xs)
      | Char.isAlphaNum x = Char.toUpper x : go xs
      | otherwise = goUpper xs
    goLower [] = []
    goLower (x:xs)
      | Char.isAlphaNum x = Char.toLower x : go xs
      | otherwise = goLower xs
