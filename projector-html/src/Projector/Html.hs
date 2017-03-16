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
  , TemplateNameMap
  , ModuleNamer (..)
  , CodeGenNamer (..)
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
  , codeGenNamerSimple
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
import qualified Projector.Html.Core.Library as Library
import qualified Projector.Html.Core.Prim as Prim
import           Projector.Html.Data.Annotation
import qualified Projector.Html.Data.Module as HB
import           Projector.Html.Data.Position  (Range, renderRange)
import           Projector.Html.Data.Prim
import           Projector.Html.Data.Template  (Template)
import           Projector.Html.ModuleGraph
import           Projector.Html.Syntax (SyntaxError (..), renderSyntaxError)
import qualified Projector.Html.Syntax as Syntax

import qualified System.FilePath.Posix as FilePath
import           System.IO  (FilePath)

import           X.Control.Monad.Trans.Either (sequenceEither)


-- -----------------------------------------------------------------------------
-- Top-level errors

data HtmlError
  = HtmlSyntaxError SyntaxError
  | HtmlCoreError (CoreError SrcAnnotation)
  | HtmlCoreWarning (HtmlWarning SrcAnnotation)
  | HtmlModuleGraphError GraphError
  deriving (Eq, Show)

renderHtmlError :: HtmlError -> Text
renderHtmlError he =
  case he of
    HtmlSyntaxError e ->
      renderSyntaxError e
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
  first HtmlSyntaxError . Syntax.templateFromText f

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
  . fmap (fmap (PC.whnf toSubstitute))
  . (>>= (maybe (Left (HC.HtmlTypeError [])) pure . M.lookup (PC.Name "it")))
  . HC.typeCheckIncremental decls (conFunTypes <> libraryExprs <> (M.mapKeys PC.Name known))
  . M.singleton (PC.Name "it")
  where
    conFunTypes = HC.constructorFunctionTypes decls
    conFunExprs = HC.constructorFunctions decls
    toSubstitute = fmap snd (Library.exprs <> Prim.exprs <> conFunExprs)

checkModule ::
     HtmlDecls
  -> HB.Module a PrimT SrcAnnotation
  -> Either HtmlError (HB.Module HtmlType PrimT (HtmlType, SrcAnnotation))
checkModule decls (HB.Module typs imps exps) = do
  exps' <- first HtmlCoreError (HC.typeCheckIncremental (decls <> typs) (conFunTypes <> libraryExprs) (fmap snd exps))
  let exps'' = with exps' . uncurry $ \ty expr -> (ty, PC.whnf toSubstitute expr)
  pure (HB.Module typs imps exps'')
  where
    conFunTypes = HC.constructorFunctionTypes decls
    conFunExprs = HC.constructorFunctions decls
    toSubstitute = fmap snd (Library.exprs <> Prim.exprs <> conFunExprs)

-- | Figure out the dependency order of a set of modules, then
-- typecheck them all in that order.
checkModules ::
     HtmlDecls
  -> Map PC.Name (HtmlType, SrcAnnotation)
  -> Map HB.ModuleName (HB.Module a PrimT SrcAnnotation)
  -> Either HtmlError (Map HB.ModuleName (HB.Module HtmlType PrimT (HtmlType, SrcAnnotation)))
checkModules decls known exprs =
  -- FIX Check for duplicate function names here somewhere
  first HtmlCoreError (fmap fst (foldM fun (mempty, conFunTypes <> libraryExprs <> known) deps))
  where
    deps = dependencyOrder (buildDependencyGraph (buildModuleGraph exprs))
    conFunTypes = HC.constructorFunctionTypes decls
    --
    conFunExprs = HC.constructorFunctions decls
    toSubstitute = fmap snd (Library.exprs <> Prim.exprs <> conFunExprs)
    --
    fun (res, acc) n =
      maybe (pure (res, acc)) (\mo -> doit n mo res acc) (M.lookup n exprs)
    --
    doit n (HB.Module types imports exps) res acc = do
      exps' <- HC.typeCheckIncremental (decls <> types) acc (fmap snd exps)
      let exps'' = with exps' . uncurry $ \ty expr ->
            (ty, PC.whnf toSubstitute expr)
          result = HB.Module types imports exps''
          newacc = M.union (fmap (PC.extractAnnotation . snd) exps') acc
      pure (M.insert n result res, newacc)

-- | Generate code for a provided backend.
--
-- TODO This should be a lazy stream instead of Either. Either limits throughput.
codeGen :: HB.Backend SrcAnnotation e -> CodeGenNamer -> BuildArtefacts -> Either [e] [(FilePath, Text)]
codeGen backend cgn (BuildArtefacts nmap checked) = do
  -- Run the backend's validation predicates
  validateModules backend checked
  -- Apply the CodeGenNamer
  let renamed = with checked (codeGenRename nmap cgn)
  -- Generate code.
  fmap M.elems $ first pure (M.traverseWithKey (codeGenModule backend) renamed)

codeGenModule ::
     HB.Backend a e
  -> HB.ModuleName
  -> HB.Module HtmlType PrimT (HtmlType, a)
  -> Either e (FilePath, Text)
codeGenModule =
  HB.renderModule

-- | Apply a 'CodeGenNamer' to some module.
codeGenRename :: TemplateNameMap -> CodeGenNamer -> HB.Module a b c -> HB.Module a b c
codeGenRename (TemplateNameMap nmap) cgn (HB.Module ts is es) =
  let renameDef n = maybe n (uncurry (templateNameToBackendName cgn n)) (M.lookup n nmap)
      renameVal n = maybe n (uncurry (templateDefToBackendDef cgn n)) (M.lookup n nmap)
  in HB.Module ts is $ with (M.mapKeys renameDef es) (fmap (PC.mapFree renameVal))

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
    buildArtefactsNameMap :: TemplateNameMap
  , buildArtefactsHtmlModules :: HtmlModules
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

-- | All the generated names for templates.
newtype TemplateNameMap = TemplateNameMap {
    unTemplateNameMap :: Map PC.Name (HB.ModuleName, FilePath)
  } deriving (Eq, Ord, Show, Monoid)

addToTemplateNameMap :: PC.Name -> HB.ModuleName -> FilePath -> TemplateNameMap -> TemplateNameMap
addToTemplateNameMap n mn fp =
  TemplateNameMap . M.insert n (mn, fp) . unTemplateNameMap

-- | Turns generated template names (used in syntax) into the backend's preferred names.
data CodeGenNamer = CodeGenNamer {
    templateDefToBackendDef :: PC.Name -> HB.ModuleName -> FilePath -> PC.Name
  , templateNameToBackendName :: PC.Name -> HB.ModuleName -> FilePath -> PC.Name
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
  (mg, nmap, mmap) <- smush mdm mnr hms rts
  -- Check it for import cycles
  (_ :: ()) <- first (pure . HtmlModuleGraphError) (detectCycles mg)
  -- Check all modules (this could be a lazy stream)
  -- TODO the Map forces all of this at once, remove
  BuildArtefacts nmap <$> first pure (checkModules decls (HB.extractModuleBindings hms) mmap)

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
-- * we currently expect all user datatypes to be exported from a single module
smush ::
     [DataModuleName]
  -> ModuleNamer
  -> HtmlModules
  -> RawTemplates
  -> Either
       [HtmlError]
       (ModuleGraph, TemplateNameMap, Map HB.ModuleName (HB.Module () PrimT SrcAnnotation))
smush mdm mnr hms (RawTemplates templates) = do
  let
    known = fmap (M.keysSet . HB.moduleExprs) hms
    mkmod (nmap, acc) (fp, body) = do
      ast <- first (:[]) (parseTemplate fp body)
      let core = Elab.elaborate ast
          modn = pathToModuleName mnr fp
          expn = filePathToExprName mnr fp
          res = (modn,  HB.Module {
              HB.moduleTypes = mempty
            , HB.moduleImports = M.fromList $
                fmap (\(DataModuleName dm) -> (dm, HB.OpenImport)) mdm
            , HB.moduleExprs = M.singleton expn ((), core)
            })
      pure (addToTemplateNameMap expn modn fp nmap, res:acc)
  -- Produce a module for each template and build up the template name map
  (nmap, modls) <- foldM mkmod mempty templates
  -- Derive the module map and its import graph
  let mmap = deriveImportsIncremental known (M.fromList modls)
  pure (buildModuleGraph mmap, nmap, mmap)

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

codeGenNamerSimple :: CodeGenNamer
codeGenNamerSimple =
  CodeGenNamer
    (\n _ _ -> n)
    (\n _ _ -> n)
