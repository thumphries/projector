{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Projector.Html.Backend.Purescript where


import qualified Data.Map.Strict as M

import           Disorder.Core
import           Disorder.Jack

import           P

import           Projector.Core.Eval (whnf)
import           Projector.Html.Backend.Purescript (purescriptBackend)
import           Projector.Html.Data.Annotation
import           Projector.Html.Data.Backend
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim
import           Projector.Html.Core
import qualified Projector.Html.Core.Library as Lib
import qualified Projector.Html.Core.Prim as Prim

import           System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

import           Test.IO.Projector.Html.Backend.Property (fileProp, helloWorld, processProp)
import           Test.Projector.Html.Arbitrary


-- -----------------------------------------------------------------------------

prop_empty_module =
  once (moduleProp (ModuleName "Test.Purescript.Module") mempty)

prop_library_module =
  once . moduleProp (ModuleName "Test.Purescript.Library") $ Module {
      moduleTypes = Lib.types <> Prim.types
    , moduleImports = M.fromList [(htmlRuntime, OpenImport)]
    , moduleExprs = Lib.exprs <> M.fromList [
          helloWorld
        ]
    }

prop_welltyped :: Property
prop_welltyped =
  gamble genHtmlTypeDecls $ \decls ->
    gamble (chooseInt (0,  100)) $ \k ->
      gamble (genWellTypedHtmlModule k decls) $ \modl ->
        moduleProp (ModuleName "Test.Purescript.Arbitrary.WellTyped") $ modl {
            -- TODO once the backend actually does something, remove this setter
            moduleTypes = decls
          , moduleExprs = cleanExprs decls (libExprs <> moduleExprs modl)
          }

-- Inline all the constructor functions
-- FIX temporary hack - this should happen somewhere else
cleanExprs decls exprs =
  let confuns = constructorFunctionExprs decls
  in fmap (fmap (whnf confuns)) exprs

libExprs =
  fmap (fmap (fmap (fmap (const EmptyAnnotation)))) Lib.exprs

-- -----------------------------------------------------------------------------

moduleProp :: ModuleName -> Module HtmlType PrimT (HtmlType, a) -> Property
moduleProp mn =
  uncurry pscProp . either (fail . show) id . renderModule purescriptBackend mn

pscProp mname modl =
  fileProp mname modl
    (\path ->
      let crpr = (proc "npm" ["run", "-s", "build", "--", path]) { cwd = Just "test/purescript" }
      in readCreateProcessWithExitCode crpr [])
    (processProp (const (property True)))


return []
tests = $disorderCheckEnvAll TestRunFewer
