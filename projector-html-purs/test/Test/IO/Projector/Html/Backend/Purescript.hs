{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Projector.Html.Backend.Purescript where


import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Disorder.Core
import           Disorder.Jack

import           P

import qualified Projector.Html as Html
import           Projector.Html.Backend.Purescript
import           Projector.Html.Data.Annotation
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim
import qualified Projector.Html.Core.Library as Lib
import qualified Projector.Html.Core.Prim as Prim

import           System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

import           Test.IO.Projector.Html.Backend.Property (fileProp, helloWorld, processProp)
import           Test.Projector.Html.Arbitrary


-- -----------------------------------------------------------------------------

prop_empty_module =
  once (moduleProp baseDecls (ModuleName "Test.Purescript.Module") mempty)

prop_library_module =
  once . modulePropCheck baseDecls (ModuleName "Test.Purescript.Library") $ Module {
      moduleTypes = baseDecls
    , moduleImports = mempty
    , moduleExprs = M.fromList [
          helloWorld
        ]
    }

prop_welltyped :: Property
prop_welltyped =
  gamble genHtmlTypeDecls $ \decls ->
    gamble (chooseInt (0,  100)) $ \k ->
      gamble (genWellTypedHtmlModule k decls) $ \modl ->
        moduleProp decls (ModuleName "Test.Purescript.Arbitrary.WellTyped") $ modl {
            -- TODO once the backend actually does something, remove this setter
            moduleTypes = decls
          , moduleExprs = moduleExprs modl
          }

-- -----------------------------------------------------------------------------

baseDecls :: HtmlDecls
baseDecls =
  Lib.types <> Prim.types

moduleProp :: HtmlDecls -> ModuleName -> Module HtmlType PrimT (HtmlType, a) -> Property
moduleProp decls mn =
  uncurry pscProp . either (fail . show) id . Html.codeGenModule purescriptBackend decls mn

modulePropCheck :: HtmlDecls -> ModuleName -> Module (Maybe HtmlType) PrimT SrcAnnotation -> Property
modulePropCheck decls mn modl@(Module tys _ _) =
  uncurry pscProp . either (fail . T.unpack) id $ do
    modl' <- first Html.renderHtmlError (Html.checkModule tys mempty modl)
    first renderPurescriptError (Html.codeGenModule purescriptBackend decls mn modl')

pscProp mname modl =
  fileProp mname modl
    (\path ->
      let crpr = (proc "npm" ["run", "-s", "build", "--", path]) { cwd = Just "test/purescript" }
      in readCreateProcessWithExitCode crpr [])
    (processProp (const (property True)))


return []
tests = $disorderCheckEnvAll TestRunFewer
