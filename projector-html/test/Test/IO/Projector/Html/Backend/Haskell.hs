{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Projector.Html.Backend.Haskell where


import qualified Data.Map.Strict as M

import           Disorder.Core
import           Disorder.Jack

import           P

import           Projector.Core
import           Projector.Html.Backend (checkModule, haskellBackend)
import           Projector.Html.Core
import           Projector.Html.Data.Backend
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim

import           System.Process (readProcessWithExitCode)

import           Test.IO.Projector.Html.Backend.Property  (processProp, fileProp, helloWorld)
import           Test.Projector.Html.Arbitrary


prop_empty_module =
  once (moduleProp (ModuleName "Test.Haskell.Module") mempty)

prop_library_runtime =
  once . moduleProp (ModuleName "Test.Haskell.Runtime") $ Module {
      moduleTypes = mempty
    , moduleImports = M.fromList [
          (htmlRuntime, OpenImport)
        ]
    , moduleExprs = M.fromList [
          helloWorld
        ]
    }

prop_hello_world =
  once $ runProp name (text <> "\nmain = putStr (toText helloWorld)\n")
    (=== "Hello, world!<div class=\"table\"> </div>")
  where
    (name, text) =
      renderModule haskellBackend (ModuleName "Main") $ Module {
          moduleTypes = mempty
        , moduleImports = M.fromList [
              (htmlRuntime, OpenImport)
            , (ModuleName "Data.Text.IO", OnlyImport [Name "putStr"])
            , (ModuleName "Hydrant", OnlyImport [Name "toText"])
            ]
        , moduleExprs = M.fromList [
              helloWorld
            ]
        }

prop_welltyped :: Property
prop_welltyped =
  gamble (genHtmlTypeDecls) $ \decls ->
    gamble (chooseInt (0, 100)) $ \k ->
      gamble (genWellTypedHtmlModule k decls `suchThat` (isRight . checkModule haskellBackend)) $ \modl ->
        moduleProp (ModuleName "Test.Haskell.Arbitrary.WellTyped") modl

-- -----------------------------------------------------------------------------

moduleProp :: ModuleName -> Module HtmlType PrimT a -> Property
moduleProp mn =
  uncurry ghcProp . renderModule haskellBackend mn

-- Compiles with GHC in the current sandbox, failing if exit status is nonzero.
ghcProp mname modl =
  fileProp mname modl
    (\path -> readProcessWithExitCode "cabal" ["exec", "--", "ghc", path] "")
    (processProp (const (property True)))

runProp mname modl cb =
  fileProp mname modl
    (\path -> readProcessWithExitCode "cabal" ["exec", "--", "runhaskell", path] "")
    (processProp cb)


return []
tests = $disorderCheckEnvAll TestRunFewer
