{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Projector.Html.Backend.Haskell where


import qualified Data.List as L
import qualified Data.Map.Strict as M

import           Disorder.Core
import           Disorder.Jack

import           P

import           Projector.Core
import qualified Projector.Html.Core.Prim as Prim
import qualified Projector.Html.Core.Library as Lib
import           Projector.Html.Backend.Data
import           Projector.Html.Backend.Haskell

import           System.Process (readProcessWithExitCode)

import           Test.IO.Projector.Html.Backend.Property  (processProp, fileProp, helloWorld)
import           Test.Projector.Core.Arbitrary (freshNames)
import           Test.Projector.Html.Arbitrary


prop_empty_module =
  once (moduleProp (ModuleName "Test.Haskell.Module") mempty)

prop_library_module =
  once . moduleProp (ModuleName "Test.Haskell.Library") $ Module {
      moduleTypes = Lib.types
    , moduleImports = M.fromList [
          (htmlRuntimePrim, OpenImport)
        ]
    , moduleExprs = M.fromList [
          helloWorld
        ]
    }

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
  once $ runProp name (text <> "\nmain = putStr (renderHtml helloWorld)\n")
    (=== "Hello, world!")
  where
    (name, text) =
      renderModule (ModuleName "Main") $ Module {
          moduleTypes = mempty
        , moduleImports = M.fromList [
              (htmlRuntime, OpenImport)
            , (ModuleName "Data.Text.IO", OnlyImport [Name "putStr"])
            ]
        , moduleExprs = M.fromList [
              helloWorld
            ]
        }

prop_welltyped :: Property
prop_welltyped =
  gamble genHtmlTypeDecls $ \decls ->
    gamble (chooseInt (0, 100)) $ \k ->
      gamble (vectorOf k (genWellTypedHtmlExpr decls)) $ \exprs ->
        moduleProp (ModuleName "Test.Haskell.Arbitrary.WellTyped") $ Module {
            moduleTypes = subtractTypes decls (Lib.types <> Prim.types)
          , moduleImports = M.fromList [
                (htmlRuntime, OpenImport)
              ]
          , moduleExprs = M.fromList (L.zip (freshNames "expr") exprs)
          }

-- -----------------------------------------------------------------------------

moduleProp :: ModuleName -> Module a -> Property
moduleProp mn =
  uncurry ghcProp . renderModule mn

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
