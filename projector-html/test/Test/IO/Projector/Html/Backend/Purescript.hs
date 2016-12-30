{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Projector.Html.Backend.Purescript where


import qualified Data.List as L
import qualified Data.Map.Strict as M

import           Disorder.Core
import           Disorder.Jack

import           P

import           Projector.Html.Backend (Backend (..), purescriptBackend)
import           Projector.Html.Backend.Data
import qualified Projector.Html.Core.Library as Lib
import qualified Projector.Html.Core.Prim as Prim

import           System.Process (readProcessWithExitCode)

import           Test.IO.Projector.Html.Backend.Property (processProp, fileProp, helloWorld)
import           Test.Projector.Core.Arbitrary (freshNames)
import           Test.Projector.Html.Arbitrary


-- -----------------------------------------------------------------------------

prop_empty_module =
  once (moduleProp (ModuleName "Test.Purescript.Module") mempty)

prop_library_module =
  once . moduleProp (ModuleName "Test.Purescript.Library") $ Module {
      moduleTypes = Lib.types <> Prim.types
    , moduleImports = mempty
    , moduleExprs = M.fromList [
          helloWorld
        ]
    }

prop_welltyped :: Property
prop_welltyped =
  gamble genHtmlTypeDecls $ \decls ->
    gamble (chooseInt (0,  100)) $ \k ->
      gamble (vectorOf k (genWellTypedHtmlExpr decls)) $ \exprs ->
        moduleProp (ModuleName "Test.Purescript.Arbitrary.WellTyped") $ Module {
            moduleTypes = decls  -- subtractTypes decls (Lib.types <> Prim.types)
          , moduleImports = mempty
          , moduleExprs = M.fromList (L.zip (freshNames "expr") exprs)
          }

-- -----------------------------------------------------------------------------

moduleProp :: ModuleName -> Module a -> Property
moduleProp mn =
  uncurry pscProp . renderModule purescriptBackend mn

pscProp mname modl =
  fileProp mname modl
    (\path -> readProcessWithExitCode "psc" [path] "")
    (processProp (const (property True)))


return []
tests = $disorderCheckEnvAll TestRunFewer
