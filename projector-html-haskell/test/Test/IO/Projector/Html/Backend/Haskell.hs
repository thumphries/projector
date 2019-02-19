{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Projector.Html.Backend.Haskell where


import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Projector.Core.Prelude

import           Projector.Core
import qualified Projector.Html as Html
import           Projector.Html.Backend (checkModule)
import           Projector.Html.Backend.Haskell
import           Projector.Html.Core
import qualified Projector.Html.Core.Library as Lib
import qualified Projector.Html.Core.Prim as Prim
import           Projector.Html.Data.Annotation
import           Projector.Html.Data.Backend ()
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim

import           System.IO (IO, FilePath)
import           System.Process (readProcessWithExitCode)

import           Test.IO.Projector.Html.Backend.Property  (processProp, fileProp, helloWorld)
import           Test.Projector.Html.Gen


prop_empty_module :: Property
prop_empty_module =
  once (moduleProp baseDecls (ModuleName "Test.Haskell.Module") mempty)

prop_library_runtime :: Property
prop_library_runtime =
  once . modulePropCheck baseDecls (ModuleName "Test.Haskell.Runtime") $ Module {
      moduleTypes = mempty
    , moduleImports = mempty
    , moduleExprs = M.fromList [
          helloWorld
        ]
    }

prop_hello_world :: Property
prop_hello_world =
  once $ runProp name (text <> "\nmain = putStr (Projector.Hydrant.toText helloWorld)\n")
    (=== "Hello, world!<div class=\"table\"></div>")
  where
    (name, text) =
      either (fail . T.unpack) id $ do
        let modl = Module {
                moduleTypes = mempty
              , moduleImports = M.fromList [
                    (ModuleName "Data.Text.IO", OnlyImport [Name "putStr"])
                  , (ModuleName "Projector.Hydrant", ImportQualified)
                  ]
              , moduleExprs = M.fromList [
                    helloWorld
                  ]
              }
        modl' <- first Html.renderHtmlError (Html.checkModule mempty mempty modl)
        first renderHaskellError (Html.codeGenModule haskellBackend baseDecls (ModuleName "Main") modl')

prop_welltyped :: Property
prop_welltyped =
  withShrinks 0 . property  $ do
    decls <- forAll genHtmlTypeDecls
    k <- forAll $ Gen.int (Range.linear 0 100)
    modl <- forAll $ Gen.filter (isRight . checkModule haskellBackend) $ genWellTypedHtmlModule k decls
    moduleProp decls (ModuleName "Test.Haskell.Arbitrary.WellTyped") modl

-- -----------------------------------------------------------------------------

baseDecls :: HtmlDecls
baseDecls =
  Lib.types <> Prim.types

modulePropCheck :: HtmlDecls -> ModuleName -> Module (Maybe HtmlType) PrimT SrcAnnotation -> PropertyT IO ()
modulePropCheck decls mn modl@(Module tys _ _) =
  uncurry ghcProp . either (fail . T.unpack) id $ do
    modl' <- first Html.renderHtmlError (Html.checkModule tys mempty modl)
    first renderHaskellError (Html.codeGenModule haskellBackend decls mn modl')

moduleProp :: HtmlDecls -> ModuleName -> Module HtmlType PrimT (HtmlType, SrcAnnotation) -> PropertyT IO ()
moduleProp decls mn =
  uncurry ghcProp .
  either (fail . T.unpack) id .
  first renderHaskellError . Html.codeGenModule haskellBackend decls mn

-- Compiles with GHC in the current sandbox, failing if exit status is nonzero.
ghcProp :: FilePath -> Text -> PropertyT IO ()
ghcProp mname modl =
  fileProp mname modl
    (\path -> readProcessWithExitCode "cabal" ["exec", "--", "ghc", path] "")
    (processProp (const success))

runProp :: FilePath -> Text -> ([Char] -> PropertyT IO ()) -> PropertyT IO ()
runProp mname modl cb =
  fileProp mname modl
    (\path -> readProcessWithExitCode "cabal" ["exec", "--", "runhaskell", path] "")
    (processProp cb)

once :: PropertyT IO () -> Property
once =
  withTests 1 . property

tests :: IO Bool
tests =
  checkParallel $$(discover)
