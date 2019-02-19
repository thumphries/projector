{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Projector.Html.Backend.Purescript where


import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Projector.Core.Prelude

import qualified Projector.Html as Html
import           Projector.Html.Backend (checkModule)
import           Projector.Html.Backend.Purescript
import           Projector.Html.Data.Annotation
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim
import qualified Projector.Html.Core.Library as Lib
import qualified Projector.Html.Core.Prim as Prim

import           System.IO (IO, FilePath)
import           System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

import           Test.IO.Projector.Html.Backend.Property (fileProp, helloWorld, processProp)
import           Test.Projector.Html.Gen


-- -----------------------------------------------------------------------------

prop_empty_module :: Property
prop_empty_module =
  once (moduleProp baseDecls (ModuleName "Test.Purescript.Module") mempty)

prop_library_module :: Property
prop_library_module =
  once . modulePropCheck baseDecls (ModuleName "Test.Purescript.Library") $ Module {
      moduleTypes = mempty
    , moduleImports = mempty
    , moduleExprs = M.fromList [
          helloWorld
        ]
    }

prop_welltyped :: Property
prop_welltyped =
  withShrinks 0 . property  $ do
    decls <- forAll genHtmlTypeDecls
    k <- forAll $ Gen.int (Range.linear 0 100)
    modl <- forAll $ Gen.filter (isRight . checkModule purescriptBackend) $ genWellTypedHtmlModule k decls
    moduleProp decls (ModuleName "Test.Purescript.Arbitrary.WellTyped") $ modl {
        moduleExprs = moduleExprs modl
      }

-- -----------------------------------------------------------------------------

baseDecls :: HtmlDecls
baseDecls =
  Lib.types <> Prim.types

moduleProp :: HtmlDecls -> ModuleName -> Module HtmlType PrimT (HtmlType, a) -> PropertyT IO ()
moduleProp decls mn =
  uncurry pscProp . either (fail . show) id . fmap pluck . Html.codeGenModule purescriptBackend decls mn

modulePropCheck :: HtmlDecls -> ModuleName -> Module (Maybe HtmlType) PrimT SrcAnnotation -> PropertyT IO ()
modulePropCheck decls mn modl@(Module tys _ _) =
  uncurry pscProp . either (fail . T.unpack) id . fmap pluck $ do
    modl' <- first Html.renderHtmlError (Html.checkModule tys mempty modl)
    first renderPurescriptError (Html.codeGenModule purescriptBackend decls mn modl')

pscProp :: FilePath -> Text -> PropertyT IO ()
pscProp mname modl =
  fileProp mname modl
    (\path ->
      let crpr = (proc "npm" ["run", "-s", "build", "--", path]) { cwd = Just "test/purescript" }
      in readCreateProcessWithExitCode crpr [])
    (processProp (const success))

pluck :: (a, b, c) -> (b, c)
pluck (_, b, c) =
  (b, c)

once :: PropertyT IO () -> Property
once =
  withTests 1 . property

tests :: IO Bool
tests =
  checkParallel $$(discover)
