{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Projector.Html.Backend.Haskell where


import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Disorder.Core
import           Disorder.Core.IO
import           Disorder.Jack

import           P

import           Projector.Core
import qualified Projector.Html.Core.Prim as Prim
import qualified Projector.Html.Core.Library as Lib
import           Projector.Html.Backend.Data
import           Projector.Html.Backend.Haskell

import           System.Directory (createDirectoryIfMissing)
import           System.Exit (ExitCode(..))
import           System.FilePath.Posix ((</>), (<.>), takeDirectory)
import           System.IO (FilePath)
import           System.IO.Temp (withTempDirectory)
import           System.Process (readProcessWithExitCode)


prop_empty_module =
  once (moduleProp (ModuleName "Test.Haskell.Module") mempty)

prop_library_module =
  once . moduleProp (ModuleName "Test.Haskell.Library") $ Module {
      moduleTypes = Lib.types
    , moduleImports = M.fromList [
          (htmlRuntimePrim, OpenImport)
        ]
    , moduleExprs = M.fromList [
          (Name "helloWorld", (Lib.tHtml,
            ECon (Constructor "Plain") Lib.nHtml [ELit (Prim.VString "Hello, world!")]))
        ]
    }

prop_library_runtime =
  once . moduleProp (ModuleName "Test.Haskell.Runtime") $ Module {
      moduleTypes = mempty
    , moduleImports = M.fromList [
          (htmlRuntime, OpenImport)
        ]
    , moduleExprs = M.fromList [
          (Name "helloWorld", (Lib.tHtml,
            ECon (Constructor "Plain") Lib.nHtml [ELit (Prim.VString "Hello, world!")]))
        ]
    }


moduleProp :: ModuleName -> Module -> Property
moduleProp mn =
  uncurry ghcProp . renderModule mn

-- Compiles with GHC in the current sandbox, failing if exit status is nonzero.
ghcProp :: FilePath -> Text -> Property
ghcProp mname modl =
  testIO . withTempDirectory "./dist/" "gen-hs-XXXXXX" $ \tmpDir -> do
    let path = tmpDir </> mname <.> "hs"
        dir = takeDirectory path
    createDirectoryIfMissing True dir
    T.writeFile path modl
    (code, _out, err) <- readProcessWithExitCode "cabal" ["exec", "--", "ghc", path] ""
    case code of
      ExitSuccess ->
        pure (property True)
      ExitFailure i ->
        let errm = L.unlines [
                "GHC exited with failing status: " <> T.unpack (renderIntegral i)
              , err
              ]
        in pure $ counterexample errm (property False)


return []
tests = $disorderCheckEnvAll TestRunNormal
