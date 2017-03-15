{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Test.IO.Projector.Html.Backend.Property where


import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Disorder.Core.IO
import           Disorder.Jack

import           P

import           Projector.Core
import           Projector.Html.Data.Annotation
import qualified Projector.Html.Data.Prim as Prim
import qualified Projector.Html.Core.Library as Lib

import           System.Directory (createDirectoryIfMissing, makeAbsolute)
import           System.Exit (ExitCode(..))
import           System.FilePath.Posix ((</>), takeDirectory)
import           System.IO (FilePath, IO)
import           System.IO.Temp (withTempDirectory)


processProp :: ([Char] -> Property) -> (ExitCode, [Char], [Char]) -> Property
processProp f (code, out, err) =
  case code of
    ExitSuccess ->
      f out
    ExitFailure i ->
      let errm = L.unlines [
              "Process exited with failing status: " <> T.unpack (renderIntegral i)
            , err
            ]
      in counterexample errm (property False)


fileProp :: FilePath -> Text -> (FilePath -> IO a) -> (a -> Property) -> Property
fileProp mname modl f g =
  testIO . withTempDirectory "./dist/" "gen-XXXXXX" $ \tmpDir -> do
    let path = tmpDir </> mname
        dir = takeDirectory path
    createDirectoryIfMissing True dir
    T.writeFile path modl
    path' <- makeAbsolute path
    fmap g (f path')


helloWorld :: (Name, (Prim.HtmlType, Prim.HtmlExpr (Annotation a)))
helloWorld =
  ( Name "helloWorld"
  , ( Lib.tHtml
    , fmap (const EmptyAnnotation) $ ECon () (Constructor "Nested") Lib.nHtml [
        EList () [
            ECon () (Constructor "Plain") Lib.nHtml [ELit () (Prim.VString "Hello,")]
          , ECon () (Constructor "Whitespace") Lib.nHtml []
          , EApp () (EVar () Lib.nHtmlText) (ELit () (Prim.VString "world!"))
          , ECon () (Constructor "Element") Lib.nHtml [
                ECon () (Constructor "Tag") Lib.nTag [ELit () (Prim.VString "div")]
              , EList () [
                    ECon () (Constructor "Attribute") Lib.nAttribute [
                      ECon () (Constructor "AttributeKey") Lib.nAttributeKey [ELit () (Prim.VString "class")]
                    , EApp () (EVar () Lib.nHtmlAttrValue) (ELit () (Prim.VString "table"))
                    ]
                  ]
              , ECon () (Constructor "Nested") Lib.nHtml [EList () [ECon () (Constructor "Whitespace") Lib.nHtml []]]
              ]
          , EVar () Lib.nHtmlBlank
          ]
      ]))
