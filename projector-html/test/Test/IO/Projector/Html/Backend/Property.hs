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


helloWorld :: (Name, (Prim.HtmlType, Prim.HtmlExpr (Prim.HtmlType, ())))
helloWorld =
  ( Name "helloWorld"
  , ( Lib.tHtml
    , fmap (,()) $ ECon Lib.tHtml (Constructor "Nested") Lib.nHtml [
        EList (TList Lib.tHtml) [
            ECon Lib.tHtml (Constructor "Plain") Lib.nHtml [ELit (TLit Prim.TString) (Prim.VString "Hello,")]
          , ECon Lib.tHtml (Constructor "Whitespace") Lib.nHtml []
          , EApp Lib.tHtml (EVar Lib.tHtmlText Lib.nHtmlText) (ELit (TLit Prim.TString) (Prim.VString "world!"))
          , ECon Lib.tHtml (Constructor "Element") Lib.nHtml [
                ECon Lib.tTag (Constructor "Tag") Lib.nTag [ELit (TLit Prim.TString) (Prim.VString "div")]
              , EList (TList Lib.tAttribute) [
                    ECon Lib.tAttribute (Constructor "Attribute") Lib.nAttribute [
                      ECon Lib.tAttributeKey (Constructor "AttributeKey") Lib.nAttributeKey [ELit (TLit Prim.TString) (Prim.VString "class")]
                    , EApp Lib.tAttributeValue (EVar Lib.tHtmlAttrValue Lib.nHtmlAttrValue) (ELit (TLit Prim.TString) (Prim.VString "table"))
                    ]
                  ]
              , ECon Lib.tHtml (Constructor "Nested") Lib.nHtml [EList (TList Lib.tHtml) [ECon Lib.tHtml (Constructor "Whitespace") Lib.nHtml []]]
              ]
          , EVar Lib.tHtml Lib.nHtmlBlank
          ]
      ]))
