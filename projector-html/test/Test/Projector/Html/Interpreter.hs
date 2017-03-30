{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Interpreter where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Disorder.Core

import           P

import           Projector.Core
import           Projector.Html
import           Projector.Html.Data.Annotation
import           Projector.Html.Data.Prim
import           Projector.Html.Interpreter
import           Projector.Html.Syntax
import           Projector.Html.Syntax.QQ  (template)

import           System.IO (FilePath, IO)

import           Test.Projector.Html.Expect
import           Test.QuickCheck.Jack

import           Text.Show.Pretty (ppShow)


-- FIX We need a generator to round-trip property test this
prop_interpret_unit =
  once . either (flip counterexample False) id $ do
     (at, a) <- first show . checkTemplate mempty $
       [template|\t : String -> Html = <div id="a" class="{ t }">{ text t }</div>|]
     let
       ma = M.fromList [("a", (at, LibraryFunction (Name "a")))]
       na = M.fromList [(Name "a", a)]
     (_, b) <- first show . checkTemplateIncremental mempty ma $
       [template|<a>{ a "b" }</a><!-- c --><hr id="d" />|]
     h <- first show . interpret mempty na $ b
     pure $
       h
       ===
       Nested
         [ Element
             "a"
             []
             (Element "div"
               [ Attribute "id" "a"
               , Attribute "class" "b"
               ]
               (Plain "b"))
         , Comment " c "
         , VoidElement "hr" [ Attribute "id" "d" ]
         ]

prop_interpreter_unit_foobar =
  regressionFile "foobar"

prop_interpreter_unit_pre =
  regressionFile "pre"


-- -----------------------------------------------------------------------------

decls :: HtmlDecls
decls =
  mempty

bnds :: Map Name (HtmlExpr (HtmlType, SrcAnnotation))
bnds =
  mempty

bndst :: Map Text (HtmlType, SrcAnnotation)
bndst =
  mempty

interpretText :: FilePath -> Text -> Either Text Html
interpretText fp t = do
  t <- first renderHtmlError (parseTemplate fp t)
  (_ty, expr) <- first renderHtmlError (checkTemplateIncremental decls bndst t)
  first (T.pack . ppShow) (interpret decls bnds expr)

regressionFile :: FilePath -> Property
regressionFile fp =
  expectFile "test/interpreter" fp id (interpretText fp)

mkRegression :: FilePath -> Text -> IO ()
mkRegression fp t =
  ecase (interpretText fp t) (fail . T.unpack) (mkExpect "test/interpreter" fp t)

updateRegression :: FilePath -> IO ()
updateRegression fp =
  updateExpect "test/interpreter" fp (interpretText fp)


return []
tests = $disorderCheckEnvAll TestRunNormal
