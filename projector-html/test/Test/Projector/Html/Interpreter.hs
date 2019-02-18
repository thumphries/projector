{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Projector.Html.Interpreter where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Hedgehog

import           Projector.Core.Prelude

import           Projector.Core
import           Projector.Html
import           Projector.Html.Data.Annotation
import           Projector.Html.Data.Prim
import           Projector.Html.Interpreter
import           Projector.Html.Syntax.QQ  (template)

import           System.IO (FilePath, IO)

import           Test.Projector.Html.Expect

import           Text.Show.Pretty (ppShow)


-- FIX We need a generator to round-trip property test this
prop_interpret_unit :: Property
prop_interpret_unit =
  once $ do
     (at, a) <- evalEither . checkTemplate mempty $
       [template|\t : String -> Html = <div id="a" class="{{ t }}">{{ t }}</div>|]
     let
       ma = M.fromList [("a", (at, LibraryFunction (Name "a")))]
       na = M.fromList [(Name "a", a)]
     (_, b) <- evalEither . checkTemplateIncremental mempty ma $
       [template|<a>{ a "b" }</a><!-- c --><hr id="d" />|]
     h <- evalEither . interpret mempty na $ b
     h ===
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

prop_interpreter_unit_foobar :: Property
prop_interpreter_unit_foobar =
  regressionFile "foobar"

prop_interpreter_unit_pre :: Property
prop_interpreter_unit_pre =
  regressionFile "pre"

prop_interpreter_unit_pre_multiline :: Property
prop_interpreter_unit_pre_multiline =
  regressionFile "pre_multiline"

prop_interpreter_unit_whitespace :: Property
prop_interpreter_unit_whitespace =
  regressionFile "whitespace"

prop_interpreter_unit_foo :: Property
prop_interpreter_unit_foo =
  regressionFile "foo"

prop_interpreter_unit_foo_each :: Property
prop_interpreter_unit_foo_each =
  regressionFile "foo_each"

prop_interpreter_unit_fizz :: Property
prop_interpreter_unit_fizz =
  regressionFile "fizz"

prop_interpreter_unit_bool_con :: Property
prop_interpreter_unit_bool_con =
  regressionFile "bool_con"

prop_interpreter_unit_bool_con_false :: Property
prop_interpreter_unit_bool_con_false =
  regressionFile "bool_con_false"

prop_interpreter_unit_attribute_concat :: Property
prop_interpreter_unit_attribute_concat =
  regressionFile "attribute_concat"

prop_interpreter_unit_attribute_concat_empty :: Property
prop_interpreter_unit_attribute_concat_empty =
  regressionFile "attribute_concat_empty"

prop_interpreter_unit_attribute_expr_empty :: Property
prop_interpreter_unit_attribute_expr_empty =
  regressionFile "attribute_expr_empty"

prop_interpreter_unit_attribute_expr_singleton :: Property
prop_interpreter_unit_attribute_expr_singleton =
  regressionFile "attribute_expr_singleton"

prop_interpreter_unit_is_empty_true :: Property
prop_interpreter_unit_is_empty_true =
  regressionFile "is_empty_true"

prop_interpreter_unit_is_empty_false :: Property
prop_interpreter_unit_is_empty_false =
  regressionFile "is_empty_false"

prop_interpreter_unit_append :: Property
prop_interpreter_unit_append =
  regressionFile "append"

-- -----------------------------------------------------------------------------

decls :: HtmlDecls
decls =
    declareType (TypeName "Fizz") (DRecord [] [(FieldName "name", TLit TString)])
  $ mempty

bnds :: Map Name (HtmlExpr (HtmlType, SrcAnnotation))
bnds =
  M.fromList [
      (Name "foo", ELit (TLit TString, LibraryFunction (Name "foo")) (VString "foo"))
    ]

bndst :: Map Text (HtmlType, SrcAnnotation)
bndst =
  fmap (extractAnnotation) (M.mapKeys unName bnds)

interpretText :: FilePath -> Text -> Either Text Html
interpretText fp t = do
  ast <- first renderHtmlError (parseTemplate fp t)
  (_ty, expr) <- first renderHtmlError (checkTemplateIncremental decls bndst ast)
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


tests :: IO Bool
tests =
  checkParallel $$(discover)
