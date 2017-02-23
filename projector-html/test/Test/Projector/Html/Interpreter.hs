{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Interpreter where

import qualified Data.Map.Strict as M

import           Disorder.Core

import           P

import           Projector.Core
import           Projector.Html
import           Projector.Html.Core
import           Projector.Html.Data.Annotation
import           Projector.Html.Data.Prim
import           Projector.Html.Interpreter
import           Projector.Html.Parser.QQ  (template)

import           Test.Projector.Html.Arbitrary
import           Test.QuickCheck.Jack


-- FIX We need a generator to round-trip property test this
prop_interpret_unit =
  once . either (flip counterexample False) id $ do
     let
       zd = TypeDecls $ M.fromList [
           (TypeName "Foo", DVariant [(Constructor "Bar", [TLit TString])])
         ]
       za = M.fromList $ (fmap (first unName) . M.toList . constructorFunctions) zd
     (at, a) <- first show . checkTemplateIncremental zd za $
       [template|\i : String
         f : Foo -> <div id="a" class="{ i }">{ case f of Bar t -> text t }</div>|]
     let
       ma = M.fromList [("a", (at, LibraryFunction (Name "a")))]
       na = M.fromList [(Name "a", a)]
     (_, b) <- first show . checkTemplateIncremental zd (ma <> za) $
       [template|<a>{ a "m" (Bar "b") }</a><!-- c --><hr id="d" />e|]
     h <- first show . interpret na $ b
     pure $
       h
       ===
       Html [
           Element "a" [] . Html $ [
               Whitespace " "
             , Element "div" [Attribute "id" "a", Attribute "class" "m"] . Html $ [ Plain "b" ]
             ]
         , Comment " c "
         , VoidElement "hr" [Attribute "id" "d"]
         , Raw "e"
         ]


return []
tests = $disorderCheckEnvAll TestRunNormal
