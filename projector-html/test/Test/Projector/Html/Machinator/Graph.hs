{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Projector.Html.Machinator.Graph where


import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           Disorder.Core
import           Disorder.Jack

import           Projector.Html.Machinator.Data.Definition
import           Projector.Html.Machinator.Graph

import           Projector.Core.Prelude

import           System.IO (IO)


-- FIX these are some shameful unit tests
--     there's equivalent properties and generators in projector-html

prop_filegraph_unit :: Property
prop_filegraph_unit =
  once (buildFileGraph testDefs === testGraph)

testDefs :: [DefinitionFile]
testDefs = [
    DefinitionFile "foo.mcn" [
        (Definition (Name "Foo") . Variant $
             (Name "Foo", [GroundT StringT, GroundT StringT, Variable (Name "Baz")]) :| [])
      , (Definition (Name "Bar") . Variant $
             (Name "BarFoo", [Variable (Name "Foo")])
          :| [ (Name "BarBaz", [Variable (Name "Baz")])
             , (Name "BarFooBaz", [Variable (Name "Foo"), Variable (Name "Baz")])
             ])
      , (Definition (Name "Baz") . Variant $
             (Name "Baz", []) :| [])
      ]
  , DefinitionFile "heck.mcn" [
        (Definition (Name "Heck") . Variant $
          (Name "HeckFooBar", [Variable (Name "Foo"), Variable (Name "Bar")]) :| [])
      , (Definition (Name "Hell") . Variant $
          (Name "HellBaz", [Variable (Name "Baz")]) :| [])
      ]
  , DefinitionFile "pel.mcn" [
        (Definition (Name "Pel") . Variant $
             (Name "Pel", [Variable (Name "Heck"), Variable (Name "Hell")])
          :| [(Name "Pil", [Variable (Name "Foo"), Variable (Name "Bar")])])
      ]
  ]

testGraph :: DefinitionFileGraph
testGraph =
  DefinitionFileGraph $ M.fromList [
      ("foo.mcn", S.fromList [])
    , ("heck.mcn", S.fromList ["foo.mcn"])
    , ("pel.mcn", S.fromList ["foo.mcn", "heck.mcn"])
    ]

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunNormal
