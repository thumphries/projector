{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Html.Machinator.Lexer where

import qualified Data.Text as T

import           Disorder.Core hiding (tripping)
import           Disorder.Jack

import           Projector.Html.Machinator.Data.Position
import           Projector.Html.Machinator.Data.Token
import           Projector.Html.Machinator.Data.Version
import           Projector.Html.Machinator.Lexer

import           Projector.Core.Prelude


prop_lexer_v1_no_comments =
  let
    r =
      lexVersioned "lexer_test" $
        T.unlines [
            "-- machinator @ v1"
          , "data Bar = Bar " <> "-" <> "- data Foo"
          ]
  in
    once $
      isLeft r

prop_lexer_v2_comments =
  let
    r =
      lexVersioned "lexer_test" $
        T.unlines [
            "-- machinator @ v2"
          , "{" <> "-"
          , "  data Foo"
          , "     -} data Foo = Foo"
          , "data Bar = Bar " <> "-" <> "- data Foo"
          ]
  in
    once $
      fmap (fmap (fmap extractPositioned)) r
      ===
      Right (Versioned MachinatorV2 [
          TData , TIdent "Foo" , TEquals , TIdent "Foo"
        , TData , TIdent "Bar" , TEquals , TIdent "Bar"
        ])


return []
tests = $disorderCheckEnvAll TestRunNormal
