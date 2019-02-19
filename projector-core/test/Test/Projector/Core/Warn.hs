{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Projector.Core.Warn where


import qualified Data.List as L
import qualified Data.Set as S

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Projector.Core.Prelude

import           Projector.Core.Syntax
import           Projector.Core.Type
import           Projector.Core.Warn

import           System.IO (IO)

import           Test.Projector.Core.Gen


-- big lambda billy
buildExpr :: Int -> Expr TestLitT ()
buildExpr n = case n of
  0 -> var_ "billy"
  m -> app
    (lam_ "billy" Nothing (lit (VBool True)))
    (case_ (var_ "billy") [
        (pvar_ "billy",  (buildExpr (m - 1)))
      ])

prop_warn_shadowing :: Property
prop_warn_shadowing =
  property $ do
    n <- forAll $ Gen.int (Range.linear 1 1000)
    warnShadowing (S.singleton (Name "billy")) (buildExpr n)
      ===
        Left (L.take (n*2) (L.repeat (ShadowedName () (Name "billy"))))

prop_warn_shadowing_vac :: Property
prop_warn_shadowing_vac =
  once $
    warnShadowing (S.singleton (Name "billy")) (buildExpr 0)
    ===
    Right ()

-- -----------------------------------------------------------------------------

prop_warn_exhaustivity :: Property
prop_warn_exhaustivity =
  property $ do
    (_ty, decls, expr) <- forAll genWellTypedTestExpr'
    warnExhaustivity decls expr === Right ()

prop_warn_exhaustivity_casey_pos :: Property
prop_warn_exhaustivity_casey_pos =
  property $ do
    n <- forAll $ Gen.int (Range.linear 0 1000)
    warnExhaustivity caseyCtx (buildCase n) === Right ()

prop_warn_exhaustivity_casey_neg :: Property
prop_warn_exhaustivity_casey_neg =
  property $ do
    n <- forAll $ Gen.int (Range.linear 1 1000)
    warnExhaustivity caseyCtx (buildCaseInex n)
      ===
        Left (L.take n (L.repeat (InexhaustiveCase () [ Constructor "Foo" ] )))


buildCase :: Int -> Expr TestLitT ()
buildCase n = case n of
  0 -> var_ "x"
  m -> case_ (con (Constructor "Casey") (TypeName "Casey") [lit (VBool True)]) [
      (pcon_ "Casey" [pvar_ "x"], buildCase (m `div` 2))
    , (pcon_ "Foo" [pcon_ "Casey" [pvar_ "x"]], buildCase (m `div` 2))
    , (pvar_ "x", var_ "x")
    ]

buildCaseInex :: Int -> Expr TestLitT ()
buildCaseInex n =
  case n of
    0 ->
      var_ "x"
    m ->
      case_
        (con (Constructor "Casey") (TypeName "Casey") [lit (VBool True)])
        [ ( pcon_ "Casey" [pvar_ "x"] , var_ "x" )
        , ( pcon_ "Foo" [pcon_ "Casey" [pvar_ "x"]]
          , (if n == 0 then var_ "x" else buildCaseInex (m - 1)))
        ]

tcasey :: Decl TestLitT
tcasey =
  DVariant [] [
      (Constructor "Casey", [TLit TBool])
    , (Constructor "Foo", [TVar (TypeName "Casey")])
    ]

caseyCtx :: TypeDecls TestLitT
caseyCtx =
  declareType (TypeName "Casey") tcasey mempty

once :: PropertyT IO () -> Property
once =
  withTests 1 . property

tests :: IO Bool
tests =
  checkParallel $$(discover)
