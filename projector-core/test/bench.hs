{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


import           Criterion.Main (defaultConfig, defaultMainWith, bgroup, bench)
import           Criterion.Types (Config (..))
import qualified Criterion.Main as C

import           P

import           Projector.Core

import           System.IO (IO)


-- very simple set of ground types

data BenchLitT
  = TUnit
  deriving (Eq, Ord, Show)

instance Ground BenchLitT where
  data Value BenchLitT = VUnit deriving (Eq, Ord, Show)
  typeOf _ = TUnit
  ppGroundType _ = "()"
  ppGroundValue _ = "()"

unit :: Type BenchLitT
unit =
  TLit TUnit

-- church numerals

church :: Type BenchLitT
church =
  TArrow (TArrow unit unit) (TArrow unit unit)

zero :: Expr BenchLitT
zero =
  lam_ "f" (TArrow unit unit)
    (lam_ "x" unit (var_ "x"))

succ :: Expr BenchLitT
succ =
  lam_ "n" church
    (lam_ "f" (TArrow unit unit)
      (lam_ "x" unit
        (EApp
          (var_ "f")
          (EApp (EApp (var_ "n") (var_ "f")) (var_ "x")))))

mult :: Expr BenchLitT
mult =
  lam_ "m" church
    (lam_ "n" church
      (lam_ "f" (TArrow unit unit)
        (EApp
          (var_ "m")
          (EApp (var_ "n") (var_ "f")))))

nth :: Int -> Expr BenchLitT
nth 0 = zero
nth n = EApp succ (nth (n - 1))

mul2 :: Int -> Expr BenchLitT
mul2 n =
  EApp (EApp mult (nth 2)) (nth n)

-- big lambda billy

buildExpr :: Int -> Expr BenchLitT
buildExpr n = case n of
  0 -> var_ "billy"
  m -> EApp (lam_ "billy" unit (buildExpr (m - 1))) (ELit VUnit)

main :: IO ()
main = do
  let cfg =
        defaultConfig {
            reportFile = Just "dist/build/projector-bench.html"
          , csvFile = Just "dist/build/projector-bench.csv"
          }

      norm f = C.whnf nf . f
      tc f = C.whnf typeCheck . f

  defaultMainWith cfg [
      bgroup "normalise-billy" [
          bench "normalise-billy-100" $ norm buildExpr 100
        , bench "normalise-billy-200" $ norm buildExpr 200
        , bench "normalise-billy-1000" $ norm buildExpr 1000
        ]
    , bgroup "check-billy" [
          bench "check-billy-100" $ tc buildExpr 100
        , bench "check-billy-200" $ tc buildExpr 200
        , bench "check-billy-1000" $ tc buildExpr 1000
        ]
    -- this one is kinda cheating as we never substitute
    , bgroup "normalise-church" [
          bench "normalise-church-100" $ norm nth 100
        , bench "normalise-church-200" $ norm nth 200
        , bench "normalise-church-1000" $ norm nth 1000
        ]
    , bgroup "check-church" [
          bench "check-church-100" $ tc nth 100
        , bench "check-church-200" $ tc nth 200
        , bench "check-church-1000" $ tc nth 1000
        ]
    , bgroup "normalise-church-mul2" [
          bench "normalise-church-mul2-100" $ norm mul2 100
        , bench "normalise-church-mul2-200" $ norm mul2 200
        , bench "normalise-church-mul2-1000" $ norm mul2 1000
        ]
    , bgroup "check-church-mul2" [
          bench "check-church-mul2-100" $ tc mul2 100
        , bench "check-church-mul2-200" $ tc mul2 200
        , bench "check-church-mul2-1000" $ tc mul2 1000
        ]
    ]
