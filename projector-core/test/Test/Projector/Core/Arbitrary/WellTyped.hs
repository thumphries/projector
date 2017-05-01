{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Projector.Core.Arbitrary.WellTyped where


import           Control.Monad.Trans.Reader (ReaderT, ask, local, runReaderT)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P

import           Projector.Core.Eval (whnf)
import           Projector.Core.Syntax
import           Projector.Core.Type

import           Test.Projector.Core.Arbitrary.Ground (TestLitT)
import qualified Test.Projector.Core.Arbitrary.Ground as Ground
import qualified Test.Projector.Core.Arbitrary.Name as Name
import qualified Test.Projector.Core.Arbitrary.Types as Types


-- -----------------------------------------------------------------------------

genWellTypedTestExpr ::
     Monad m
  => TypeDecls TestLitT
  -> Type TestLitT
  -> Gen m (Expr TestLitT ())
genWellTypedTestExpr decls ty =
  genWellTypedExpr
    decls
    ty
    (Types.genTestTypeFromContext decls)
    Ground.genWellTypedTestLitValue

-- -----------------------------------------------------------------------------

genWellTypedExpr ::
     (Ground l, Ord l, Monad m)
  => TypeDecls l
  -> Type l
  -> Gen m (Type l)
  -> (l -> Gen m (Value l))
  -> Gen m (Expr l ())
genWellTypedExpr decls ty genType genVal =
  undefined

type GenExpr m = Gen (ReaderT Env m)





-- -----------------------------------------------------------------------------
-- Statey primitives

data Env = Env
  deriving (Eq, Ord, Show)


-- TODO
-- - Come up with a stronger notion of 'path'
--   - Separate 'simple' paths from complex ones (complex ones are monadic, simple pure)
--   - Some kind of trie to handle type variables
