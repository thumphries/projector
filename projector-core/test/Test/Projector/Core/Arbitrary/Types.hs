{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Test.Projector.Core.Arbitrary.Types where


import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, get, put, evalStateT)

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P

import           Projector.Core.Type
import           Projector.Core.Syntax

import           Test.Projector.Core.Arbitrary.Ground (TestLitT)
import qualified Test.Projector.Core.Arbitrary.Ground as Ground
import qualified Test.Projector.Core.Arbitrary.Name as Name


-- | Generate an arbitrary type for TestLitT.
genTestType :: Monad m => Gen m (Type TestLitT)
genTestType =
  genType Ground.genTestLitT

genTestTypeDecls :: Monad m => Gen m (TypeDecls TestLitT)
genTestTypeDecls =
  genTypeDecls mempty Ground.genTestLitT

-- -----------------------------------------------------------------------------

-- | Generate a completely arbitrary type out of primitives.
genType :: Monad m => Gen m l -> Gen m (Type l)
genType g = do
  Gen.recursive Gen.choice [
      TLit <$> g
    ] [
      Gen.subterm (genType g) TList
    , Gen.subterm2 (genType g) (genType g) TArrow
    ]

-- | Generate a type from a set of declarations.
genTypeFromContext :: Monad m => TypeDecls l -> Gen m l -> Gen m (Type l)
genTypeFromContext tc@(TypeDecls m) genLit =
  Gen.recursive Gen.choice
    nonrec
    [ Gen.subterm2 (genTypeFromContext tc genLit) (genTypeFromContext tc genLit) TArrow ]
  where
    -- TODO this could probably be better, but 'element' throws 'error'
    nonrec =
      if M.null m
        then [
            TLit <$> genLit
          ]
        else [
            TVar <$> Gen.element (M.keys m)
          , TLit <$> genLit
          ]

-- -----------------------------------------------------------------------------

-- | Generate a set of type declarations.
genTypeDecls :: (Ground l, Monad m) => TypeDecls l -> Gen m l -> Gen m (TypeDecls l)
genTypeDecls decls genLit = do
  -- Generate typenames and constructors up front
  -- individual generators take some number of them out
  nTypes <- Gen.int (Range.linear 0 20)
  nCons <- Gen.int (Range.linear 0 100)
  allNames <- Gen.set (Range.singleton (nTypes + nCons)) (Name.genIdent 10)
  let
    (types, constructors) =
      bimap (fmap (TypeName . T.toTitle)) (fmap (Constructor . T.toTitle)) (L.splitAt nTypes (toList allNames))
  hoist (flip evalStateT (types, constructors)) (genTypeDecls' decls genLit)

type GenDecl m = Gen (StateT ([TypeName], [Constructor]) m)

genTypeDecls' ::
     (Ground l, Monad m)
  => TypeDecls l
  -> Gen m l
  -> GenDecl m (TypeDecls l)
genTypeDecls' decls genLit = do
  (tn, d) <- Gen.choice [
      genVariant decls genLit
    , genRecord decls genLit
    ]
  let decls' = declareType tn d decls
  genTypeDecls' decls' genLit <|> pure decls'

-- | Generate a variant / sum type.
genVariant ::
     (Ground l, Monad m)
  => TypeDecls l
  -> Gen m l
  -> GenDecl m (TypeName, Decl l)
genVariant decls genLit = do
  tn <- getTypeName
  cn <- getConstructor
  let
    genArgs = hoist lift (Gen.list (Range.linear 0 5) (genTypeFromContext decls genLit))

  -- Generate a nonrecursive branch first
  nonrec <- (cn,) <$> genArgs

  -- Generate an arbitrary number of additional variants
  cs <- getConstructors (Range.linear 0 10)
  recs <- for cs $ \cnn -> (cnn,) <$> genArgs
  pure (tn, DVariant (nonrec:recs))


-- | Generate a record.
genRecord ::
     (Ground l, Monad m)
  => TypeDecls l
  -> Gen m l
  -> GenDecl m (TypeName, Decl l)
genRecord decls genLit = do
  tn <- getTypeName
  let
    genFieldType = hoist lift (genTypeFromContext decls genLit)
  fns <- Gen.set (Range.linear 0 5) Name.genFieldName
  fts <- for (toList fns) $ \fn -> (fn,) <$> genFieldType
  pure (tn, DRecord fts)

-- -----------------------------------------------------------------------------
-- Statey primitives

getTypeName :: Monad m => GenDecl m TypeName
getTypeName = do
  (ts, cs) <- lift get
  case ts of
    [] ->
      empty
    (t:tt) ->
      lift (put (tt, cs)) *> pure t

getConstructor :: Monad m => Gen (StateT ([TypeName], [Constructor]) m) Constructor
getConstructor = do
  (ts, cs) <- lift get
  case cs of
    [] ->
      empty
    (c:cc) ->
      lift (put (ts, cc)) *> pure c


getConstructors :: Monad m => Range Int -> Gen (StateT ([TypeName], [Constructor]) m) [Constructor]
getConstructors range = do
  (ts, cs) <- lift get
  x <- Gen.int range
  let (rs, cc) = L.splitAt x cs
  lift (put (ts, cc))
  pure rs
