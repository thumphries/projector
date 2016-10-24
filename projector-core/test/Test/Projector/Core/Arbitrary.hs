{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Projector.Core.Arbitrary where


import qualified Bound as B
import qualified Bound.Name as B
import qualified Bound.Var as B

import           Control.Comonad (Comonad (..))

import           Data.List as L
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict as M
import           Disorder.Corpus
import           Disorder.Jack

import           P

import           Projector.Core.Simplify
import           Projector.Core.Syntax
import           Projector.Core.Type


-- -----------------------------------------------------------------------------
-- Generating completely arbitrary expressions (mostly ill-typed)

genType :: Jack l -> Jack (Type l)
genType g =
  let nonrec = [
          TLit <$> g
        ]

      recc = [
          TArrow <$> genType g <*> genType g
        ]

  in oneOfRec nonrec recc

genExpr :: Jack Text -> Jack (Type l) -> Jack (Value l) -> Jack (Expr l Text Text)
genExpr n t v =
  let shrink z = case z of
        ELit _ ->
          []

        EVar _ ->
          []

        EApp x y ->
          [x, y]

        ELam _ x ->
          -- need to walk under binders with fromScope
          -- and give sensible names to all the anonymous bound variables.
          -- luckily, we were tracking their source names using Bound.Name!
          [uninstantiate (B.fromScope x)]

      nonrec = [
          ELit <$> v
        , EVar <$> n
        ]

      recc = [
          EApp <$> genExpr n t v <*> genExpr n t v
        , genLam n t v
        ]
 in reshrink shrink (oneOfRec nonrec recc)

genLam :: Jack Text -> Jack (Type l) -> Jack (Value l) -> Jack (Expr l Text Text)
genLam n t v = do
  nam <- n
  typ <- t
  -- Make it likely that we'll actually use the bound name
  let n' = oneOf [pure nam, n]
  bdy <- genExpr n' t v
  pure (lam nam typ bdy)

-- Use source metadata in Bound.Name to restore names to instantiated variables.
uninstantiate :: Expr l Text (B.Var (B.Name Text ()) Text) -> Expr l Text Text
uninstantiate =
  fmap (B.unvar B.name id)


-- -----------------------------------------------------------------------------
-- Generating well-typed expressions

-- need to track the types of things we've generated so we can use variables
-- need to be careful about shadowing
newtype Context l = Context { unContext :: Map Text (Type l) }
  deriving (Eq, Show)

centy :: Context l
centy = Context mempty

cextend :: (Ground l, Ord l) => Context l -> Type l -> Text -> Context l
cextend c t n =
  Context (M.insert n t (unContext c))

clookup :: (Ground l, Ord l) => Context l -> Type l -> Maybe [Text]
clookup c t =
   -- this is extraordinarily dumb but does the job
   (M.lookup t (foldl' (\m (k, v) -> M.insertWith (<>) v [k] m) mempty (M.toList (unContext c))))

genWellTypedExpr ::
     (Ground l, Ord l)
  => Type l
  -> Jack (Type l)
  -> (l -> Jack (Value l))
  -> Jack (Expr l Text Text)
genWellTypedExpr ty genty genval =
  sized $ \n -> do
    k <- choose (0, n)
    genWellTypedExpr' k ty centy genty genval

genWellTypedExpr' ::
     (Ground l, Ord l)
  => Int
  -> Type l
  -> Context l
  -> Jack (Type l)
  -> (l -> Jack (Value l))
  -> Jack (Expr l Text Text)
genWellTypedExpr' n ty names genty genval =
  let gen = case ty of
        TLit l ->
          if n <= 1
            then ELit <$> genval l
            else genWellTypedApp n ty names genty genval

        TArrow t1 t2 ->
          genWellTypedLam n t1 t2 names genty genval

  -- try to look something appropriate up from the context
  in case clookup names ty of
       Nothing -> gen
       Just xs ->
         if n <= 1
           then elements (fmap EVar xs)
           else gen

genWellTypedLam ::
     (Ground l, Ord l)
  => Int
  -> Type l -- bound type
  -> Type l -- result type
  -> Context l
  -> Jack (Type l)
  -> (l -> Jack (Value l))
  -> Jack (Expr l Text Text)
genWellTypedLam n bnd ty names genty genval = do
  name <- elements muppets -- FIX parameterise this
  bdy <- genWellTypedExpr' (n `div` 2) ty (cextend names bnd name) genty genval
  pure (lam name bnd bdy)

genWellTypedApp ::
     (Ground l, Ord l)
  => Int
  -> Type l
  -> Context l
  -> Jack (Type l)
  -> (l -> Jack (Value l))
  -> Jack (Expr l Text Text)
genWellTypedApp n ty names genty genval = do
  bnd <- genty
  fun <- genWellTypedLam (n `div` 2) bnd ty names genty genval
  arg <- genWellTypedExpr' (n `div` 2) bnd names genty genval
  reshrink (\x -> [whnf x]) $
    pure (EApp fun arg)


-- -----------------------------------------------------------------------------
-- Generating ill-typed expressions

genIllTypedExpr ::
     (Ground l, Ord l)
  => Jack (Type l)
  -> (l -> Jack (Value l))
  -> Jack (Expr l Text Text)
genIllTypedExpr genty genval =
  sized $ \n -> do
    k <- choose (0, n)
    genIllTypedExpr' k centy genty genval

genIllTypedExpr' ::
     (Ground l, Ord l)
  => Int
  -> Context l
  -> Jack (Type l)
  -> (l -> Jack (Value l))
  -> Jack (Expr l Text Text)
genIllTypedExpr' n names genty genval = do
  -- it can only be an app afaict
  ty <- genty
  bnd <- genty
  nbnd <- genty `suchThat` (/= bnd)
  fun <- genWellTypedLam (n `div` 2) bnd ty names genty genval
  arg <- genWellTypedExpr' (n `div` 2) nbnd names genty genval
  pure (EApp fun arg)


-- -----------------------------------------------------------------------------
-- XXX Useful Jack combinators

genUniquePair :: Eq a => Jack a -> Jack (a, a)
genUniquePair g = do
  a <- g
  b <- g `suchThat` (/= a)
  pure (a, b)

-- | a dodgy way to test jack shrinking invariants
jackShrinkProp :: Show a => Int -> Jack a -> (a -> Property) -> Property
jackShrinkProp n gen prop =
  gamble (mapTree duplicate gen) $ \t ->
    conjoin . take n $ fmap (prop . outcome) (shrinks t)


-- -----------------------------------------------------------------------------
-- A simple set of literals for testing purposes

data TestLitT
  = TBool
  | TInt
  | TString
  deriving (Eq, Ord, Show)

instance Ground TestLitT where
  data Value TestLitT
    = VBool Bool
    | VInt Int
    | VString Text
    deriving (Eq, Ord, Show)

  typeOf v = case v of
    VBool _ -> TBool
    VInt _ -> TInt
    VString _ -> TString

genTestLitT :: Jack TestLitT
genTestLitT =
  elements [
      TBool
    , TInt
    , TString
    ]

genTestLitValue :: Jack (Value TestLitT)
genTestLitValue =
  oneOf [
      VBool <$> arbitrary
    , VInt <$> chooseInt (0, 100)
    , VString <$> elements muppets
    ]

genWellTypedTestLitValue :: TestLitT -> Jack (Value TestLitT)
genWellTypedTestLitValue t =
  case t of
    TBool -> VBool <$> arbitrary
    TInt -> VInt <$> chooseInt (0, 100)
    TString -> VString <$> elements cooking

-- -----------------------------------------------------------------------------
-- Generators you might actually use

genTestExpr :: Jack (Expr TestLitT Text Text)
genTestExpr =
  genExpr (elements muppets) (genType genTestLitT) genTestLitValue

genWellTypedTestExpr :: Type TestLitT -> Jack (Expr TestLitT Text Text)
genWellTypedTestExpr ty = do
  genWellTypedExpr ty (genType genTestLitT) genWellTypedTestLitValue

genIllTypedTestExpr :: Jack (Expr TestLitT Text Text)
genIllTypedTestExpr = do
  genIllTypedExpr (genType genTestLitT) genWellTypedTestLitValue
