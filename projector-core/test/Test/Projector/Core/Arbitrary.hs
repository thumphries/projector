{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Projector.Core.Arbitrary where


import           Control.Comonad (Comonad (..))

import           Data.List as L
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

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
        , TVariant <$> genTypeName <*> genVariants g
        ]

  in oneOfRec nonrec recc

genTypeName :: Jack TypeName
genTypeName =
  fmap (TypeName . T.toTitle) (elements boats)

genConstructor :: Jack Constructor
genConstructor =
  fmap (Constructor . T.toTitle) (elements waters)

genVariants :: Jack l -> Jack [(Constructor, [Type l])]
genVariants t =
  fmap (M.toList . M.fromList . NE.toList) . listOf1 $
    (,) <$> genConstructor
        <*> listOf (genType t)

genExpr :: Jack Name -> Jack (Type l) -> Jack (Value l) -> Jack (Expr l)
genExpr n t v =
  let shrink z = case z of
        ELit _ ->
          []

        EVar _ ->
          []

        EApp x y ->
          [x, y]

        ELam _ _ e ->
          [e]

        ECon _ _ es ->
          es

        ECase e pes ->
          e : fmap snd pes

      nonrec = [
          ELit <$> v
        , EVar <$> n
        ]

      recc = [
          EApp <$> genExpr n t v <*> genExpr n t v
        , genLam n t v
        , genCon t (genExpr n t v)
        , genCase (genExpr n t v) (genPattern genConstructor n) 
        ]
 in reshrink shrink (oneOfRec nonrec recc)

genLam :: Jack Name -> Jack (Type l) -> Jack (Value l) -> Jack (Expr l)
genLam n t v = do
  nam <- n
  typ <- t
  -- Make it likely that we'll actually use the bound name
  let n' = oneOf [pure nam, n]
  bdy <- genExpr n' t v
  pure (lam nam typ bdy)

genCon :: Jack (Type l) -> Jack (Expr l) -> Jack (Expr l)
genCon t v =
  ECon
    <$> genConstructor
    <*> t
    <*> listOf v

genCase :: Jack (Expr l) -> Jack Pattern -> Jack (Expr l)
genCase e p =
  ECase
    <$> e
    <*> (fmap NE.toList (listOf1 $ (,) <$> p <*> e))

genPattern :: Jack Constructor -> Jack Name -> Jack Pattern
genPattern c n =
  oneOfRec [fmap PVar n] [PCon <$> c <*> listOf (genPattern c n)]

-- -----------------------------------------------------------------------------
-- Generating well-typed expressions

-- need to track the types of things we've generated so we can use variables
-- need to be careful about shadowing
newtype Context l = Context { unContext :: Map Name (Type l) }
  deriving (Eq, Show)

centy :: Context l
centy = Context mempty

cextend :: (Ground l, Ord l) => Context l -> Type l -> Name -> Context l
cextend c t n =
  Context (M.insert n t (unContext c))

clookup :: (Ground l, Ord l) => Context l -> Type l -> Maybe [Name]
clookup c t =
   -- this is extraordinarily dumb but does the job
   (M.lookup t (foldl' (\m (k, v) -> M.insertWith (<>) v [k] m) mempty (M.toList (unContext c))))

genWellTypedExpr ::
     (Ground l, Ord l)
  => Type l
  -> Jack (Type l)
  -> (l -> Jack (Value l))
  -> Jack (Expr l)
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
  -> Jack (Expr l)
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
  -> Jack (Expr l)
genWellTypedLam n bnd ty names genty genval = do
  name <- fmap Name (elements muppets) -- FIX parameterise this
  bdy <- genWellTypedExpr' (n `div` 2) ty (cextend names bnd name) genty genval
  pure (lam name bnd bdy)

genWellTypedApp ::
     (Ground l, Ord l)
  => Int
  -> Type l
  -> Context l
  -> Jack (Type l)
  -> (l -> Jack (Value l))
  -> Jack (Expr l)
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
  -> Jack (Expr l)
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
  -> Jack (Expr l)
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

  ppGroundType t = case t of
    TBool -> "Bool"
    TInt -> "Int"
    TString -> "String"

  ppGroundValue v = case v of
    VBool b ->
      if b then "true" else "false"

    VInt n ->
      renderIntegral n

    VString s ->
      T.pack (show s)

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

genTestExpr :: Jack (Expr TestLitT)
genTestExpr =
  genExpr (fmap Name (elements muppets)) (genType genTestLitT) genTestLitValue

genWellTypedTestExpr :: Type TestLitT -> Jack (Expr TestLitT)
genWellTypedTestExpr ty = do
  genWellTypedExpr ty (genType genTestLitT) genWellTypedTestLitValue

genIllTypedTestExpr :: Jack (Expr TestLitT)
genIllTypedTestExpr = do
  genIllTypedExpr (genType genTestLitT) genWellTypedTestLitValue


-- equal up to alpha
-- TODO would be nice to bring the Eq along for free
(=@@=) ::
     (Eq (Value l), Show l, Show (Value l), Ground l)
  => Expr l -> Expr l -> Property
(=@@=) = (===) `on` alphaNf
