{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Projector.Core.Arbitrary where


import           Control.Comonad (Comonad (..))

import           Data.Char (isAsciiLower)
import           Data.List as L
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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
genType g = do
    m <- chooseInt (1, 20)
    n <- chooseInt (0, 10)
    genType' m n g

genType' :: Int -> Int -> Jack l -> Jack (Type l)
genType' m n g =
  let nonrec = [
          TLit <$> g
        ]

      recc = [
          TArrow <$> rtype <*> rtype
        , TVariant <$> genTypeName <*> genVariants m n g
        , TList <$> rtype
        ]

      rtype = genType' (max 1 (m `div` 2)) (n `div` 2) g

  in oneOfRec nonrec recc

genVariants :: Int -> Int -> Jack l -> Jack [(Constructor, [Type l])]
genVariants m n t =
  fmap (M.toList . M.fromList) . listOfN 1 m $
    (,) <$> genConstructor
        <*> listOfN 0 n (genType' (max 1 (m `div` 2)) (n `div` 2) t)

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

        EList _ es ->
          es

        EForeign _ _ ->
          []

      nonrec = [
          ELit <$> v
        , EVar <$> n
        , EForeign <$> n <*> t
        ]

      recc = [
          EApp <$> genExpr n t v <*> genExpr n t v
        , genLam n t v
        , genCon (fmap (TypeName . unName) n) (genExpr n t v)
        , genCase (genExpr n t v) (genPattern genConstructor n)
        , EList <$> t <*> listOf (genExpr n t v)
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

genCon :: Jack TypeName -> Jack (Expr l) -> Jack (Expr l)
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

-- generate a set of typenames
-- generate a set of constructors

genTypeContext ::
     Ground l
  => Jack TypeName -> Jack Constructor -> Jack l -> Jack (TypeContext l)
genTypeContext tn cs gt = do
  nTypes <- chooseInt (0, 20)
  nCons <- chooseInt (0, 100)
  types <- S.toList <$> genSizedSet nTypes tn
  constructors <- S.toList <$> genSizedSet nCons cs
  genTypeContext' gt types constructors tempty

genTypeContext' ::
     Ground l
  => Jack l
  -> [TypeName]
  -> [Constructor]
  -> TypeContext l
  -> Jack (TypeContext l)
genTypeContext' _ [] _ tc =
  pure tc
genTypeContext' _ _ [] tc =
  pure tc
genTypeContext' g (t:ts) (c:cs) tc = do
  (vars, cs') <- genVariantsFromContext' g c cs tc
  let ty = TVariant t vars
  genTypeContext' g ts cs' (textend t ty tc)

genVariantsFromContext' ::
     Ground l
  => Jack l
  -> Constructor
  -> [Constructor]
  -> TypeContext l
  -> Jack ([(Constructor, [Type l])], [Constructor])
genVariantsFromContext' g c cs tc = do
  -- Generate a nonrecursive branch first
  nonrec <- (c,) <$> listOfN 0 5 (genTypeFromContext tempty g)

  -- Generate arbitrary number of additional variants
  k <- chooseInt (0, 10)
  let (cs', cs'') = L.splitAt k cs
  recs <- traverse (\cn -> (cn,) <$> listOfN 0 5 (genTypeFromContext tc g)) cs'
  pure (nonrec:recs, cs'')

-- Generate simple types, or pull one from the context.
genTypeFromContext :: Ground l => TypeContext l -> Jack l -> Jack (Type l)
genTypeFromContext tc@(TypeContext m) g =
  if m == mempty
    then oneOfRec [
             TLit <$> g
           ] [
             TArrow
               <$> genTypeFromContext tc g
               <*> genTypeFromContext tc g
           ]
    else oneOfRec [
             TVar <$> elements (M.keys m)
           , TLit <$> g
           ] [
             TArrow
               <$> genTypeFromContext tc g
               <*> genTypeFromContext tc g
           ]

-- need to track the types of things we've generated so we can use variables
-- need to be careful about shadowing
data Context l = Context {
    cnames :: Map Name (Type l)
  , cpaths :: Map (Type l) [(Name, Type l)]
  } deriving (Eq, Show)

centy :: Ord l => Context l
centy =
  Context mempty mempty

cextend ::
     (Ground l, Ord l)
  => TypeContext l
  -> Context l
  -> Type l
  -> Name
  -> Context l
cextend ctx (Context ns p) t n =
  pinsert ctx (Context (M.insert n t ns) p) n t

clookup :: (Ground l, Ord l) => Context l -> Type l -> Maybe [Name]
clookup c t =
   -- this is extraordinarily dumb but does the job
   (M.lookup t (foldl' (\m (k, v) -> M.insertWith (<>) v [k] m) mempty (M.toList (cnames c))))

-- Look up any values that give us a path to the given type.
-- needs to be filtered to remove any shadowed values.
plookup :: (Ground l, Ord l) => Context l -> Type l -> Maybe [(Name, Type l)]
plookup ctx want =
  with (M.lookup want (cpaths ctx)) $ \nts ->
    catMaybes (fmap (\(n, t1) -> M.lookup n (cnames ctx) >>= \t2 -> guard (t2 == t1) *> pure (n, t1)) nts)

-- record all the types we can reach via the recorded type
pinsert :: (Ground l, Ord l) => TypeContext l -> Context l -> Name -> Type l -> Context l
pinsert ctx names@(Context ns p) n t =
  Context ns . mcons t (n, t) $ case t of
    TLit _ ->
      p

    TArrow _ to ->
      mcons to (n, t) p

    TVariant _ cts ->
      -- break it apart just one tier
      -- TODO: try recursing, might be cool
      foldl'
        (\p' (_, ts) ->
          foldl'
            (\m u ->
              mcons u (n, t) m)
            p'
            ts)
        p
        cts

    TList _ ->
      p -- list might be empty, also we don't have any primitives

    TVar x ->
      maybe
        p
        (\ty -> cpaths (pinsert ctx names n ty))
        (tlookup x ctx)

mcons :: Ord k => k -> v -> Map k [v] -> Map k [v]
mcons k v =
  M.alter (\x -> Just (v : fromMaybe [] x)) k

genWellTypedExpr ::
     (Ground l, Ord l)
  => TypeContext l
  -> Type l
  -> Jack (Type l)
  -> (l -> Jack (Value l))
  -> Jack (Expr l)
genWellTypedExpr ctx ty genty genval =
  sized $ \n -> do
    k <- choose (0, n)
    genWellTypedExpr' k ty ctx centy genty genval

genWellTypedExpr' ::
     (Ground l, Ord l)
  => Int
  -> Type l
  -> TypeContext l
  -> Context l
  -> Jack (Type l)
  -> (l -> Jack (Value l))
  -> Jack (Expr l)
genWellTypedExpr' n ty ctx names genty genval =
  let gen = case ty of
        TLit l ->
          if n <= 1
            then ELit <$> genval l
            else genWellTypedApp n ty ctx names genty genval

        TVar x ->
          -- Look it up in ctx
          -- recur with that type substituted
          maybe
            (fail "free type variable!")
            (\ty' -> genWellTypedExpr' n ty' ctx names genty genval)
            (tlookup x ctx)

        TArrow t1 t2 ->
          genWellTypedLam n t1 t2 ctx names genty genval

        TVariant tn cts -> do
          (con, tys) <- elements cts
          ECon con tn <$> traverse (\t -> genWellTypedExpr' (n `div` (length tys)) t ctx names genty genval) tys

        TList lty -> do
          k <- chooseInt (0, n)
          EList lty <$> replicateM k (genWellTypedExpr' (n `div` (max 1 (n - k))) lty names genty genval)

  -- try to look something appropriate up from the context
  in case plookup names ty of
       Nothing -> gen
       Just xs ->
         let (nonrec, recc) = partitionPaths xs
             oneOfOr ys = if isJust (P.head ys) then oneOf ys else gen
             genPath = uncurry (genWellTypedPath ctx names (\c t -> genWellTypedExpr' (n `div` 2) t ctx c genty genval) ty)
         in (oneOfOr . fmap genPath) $ if n <= 1 then nonrec else recc

-- Separate simple paths from complicated ones. should probably do this structurally
partitionPaths :: [(Name, Type l)] -> ([(Name, Type l)], [(Name, Type l)])
partitionPaths =
  L.partition $ \(_, ty) ->
    case ty of
      TLit _ ->
        True
      _ ->
        False

-- Given a known path to some type, generate an expression of that type.
genWellTypedPath ::
     (Ord l, Ground l)
  => TypeContext l
  -> Context l
  -> (Context l -> Type l -> Jack (Expr l))
  -> Type l
  -> Name
  -> Type l
  -> Jack (Expr l)
genWellTypedPath ctx names more want x have =
  if want == have
    then pure (EVar x) -- straightforward lookup
    else case have of
      TVariant _ cts ->
        ECase (EVar x) <$> genAlternatives ctx names more cts want

      TArrow from _ -> do
        arg <- more names from
        pure (EApp (EVar x) arg)

      TLit _ ->
        -- impossible
        pure (EVar x)

      TList _ ->
        -- impossible
        pure (EVar x)

      TVar n ->
        -- look up in ctx
        -- run with that
        maybe
          (fail "gWTP: free type variable!")
          (\ty -> genWellTypedPath ctx names more want x ty)
          (tlookup n ctx)

genAlternatives ::
     (Ord l, Ground l)
  => TypeContext l
  -> Context l
  -> (Context l -> Type l -> Jack (Expr l))
  -> [(Constructor, [Type l])]
  -> Type l
  -> Jack [(Pattern, Expr l)]
genAlternatives ctx names more cts want =
  for cts $ \(c, tys) -> do
    let bnds = L.take (length tys) (freshNames "x")
        pat = PCon c (fmap PVar bnds)
    let ctx' = foldl' (\cc (ty, na) -> cextend ctx cc ty na) names (L.zip tys bnds)
    ex <- more ctx' want
    pure (pat, ex)

-- From a stem, an infinite list of unique names.
freshNames :: Text -> [Name]
freshNames stem =
  fresh' stem (0 :: Int)
  where
    fresh' n k = Name (n <> "_" <> renderIntegral k) : fresh' n (k+1)

genWellTypedLam ::
     (Ground l, Ord l)
  => Int
  -> Type l -- bound type
  -> Type l -- result type
  -> TypeContext l
  -> Context l
  -> Jack (Type l)
  -> (l -> Jack (Value l))
  -> Jack (Expr l)
genWellTypedLam n bnd ty ctx names genty genval = do
  name <- fmap Name (elements muppets)
  bdy <- genWellTypedExpr' (n `div` 2) ty ctx (cextend ctx names bnd name) genty genval
  pure (lam name bnd bdy)

genWellTypedApp ::
     (Ground l, Ord l)
  => Int
  -> Type l
  -> TypeContext l
  -> Context l
  -> Jack (Type l)
  -> (l -> Jack (Value l))
  -> Jack (Expr l)
genWellTypedApp n ty ctx names genty genval = do
  bnd <- genty
  fun <- genWellTypedLam (n `div` 2) bnd ty ctx names genty genval
  arg <- genWellTypedExpr' (n `div` 2) bnd ctx names genty genval
  reshrink (\x -> [whnf x]) $
    pure (EApp fun arg)


-- -----------------------------------------------------------------------------
-- Generating ill-typed expressions

{-
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
genIllTypedExpr' n names genty genval =
  -- This function should encode all the concrete, inner ways you can
  -- cause a type error.
  --
  -- TODO: find a way to 'grow' this expression upwards inside a
  -- well-typed program while still shrinking correctly.
  let badApp = do
        ty <- genty
        bnd <- genty
        nbnd <- genty `suchThat` (/= bnd)
        fun <- genWellTypedLam (n `div` 2) bnd ty tempty names genty genval
        arg <- genWellTypedExpr' (n `div` 2) nbnd names tempty genty genval
        pure (EApp fun arg)

      -- lazy, this doesn't discard a whole lot
      isVariant ty = case ty of TVariant _ _ -> True; _ -> False
      genVar = genty `suchThat` isVariant

      badCase1 = do
        -- generate a variant type and a name for it
        -- plus a different type and a name for it
        ty@(TVariant _ cts) <- genVar
        nty <- genty `suchThat` (/= ty)
        na <- fmap Name (elements muppets)
        nn <- fmap Name (elements southpark)
        bty <- genty

        -- update the context
        let names' = cextend (cextend names ty na) nty nn

        -- generate patterns and alternatives
        pes <- genAlternatives names' (\c t -> genWellTypedExpr' (n `div` 2) t c genty genval) cts bty

        -- put a different thing in the e
        pure (ELam nn bty (ECase (EVar nn) pes))

      badCase2 = do
        -- generate a variant type
        ty@(TVariant _ cts) <- genVar
        nn <- fmap Name (elements muppets)

        -- update the context
        let names' = cextend names ty nn

        -- pick a type for the expression
        ety <- genty
        nety <- genty `suchThat` (/= ety)

        -- generate at least 1 body of the wrong type
        pes <- genAlternatives names' (\c t -> genWellTypedExpr' (n `div` 2) t c genty genval) cts ety
        bat <- genWellTypedExpr' (n `div` 2) nety names' genty genval
        let pes' = case pes of
              -- TODO could easily include at a random branch
              ((pat, _):ps) -> (pat, bat):ps
              _ -> [(pvar_ "x", bat)] -- impossible

        pure (ELam nn ty (ECase (EVar nn) pes'))

      badCon = do
        -- generate a variant type
        -- FIX THIS IS WRONG needs to come from ctx
        (TVariant tn cts) <- genVar

        -- construct it wrong
        (con, tys) <- elements cts
        fmap (ECon con tn) (for tys $ \t -> do
          nty <- genty `suchThat` (/= t)
          genWellTypedExpr' (n `div` (length tys)) nty names genty genval)

  in oneOf [badApp, badCase1, badCase2, badCon]
-}


-- -----------------------------------------------------------------------------
-- XXX Useful Jack combinators

genUniquePair :: Eq a => Jack a -> Jack (a, a)
genUniquePair g = do
  a <- g
  b <- g `suchThat` (/= a)
  pure (a, b)

genSizedSet :: Ord l => Int -> Jack l -> Jack (S.Set l)
genSizedSet n gen =
  go n gen S.empty
  where
    go 0 _ s = pure s
    go n g s = do
      e <- g `suchThat` (`S.notMember` s)
      go (n-1) g (S.insert e s)

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

genTypeName :: Jack TypeName
genTypeName =
  fmap (TypeName . T.toTitle) $ oneOf [
      elements boats
    , T.pack <$> vectorOf 8 (arbitrary `suchThat` isAsciiLower)
    ]

genConstructor :: Jack Constructor
genConstructor =
  fmap (Constructor . T.toTitle) $ oneOf [
      elements waters
    , T.pack <$> vectorOf 8 (arbitrary `suchThat` isAsciiLower)
    ]

-- -----------------------------------------------------------------------------
-- Generators you might actually use

genTestExpr :: Jack (Expr TestLitT)
genTestExpr =
  genExpr (fmap Name (elements muppets)) (genType genTestLitT) genTestLitValue

genWellTypedTestExpr :: TypeContext TestLitT -> Type TestLitT -> Jack (Expr TestLitT)
genWellTypedTestExpr ctx ty = do
  genWellTypedExpr ctx ty (genTypeFromContext ctx genTestLitT) genWellTypedTestLitValue

genWellTypedTestExpr' :: Jack (Type TestLitT, TypeContext TestLitT, Expr TestLitT)
genWellTypedTestExpr' = do
  ctx <- genTestTypeContext
  ty <- genTestType ctx
  (ty, ctx,) <$> genWellTypedTestExpr ctx ty

{-
genIllTypedTestExpr :: Jack (Expr TestLitT)
genIllTypedTestExpr = do
  genIllTypedExpr (genType genTestLitT) genWellTypedTestLitValue
-}

genTestTypeContext :: Jack (TypeContext TestLitT)
genTestTypeContext
  = genTypeContext genTypeName genConstructor genTestLitT

genTestType :: TypeContext TestLitT -> Jack (Type TestLitT)
genTestType tc =
  genTypeFromContext tc genTestLitT

-- equal up to alpha
-- TODO would be nice to bring the Eq along for free
(=@@=) ::
     (Eq (Value l), Show l, Show (Value l), Ground l)
  => Expr l -> Expr l -> Property
(=@@=) = (===) `on` alphaNf
