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
        , TList <$> rtype
        ]

      rtype = genType' (max 1 (m `div` 2)) (n `div` 2) g

  in oneOfRec nonrec recc

genVariants :: Int -> Int -> Jack l -> Jack [(Constructor, [Type l])]
genVariants m n t =
  fmap (M.toList . M.fromList) . listOfN 1 m $
    (,) <$> genConstructor
        <*> listOfN 0 n (genType' (max 1 (m `div` 2)) (n `div` 2) t)

genExpr :: Jack Name -> Jack (Type l) -> Jack (Value l) -> Jack (Expr l ())
genExpr n t v =
  let shrink z = case z of
        ELit _ _ ->
          []

        EVar _ _ ->
          []

        EApp _ x y ->
          [x, y]

        ELam _ _ _ e ->
          [e]

        ECon _ _ _ es ->
          es

        ECase _ e pes ->
          e : fmap snd pes

        EList _ _ es ->
          es

        EForeign _ _ _ ->
          []

      nonrec = [
          lit <$> v
        , var <$> n
        , foreign_ <$> n <*> t
        ]

      recc = [
          app <$> genExpr n t v <*> genExpr n t v
        , genLam n t v
        , genCon (fmap (TypeName . unName) n) (genExpr n t v)
        , genCase (genExpr n t v) (genPattern genConstructor n)
        , list <$> t <*> listOf (genExpr n t v)
        ]
 in reshrink shrink (oneOfRec nonrec recc)

genLam :: Jack Name -> Jack (Type l) -> Jack (Value l) -> Jack (Expr l ())
genLam n t v = do
  nam <- n
  typ <- t
  -- Make it likely that we'll actually use the bound name
  let n' = oneOf [pure nam, n]
  bdy <- genExpr n' t v
  pure (lam nam typ bdy)

genCon :: Jack TypeName -> Jack (Expr l ()) -> Jack (Expr l ())
genCon t v =
  con
    <$> genConstructor
    <*> t
    <*> listOf v

genCase :: Jack (Expr l ()) -> Jack (Pattern ()) -> Jack (Expr l ())
genCase e p =
  case_
    <$> e
    <*> (fmap NE.toList (listOf1 $ (,) <$> p <*> e))

genPattern :: Jack Constructor -> Jack Name -> Jack (Pattern ())
genPattern c n =
  oneOfRec [fmap pvar n] [pcon <$> c <*> listOf (genPattern c n)]

-- -----------------------------------------------------------------------------
-- Generating well-typed expressions

-- generate a set of typenames
-- generate a set of constructors

genTypeDecls ::
     Ground l
  => TypeDecls l
  -> Jack TypeName
  -> Jack Constructor
  -> Jack l
  -> Jack (TypeDecls l)
genTypeDecls tc tn cs gt = do
  nTypes <- chooseInt (0, 20)
  nCons <- chooseInt (0, 100)
  types <- S.toList <$> genSizedSet nTypes tn
  constructors <- S.toList <$> genSizedSet nCons cs
  genTypeDecls' gt types constructors tc

genTypeDecls' ::
     Ground l
  => Jack l
  -> [TypeName]
  -> [Constructor]
  -> TypeDecls l
  -> Jack (TypeDecls l)
genTypeDecls' _ [] _ tc =
  pure tc
genTypeDecls' _ _ [] tc =
  pure tc
genTypeDecls' g (t:ts) (c:cs) tc = do
  (vars, cs') <- genVariantsFromContext' g c cs tc
  let ty = DVariant vars
  genTypeDecls' g ts cs' (declareType t ty tc)

genVariantsFromContext' ::
     Ground l
  => Jack l
  -> Constructor
  -> [Constructor]
  -> TypeDecls l
  -> Jack ([(Constructor, [Type l])], [Constructor])
genVariantsFromContext' g c cs tc = do
  -- Generate a nonrecursive branch first
  nonrec <- (c,) <$> listOfN 0 5 (genTypeFromContext mempty g)

  -- Generate arbitrary number of additional variants
  k <- chooseInt (0, 10)
  let (cs', cs'') = L.splitAt k cs
  recs <- traverse (\cn -> (cn,) <$> listOfN 0 5 (genTypeFromContext tc g)) cs'
  pure (nonrec:recs, cs'')

-- Generate simple types, or pull one from the context.
genTypeFromContext :: Ground l => TypeDecls l -> Jack l -> Jack (Type l)
genTypeFromContext tc@(TypeDecls m) g =
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
type Paths l = Map (Type l) [(Name, Type l)]
data Context l = Context {
    cnames :: Map Name (Type l)
  , cpaths :: Paths l
  } deriving (Eq, Show)

centy :: Ord l => Context l
centy =
  Context mempty mempty

cextend ::
     (Ground l, Ord l)
  => TypeDecls l
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
pinsert :: (Ground l, Ord l) => TypeDecls l -> Context l -> Name -> Type l -> Context l
pinsert ctx (Context ns p) n t =
  Context ns . mcons t (n, t) $ case t of
    Type (TLitF _) ->
      p

    Type (TArrowF _ to) ->
      mcons to (n, t) p

    Type (TListF _) ->
      p

    Type (TVarF x) ->
      maybe p (declPaths n t p) (lookupType x ctx)

declPaths :: Ord l => Name -> Type l -> Paths l -> Decl l -> Paths l
declPaths n t p ty =
  case ty of
    DVariant cts ->
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

mcons :: Ord k => k -> v -> Map k [v] -> Map k [v]
mcons k v =
  M.alter (\x -> Just (v : fromMaybe [] x)) k

genWellTypedExpr ::
     (Ground l, Ord l)
  => TypeDecls l
  -> Type l
  -> Jack (Type l)
  -> (l -> Jack (Value l))
  -> Jack (Expr l ())
genWellTypedExpr ctx ty genty genval =
  sized $ \n -> do
    k <- choose (0, n)
    genWellTypedExpr' k ty ctx centy genty genval

genWellTypedExpr' ::
     (Ground l, Ord l)
  => Int
  -> Type l
  -> TypeDecls l
  -> Context l
  -> Jack (Type l)
  -> (l -> Jack (Value l))
  -> Jack (Expr l ())
genWellTypedExpr' n ty ctx names genty genval =
  let gen = case ty of
        Type (TLitF l) ->
          if n <= 1
            then lit <$> genval l
            else genWellTypedApp n ty ctx names genty genval

        Type (TVarF x) ->
          -- Look it up in ctx
          case lookupType x ctx of
            Just (DVariant cts) -> do
              (conn, tys) <- elements cts
              con conn x <$> traverse (\t -> genWellTypedExpr' (n `div` (length tys)) t ctx names genty genval) tys

            Nothing ->
              fail "free type variable!"

        Type (TArrowF t1 t2) ->
          genWellTypedLam n t1 t2 ctx names genty genval

        Type (TListF lty) -> do
          k <- chooseInt (0, n)
          list lty <$> replicateM k (genWellTypedExpr' (n `div` (max 1 (n - k))) lty ctx names genty genval)

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
  => TypeDecls l
  -> Context l
  -> (Context l -> Type l -> Jack (Expr l ()))
  -> Type l
  -> Name
  -> Type l
  -> Jack (Expr l ())
genWellTypedPath ctx names more want x have =
  if want == have
    then pure (var x) -- straightforward lookup
    else case have of
      Type (TArrowF from _) -> do
        arg <- more names from
        pure (app (var x) arg)

      Type (TLitF _) ->
        -- impossible
        pure (var x)

      Type (TListF _) ->
        -- impossible
        pure (var x)

      Type (TVarF n) ->
        -- look up in ctx, run with that
        case lookupType n ctx of
          Just (DVariant cts) ->
            case_ (var x) <$> genAlternatives ctx names more cts want
          Nothing ->
            fail "free type variable!"

genAlternatives ::
     (Ord l, Ground l)
  => TypeDecls l
  -> Context l
  -> (Context l -> Type l -> Jack (Expr l ()))
  -> [(Constructor, [Type l])]
  -> Type l
  -> Jack [(Pattern (), Expr l ())]
genAlternatives ctx names more cts want =
  for cts $ \(c, tys) -> do
    let bnds = L.take (length tys) (freshNames "x")
        pat = pcon c (fmap pvar bnds)
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
  -> TypeDecls l
  -> Context l
  -> Jack (Type l)
  -> (l -> Jack (Value l))
  -> Jack (Expr l ())
genWellTypedLam n bnd ty ctx names genty genval = do
  name <- fmap Name (elements muppets)
  bdy <- genWellTypedExpr' (n `div` 2) ty ctx (cextend ctx names bnd name) genty genval
  pure (lam name bnd bdy)

genWellTypedApp ::
     (Ground l, Ord l)
  => Int
  -> Type l
  -> TypeDecls l
  -> Context l
  -> Jack (Type l)
  -> (l -> Jack (Value l))
  -> Jack (Expr l ())
genWellTypedApp n ty ctx names genty genval = do
  bnd <- genty
  fun <- genWellTypedLam (n `div` 2) bnd ty ctx names genty genval
  arg <- genWellTypedExpr' (n `div` 2) bnd ctx names genty genval
  reshrink (\x -> [whnf x]) $
    pure (app fun arg)


-- -----------------------------------------------------------------------------
-- Generating ill-typed expressions


genIllTypedExpr ::
     (Ground l, Ord l)
  => TypeDecls l
  -> Jack (Type l)
  -> (l -> Jack (Value l))
  -> Jack (Expr l ())
genIllTypedExpr ctx genty genval =
  sized $ \n -> do
    k <- choose (0, n)
    genIllTypedExpr' k ctx centy genty genval

genIllTypedExpr' ::
     (Ground l, Ord l)
  => Int
  -> TypeDecls l
  -> Context l
  -> Jack (Type l)
  -> (l -> Jack (Value l))
  -> Jack (Expr l ())
genIllTypedExpr' n ctx names genty genval =
  -- This function should encode all the concrete, inner ways you can
  -- cause a type error.
  --
  -- TODO: find a way to 'grow' this expression upwards inside a
  -- well-typed program while still shrinking correctly.
  -- Right now we cause the error at the top and let the expr grow downwards.
  let badApp = do
        ty <- genty
        bnd <- genty
        nbnd <- genty `suchThat` (/= bnd)
        fun <- genWellTypedLam (n `div` 2) bnd ty ctx names genty genval
        arg <- genWellTypedExpr' (n `div` 2) nbnd ctx names genty genval
        pure (app fun arg)

      badCase1 = do
        -- generate patterns and alternatives for a known variant,
        -- then put some bound variable of a different type in the case statement
        (tn, DVariant cts) <- elements (M.toList (unTypeDecls ctx))
        nty <- genty `suchThat` (/= TVar tn)
        na <- fmap Name (elements muppets) -- name for the value of Variant type
        nn <- fmap Name (elements southpark) -- name for the new bound variable
        bty <- genty -- arbitrary type for the body of the expression

        -- update the context with both the actually-bound name and the was-gonna-be-bound name
        let names' = cextend ctx (cextend ctx names (TVar tn) na) nty nn

        -- generate patterns and alternatives
        let k = n `div` (max 2 (length cts))
        pes <- genAlternatives ctx names' (\c t -> genWellTypedExpr' k t ctx c genty genval) cts bty

        -- put a different thing in the e
        pure (lam nn nty (case_ (var nn) pes))

      badCase2 = do
        -- Create a valid case statement, then swap one of the
        -- branches for one of a different type.

        (tn, DVariant cts) <- elements (M.toList (unTypeDecls ctx))
        nn <- fmap Name (elements muppets)
        let names' = cextend ctx names (TVar tn) nn

        -- pick two types for the body expression
        ety <- genty
        nety <- genty `suchThat` (/= ety)

        -- generate at least 1 body of the wrong type
        let k = (n `div` (max 2 (length cts)))
        pes <- genAlternatives ctx names' (\c t -> genWellTypedExpr' k t ctx c genty genval) cts ety
        bat <- genWellTypedExpr' k nety ctx names' genty genval
        let pes' = (pvar_ "x", bat) : pes

        pure (lam nn (TVar tn) (case_ (var nn) pes'))

      badCon = do
        -- grab some variant
        (tn, DVariant cts) <- elements (M.toList (unTypeDecls ctx))

        -- construct it wrong
        (conn, tys) <- elements cts
        fmap (con conn tn) $
          case tys of
            [] -> do -- cosntructor with no arguments, give it extra
              extraType <- genty
              e <- genWellTypedExpr' (n `div` 2) extraType ctx names genty genval
              pure [e]

            xs -> for xs $ \t -> do -- satisfy every type incorrectly
              nty <- genty `suchThat` (/= t)
              genWellTypedExpr' (n `div` (max 2 (length xs))) nty ctx names genty genval

  in oneOf
       (if (unTypeDecls ctx == mempty)
         then [badApp] -- most of these need at least one variant in scope
         else [badApp, badCase1, badCase2, badCon])




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
    go k g s = do
      e <- g `suchThat` (`S.notMember` s)
      go (k-1) g (S.insert e s)

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

genTestExpr :: Jack (Expr TestLitT ())
genTestExpr =
  genExpr (fmap Name (elements muppets)) (genType genTestLitT) genTestLitValue

genWellTypedTestExpr :: TypeDecls TestLitT -> Type TestLitT -> Jack (Expr TestLitT ())
genWellTypedTestExpr ctx ty = do
  genWellTypedExpr ctx ty (genTypeFromContext ctx genTestLitT) genWellTypedTestLitValue

genWellTypedTestExpr' :: Jack (Type TestLitT, TypeDecls TestLitT, Expr TestLitT ())
genWellTypedTestExpr' = do
  ctx <- genTestTypeDecls
  ty <- genTestType ctx
  (ty, ctx,) <$> genWellTypedTestExpr ctx ty

genIllTypedTestExpr :: TypeDecls TestLitT -> Jack (Expr TestLitT ())
genIllTypedTestExpr ctx = do
  genIllTypedExpr ctx (genTypeFromContext ctx genTestLitT) genWellTypedTestLitValue

genIllTypedTestExpr' :: Jack (TypeDecls TestLitT, Expr TestLitT ())
genIllTypedTestExpr' = do
  ctx <- genTestTypeDecls
  (ctx,) <$> genIllTypedExpr ctx (genTypeFromContext ctx genTestLitT) genWellTypedTestLitValue

genTestTypeDecls :: Jack (TypeDecls TestLitT)
genTestTypeDecls
  = genTypeDecls mempty genTypeName genConstructor genTestLitT

genTestType :: TypeDecls TestLitT -> Jack (Type TestLitT)
genTestType tc =
  genTypeFromContext tc genTestLitT

-- equal up to alpha
-- TODO would be nice to bring the Eq along for free
(=@@=) ::
     (Eq (Value l), Show l, Show (Value l), Ground l)
  => Expr l () -> Expr l () -> Property
(=@@=) = (===) `on` alphaNf
