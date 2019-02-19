{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Projector.Core.Gen where


import           Data.List as L
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Projector.Core.Prelude

import           Projector.Core.Eval (whnf)
import           Projector.Core.Syntax
import           Projector.Core.Type


-- -----------------------------------------------------------------------------
-- Generating completely arbitrary expressions (mostly ill-typed)

genType :: Gen l -> Gen (Type l)
genType g = do
    m <- Gen.int (Range.linear 1 20)
    n <- Gen.int (Range.linear 0 10)
    genType' m n g

genType' :: Int -> Int -> Gen l -> Gen (Type l)
genType' m n g =
  let nonrec = [
          TLit <$> g
        ]

      recc = [
          TArrow <$> rtype <*> rtype
        , TList <$> rtype
        ]

      rtype = genType' (max 1 (m `div` 2)) (n `div` 2) g

  in Gen.recursive Gen.choice nonrec recc

genIdent :: Int -> Gen Text
genIdent x =
  Gen.choice [
      Gen.element waters
    , Gen.text (Range.singleton x) Gen.lower
    ]

genConstructor :: Gen Constructor
genConstructor =
  fmap (Constructor . T.toTitle) (genIdent 8)


genVariants :: Int -> Int -> Gen l -> Gen [(Constructor, [Type l])]
genVariants m n t =
  fmap (M.toList . M.fromList) . Gen.list (Range.linear 1 m) $
    (,) <$> genConstructor
        <*> Gen.list (Range.linear 0 n) (genType' (max 1 (m `div` 2)) (n `div` 2) t)

genLam :: Gen Name -> Gen (Type l) -> Gen (Value l) -> Gen (Expr l ())
genLam n t v = do
  nam <- n
  typ <- t
  -- Make it likely that we'll actually use the bound name
  let n' = Gen.choice [pure nam, n]
  -- Randomly drop the type annotation
  typ' <- Gen.element [Just typ, empty]
  bdy <- genExpr n' t v
  pure (lam nam typ' bdy)

genCon :: Gen TypeName -> Gen (Expr l ()) -> Gen (Expr l ())
genCon t v =
  con
    <$> genConstructor
    <*> t
    <*> Gen.list (Range.linear 0 10) v

genCase :: Gen (Expr l ()) -> Gen (Pattern ()) -> Gen (Expr l ())
genCase e p =
  case_
    <$> e
    <*> (fmap NE.toList (Gen.nonEmpty (Range.linear 1 10) $ (,) <$> p <*> e))

genPattern :: Gen Constructor -> Gen Name -> Gen (Pattern ())
genPattern c n =
  Gen.recursive Gen.choice [fmap pvar n] [pcon <$> c <*> Gen.list (Range.linear 0 10) (genPattern c n)]

genExpr :: Gen Name -> Gen (Type l) -> Gen (Value l) -> Gen (Expr l ())
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

        ERec _ _ fes ->
          fmap snd fes

        EPrj _ e _ ->
          [e]

        EList _ es ->
          es

        EMap _ f g ->
          [f, g]

        EForeign _ _ _ ->
          []

        EHole _ ->
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
        , list <$> Gen.list (Range.linear 0 10) (genExpr n t v)
        , EMap () <$> genExpr n t v <*> genExpr n t v
        ]
 in Gen.shrink shrink (Gen.recursive Gen.choice nonrec recc)

-- -----------------------------------------------------------------------------
-- Generating well-typed expressions

-- generate a set of typenames
-- generate a set of constructors

genTypeDecls ::
     Ground l
  => TypeDecls l
  -> Gen l
  -> Gen (TypeDecls l)
genTypeDecls tc gt = do
  nTypes <- Gen.int (Range.linear 0 10)
  nCons <- Gen.int (Range.linear 0 20)
  allNames <- S.toList <$> genSizedSet (nTypes + nCons) (fmap T.toTitle (genIdent 10))
  let (types, constructors) = bimap (fmap TypeName) (fmap Constructor) (L.splitAt nTypes allNames)
  genTypeDecls' gt types constructors tc

genTypeDecls' ::
     Ground l
  => Gen l
  -> [TypeName]
  -> [Constructor]
  -> TypeDecls l
  -> Gen (TypeDecls l)
genTypeDecls' _ [] _ tc =
  pure tc
genTypeDecls' _ _ [] tc =
  pure tc
genTypeDecls' g tts@(t:ts) (c:cs) tc =
  Gen.choice [
      do
       (vars, cs') <- genVariantsFromContext' g c cs tc
       let ty = DVariant [] vars
       genTypeDecls' g ts cs' (declareType t ty tc)
    , do
       fts <- genRecordFromContext' g tc
       let ty = DRecord [] fts
           tn = TypeName (unConstructor c)
       genTypeDecls' g tts cs (declareType tn ty tc)
    ]

genRecordFromContext' :: Ground l => Gen l -> TypeDecls l -> Gen [(FieldName, Type l)]
genRecordFromContext' g tc = do
  k <- Gen.int (Range.linear 0 5)
  fns <- fmap toList (genSizedSet k genFieldName)
  fts <- Gen.list (Range.linear 0 k) (genTypeFromContext tc g)
  pure (L.zip fns fts)

-- field names are not globally unique so we can do whatever
genFieldName :: Gen FieldName
genFieldName =
  FieldName <$> Gen.element waters

genVariantsFromContext' ::
     Ground l
  => Gen l
  -> Constructor
  -> [Constructor]
  -> TypeDecls l
  -> Gen ([(Constructor, [Type l])], [Constructor])
genVariantsFromContext' g c cs tc = do
  -- Generate a nonrecursive branch first
  nonrec <- (c,) <$> Gen.list (Range.linear 0 5) (genTypeFromContext mempty g)

  -- Generate arbitrary number of additional variants
  k <- Gen.int (Range.linear 0 10)
  let (cs', cs'') = L.splitAt k cs
  recs <- traverse (\cn -> (cn,) <$> Gen.list (Range.linear 0 5) (genTypeFromContext tc g)) cs'
  pure (nonrec:recs, cs'')

-- Generate simple types, or pull one from the context.
genTypeFromContext :: Ground l => TypeDecls l -> Gen l -> Gen (Type l)
genTypeFromContext tc@(TypeDecls m) g =
  if m == mempty
    then Gen.recursive Gen.choice [
             TLit <$> g
           ] [
             TArrow
               <$> genTypeFromContext tc g
               <*> genTypeFromContext tc g
           ]
    else Gen.recursive Gen.choice [
             TVar <$> Gen.element (simpleTypes tc)
           , TLit <$> g
           ] [
             TArrow
               <$> genTypeFromContext tc g
               <*> genTypeFromContext tc g
           ]

-- Cheat a bit and only generate simple types
simpleTypes :: TypeDecls l -> [TypeName]
simpleTypes (TypeDecls m) =
  M.keys . flip M.filter m $ \decl ->
    case decl of
      DVariant [] _ ->
        True
      DRecord [] _ ->
        True
      _ ->
        False

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

    Type (TAppF _ _) ->
      p

    Type (TListF _) ->
      p

    Type (TVarF x) ->
      maybe p (declPaths n t p) (lookupType x ctx)

    Type (TForallF _x _t) ->
      -- TODO could do better
      p

-- figure out all the types we can reach via this type declaration
declPaths :: Ord l => Name -> Type l -> Paths l -> Decl l -> Paths l
declPaths n t p ty =
  case ty of
    DVariant _ps cts ->
      -- break it apart just one tier (top level types in in our constructors)
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
    DRecord _ps fts ->
      foldl' (\m (_fn, ft) -> mcons ft (n, t) m) p fts

mcons :: Ord k => k -> v -> Map k [v] -> Map k [v]
mcons k v =
  M.alter (\x -> Just (v : fromMaybe [] x)) k

genWellTypedExpr ::
     (Ground l, Ord l)
  => TypeDecls l
  -> Type l
  -> Gen (Type l)
  -> (l -> Gen (Value l))
  -> Gen (Expr l ())
genWellTypedExpr ctx ty genty genval =
  Gen.sized $ \n -> do
    k <- Gen.int (Range.linear 1 (fromIntegral n + 1))
    genWellTypedExpr' k ty ctx centy genty genval

genWellTypedExpr' ::
     (Ground l, Ord l)
  => Int
  -> Type l
  -> TypeDecls l
  -> Context l
  -> Gen (Type l)
  -> (l -> Gen (Value l))
  -> Gen (Expr l ())
genWellTypedExpr' n ty ctx names genty genval =
  let gen = case ty of
        Type (TLitF l) ->
          if n <= 1
            then lit <$> genval l
            else genWellTypedApp n ty ctx names genty genval

        Type (TVarF x) ->
          -- Look it up in ctx
          case lookupType x ctx of
            Just (DVariant _ps cts) -> do
              (conn, tys) <- Gen.element cts
              con conn x <$> traverse (\t -> genWellTypedExpr' (n `div` (length tys)) t ctx names genty genval) tys

            Just (DRecord _ps fts) -> do
              rec_ x
                <$> traverse (traverse (\t -> genWellTypedExpr' (n `div` (max 1 (length fts))) t ctx names genty genval)) fts

            Nothing ->
              fail "free type variable!"

        Type (TArrowF t1 t2) ->
          genWellTypedLam (max 1 (n `div` 2)) t1 t2 ctx names genty genval

        Type (TAppF _ _) ->
          fail "can't generate quantified types"

        Type (TListF lty) -> do
          k <- Gen.int (Range.linear 1 (n+1))
          list <$> replicateM k (genWellTypedExpr' (n `div` (max 1 (n - k))) lty ctx names genty genval)

        Type (TForallF _xs _t1) ->
          fail "can't generate foralls"
          -- TODO case on t1, we can probably generate certain types of forall

  -- try to look something appropriate up from the context
  in case plookup names ty of
       Nothing -> gen
       Just xs ->
         let (nonrec, recc) = partitionPaths xs
             oneOfOr ys = if isJust (Projector.Core.Prelude.head ys) then Gen.choice ys else gen
             genPath = uncurry (genWellTypedPath ctx names (\c t -> genWellTypedExpr' (n `div` 2) t ctx c genty genval) ty)
         in (oneOfOr . fmap genPath) $ if n <= 1 then nonrec else recc

-- Separate simple paths from complicated ones. should probably do this structurally
-- a simple path is any that leads directly to a literal
-- this doesn't say much about its size, only that it will halt pretty quickly and
-- without calling other sized generators
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
  -> (Context l -> Type l -> Gen (Expr l ()))
  -> Type l
  -> Name
  -> Type l
  -> Gen (Expr l ())
genWellTypedPath ctx names more want x have =
  if want == have
    then pure (var x) -- straightforward lookup
    else case have of
      Type (TArrowF from _) -> do
        arg <- more names from
        pure (app (var x) arg)

      Type (TAppF _ _) ->
        -- impossible
        pure (var x)

      Type (TLitF _) ->
        -- impossible
        pure (var x)

      Type (TListF _) ->
        -- impossible
        pure (var x)

      Type (TForallF _xs _t1) ->
        -- TODO
        -- case on t1,
        --   - if it's a lam we can apply it until it looks how we want
        --   - if its a constant we might be able to look one up
        fail "can't generate foralls"

      Type (TVarF n) ->
        -- look up in ctx, run with that
        case lookupType n ctx of
          Just (DVariant _ps cts) ->
            case_ (var x) <$> genAlternatives ctx names more cts want
          Just (DRecord _ps fts) ->
            maybe (fail "invariant fail: can't find type in record!")
              (\(fn, _ft) -> pure (prj (var x) fn))
              (Projector.Core.Prelude.find (\(_fn,ft) -> ft == want) fts)
          Nothing ->
            fail "free type variable!"

genAlternatives ::
     (Ord l, Ground l)
  => TypeDecls l
  -> Context l
  -> (Context l -> Type l -> Gen (Expr l ()))
  -> [(Constructor, [Type l])]
  -> Type l
  -> Gen [(Pattern (), Expr l ())]
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
  -> Gen (Type l)
  -> (l -> Gen (Value l))
  -> Gen (Expr l ())
genWellTypedLam n bnd ty ctx names genty genval = do
  name <- fmap Name (Gen.element muppets)
  bdy <- genWellTypedExpr' (n `div` 2) ty ctx (cextend ctx names bnd name) genty genval
  pure (lam name (Just bnd) bdy)

genWellTypedApp ::
     (Ground l, Ord l)
  => Int
  -> Type l
  -> TypeDecls l
  -> Context l
  -> Gen (Type l)
  -> (l -> Gen (Value l))
  -> Gen (Expr l ())
genWellTypedApp n ty ctx names genty genval = do
  bnd <- genty
  fun <- genWellTypedLam (n `div` 2) bnd ty ctx names genty genval
  fun' <- case fun of
    ELam a nn (Just bt) eexp -> do
      -- Erase the type annotation randomly
      bt' <- Gen.element [pure bt, empty]
      pure (ELam a nn bt' eexp)
    _ ->
      pure fun
  arg <- genWellTypedExpr' (n `div` 2) bnd ctx names genty genval
  Gen.shrink (\x -> [whnf mempty x]) $
    pure (app fun' arg)

genWellTypedLetrec ::
     (Ground l, Ord l)
  => Int
  -> TypeDecls l
  -> Map Name (Type l)
  -> Gen (Type l)
  -> (l -> Gen (Value l))
  -> Gen (Map Name (Type l, Expr l ()))
genWellTypedLetrec n decls known genty genval = do
    k <- Gen.int (Range.linear 0 n)
    -- generate n names and types
    ntys <- genSizedMap k genName genty
    let
      ctxi = M.foldrWithKey (\name t c -> cextend decls c t name) centy known
    (_, res) <- foldM (\(ctx, acc) (na, ty) -> do
      m <- Gen.int (Range.linear 0 (n `div` k))
      e <- genWellTypedExpr' m ty decls ctx genty genval
      pure (cextend decls ctx ty na, M.insert na (ty, e) acc)) (ctxi, mempty) (M.toList ntys)
    pure res

-- -----------------------------------------------------------------------------
-- Generating ill-typed expressions


genIllTypedExpr ::
     (Ground l, Ord l)
  => TypeDecls l
  -> Gen (Type l)
  -> (l -> Gen (Value l))
  -> Gen (Expr l ())
genIllTypedExpr ctx genty genval =
  Gen.sized $ \n -> do
    k <- Gen.int (Range.linear 0 (fromIntegral n))
    genIllTypedExpr' k ctx centy genty genval

genIllTypedExpr' ::
     (Ground l, Ord l)
  => Int
  -> TypeDecls l
  -> Context l
  -> Gen (Type l)
  -> (l -> Gen (Value l))
  -> Gen (Expr l ())
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
        nbnd <- Gen.filter (/= bnd) genty
        fun <- genWellTypedLam (n `div` 2) bnd ty ctx names genty genval
        arg <- genWellTypedExpr' (n `div` 2) nbnd ctx names genty genval
        pure (app fun arg)

      badCase1 = do
        -- generate patterns and alternatives for a known variant,
        -- then put some bound variable of a different type in the case statement
        (tn, decl) <- Gen.element (M.toList (unTypeDecls ctx))
        let cts = case decl of
              DVariant _ps dts ->
                dts
              DRecord _ps fts ->
                [(Constructor (unTypeName tn), fmap snd fts)]
        nty <- Gen.filter (/= TVar tn) genty
        na <- fmap Name (Gen.element muppets) -- name for the value of Variant type
        nn <- fmap Name (Gen.element southpark) -- name for the new bound variable
        bty <- genty -- arbitrary type for the body of the expression

        -- update the context with both the actually-bound name and the was-gonna-be-bound name
        let names' = cextend ctx (cextend ctx names (TVar tn) na) nty nn

        -- generate patterns and alternatives
        let k = n `div` (max 2 (length cts))
        pes <- genAlternatives ctx names' (\c t -> genWellTypedExpr' k t ctx c genty genval) cts bty

        -- put a different thing in the e
        pure (lam nn (Just nty) (case_ (var nn) pes))

      badCase2 = do
        -- Create a valid case statement, then swap one of the
        -- branches for one of a different type.

        (tn, decl) <- Gen.element (M.toList (unTypeDecls ctx))
        let cts = case decl of
              DVariant _ps dts ->
                dts
              DRecord _ps fts ->
                [(Constructor (unTypeName tn), fmap snd fts)]

        nn <- fmap Name (Gen.element muppets)
        let names' = cextend ctx names (TVar tn) nn

        -- pick two types for the body expression
        ety <- genty
        nety <- Gen.filter (/= ety) genty

        -- generate at least 1 body of the wrong type
        let k = (n `div` (max 2 (length cts)))
        pes <- genAlternatives ctx names' (\c t -> genWellTypedExpr' k t ctx c genty genval) cts ety
        bat <- genWellTypedExpr' k nety ctx names' genty genval
        let pes' = (pvar_ "x", bat) : pes

        pure (lam nn (Just (TVar tn)) (case_ (var nn) pes'))

      badCon = do
        -- grab some variant
        (tn, decl) <- Gen.element (M.toList (unTypeDecls ctx))
        let cts = case decl of
              DVariant _ps dts ->
                dts
              DRecord _ps fts ->
                [(Constructor (unTypeName tn), fmap snd fts)]


        -- construct it wrong
        (conn, tys) <- Gen.element cts
        fmap (con conn tn) $
          case tys of
            [] -> do -- cosntructor with no arguments, give it extra
              extraType <- genty
              e <- genWellTypedExpr' (n `div` 2) extraType ctx names genty genval
              pure [e]

            xs -> for xs $ \t -> do -- satisfy every type incorrectly
              nty <- Gen.filter (/= t) genty
              genWellTypedExpr' (n `div` (max 2 (length xs))) nty ctx names genty genval

      freeVar = do
        -- Create some lambda
        ty <- genty
        bnd <- genty
        fun <- genWellTypedLam (n `div` 2) bnd ty ctx names genty genval
        -- apply it to an unbound variable
        varn <- fmap (var . Name) (Gen.element simpsons)
        pure (app fun varn)

  in Gen.choice
       (if (unTypeDecls ctx == mempty)
         then [badApp, freeVar] -- most of these need at least one variant in scope
         else [badApp, freeVar, badCase1, badCase2, badCon])




-- -----------------------------------------------------------------------------
-- XXX Useful Gen combinators

genUniquePair :: Eq a => Gen a -> Gen (a, a)
genUniquePair g = do
  a <- g
  b <- Gen.filter (/= a) g
  pure (a, b)

genSizedSet :: Ord l => Int -> Gen l -> Gen (S.Set l)
genSizedSet n gen =
  go n gen S.empty
  where
    go 0 _ s = pure s
    go k g s = do
      e <- Gen.filter (`S.notMember` s) g
      go (k-1) g (S.insert e s)

genSizedMap :: Ord k => Int -> Gen k -> Gen v -> Gen (Map k v)
genSizedMap n gk gv = do
  keys <- genSizedSet n gk
  fmap M.fromList . for (S.toList keys) $ \k -> (k,) <$> gv

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

genTestLitT :: Gen TestLitT
genTestLitT =
  Gen.element [
      TBool
    , TInt
    , TString
    ]

genTestLitValue :: Gen (Value TestLitT)
genTestLitValue =
  Gen.choice [
      VBool <$> Gen.enumBounded
    , VInt <$> Gen.int (Range.linear 0 100)
    , VString <$> Gen.element muppets
    ]

genWellTypedTestLitValue :: TestLitT -> Gen (Value TestLitT)
genWellTypedTestLitValue t =
  case t of
    TBool -> VBool <$> Gen.enumBounded
    TInt -> VInt <$> Gen.int (Range.linear 0 100)
    TString -> VString <$> Gen.element cooking

genName :: Gen Name
genName =
  fmap Name (genIdent 12)

genTypeName :: Gen TypeName
genTypeName =
  fmap (TypeName . T.toTitle) (genIdent 8)

-- -----------------------------------------------------------------------------
-- Generators you might actually use

genTestExpr :: Gen (Expr TestLitT ())
genTestExpr =
  genExpr (fmap Name (Gen.element muppets)) (genType genTestLitT) genTestLitValue

genWellTypedTestExpr :: TypeDecls TestLitT -> Type TestLitT -> Gen (Expr TestLitT ())
genWellTypedTestExpr ctx ty = do
  genWellTypedExpr ctx ty (genTypeFromContext ctx genTestLitT) genWellTypedTestLitValue

genWellTypedTestExpr' :: Gen (Type TestLitT, TypeDecls TestLitT, Expr TestLitT ())
genWellTypedTestExpr' = do
  ctx <- genTestTypeDecls
  ty <- genTestType ctx
  (ty, ctx,) <$> genWellTypedTestExpr ctx ty

genWellTypedTestLetrec :: Gen (TypeDecls TestLitT, Map Name (Type TestLitT, Expr TestLitT ()))
genWellTypedTestLetrec =
  Gen.sized $ \n -> do
    k <- Gen.int (Range.linear 0 (fromIntegral n))
    ctx <- genTestTypeDecls
    res <- genWellTypedLetrec k ctx mempty (genTypeFromContext ctx genTestLitT) genWellTypedTestLitValue
    pure (ctx, res)

genIllTypedTestExpr :: TypeDecls TestLitT -> Gen (Expr TestLitT ())
genIllTypedTestExpr ctx = do
  genIllTypedExpr ctx (genTypeFromContext ctx genTestLitT) genWellTypedTestLitValue

genIllTypedTestExpr' :: Gen (TypeDecls TestLitT, Expr TestLitT ())
genIllTypedTestExpr' = do
  ctx <- genTestTypeDecls
  (ctx,) <$> genIllTypedExpr ctx (genTypeFromContext ctx genTestLitT) genWellTypedTestLitValue

genTestTypeDecls :: Gen (TypeDecls TestLitT)
genTestTypeDecls
  = genTypeDecls mempty genTestLitT

genTestType :: TypeDecls TestLitT -> Gen (Type TestLitT)
genTestType tc =
  genTypeFromContext tc genTestLitT

muppets :: [Text]
muppets = [
    "gonzo"
  , "kermit"
  , "statler"
  , "waldorf"
  ]

waters :: [Text]
waters = [
    "ocean"
  , "lake"
  , "swamp"
  , "stream"
  ]

cooking :: [Text]
cooking = [
    "boiled"
  , "salted"
  , "stewed"
  , "roasted"
  ]

simpsons :: [Text]
simpsons = [
    "homer"
  , "marge"
  , "bart"
  , "lisa"
  , "maggie"
  , "maude"
  , "flanders"
  , "moe"
  , "barney"
  , "apu"
  ]

southpark :: [Text]
southpark = [
    "kyle"
  , "stan"
  , "cartman"
  , "kenny"
  , "timmy"
  , "chef"
  , "token"
  ]
