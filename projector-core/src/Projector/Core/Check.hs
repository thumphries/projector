{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
module Projector.Core.Check (
  -- * Interface
    TypeError (..)
  , typeCheckIncremental
  , typeCheckAll
  , typeCheck
  , typeTree
  -- * Guts
  , generateConstraints
  , solveConstraints
  , Substitutions (..)
  ) where


import           Control.Monad.ST (ST, runST)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.State.Strict (State, runState, gets, modify')

import           Data.Char (chr, ord)
import           Data.DList (DList)
import qualified Data.DList as D
import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
#if MIN_VERSION_containers(0, 5, 9)
import qualified Data.Map.Merge.Strict as M
#else
import qualified Data.Map.Strict.Merge as M
#endif
import qualified Data.Set as S
import           Data.STRef (STRef)
import qualified Data.STRef as ST
import qualified Data.Text as T
import qualified Data.UnionFind.ST as UF

import           P

import           Projector.Core.Syntax
import           Projector.Core.Type

import           X.Control.Monad.Trans.Either (EitherT, left, runEitherT)
import qualified X.Control.Monad.Trans.Either as ET


data TypeError l a
  = UnificationError (Type l, a) (Type l, a)
  | FreeVariable Name a
  | UndeclaredType TypeName a
  | BadConstructorName Constructor TypeName (Decl l) a
  | BadConstructorArity Constructor Int Int a
  | BadPatternArity Constructor (Type l) Int Int a
  | BadPatternConstructor Constructor a
  | MissingRecordField TypeName FieldName (Type l, a) a
  | ExtraRecordField TypeName FieldName (Type l, a) a
  | DuplicateRecordFields TypeName [FieldName] a
  | InferenceError a
  | TypeHole (Type l, a) a
  | RecordInferenceError [(FieldName, Type l)] a
  | InfiniteType (Type l, a) (Type l, a)

deriving instance (Ground l, Eq a) => Eq (TypeError l a)
deriving instance (Ground l, Show a) => Show (TypeError l a)
deriving instance (Ground l, Ord a) => Ord (TypeError l a)


-- | Like 'typeCheckAll', but accepting a map of expressions of known
-- type. This is appropriate for use in a left fold for incremental
-- (dependency-ordered) typechecking.
typeCheckIncremental ::
     Ground l
  => TypeDecls l
  -> Map Name (Type l, a)
  -> Map Name (Expr l a)
  -> Either [TypeError l a] (Map Name (Expr l (Type l, a)))
typeCheckIncremental decls known exprs =
  typeCheckAll' decls (fmap (\(t,a) -> hoistType decls a t) known) exprs

-- | Typecheck an interdependent set of named expressions.
-- This is essentially top-level letrec.
--
-- TODO: This admits general recursion. We need to write a totality checker.
--
-- We cannot cache the intermediate results here, so we need to
-- recheck everything each time. To support incremental builds we need
-- to group expressions into "modules", figure out the module
-- dependency DAG, and traverse only the dirty subtrees of that DAG.
typeCheckAll ::
     Ground l
  => TypeDecls l
  -> Map Name (Expr l a)
  -> Either [TypeError l a] (Map Name (Expr l (Type l, a)))
typeCheckAll decls exprs =
  typeCheckAll' decls mempty exprs

typeCheckAll' ::
     Ground l
  => TypeDecls l
  -> Map Name (IType l a)
  -> Map Name (Expr l a)
  -> Either [TypeError l a] (Map Name (Expr l (Type l, a)))
typeCheckAll' decls known exprs = do
  -- for each declaration, generate constraints and assumptions
  (annotated, sstate) <- runCheck (sequenceCheck (fmap (generateConstraints' decls) exprs))
  -- build up new global set of constraints from the assumptions
  let -- the inferred types (thus far) for our exprs
      exprTypes = fmap extractType annotated
      -- constraints we figured out in generateConstraints
      localConstraints = sConstraints sstate
      -- constraints provided by the user in type signatures
      userConstraints = D.fromList . M.elems $
        M.merge M.dropMissing M.dropMissing (M.zipWithMatched (const Equal)) known exprTypes
      -- assumptions we made about free variables
      Assumptions assums = sAssumptions sstate
      -- types biased towards 'known' over 'inferred', since they may be user constraints
      types = known <> exprTypes
      -- constraints for free variables we know stuff about
      globalConstraints = D.fromList . fold . M.elems . flip M.mapWithKey assums $ \n itys ->
        maybe mempty (with itys . Equal) (M.lookup n types)
      -- all the constraints mashed together in a dlist
      constraints = D.toList (localConstraints <> userConstraints <> globalConstraints)
      -- all variables let-bound
      bound = S.fromList (M.keys known <> M.keys exprs)
      -- all variables we have lingering assumptions about
      used = S.fromList (M.keys (M.filter (not . null) assums))
      -- all mystery free variables
      free = used `S.difference` bound
      -- annotating mystery free variables with location info
      freeAt = foldMap (\n -> maybe [] (fmap ((n,) . snd . flattenIType)) (M.lookup n assums)) (toList free)
  -- throw errors for any undefined variables
  if free == mempty then pure () else Left (fmap (uncurry FreeVariable) freeAt)

  -- solve all our constraints at once
  subs <- solveConstraints constraints
  -- substitute solved types, all at once
  let subbed = fmap (substitute subs) annotated
  -- lower them from IType into Type, all at once
  first D.toList (ET.sequenceEither (fmap lowerExpr subbed))

typeCheck :: Ground l => TypeDecls l -> Expr l a -> Either [TypeError l a] (Type l)
typeCheck decls =
  fmap extractType . typeTree decls

typeTree ::
     Ground l
  => TypeDecls l
  -> Expr l a
  -> Either [TypeError l a] (Expr l (Type l, a))
typeTree decls expr = do
  (expr', constraints, Assumptions assums) <- generateConstraints decls expr
  -- Any unresolved assumptions are from free variables
  if M.keys (M.filter (not . null) assums) == mempty
    then pure ()
    else Left (foldMap (\(n, itys) -> fmap (FreeVariable n . snd . flattenIType) itys) (M.toList assums))
  subs <- solveConstraints constraints
  let subbed = substitute subs expr'
  first D.toList (lowerExpr subbed)

-- -----------------------------------------------------------------------------
-- Types

-- | We have an internal notion of type inside the checker.
-- Looks a bit like regular types, extended with unification variables.
data IType l a
  = IDunno a Int
  | IHole a Int (Maybe (IType l a))
  | IVar a TypeName -- maybe remove?
  | ILit a l
  | IArrow a (IType l a) (IType l a)
  | IClosedRecord a TypeName (Fields l a)
  | IOpenRecord a Int (Fields l a)
  | IList a (IType l a)
  deriving (Eq, Ord, Show)

newtype Fields l a = Fields {
    _unFields :: Map FieldName (IType l a)
  } deriving (Eq, Ord, Show)

field :: FieldName -> IType l a -> Fields l a
field fn ty =
  Fields (M.fromList [(fn, ty)])

-- | Lift a known type into an 'IType', with an annotation.
hoistType :: Ground l => TypeDecls l -> a -> Type l -> IType l a
hoistType decls a (Type ty) =
  case ty of
    TLitF l ->
      ILit a l
    TVarF tn ->
      case lookupType tn decls of
        Just (DVariant _cns) ->
          IVar a tn
        Just (DRecord fts) ->
          IClosedRecord a tn (hoistFields decls a fts)
        Nothing ->
          -- This is an error? Should probably be in Either here.
          IVar a tn
    TArrowF f g ->
      IArrow a (hoistType decls a f) (hoistType decls a g)
    TListF f ->
      IList a (hoistType decls a f)

hoistFields :: Ground l => TypeDecls l -> a -> [(FieldName, Type l)] -> Fields l a
hoistFields decls a =
  Fields . M.fromList . fmap (fmap (hoistType decls a))

-- | Assert that we have a monotype. Returns 'InferenceError' if we
-- encounter a unification variable.
lowerIType :: IType l a -> Either (TypeError l a) (Type l)
lowerIType ity =
  case ity of
    IDunno a _ ->
      Left (InferenceError a)
    IHole a _ Nothing ->
      Left (InferenceError a)
    IHole a _ (Just i) ->
      Left (TypeHole (flattenIType i) a)
    ILit _ l ->
      pure (TLit l)
    IVar _ tn ->
      pure (TVar tn)
    IArrow _ f g ->
      TArrow <$> lowerIType f <*> lowerIType g
    IClosedRecord _ tn _fs ->
      pure (TVar tn)
    IOpenRecord a _x (Fields fs) -> do
      fs' <- traverse (traverse lowerIType) (M.toList fs)
      Left (RecordInferenceError fs' a)
    IList _ f ->
      TList <$> lowerIType f

-- Produce concrete type name for a fresh variable.
dunnoTypeVar :: Int -> TypeName
dunnoTypeVar x =
  let letter j = chr (ord 'a' + j)
  in case (x `mod` 26, x `div` 26) of
    (i, 0) ->
      TypeName (T.pack [letter i])
    (m, n) ->
      TypeName (T.pack [letter m] <> renderIntegral n)

-- Produce a regular type, concretising fresh variables.
-- This is currently used for error reporting.
flattenIType :: IType l a -> (Type l, a)
flattenIType ity =
  ( flattenIType' ity
  , case ity of
      IDunno a _ ->
        a
      IHole a _ _ ->
        a
      ILit a _ ->
        a
      IVar a _ ->
        a
      IArrow a _ _ ->
        a
      IClosedRecord a _ _ ->
        a
      IOpenRecord a _ _ ->
        a
      IList a _ ->
        a)

flattenIType' :: IType l a -> Type l
flattenIType' ity =
  case ity of
    IDunno _ x ->
      TVar (dunnoTypeVar x)
    IHole _ x _ ->
      TVar (dunnoTypeVar x)
    IVar _ tn ->
      TVar tn
    ILit _ l ->
      TLit l
    IArrow _ f g ->
      TArrow (flattenIType' f) (flattenIType' g)
    IOpenRecord _ x _ ->
      TVar (dunnoTypeVar x)
    IClosedRecord _ tn _ ->
      TVar tn
    IList _ ty ->
      TList (flattenIType' ty)

-- | Report a unification error.
unificationError :: IType l a -> IType l a -> TypeError l a
unificationError =
  UnificationError `on` flattenIType

lowerExpr :: Expr l (IType l a, a) -> Either (DList (TypeError l a)) (Expr l (Type l, a))
lowerExpr =
  ET.sequenceEither . fmap (\(ity, a) -> fmap (,a) (first D.singleton (lowerIType ity)))

typeVar :: IType l a -> Maybe Int
typeVar ty =
  case ty of
    IDunno _ x ->
      pure x
    IOpenRecord _ x _ ->
      pure x
    _ ->
      Nothing

-- -----------------------------------------------------------------------------
-- Monad stack

-- | 'Check' permits multiple errors via 'EitherT', lexically-scoped
-- state via 'ReaderT', and global accumulating state via 'State'.
newtype Check l a b = Check {
    unCheck :: EitherT (DList (TypeError l a)) (State (SolverState l a)) b
  } deriving (Functor, Applicative, Monad)

runCheck :: Check l a b -> Either [TypeError l a] (b, SolverState l a)
runCheck f =
    unCheck f
  & runEitherT
  & flip runState initialSolverState
  & \(e, st) -> fmap (,st) (first D.toList e)

data SolverState l a = SolverState {
    sConstraints :: DList (Constraint l a)
  , sAssumptions :: Assumptions l a
  , sSupply :: NameSupply
  } deriving (Eq, Ord, Show)

initialSolverState :: SolverState l a
initialSolverState =
  SolverState {
      sConstraints = mempty
    , sAssumptions = mempty
    , sSupply = emptyNameSupply
    }

newtype Assumptions l a = Assumptions {
    unAssumptions :: Map Name [IType l a]
  } deriving (Eq, Ord, Show, Monoid)

throwError :: TypeError l a -> Check l a b
throwError =
  Check . left . D.singleton

sequenceCheck :: Traversable t => t (Check l a b) -> Check l a (t b)
sequenceCheck =
  Check . ET.sequenceEitherT . fmap unCheck

-- -----------------------------------------------------------------------------
-- Name supply

-- | Supply of fresh unification variables.
newtype NameSupply = NameSupply { nextVar :: Int }
  deriving (Eq, Ord, Show)

emptyNameSupply :: NameSupply
emptyNameSupply =
  NameSupply 0

nextUnificationVar :: Check l a Int
nextUnificationVar =
  Check . lift $ do
    v <- gets (nextVar . sSupply)
    modify' (\s -> s { sSupply = NameSupply (v + 1) })
    pure v

-- | Grab a fresh type variable.
freshTypeVar :: a -> Check l a (IType l a)
freshTypeVar a =
  IDunno a <$> nextUnificationVar

freshTypeHole :: a -> Check l a (IType l a)
freshTypeHole a =
  IHole a <$> nextUnificationVar <*> pure Nothing

freshOpenRecord :: a -> Fields l a -> Check l a (IType l a)
freshOpenRecord a fs =
  IOpenRecord a <$> nextUnificationVar <*> pure fs

-- -----------------------------------------------------------------------------
-- Constraints

data Constraint l a
  = Equal (IType l a) (IType l a)
  deriving (Eq, Ord, Show)

-- | Record a new constraint.
addConstraint :: Ground l => Constraint l a -> Check l a ()
addConstraint c =
  Check . lift $
    modify' (\s -> s { sConstraints = D.snoc (sConstraints s) c })


-- -----------------------------------------------------------------------------
-- Assumptions

-- | Add an assumed type for some variable we've encountered.
addAssumption :: Ground l => Name -> IType l a -> Check l a ()
addAssumption n ty =
  Check . lift $
    modify' (\s -> s {
        sAssumptions = Assumptions (M.insertWith (<>) n [ty] (unAssumptions (sAssumptions s)))
      })

-- | Clobber the assumption set for some variable.
setAssumptions :: Ground l => Name -> [IType l a] -> Check l a ()
setAssumptions n assums =
  Check . lift $
    modify' (\s -> s {
        sAssumptions = Assumptions (M.insert n assums (unAssumptions (sAssumptions s)))
      })

-- | Delete all assumptions for some variable.
--
-- This is called when leaving the lexical scope in which the variable was bound.
deleteAssumptions :: Ground l => Name -> Check l a ()
deleteAssumptions n =
  Check . lift $
    modify' (\s -> s {
        sAssumptions = Assumptions (M.delete n (unAssumptions (sAssumptions s)))
      })

-- | Look up all assumptions for a given name. Returns the empty set if there are none.
lookupAssumptions :: Ground l => Name -> Check l a [IType l a]
lookupAssumptions n =
  Check . lift $
    fmap (fromMaybe mempty) (gets (M.lookup n . unAssumptions . sAssumptions))

-- | Run some continuation with lexically-scoped assumptions.
-- This is sorta like 'local', but we need to keep changes to other keys in the map.
withBindings :: Ground l => Traversable f => f Name -> Check l a b -> Check l a (Map Name [IType l a], b)
withBindings xs k = do
  old <- fmap (M.fromList . toList) . for xs $ \n -> do
    as <- lookupAssumptions n
    deleteAssumptions n
    pure (n, as)
  res <- k
  new <- fmap (M.fromList . toList) . for xs $ \n -> do
    as <- lookupAssumptions n
    setAssumptions n (fromMaybe mempty (M.lookup n old))
    pure (n, as)
  pure (new, res)

withBinding :: Ground l => Name -> Check l a b -> Check l a ([IType l a], b)
withBinding x k = do
  (as, b) <- withBindings [x] k
  pure (fromMaybe mempty (M.lookup x as), b)

-- -----------------------------------------------------------------------------
-- Constraint generation

generateConstraints ::
     Ground l
  => TypeDecls l
  -> Expr l a
  -> Either [TypeError l a] (Expr l (IType l a, a), [Constraint l a], Assumptions l a)
generateConstraints decls expr = do
  (e, st) <- runCheck (generateConstraints' decls expr)
  pure (e, D.toList (sConstraints st), sAssumptions st)

generateConstraints' :: Ground l => TypeDecls l -> Expr l a -> Check l a (Expr l (IType l a, a))
generateConstraints' decls expr =
  case expr of
    ELit a v ->
      -- We know the type of literals instantly.
      let ty = TLit (typeOf v)
      in pure (ELit (hoistType decls a ty, a) v)

    EVar a v -> do
      -- We introduce a new type variable representing the type of this expression.
      -- Add it to the assumption set.
      t <- freshTypeVar a
      addAssumption v t
      pure (EVar (t, a) v)

    ELam a n mta e -> do
      -- Proceed bottom-up, generating constraints for 'e'.
      -- Gather the assumed types of 'n', and constrain them to be the known (annotated) type.
      -- This expression's type is an arrow from the known type to the inferred type of 'e'.
      (as, e') <- withBinding n (generateConstraints' decls e)
      ta <- maybe (freshTypeVar a) (pure . hoistType decls a) mta
      for_ as (addConstraint . Equal ta)
      let ty = IArrow a ta (extractType e')
      pure (ELam (ty, a) n mta e')

    EApp a f g -> do
      -- Proceed bottom-up, generating constraints for 'f' and 'g'.
      -- Introduce a new type variable for the result of the expression.
      -- Constrain 'f' to be an arrow from the type of 'g' to this type.
      f' <- generateConstraints' decls f
      g' <- generateConstraints' decls g
      t <- freshTypeVar a
      addConstraint (Equal (IArrow a (extractType g') t) (extractType f'))
      pure (EApp (t, a) f' g')

    EList a es -> do
      -- Proceed bottom-up, inferring types for each expression in the list.
      -- Constrain each type to be the annotated 'ty'.
      es' <- for es (generateConstraints' decls)
      te <- freshTypeVar a
      for_ es' (addConstraint . Equal te . extractType)
      let ty = IList a te
      pure (EList (ty, a) es')

    EMap a f g -> do
      -- Special case polymorphic map. g must be List a, f must be (a -> b)
      f' <- generateConstraints' decls f
      g' <- generateConstraints' decls g
      ta <- freshTypeVar a
      tb <- freshTypeVar a
      addConstraint (Equal (IArrow a ta tb) (extractType f'))
      addConstraint (Equal (IList a ta) (extractType g'))
      let ty = IList a tb
      pure (EMap (ty, a) f' g')

    ECon a c tn es ->
      case lookupType tn decls of
        Just ty@(DVariant cns) -> do
          -- Look up the constructor, check its arity, and introduce
          -- constraints for each of its subterms, for which we expect certain types.
          ts <- maybe (throwError (BadConstructorName c tn ty a)) pure (L.lookup c cns)
          unless (length ts == length es) (throwError (BadConstructorArity c (length ts) (length es) a))
          es' <- for es (generateConstraints' decls)
          for_ (L.zip (fmap (hoistType decls a) ts) (fmap extractType es'))
            (\(expected, inferred) -> addConstraint (Equal expected inferred))
          let ty' = IVar a tn
          pure (ECon (ty', a) c tn es')

        -- Records should be constructed via ERec, not ECon
        Just ty@(DRecord _) -> do
          throwError (BadConstructorName c tn ty a)

        Nothing ->
          throwError (UndeclaredType tn a)

    ECase a e pes -> do
      -- The body of the case expression should be the same type for each branch.
      -- We introduce a new unification variable for that type.
      -- Patterns introduce new constraints and bindings, managed in 'patternConstraints'.
      e' <- generateConstraints' decls e
      ty <- freshTypeVar a
      pes' <- for pes $ \(pat, pe) -> do
        let bnds = patternBinds pat
        (_, res) <- withBindings (S.toList bnds) $ do
          -- Order matters here, patCons consumes the assumptions from genCons.
          pe' <- generateConstraints' decls pe
          pat' <- patternConstraints decls (extractType e') pat
          addConstraint (Equal ty (extractType pe'))
          pure (pat', pe')
        pure res
      pure (ECase (ty, a) e' pes')

    ERec a tn fes -> do
      case lookupType tn decls of
        Just (DRecord fts) -> do
          -- recurse into each field
          fes' <- traverse (traverse (generateConstraints' decls)) fes
          let need = M.fromList (fmap (fmap (hoistType decls a)) fts)
              have = M.fromList (fmap (fmap extractType) fes')
          _ <- M.mergeA
            -- What to do when a required field is missing
            (M.traverseMissing (\fn ty -> throwError (MissingRecordField tn fn (flattenIType ty) a)))
            -- When an extraneous field is present
            (M.traverseMissing (\fn ty -> throwError (ExtraRecordField tn fn (flattenIType ty) a)))
            -- When present in both, add a constraint
            (M.zipWithAMatched (\_fn twant thave -> addConstraint (Equal twant thave)))
            need
            have
          -- catch duplicate fields too
          when (length fes /= length fts) $
            throwError (DuplicateRecordFields tn (fmap fst fes L.\\ fmap fst fts) a)

          -- set type as the closed record
          let ty' = IClosedRecord a tn (hoistFields decls a fts)
          pure (ERec (ty', a) tn fes')

        -- Variants should be constructed via ECon, not ERec
        Just ty@(DVariant _) -> do
          throwError (BadConstructorName (Constructor (unTypeName tn)) tn ty a)

        Nothing ->
          throwError (UndeclaredType tn a)

    EPrj a e fn -> do
      e' <- generateConstraints' decls e
      tp <- freshTypeVar a
      rt <- freshOpenRecord a (field fn tp)
      addConstraint (Equal rt (extractType e'))
      pure (EPrj (tp, a) e' fn)

    EForeign a n ty -> do
      -- We know the type of foreign expressions immediately, because they're annotated.
      pure (EForeign (hoistType decls a ty, a) n ty)

    EHole a -> do
      ty <- freshTypeHole a
      pure (EHole (ty, a))

-- | Patterns are binding sites that also introduce lots of new constraints.
patternConstraints ::
     Ground l
  => TypeDecls l
  -> IType l a
  -> Pattern a
  -> Check l a (Pattern (IType l a, a))
patternConstraints decls ty pat =
  case pat of
    PVar a x -> do
      as <- lookupAssumptions x
      for_ as (addConstraint . Equal ty)
      pure (PVar (ty, a) x)

    PCon a c pats ->
      case lookupConstructor c decls of
        Just (tn, ts) -> do
          unless (length ts == length pats)
            (throwError (BadPatternArity c (TVar tn) (length ts) (length pats) a))
          let ty' = hoistType decls a (TVar tn)
          addConstraint (Equal ty' ty)
          pats' <- for (L.zip (fmap (hoistType decls a) ts) pats) (uncurry (patternConstraints decls))
          pure (PCon (ty', a) c pats')

        Nothing ->
          throwError (BadPatternConstructor c a)

extractType :: Expr l (c, a) -> c
extractType =
  fst . extractAnnotation

-- -----------------------------------------------------------------------------
-- Constraint solving

newtype Substitutions l a
  = Substitutions { unSubstitutions :: Map Int (IType l a) }
  deriving (Eq, Ord, Show)

substitute :: Ground l => Substitutions l a -> Expr l (IType l a, a) -> Expr l (IType l a, a)
substitute subs expr =
  with expr $ \(ty, a) ->
    (substituteType subs True ty, a)

substituteType :: Ground l => Substitutions l a -> Bool -> IType l a -> IType l a
substituteType subs top ty =
  case ty of
    IDunno _ x ->
      maybe ty (substituteType subs False) (M.lookup x (unSubstitutions subs))

    IHole a x _ ->
      maybe
        ty
        (\sty ->
          if top
          then IHole a x (Just sty)
          else substituteType subs False sty)
        (M.lookup x (unSubstitutions subs))

    IArrow a t1 t2 ->
      IArrow a (substituteType subs False t1) (substituteType subs False t2)

    IList a t ->
      IList a (substituteType subs False t)

    IOpenRecord _ x _ ->
      maybe ty (substituteType subs False) (M.lookup x (unSubstitutions subs))

    IClosedRecord _ _ _ ->
      ty

    ILit _ _ ->
      ty

    IVar _ _ ->
      ty
{-# INLINE substituteType #-}

newtype Points s l a = Points {
    unPoints :: Map Int (UF.Point s (IType l a))
  }

mostGeneralUnifierST ::
     Ground l
  => STRef s (Points s l a)
  -> IType l a
  -> IType l a
  -> ST s (Either [TypeError l a] ())
mostGeneralUnifierST points t1 t2 =
  runEitherT (void (mguST points t1 t2))

mguST ::
     Ground l
  => STRef s (Points s l a)
  -> IType l a
  -> IType l a
  -> EitherT [TypeError l a] (ST s) (IType l a)
mguST points t1 t2 =
  case (t1, t2) of
    (IDunno a x, _) -> do
      unifyVar points a x t2

    (_, IDunno a x) -> do
      unifyVar points a x t1

    (IHole a x _, _) -> do
      unifyVar points a x t2

    (_, IHole a x _) -> do
      unifyVar points a x t1

    (IOpenRecord a x fs, IOpenRecord b y gs) -> do
      hs <- unifyOpenFields points fs gs
      unifyVar points a x (IOpenRecord b y hs)

    (IOpenRecord a x fs, IClosedRecord _ tn gs) -> do
      _hs <- unifyClosedFields points fs tn gs
      unifyVar points a x t2

    (IClosedRecord _ tn gs, IOpenRecord a x fs) -> do
      _hs <- unifyClosedFields points fs tn gs
      unifyVar points a x t1

    (IClosedRecord _ x _fs1, IClosedRecord _ y _fs2) -> do
      unless (x == y) (left [unificationError t1 t2])
      pure t1

    (IVar _ x, IVar _ y) -> do
      unless (x == y) (left [unificationError t1 t2])
      pure t1

    (ILit _ x, ILit _ y) -> do
      unless (x == y) (left [unificationError t1 t2])
      pure t1

    (IArrow a f g, IArrow _ h i) -> do
      -- Would be nice to have an applicative newtype for this...
      [j, k] <- ET.sequenceEitherT [mguST points f h, mguST points g i]
      pure (IArrow a j k)

    (IList k a, IList _ b) -> do
      c <- mguST points a b
      pure (IList k c)

    (_, _) ->
      left [unificationError t1 t2]

unifyVar :: Ground l => STRef s (Points s l a) -> a -> Int -> IType l a -> EitherT [TypeError l a] (ST s) (IType l a)
unifyVar points a x t2 = do
  mt1 <- lift (getRepr points x)
  let safeUnion c z u2 = do
        unless (typeVar u2 == Just z)
          (ET.hoistEither (occurs c z u2) *> lift (union points (IDunno c z) u2))
        pure u2
  case mt1 of
    -- special case if the var is its class representative
    Just t1@(IDunno b y) ->
      if x == y
        then firstT pure (safeUnion b y t2)
        else mguST points t1 t2
    Just t1 ->
      mguST points t1 t2
    Nothing ->
      firstT pure (safeUnion a x t2)

-- | Check that a given unification variable isn't present inside the
-- type it's being unified with. This is necessary for typechecking
-- to be sound, it prevents us from constructing the infinite type.
occurs :: a -> Int -> IType l a -> Either (TypeError l a) ()
occurs a q ity =
  go q ity
  where
    go x i =
      case i of
        IDunno _ y ->
          when (x == y)
            (Left (InfiniteType (flattenIType (IDunno a x)) (flattenIType ity)))
        IHole _ y _ ->
          when (x == y)
            (Left (InfiniteType (flattenIType (IDunno a x)) (flattenIType ity)))
        IVar _ _ ->
          pure ()
        ILit _ _ ->
          pure ()
        IArrow _ f g ->
          go x f *> go x g
        IClosedRecord _ _ (Fields fs) ->
          traverse_ (go x) fs
        IOpenRecord _ y (Fields fs) ->
          if x == y
            then Left (InfiniteType (flattenIType (IDunno a x)) (flattenIType ity))
            else traverse_ (go x) fs
        IList _ f ->
          go x f
{-# INLINE occurs #-}

unifyOpenFields ::
     Ground l
  => STRef s (Points s l a)
  -> Fields l a
  -> Fields l a
  -> EitherT [TypeError l a] (ST s) (Fields l a)
unifyOpenFields points (Fields fs1) (Fields fs2) = do
  fmap Fields . ET.sequenceEitherT $
    M.merge
      -- missing fields either side just get propagated
      (M.mapMissing (const (firstT pure . pure)))
      (M.mapMissing (const (firstT pure . pure)))
      -- unify any matching fields normally
      (M.zipWithMatched (\_fn t1 t2 -> mguST points t1 t2))
      fs1
      fs2

unifyClosedFields ::
     Ground l
  => STRef s (Points s l a)
  -> Fields l a
  -> TypeName
  -> Fields l a
  -> EitherT [TypeError l a] (ST s) (Fields l a)
unifyClosedFields points (Fields have) tn (Fields want) = do
  fmap Fields . ET.sequenceEitherT $
    M.merge
      -- invocation order really matters here
      -- extra fields in 'have' are very bad
      (M.mapMissing (\fn t1 -> left [ExtraRecordField tn fn (flattenIType t1) (snd (flattenIType t1))]))
      -- extra fields in 'want' are to be expected
      (M.mapMissing (const (firstT pure . pure)))
      -- unify all the other fields
      (M.zipWithMatched (\_fn t1 t2 -> mguST points t1 t2))
      have
      want

solveConstraints :: Ground l => Traversable f => f (Constraint l a) -> Either [TypeError l a] (Substitutions l a)
solveConstraints constraints =
  runST $ do
    -- Initialise mutable state.
    points <- ST.newSTRef (Points M.empty)

    -- Solve all the constraints independently.
    es <- fmap ET.sequenceEither . for constraints $ \c ->
      case c of
        Equal t1 t2 ->
          fmap (first D.fromList) (mostGeneralUnifierST points t1 t2)

    -- Retrieve the remaining points and produce a substitution map
    solvedPoints <- ST.readSTRef points
    for (first D.toList es) $ \_ -> do
      substitutionMap solvedPoints

substitutionMap :: Points s l a -> ST s (Substitutions l a)
substitutionMap points = do
  subs <- for (unPoints points) (UF.descriptor <=< UF.repr)
  pure (Substitutions (M.filterWithKey (\k v -> case v of IDunno _ x -> k /= x; _ -> True) subs))

union :: STRef s (Points s l a) -> IType l a -> IType l a -> ST s ()
union points t1 t2 = do
  p1 <- getPoint points t1
  p2 <- getPoint points t2
  UF.union p1 p2

-- | Fills the 'lookup' API hole in the union-find package.
getPoint :: STRef s (Points s l a) -> IType l a -> ST s (UF.Point s (IType l a))
getPoint mref ty =
  case ty of
    IDunno _ x -> do
      ps <- ST.readSTRef mref
      case M.lookup x (unPoints ps) of
        Just point ->
          pure point
        Nothing -> do
          point <- UF.fresh ty
          ST.modifySTRef' mref (Points . M.insert x point . unPoints)
          pure point
    -- TODO maybe this is a bad idea...
    _ ->
      UF.fresh ty
{-# INLINE getPoint #-}

getRepr :: STRef s (Points s l a) -> Int -> ST s (Maybe (IType l a))
getRepr points x = do
  ps <- ST.readSTRef points
  for (M.lookup x (unPoints ps)) (UF.descriptor <=< UF.repr)
