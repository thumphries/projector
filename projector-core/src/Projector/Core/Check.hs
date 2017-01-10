{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
module Projector.Core.Check (
  -- * Interface
    TypeError (..)
  , typeCheckAll
  , typeCheck
  , typeTree
  -- * Guts
  , generateConstraints
  , solveConstraints
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
import qualified Data.Set as S
import           Data.STRef (STRef)
import qualified Data.STRef as ST
import qualified Data.Text as T
import qualified Data.UnionFind.ST as UF

import           P

import           Projector.Core.Syntax
import           Projector.Core.Type

import           X.Control.Monad.Trans.Either (EitherT, left, runEitherT, sequenceEither)


data TypeError l a
  = UnificationError (Type l, a) (Type l, a)
  | UndeclaredType TypeName a
  | BadConstructorName Constructor TypeName (Decl l) a
  | BadConstructorArity Constructor (Decl l) Int a
  | BadPatternArity Constructor (Type l) Int Int a
  | BadPatternConstructor Constructor a
  | InferenceError a

deriving instance (Eq l, Eq (Value l), Eq a) => Eq (TypeError l a)
deriving instance (Show l, Show (Value l), Show a) => Show (TypeError l a)
deriving instance (Ord l, Ord (Value l), Ord a) => Ord (TypeError l a)


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
typeCheckAll =
  -- for each declaration,
  --   - generate constraints and assumptions
  --   - build up new global set of constraints from the assumptions
  --   - solve them all at once
  --   - substitute them all at once
  --   - lower them all at once
  undefined

typeCheck :: Ground l => TypeDecls l -> Expr l a -> Either [TypeError l a] (Type l)
typeCheck decls =
  fmap extractType . typeTree decls

typeTree ::
     Ground l
  => TypeDecls l
  -> Expr l a
  -> Either [TypeError l a] (Expr l (Type l, a))
typeTree decls expr = do
  (expr', constraints, _assums) <- generateConstraints decls expr
  subs <- solveConstraints constraints
  let subbed = substitute expr' subs
  sequenceEither (fmap (\(i, a) -> fmap (,a) (first pure (lowerIType i))) subbed)

-- -----------------------------------------------------------------------------
-- Types

-- | 'IType l a' is a fixpoint of 'IVar a (TypeF l)'.
--
-- i.e. regular types, recursively extended with annotations and an
-- extra constructor, 'IDunno', representing fresh type/unification variables.
newtype IType l a = I (IVar a (TypeF l (IType l a)))
  deriving (Eq, Ord, Show)

-- | 'IVar' is an open functor equivalent to an annotated 'Either Int'.
data IVar ann a
  = Dunno ann Int
  | Am ann a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Lift a known type into an 'IType', with an annotation.
hoistType :: a -> Type l -> IType l a
hoistType a (Type ty) =
  I (Am a (fmap (hoistType a) ty))

-- | Assert that we have a monotype. Returns 'InferenceError' if we
-- encounter a unification variable.
lowerIType :: IType l a -> Either (TypeError l a) (Type l)
lowerIType (I v) =
  case v of
    Dunno a _ ->
      Left (InferenceError a)
    Am _ ty ->
      fmap Type (traverse lowerIType ty)

typeVar :: IType l a -> Maybe Int
typeVar ty =
  case ty of
    I (Dunno _ x) ->
      pure x
    I (Am _ _) ->
      Nothing

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
flattenIType i@(I v) =
  (flattenIType' i,
    case v of
      Dunno a _ ->
        a
      Am a _ ->
        a)

flattenIType' :: IType l a -> Type l
flattenIType' (I v) =
  case v of
    Dunno _ x ->
      TVar (dunnoTypeVar x)

    Am _ ty ->
      Type (fmap flattenIType' ty)

-- | Report a unification error.
unificationError :: IType l a -> IType l a -> TypeError l a
unificationError =
  UnificationError `on` flattenIType

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

-- -----------------------------------------------------------------------------
-- Name supply

-- | Supply of fresh unification variables.
newtype NameSupply = NameSupply { nextVar :: Int }
  deriving (Eq, Ord, Show)

emptyNameSupply :: NameSupply
emptyNameSupply =
  NameSupply 0

-- | Grab a fresh type variable.
freshTypeVar :: a -> Check l a (IType l a)
freshTypeVar a =
  Check . lift $ do
    v <- gets (nextVar . sSupply)
    modify' (\s -> s { sSupply = NameSupply (v + 1) })
    return (I (Dunno a v))

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
      in pure (ELit (hoistType a ty, a) v)

    EVar a v -> do
      -- We introduce a new type variable representing the type of this expression.
      -- Add it to the assumption set.
      t <- freshTypeVar a
      addAssumption v t
      pure (EVar (t, a) v)

    ELam a n ta e -> do
      -- Proceed bottom-up, generating constraints for 'e'.
      -- Gather the assumed types of 'n', and constrain them to be the known (annotated) type.
      -- This expression's type is an arrow from the known type to the inferred type of 'e'.
      (as, e') <- withBinding n (generateConstraints' decls e)
      for_ as (addConstraint . Equal (hoistType a ta))
      let ty = I (Am a (TArrowF (hoistType a ta) (extractType e')))
      pure (ELam (ty, a) n ta e')

    EApp a f g -> do
      -- Proceed bottom-up, generating constraints for 'f' and 'g'.
      -- Introduce a new type variable for the result of the expression.
      -- Constrain 'f' to be an arrow from the type of 'g' to this type.
      f' <- generateConstraints' decls f
      g' <- generateConstraints' decls g
      t <- freshTypeVar a
      addConstraint (Equal (I (Am a (TArrowF (extractType g') t))) (extractType f'))
      pure (EApp (t, a) f' g')

    EList a te es -> do
      -- Proceed bottom-up, inferring types for each expression in the list.
      -- Constrain each type to be the annotated 'ty'.
      es' <- for es (generateConstraints' decls)
      for_ es' (addConstraint . Equal (hoistType a te) . extractType)
      let ty = I (Am a (TListF (hoistType a te)))
      pure (EList (ty, a) te es')

    ECon a c tn es ->
      case lookupType tn decls of
        Just ty@(DVariant cns) -> do
          -- Look up the constructor, check its arity, and introduce
          -- constraints for each of its subterms, for which we expect certain types.
          ts <- maybe (throwError (BadConstructorName c tn ty a)) pure (L.lookup c cns)
          unless (length ts == length es) (throwError (BadConstructorArity c ty (length es) a))
          es' <- for es (generateConstraints' decls)
          for_ (L.zip (fmap (hoistType a) ts) (fmap extractType es'))
            (\(expected, inferred) -> addConstraint (Equal expected inferred))
          let ty' = I (Am a (TVarF tn))
          pure (ECon (ty', a) c tn es')

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

    EForeign a n ty -> do
      -- We know the type of foreign expressions immediately, because they're annotated.
      pure (EForeign (hoistType a ty, a) n ty)

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
          let ty' = I (Am a (TVarF tn))
          addConstraint (Equal ty' ty)
          pats' <- for (L.zip (fmap (hoistType a) ts) pats) (uncurry (patternConstraints decls))
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

substitute :: Ground l => Expr l (IType l a, a) -> Substitutions l a -> Expr l (IType l a, a)
substitute expr subs =
  with expr $ \(ty, a) ->
    (substituteType subs ty, a)

substituteType :: Ground l => Substitutions l a -> IType l a -> IType l a
substituteType subs ty =
  case ty of
    I (Dunno _ x) ->
      maybe ty (substituteType subs) (M.lookup x (unSubstitutions subs))

    I (Am a (TArrowF t1 t2)) ->
      I (Am a (TArrowF (substituteType subs t1) (substituteType subs t2)))

    I (Am a (TListF t)) ->
      I (Am a (TListF (substituteType subs t)))

    I (Am _ (TLitF _)) ->
      ty

    I (Am _ (TVarF _)) ->
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
  -> ST s (Either (TypeError l a) ())
mostGeneralUnifierST points t1 t2 =
  runEitherT (mguST points t1 t2)

-- FIX need occurs check
mguST ::
     Ground l
  => STRef s (Points s l a)
  -> IType l a
  -> IType l a
  -> EitherT (TypeError l a) (ST s) ()
mguST points t1 t2 =
  case (t1, t2) of
    (I (Dunno _ x), _) -> do
      mty <- lift (getRepr points x)
      case mty of
        Just ty ->
          if typeVar ty == Just x then lift (union points t1 t2)
          else mguST points ty t2
        Nothing ->
          lift (union points t1 t2)

    (_, I (Dunno _ x)) -> do
      mty <- lift (getRepr points x)
      case mty of
        Just ty ->
          if typeVar ty == Just x then lift (union points t2 t1)
          else mguST points t1 ty
        Nothing ->
          lift (union points t2 t1)

    (I (Am _ (TVarF x)), I (Am _ (TVarF y))) ->
      unless (x == y) (left (unificationError t1 t2))

    (I (Am _ (TLitF x)), I (Am _ (TLitF y))) ->
      unless (x == y) (left (unificationError t1 t2))

    (I (Am _ (TArrowF f g)), I (Am _ (TArrowF h i))) -> do
      mguST points f h
      mguST points g i

    (I (Am _ (TListF a)), I (Am _ (TListF b))) ->
      mguST points a b

    (_, _) ->
      left (unificationError t1 t2)

solveConstraints :: Ground l => Traversable f => f (Constraint l a) -> Either [TypeError l a] (Substitutions l a)
solveConstraints constraints =
  runST $ do
    -- Initialise mutable state.
    points <- ST.newSTRef (Points M.empty)

    -- Solve all the constraints independently.
    es <- fmap sequenceEither . for constraints $ \c ->
      case c of
        Equal t1 t2 ->
          fmap (first D.singleton) (mostGeneralUnifierST points t1 t2)

    -- Retrieve the remaining points and produce a substitution map
    solvedPoints <- ST.readSTRef points
    for (first D.toList es) $ \_ -> do
      fmap Substitutions (for (unPoints solvedPoints) (UF.descriptor <=< UF.repr))

union :: STRef s (Points s l a) -> IType l a -> IType l a -> ST s ()
union points t1 t2 = do
  p1 <- getPoint points t1
  p2 <- getPoint points t2
  UF.union p1 p2

-- | Fills the 'lookup' API hole in the union-find package.
getPoint :: STRef s (Points s l a) -> IType l a -> ST s (UF.Point s (IType l a))
getPoint mref ty =
  case ty of
    I (Dunno _ x) -> do
      ps <- ST.readSTRef mref
      case M.lookup x (unPoints ps) of
        Just point ->
          pure point
        Nothing -> do
          point <- UF.fresh ty
          ST.modifySTRef' mref (Points . M.insert x point . unPoints)
          pure point

    I (Am _ _) ->
      UF.fresh ty
{-# INLINE getPoint #-}

getRepr :: STRef s (Points s l a) -> Int -> ST s (Maybe (IType l a))
getRepr points x = do
  ps <- ST.readSTRef points
  for (M.lookup x (unPoints ps)) (UF.descriptor <=< UF.repr)
