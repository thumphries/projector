{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
module Projector.Core.Check (
    typeCheck
  , typeTree
  ) where


import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.State.Strict (State, runState, gets, modify')

import           Data.DList (DList)
import qualified Data.DList as D
import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S

import           P

import           Projector.Core.Syntax
import           Projector.Core.Type

import           X.Control.Monad.Trans.Either


data TypeError l a
  = Mismatch (Type l) (Type l) a
  | FreeVariable Name a
  | UndeclaredType TypeName a
  | BadConstructorName Constructor TypeName (Decl l) a
  | BadConstructorArity Constructor (Decl l) Int a
  | BadPatternArity Constructor (Type l) Int Int a
  | BadPatternConstructor Constructor a
  | NonExhaustiveCase (Expr l a) (Type l) a
  | InferenceError a

deriving instance (Eq l, Eq (Value l), Eq a) => Eq (TypeError l a)
deriving instance (Show l, Show (Value l), Show a) => Show (TypeError l a)
deriving instance (Ord l, Ord (Value l), Ord a) => Ord (TypeError l a)


typeCheck :: Ground l => TypeDecls l -> Expr l a -> Either [TypeError l a] (Type l)
typeCheck decls =
  fmap extractType . typeTree decls

typeTree ::
     Ground l
  => TypeDecls l
  -> Expr l a
  -> Either [TypeError l a] (Expr l (Type l, a))
typeTree =
  undefined

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
  = IDunno ann Int
  | IAm ann a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Lift a known type into an 'IType', with an annotation.
hoistType :: a -> Type l -> IType l a
hoistType a (Type ty) =
  I (IAm a (fmap (hoistType a) ty))

-- | Assert that we have a monotype. Returns 'InferenceError' if we
-- encounter a unification variable.
lowerIType :: IType l a -> Either (TypeError l a) (Type l)
lowerIType (I v) =
  case v of
    IDunno a _ ->
      Left (InferenceError a)
    IAm _ ty ->
      fmap Type (traverse lowerIType ty)

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
  , sAssumptions :: Map Name (Set (IType l a))
  , sSupply :: NameSupply
  } deriving (Eq, Ord, Show)

initialSolverState :: SolverState l a
initialSolverState =
  SolverState {
      sConstraints = mempty
    , sAssumptions = mempty
    , sSupply = emptyNameSupply
    }

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
    return (I (IDunno a v))

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
addAssumption :: Ground l => Ord a => Name -> IType l a -> Check l a ()
addAssumption n ty =
  Check . lift $
    modify' (\s -> s { sAssumptions = M.insertWith (<>) n (S.singleton ty) (sAssumptions s)})

-- | Delete all assumptions for some variable.
--
-- This is called when leaving the lexical scope in which the variable was bound.
deleteAssumptions :: Ground l => Name -> Check l a ()
deleteAssumptions n =
  Check . lift $
    modify' (\s -> s { sAssumptions = M.delete n (sAssumptions s)})

-- | Look up all assumptions for a given name. Returns the empty set if there are none.
lookupAssumptions :: Ground l => Ord a => Name -> Check l a (Set (IType l a))
lookupAssumptions n =
  Check . lift $
    fmap (fromMaybe mempty) (gets (M.lookup n . sAssumptions))

-- -----------------------------------------------------------------------------
-- Constraint generation

generateConstraints :: Ground l => Ord a => TypeDecls l -> Expr l a -> Either [TypeError l a] (Expr l (IType l a, a), [Constraint l a])
generateConstraints decls expr = do
  (fmap (second (D.toList . sConstraints)) (runCheck (generateConstraints' decls expr)))

generateConstraints' :: Ground l => Ord a => TypeDecls l -> Expr l a -> Check l a (Expr l (IType l a, a))
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
      e' <- generateConstraints' decls e
      as <- lookupAssumptions n
      deleteAssumptions n
      for_ (S.toList as) (addConstraint . Equal (hoistType a ta))
      let ty = I (IAm a (TArrowF (hoistType a ta) (extractType e')))
      pure (ELam (ty, a) n ta e')

    EApp a f g -> do
      -- Proceed bottom-up, generating constraints for 'f' and 'g'.
      -- Introduce a new type variable for the result of the expression.
      -- Constrain 'f' to be an arrow from the type of 'g' to this type.
      f' <- generateConstraints' decls f
      g' <- generateConstraints' decls g
      t <- freshTypeVar a
      addConstraint (Equal (I (IAm a (TArrowF (extractType g') t))) (extractType f'))
      pure (EApp (t, a) f' g')

    EList a te es -> do
      -- Proceed bottom-up, inferring types for each expression in the list.
      -- Constrain each type to be the annotated 'ty'.
      es' <- for es (generateConstraints' decls)
      for_ es' (addConstraint . Equal (hoistType a te) . extractType)
      let ty = I (IAm a (TListF (hoistType a te)))
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
          let ty' = I (IAm a (TVarF tn))
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
        pe' <- generateConstraints' decls pe
        addConstraint (Equal ty (extractType pe'))
        pat' <- patternConstraints decls (extractType e') pat
        pure (pat', pe')
      pure (ECase (ty, a) e' pes')

    EForeign a n ty -> do
      -- We know the type of foreign expressions immediately, because they're annotated.
      pure (EForeign (hoistType a ty, a) n ty)

-- | Patterns are binding sites that also introduce lots of new constraints.
patternConstraints :: Ground l => Ord a => TypeDecls l -> IType l a -> Pattern a -> Check l a (Pattern (IType l a, a))
patternConstraints decls ty pat =
  case pat of
    PVar a x -> do
      as <- lookupAssumptions x
      for_ as (addConstraint . Equal ty)
      deleteAssumptions x
      pure (PVar (ty, a) x)

    PCon a c pats ->
      case lookupConstructor c decls of
        Just (tn, ts) -> do
          unless (length ts == length pats)
            (throwError (BadPatternArity c (TVar tn) (length ts) (length pats) a))
          let ty' = I (IAm a (TVarF tn))
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
