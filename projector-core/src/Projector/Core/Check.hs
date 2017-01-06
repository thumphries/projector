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
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as R
import           Control.Monad.Trans.State.Strict (State, evalState, gets, modify')

import           Data.DList (DList)
import qualified Data.DList as D
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
  | FreeTypeVariable TypeName a
  | BadConstructorName Constructor TypeName (Decl l) a
  | BadConstructorArity Constructor (Decl l) Int a
  | BadPatternArity Constructor (Type l) Int Int a
  | BadPatternConstructor Constructor (Type l) a
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
    unCheck :: EitherT (DList (TypeError l a)) (ReaderT (Env l a) (State (SolverState l a))) b
  } deriving (Functor, Applicative, Monad)

runCheck :: Check l a b -> Either [TypeError l a] b
runCheck f =
    unCheck f
  & runEitherT
  & flip runReaderT mempty
  & flip evalState initialSolverState
  & first D.toList

local :: (Env l a -> Env l a) -> Check l a b -> Check l a b
local f =
  Check . mapEitherT (R.local f) . unCheck

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
  Check . lift . lift $ do
    v <- gets (nextVar . sSupply)
    modify' (\s -> s { sSupply = NameSupply (v + 1) })
    return (I (IDunno a v))

-- -----------------------------------------------------------------------------
-- Env

-- | Known types for local bindings.
newtype Env l a = Env { unEnv :: Map Name (IType l a) }
  deriving (Eq, Ord, Show, Monoid)

eBind :: Name -> IType l a -> Env l a -> Env l a
eBind n ty =
  Env . M.insert n ty . unEnv

eLookup :: Name -> Env l a -> Maybe (IType l a)
eLookup n =
  M.lookup n . unEnv

-- | Record a known type in a lexical scope.
know :: Name -> IType l a -> Check l a b -> Check l a b
know n ty k =
  local (eBind n ty) k

-- -----------------------------------------------------------------------------
-- Constraints

data Constraint l a
  = Equal (IType l a) (IType l a)
  deriving (Eq, Ord, Show)

-- | Record a new constraint.
addConstraint :: Ground l => Constraint l a -> Check l a ()
addConstraint c =
  Check . lift . lift $
    modify' (\s -> s { sConstraints = D.snoc (sConstraints s) c })

-- -----------------------------------------------------------------------------
-- Assumptions

-- | Add an assumed type for some variable we've encountered.
addAssumption :: Ground l => Ord a => Name -> IType l a -> Check l a ()
addAssumption n ty =
  Check . lift . lift $
    modify' (\s -> s { sAssumptions = M.insertWith (<>) n (S.singleton ty) (sAssumptions s)})

-- | Delete all assumptions for some variable.
--
-- This is called when leaving the lexical scope in which the variable was bound.
deleteAssumptions :: Ground l => Name -> Check l a ()
deleteAssumptions n =
  Check . lift . lift $
    modify' (\s -> s { sAssumptions = M.delete n (sAssumptions s)})

-- | Look up all assumptions for a given name. Returns the empty set if there are none.
lookupAssumptions :: Ground l => Ord a => Name -> Check l a (Set (IType l a))
lookupAssumptions n =
  Check . lift . lift $
    fmap (fromMaybe mempty) (gets (M.lookup n . sAssumptions))

-- -----------------------------------------------------------------------------

extractType :: Expr l (Type l, a) -> Type l
extractType =
  fst . extractAnnotation
