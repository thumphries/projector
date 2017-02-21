{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.Eval (
  -- * Normalising terms
    whnf
  , nf
  -- * Fine-grained control
  , Eval
  , runEval
  , EvalState (..)
  , whnf'
  , nf'
  , beta
  , eta
  , subst
  ) where


import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.State (State, runState)
import qualified Control.Monad.Trans.State as State

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S

import           P

import           Projector.Core.Syntax

import           Umami.Monad.FixT (FixT (..))
import qualified Umami.Monad.FixT as U


-- -----------------------------------------------------------------------------

-- | Reduce to weak head normal form with an initial set of substitutions.
whnf :: Map Name (Expr l a) -> Expr l a -> Expr l a
whnf bnds expr =
  fst . runEval (EvalState 0) $ do
    expr' <- substAll bnds expr
    whnf' expr'

-- | Reduce to beta-eta normal form with an initial set of substitutions.
nf :: Map Name (Expr l a) -> Expr l a -> Expr l a
nf bnds expr =
  fst . runEval (EvalState 0) $ do
    expr' <- substAll bnds expr
    nf' expr'

-- -----------------------------------------------------------------------------

newtype Eval l a b = Eval {
    unEval :: State (EvalState l a) b
  } deriving (Functor, Applicative, Monad)

data EvalState l a = EvalState {
    esSupply :: Int
  }

runEval :: EvalState l a -> Eval l a b -> (b, EvalState l a)
runEval =
  flip (runState . unEval)

next :: Eval l a Int
next =
  Eval $ do
    v <- State.gets esSupply
    State.modify' (\s -> s { esSupply = v + 1 })
    pure v

-- -----------------------------------------------------------------------------

-- | Apply 'beta' and 'eta' until in weak head normal form, i.e. not outwardly reducible.
whnf' :: Expr l a -> Eval l a (Expr l a)
whnf' =
  U.fixpoint (beta >=> eta)

-- | Apply 'beta' and 'eta' everywhere until they can no longer be applied.
-- This includes reducing under abstractions.
nf' :: Expr l a -> Eval l a (Expr l a)
nf' expr = do
  expr' <- whnf' expr
  case expr' of
    ELam a x ty f ->
      ELam a x ty <$> nf' f
    ECon a c tn es ->
      ECon a c tn <$> traverse nf' es
    ECase a e pes ->
      ECase a <$> nf' e <*> traverse (traverse nf') pes
    EList a es ->
      EList a <$> traverse nf' es
    EMap a f g ->
      EMap a <$> nf' f <*> nf' g
    EApp a f g ->
      EApp a <$> nf' f <*> nf' g

    -- uninteresting cases:
    ELit _ _ ->
      pure expr'
    EVar _ _ ->
      pure expr'
    EForeign _ _ _ ->
      pure expr'

-- | Beta reduction. Evaluate one step.
beta :: Expr l a -> FixT (Eval l a) (Expr l a)
beta expr =
  case expr of
    -- applications of lambdas
    EApp a e1 e2 ->
      case e1 of
        (ELam _ x _ f) -> do
          -- Substitute e2 for x in the lambda body
          expr' <- lift (subst x e2 f)
          U.progress (setAnnotation a expr')
        _ ->
          -- Try to make e1 reducible
          EApp a <$> beta e1 <*> pure e2

    -- maps of lambdas over lists
    EMap a e1 e2 ->
      case (e1, e2) of
        (ELam _ x _ f, EList _ es) -> do
          -- Apply the function to every element in the list
          es' <- traverse (\e -> lift (subst x e f)) es
          U.progress (EList a es')
        _ ->
          -- Try to get to a redex
          EMap a <$> beta e1 <*> beta e2

    -- case of constructor
    ECase _a _e _ps ->
      -- work to do here!
      -- need to be careful with this - don't take the wrong branch
      -- this requires pattern splitting, etc, to do correctly.
      pure expr

    -- Not reducible:
    ELam _ _ _ _ ->
      pure expr
    ELit _ _ ->
      pure expr
    EVar _ _ ->
      -- We substitute early and eagerly, not here.
      pure expr
    EForeign _ _ _ ->
      pure expr
    ECon _ _ _ _ ->
      pure expr
    EList _ _ ->
      pure expr


-- | Eta reduction. Eliminate redundant lambda abstractions. Runs only one step.
eta :: Expr l a -> FixT (Eval l a) (Expr l a)
eta expr =
  case expr of
    ELam a x _ (EApp _ f (EVar _ y)) ->
      if x == y && not (S.member x (gatherFree f))
        then U.progress (setAnnotation a f)
        else pure expr
    _ ->
      pure expr

-- | Substitute a variable into an expression, freshening bound names where necessary.
subst :: Name -> Expr l a -> Expr l a -> Eval l a (Expr l a)
subst x y expr = do
  let free = S.singleton x <> gatherFree y <> gatherFree expr -- this is expensive and bad
      subs = M.singleton x y
  subst' subs free expr

substAll :: Map Name (Expr l a) -> Expr l a -> Eval l a (Expr l a)
substAll subs =
  let free = foldMap gatherFree subs
  in subst' subs free

-- | Batch substitution.
subst' :: Map Name (Expr l a) -> Set Name -> Expr l a -> Eval l a (Expr l a)
subst' subs free expr =
  case expr of
    EVar a z ->
      pure (mcase (M.lookup z subs) expr (setAnnotation a))

    ELam a z ty f ->
      if S.member z free
        then
          do z' <- fresh z free
             -- it might be safe to just add it to the subst map and proceed in one pass?
             -- worth a try once this implementation is demonstrably correct
             f' <- subst z (EVar a z') f
             ELam a z' ty <$> subst' subs free f'
        else ELam a z ty <$> subst' subs free f

    ECase a e pes ->
      ECase a <$> subst' subs free e <*> traverse (uncurry (patSubst subs free)) pes

    -- plain ol recursion
    ECon a c tn es ->
      ECon a c tn <$> traverse (subst' subs free) es
    EList a es ->
      EList a <$> traverse (subst' subs free) es
    EApp a f g ->
      EApp a <$> subst' subs free f <*> subst' subs free g
    EMap a f g ->
      EMap a <$> subst' subs free f <*> subst' subs free g

    -- uninteresting cases:
    ELit _ _ ->
      pure expr
    EForeign _ _ _ ->
      pure expr

patSubst :: Map Name (Expr l a) -> Set Name -> Pattern a -> Expr l a -> Eval l a (Pattern a, Expr l a)
patSubst subs free pat expr = do
  (pat', subs') <- patFresh free pat
  let free' = free <> S.fromList (M.elems subs') <> S.fromList (M.keys subs')
  expr' <- subst' (fmap (EVar (extractPatternAnnotation pat)) subs' <> subs) free' expr
  pure (pat', expr')

patFresh :: Set Name -> Pattern a -> Eval l a (Pattern a, Map Name Name)
patFresh free patt =
  case patt of
    PVar a n ->
      if S.member n free
        then
          do n' <- fresh n free
             pure (PVar a n', M.singleton n n')
        else pure (PVar a n, mempty)
    PCon a c pats -> do
      pats' <- traverse (patFresh free) pats
      pure (PCon a c (fmap fst pats'), foldMap snd pats')

fresh :: Name -> Set Name -> Eval l a Name
fresh n@(Name nn) free =
  if S.member n free
    then
      do i <- next
         fresh (Name (nn <> renderIntegral i)) free
    else pure n
