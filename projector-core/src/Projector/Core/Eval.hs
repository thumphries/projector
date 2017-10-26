{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.Eval (
  -- * Normalising terms
    whnf
  , nf
  , substitute
  -- * Fine-grained control
  , Eval
  , runEval
  , EvalState (..)
  , whnf'
  , whnf''
  , nf'
  , nf''
  , beta
  , eta
  , match
  , subst
  -- ** X
  , fixpoint'
  ) where


import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.State (State, runState)
import qualified Control.Monad.Trans.State as State

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S

import           P

import           Projector.Core.Match
import           Projector.Core.Syntax
import           Projector.Core.Type (Constructor (..), TypeName (..))

import           Umami.Monad.FixT (FixT (..))
import qualified Umami.Monad.FixT as U


-- -----------------------------------------------------------------------------

-- | Reduce to weak head normal form with an initial set of substitutions.
whnf :: Map Name (Expr l a) -> Expr l a -> Expr l a
whnf bnds =
  fst . runEval (EvalState 0) . (whnf' <=< substAll bnds)

-- | Reduce to beta-eta normal form with an initial set of substitutions.
nf :: Map Name (Expr l a) -> Expr l a -> Expr l a
nf bnds =
  fst . runEval (EvalState 0) . (nf' <=< substAll bnds)

-- | Safely substitute into an expression.
substitute :: Map Name (Expr l a) -> Expr l a -> Expr l a
substitute bnds =
  fst . runEval (EvalState 0) . substAll bnds

-- -----------------------------------------------------------------------------

newtype Eval l a b = Eval {
    unEval :: State (EvalState l a) b
  } deriving (Functor, Applicative, Monad)

data EvalState l a = EvalState {
    esSupply :: Int
  } deriving (Eq, Ord, Show)

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
  U.once . whnf''

whnf'' :: Expr l a -> FixT (Eval l a) (Expr l a)
whnf'' =
  fixpoint' (beta >=> eta)

-- | Apply 'beta' and 'eta' everywhere until they can no longer be applied.
-- This includes reducing under abstractions.
nf' :: Expr l a -> Eval l a (Expr l a)
nf' =
  U.once . nf'' whnf''

nf'' :: (Expr l a -> FixT (Eval l a) (Expr l a)) -> Expr l a -> FixT (Eval l a) (Expr l a)
nf'' whnf''' expr = do
  expr' <- whnf''' expr
  case expr' of
    ELam a x ty f ->
      ELam a x ty <$> nf'' whnf''' f
    ECon a c tn es ->
      ECon a c tn <$> traverse (nf'' whnf''') es
    ECase a e pes ->
      ECase a <$> nf'' whnf''' e <*> traverse (traverse (nf'' whnf''')) pes
    ERec a tn fes ->
      ERec a tn <$> traverse (traverse (nf'' whnf''')) fes
    EPrj a e fn ->
      EPrj a <$> nf'' whnf''' e <*> pure fn
    EList a es ->
      EList a <$> traverse (nf'' whnf''') es
    EMap a f g ->
      EMap a <$> nf'' whnf''' f <*> nf'' whnf''' g
    EApp a f g ->
      EApp a <$> nf'' whnf''' f <*> nf'' whnf''' g

    -- uninteresting cases:
    ELit _ _ ->
      pure expr'
    EVar _ _ ->
      pure expr'
    EForeign _ _ _ ->
      pure expr'
    EHole _ ->
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

    -- some reducible cases
    ECase a e pes ->
      case match e pes of
        Just (subs, alt) ->
          lift (substAll subs alt) >>= U.progress
        Nothing ->
          -- try to get the scrutinee to a redex
          ECase a <$> beta e <*> pure pes

    -- projections out of records
    EPrj a e fn ->
      case e of
        ERec _a _tn fes ->
          case find ((== fn) . fst) fes of
            Just (_fn, f) ->
              U.progress f
            Nothing ->
              pure expr
        _ ->
          -- Try to get to a redex
          EPrj a <$> beta e <*> pure fn

    -- Not reducible:
    EVar _ _ ->
      pure expr
    ELam _ _ _ _ ->
      pure expr
    ELit _ _ ->
      pure expr
    EForeign _ _ _ ->
      pure expr
    ECon _ _ _ _ ->
      pure expr
    ERec _ _ _ ->
      pure expr
    EList _ _ ->
      pure expr
    EHole _ ->
      pure expr

match :: Expr l a -> [(Pattern a, Expr l a)] -> Maybe (Map Name (Expr l a), Expr l a)
match scrut pes =
  (,)
    <$> matchSubs (buildMatchTree (fmap fst pes)) scrut
    <*> asum (with pes $ \(pat, alt) -> matchesExpr scrut pat *> pure alt)

-- pattern matching
-- - build match tree
-- - traverse match tree recursively with scrutinee
-- - if scrutinee is Con, ok to match
-- - if scrutinee is not con, only ok to match early wildcards
-- - builds up a _set of substitutions_
matchSubs :: MatchTree -> Expr l a -> Maybe (Map Name (Expr l a))
matchSubs (MatchTree mt') expr' =
  go False mt' expr'
  where
    go _ ((Con c, nt) : tt) expr@(ECon _ d _ es) =
      -- Scrutinee is a con and pattern is a con.
      -- If constructors match and are well-formed, we bind!
      if c == d && length nt == length es
        then fmap fold (zipWithM (go False) (fmap unMatchTree nt) es)
        else go True tt expr

    go True ((Var x, _) : _) expr@(ECon _ _ _ _) =
      -- Scrutinee is a con, so it's always safe to bind
      pure (M.singleton x expr)

    -- Repeating the two cases above for records.
    go _ ((Con c, nt) : tt) expr@(ERec _ (TypeName tn) fes) =
      -- If constructors match and are well-formed, we bind!
      if c == (Constructor tn) && length nt == length fes
        then fmap fold (zipWithM (go False) (fmap unMatchTree nt) (fmap snd fes))
        else go True tt expr

    go True ((Var x, _) : _) expr@(ERec _ _ _) =
      -- Scrutinee is a record, so it's always safe to bind
      pure (M.singleton x expr)

    go False ((Var x, _) : _) y =
      -- We haven't falsified a Con pattern yet, so it's safe to bind anything to x
      pure (M.singleton x y)

    go _ _ _ =
      Nothing

-- this duplicates work done by matchSubs, but MatchTree doesn't tell
-- us which pattern it chose. MatchTree structure needs to be changed.
matchesExpr :: Expr l a -> Pattern a -> Maybe ()
matchesExpr expr pat =
  case (pat, expr) of
    (PVar _ _, _) ->
      pure ()
    (PCon _ c1 ps, ECon _ c2 _ es) ->
      if (c1 == c2) && (length ps == length es)
        then zipWithM_ matchesExpr es ps
        else empty
    (PCon _ (Constructor c1) ps, ERec _ (TypeName c2) fes) ->
      if (c1 == c2) && (length ps == length fes)
        then zipWithM_ matchesExpr (fmap snd fes) ps
        else empty
    (PCon _ _ _, _) ->
      empty
    (PWildcard _, _) ->
      pure ()

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
  U.fixpoint (subst' subs free) expr

substAll :: Map Name (Expr l a) -> Expr l a -> Eval l a (Expr l a)
substAll subs expr = do
  let free = S.fromList (M.keys subs) <> gatherFree expr
  U.fixpoint (subst' subs free) expr

-- | Batch substitution.
subst' :: Map Name (Expr l a) -> Set Name -> Expr l a -> FixT (Eval l a) (Expr l a)
subst' subs free expr =
  case expr of
    EVar a z ->
      mcase (M.lookup z subs) (pure (setAnnotation a expr)) (U.progress . setAnnotation a)

    ELam a z ty f ->
      if S.member z free
        then
          do z' <- lift (fresh z free)
             -- it might be safe to just add it to the subst map and proceed in one pass?
             -- worth a try once this implementation is demonstrably correct
             f' <- lift (subst z (EVar a z') f)
             r' <- subst' subs (S.insert z' free) f'
             U.progress (ELam a z' ty r')
        else ELam a z ty <$> subst' subs (S.insert z free) f

    ECase a e pes ->
      ECase a <$> subst' subs free e <*> traverse (uncurry (patSubst subs free)) pes

    -- plain ol recursion
    ECon a c tn es ->
      ECon a c tn <$> traverse (subst' subs free) es
    ERec a tn fes ->
      ERec a tn <$> traverse (traverse (subst' subs free)) fes
    EPrj a e fn ->
      EPrj a <$> subst' subs free e <*> pure fn
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
    EHole _ ->
      pure expr

patSubst :: Map Name (Expr l a) -> Set Name -> Pattern a -> Expr l a -> FixT (Eval l a) (Pattern a, Expr l a)
patSubst subs free pat expr = do
  (pat', subs') <- patFresh free pat
  let free' = free <> S.fromList (M.elems subs') <> S.fromList (M.keys subs')
  expr' <- subst' (fmap (EVar (extractPatternAnnotation pat)) subs' <> subs) free' expr
  pure (pat', expr')

patFresh :: Set Name -> Pattern a -> FixT (Eval l a) (Pattern a, Map Name Name)
patFresh free patt =
  case patt of
    PVar a n ->
      if S.member n free
        then
          do n' <- lift (fresh n free)
             U.progress (PVar a n', M.singleton n n')
        else pure (PVar a n, mempty)
    PCon a c pats -> do
      pats' <- traverse (patFresh free) pats
      pure (PCon a c (fmap fst pats'), foldMap snd pats')
    PWildcard a ->
      pure (PWildcard a, mempty)

fresh :: Name -> Set Name -> Eval l a Name
fresh n@(Name nn) free =
  if S.member n free
    then
      do i <- next
         fresh (Name (nn <> renderIntegral i)) free
    else pure n

-- | Like 'U.fixpoint', but stays inside FixT.
fixpoint' :: Monad m => (a -> FixT m a) -> a -> FixT m a
fixpoint' f a = do
  (a', prog) <- lift (U.runFixT $! f a)
  case prog of
    U.RunAgain -> do
      -- the whole expression has progress
      b' <- lift (U.fixpoint f a')
      U.progress b'
    U.NoProgress ->
      pure a'
{-# INLINE fixpoint' #-}
