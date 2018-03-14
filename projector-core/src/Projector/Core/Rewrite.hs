{- | User-supplied rewrite rules -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Projector.Core.Rewrite (
    RewriteRule (..)
  , rewrite
  , rewriteFix
  -- Fine-grained control
  , rewriteT
  , applyRules
  , applyRule
  ) where


import           Data.Functor.Identity (Identity(..), runIdentity)

import           P

import           Projector.Core.Syntax
import           Projector.Core.Type

import           Control.Monad.Trans.Fix (FixT (..))
import qualified Control.Monad.Trans.Fix as U


data RewriteRule l a
  = Rewrite (Expr l a -> Maybe (Expr l a))


rewrite :: Ground l => [RewriteRule l a] -> Expr l a -> Expr l a
rewrite rules =
  runIdentity . U.once . rewriteT rules

rewriteFix :: Ground l => [RewriteRule l a] -> Expr l a -> Expr l a
rewriteFix rules =
  runIdentity . U.fixpoint (rewriteT rules)

-- | Apply a single rewrite rule to a single expr, nonrecursively.
applyRule :: Monad m => Expr l a -> RewriteRule l a -> FixT m (Expr l a)
applyRule g (Rewrite f) =
  maybe (pure g) U.progress (f g)
{-# INLINE applyRule #-}

-- | Apply a set of rewrite rules to a single expr, nonrecursively.
applyRules :: Monad m => Expr l a -> [RewriteRule l a] -> FixT m (Expr l a)
applyRules =
  foldM applyRule
{-# INLINE applyRules #-}

rewriteT :: (Ground l, Monad m) => [RewriteRule l a] -> Expr l a -> FixT m (Expr l a)
rewriteT rules expr =
  case expr of
    ELit _ _ ->
      applyRules expr rules
    EVar _ _ ->
      applyRules expr rules
    EForeign _ _ _ ->
      applyRules expr rules
    ELam a n t e -> do
      e' <- rewriteT rules e
      applyRules (ELam a n t e') rules
    EApp a e1 e2 -> do
      e1' <- rewriteT rules e1
      e2' <- rewriteT rules e2
      applyRules (EApp a e1' e2') rules
    ECon a c t es -> do
      es' <- traverse (rewriteT rules) es
      applyRules (ECon a c t es') rules
    ECase a e pes -> do
      e' <- rewriteT rules e
      pes' <- for pes (\(p, ex) -> fmap (p,) (rewriteT rules ex))
      applyRules (ECase a e' pes') rules
    ERec a tn fes -> do
      fes' <- traverse (traverse (rewriteT rules)) fes
      applyRules (ERec a tn fes') rules
    EPrj a e fn -> do
      e' <- rewriteT rules e
      applyRules (EPrj a e' fn) rules
    EList a es -> do
      es' <- traverse (rewriteT rules) es
      applyRules (EList a es') rules
    EMap a e1 e2 -> do
      e1' <- rewriteT rules e1
      e2' <- rewriteT rules e2
      applyRules (EMap a e1' e2') rules
    EHole _ ->
      applyRules expr rules
