{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Projector.Core.Simplify (
    nf
  , whnf
  ) where


import           Bound (Scope, toScope, fromScope)
import           Bound.Name (instantiate1Name)

import           P

import           Projector.Core.Syntax (Expr (..))


-- | Reduce an expression to weak head normal form, i.e. to the outermost abstraction.
whnf :: Expr l n a -> Expr l n a
whnf e = case e of
  ELit _ ->
    e

  EVar _ ->
    e

  ELam _ _ ->
    e

  EApp f a -> case whnf f of
    (ELam _ b) ->
      -- instantiate1Name enters a scope, instantiating its outermost bound variable.
      -- instantiate1Name :: Monad f => f a -> Scope n f a -> f a
      -- instantiate1Name :: Expr l a -> Scope () (Expr l) a -> Expr l a
      whnf (instantiate1Name a b)
    g ->
      -- Ill-typed term
      EApp g a

-- | Reduce an expression to normal form.
nf :: Expr l n a -> Expr l n a
nf e = case e of
  ELit _ ->
    e

  EVar _ ->
    e

  ELam t b ->
    ELam t (overScope nf b)

  EApp f a ->
    case whnf f of
      (ELam _ b) ->
        nf (instantiate1Name a b)
      g ->
        -- Ill-typed term
        EApp (nf g) (nf a)

-- | Apply a function under binders. This translates the expression
-- under a 'Scope' back into an 'Expr' for a while.
overScope ::
     (forall c. Expr l n c -> Expr l n c)
  -> Scope b (Expr l n) a
  -> Scope b (Expr l n) a
overScope f = toScope . f . fromScope
