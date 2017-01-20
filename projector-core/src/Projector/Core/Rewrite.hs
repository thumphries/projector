{- | User-supplied rewrite rules -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.Rewrite (
    RewriteRule (..)
  , rewrite
  , rewriteFix
  , applyRule
  , applyRules
  ) where


import           P

import           Projector.Core.Syntax
import           Projector.Core.Type


data RewriteRule l a
  = Rewrite (Expr l a -> Maybe (Expr l a))

-- | Apply a single rewrite rule to a single expr, nonrecursively.
applyRule :: Ground l => Expr l a -> RewriteRule l a -> Expr l a
applyRule g (Rewrite f) =
  -- This is about as stupid as it gets.
  fromMaybe g (f g)
{-# INLINE applyRule #-}

-- | Apply a set of rewrite rules to a single expr, nonrecursively.
applyRules :: Ground l => Expr l a -> [RewriteRule l a] -> Expr l a
applyRules =
  foldl' applyRule
{-# INLINE applyRules #-}

-- This scales poorly with the number of rules provided. If/when
-- performance of this code is an issue, we should fuse the rules
-- together into a trie of sorts.

-- | Perform a bottom-up single pass over an expr with a set of rewrite rules.
rewrite :: Ground l => [RewriteRule l a] -> Expr l a -> Expr l a
rewrite rules expr =
  case expr of
    ELit _ _ ->
      applyRules expr rules
    EVar _ _ ->
      applyRules expr rules
    EForeign _ _ _ ->
      applyRules expr rules
    ELam a n t e ->
      applyRules (ELam a n t (rewrite rules e)) rules
    EApp a e1 e2 ->
      applyRules (EApp a (rewrite rules e1) (rewrite rules e2)) rules
    ECon a c t es ->
      applyRules (ECon a c t (fmap (rewrite rules) es)) rules
    ECase a e pes ->
      applyRules (ECase a (rewrite rules e) (fmap (fmap (rewrite rules)) pes)) rules
    EList a t es ->
      applyRules (EList a t (fmap (rewrite rules) es)) rules

-- | Call 'rewrite' until the expression stops changing.
-- This assumes that your rule system is terminating.
rewriteFix :: Ground l => [RewriteRule l a] -> Expr l a -> Expr l a
rewriteFix rules expr =
  let expr' = rewrite rules expr
  in if fmap (const ()) expr' /= fmap (const ()) expr then rewriteFix rules expr' else expr
