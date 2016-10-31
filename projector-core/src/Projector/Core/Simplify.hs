{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.Simplify (
    nf
  , whnf
  , alphaNf
  , alpha
  ) where


import           Control.Monad.Trans.State (State, evalState, get, put)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           P

import           Projector.Core.Syntax (Expr (..), Name (..))



-- | Reduce an expression to weak head normal form, i.e. to the outermost abstraction.
--
-- This is O(N), buyer beware.
whnf :: Expr l -> Expr l
whnf =
  whnf' mempty . alpha

-- | Unsafe version of 'whnf'. This substitutes into abstractions and may lead to capture.
-- This is only safe when all bound names are unique. It is also O(N).
whnf' :: Map Name (Expr l) -> Expr l -> Expr l
whnf' ctx expr = case expr of
  ELit _ ->
    expr

  EVar x ->
    fromMaybe expr (M.lookup x ctx)

  ELam x ty e ->
    -- need to finish substituting, even though we're done.
    -- this means this is quite inefficient.
    ELam x ty (whnf'' ctx e)

  EApp f g ->
    case whnf' ctx f of
      (ELam x _ e) ->
        whnf' (M.insert x (whnf' ctx g) ctx) e

      f' ->
        -- Ill-typed term
        EApp f' g

-- propagate substitutions around :(
whnf'' :: Map Name (Expr l) -> Expr l -> Expr l
whnf'' ctx expr =
  case expr of
    ELit _ ->
      expr

    EVar x ->
      fromMaybe expr (M.lookup x ctx)

    ELam x ty e ->
      -- need to finish substituting, even though we're done.
      -- this means this is quite inefficient.
      ELam x ty (whnf'' ctx e)

    EApp f g ->
      EApp (whnf'' ctx f) (whnf'' ctx g)


-- | Reduce an expression to beta normal form.
nf :: Expr l -> Expr l
nf =
  nf' mempty . alpha

-- | Unsafe version of 'nf'. This substitutes into abstractions and may lead to capture.
-- This is only safe when all bound names are unique.
nf' :: Map Name (Expr l) -> Expr l -> Expr l
nf' ctx expr =
 case expr of
  ELit _ ->
    expr

  EVar x ->
    fromMaybe expr (M.lookup x ctx)

  ELam n ty e ->
    ELam n ty (nf' ctx e) -- wince here

  EApp f g ->
    case nf' ctx f of
      (ELam n _ e) ->
        nf' (M.insert n (nf' ctx g) ctx) e

      f' ->
        -- Ill-typed term
        EApp f' (nf' ctx g)

-- | Alpha normalisation.
--
-- This replaces all bound names with something in the format "x_1234".
-- If you care about the original names, use 'alpha'.
alphaNf :: Expr l -> Expr l
alphaNf expr =
  evalState (alpha' mempty (const (Name "x")) expr) mempty

-- | Alpha conversion.
--
-- This is very heavy-handed. A name is allowed to be bound once only.
-- All other bindings are suffixed with a number.
alpha :: Expr l -> Expr l
alpha expr =
  evalState (alpha' mempty id expr) mempty

-- | Records every name ever bound in the program, freshifying every reuse.
--
-- TODO: Should probably add a first pass to collect/protect free variables.
alpha' :: Map Name Name -> (Name -> Name) -> Expr l -> State (Map Name Int) (Expr l)
alpha' rebinds rename expr =
  case expr of
    ELit _ ->
      pure expr

    EVar x ->
      pure (EVar (fromMaybe x (M.lookup x rebinds)))

    ELam x ty e -> do
      new <- freshen (rename x)
      ELam new ty <$> alpha' (M.insert x new rebinds) rename e

    EApp f g ->
      EApp <$> alpha' rebinds rename f <*> alpha' rebinds rename g

-- | Given some name, modify it such that it is unique, and perform the bookkeeping.
freshen :: Name -> State (Map Name Int) Name
freshen m@(Name n) = do
  used <- get
  case M.lookup m used of
    Nothing -> do -- never used
      put (M.insert m 0 used)
      pure m

    Just k -> do -- has been used before
      put (M.insert m (k+1) used)
      let name = Name (n <> suffix (k+1))
      -- need to make sure no pesky humans have used this name before
      case M.lookup name used of
        Nothing -> -- we're good
          pure name

        Just _ -> -- someone's using awful names
          freshen name

-- The suffix used for the nth binding of some variable.
suffix :: Int -> Text
suffix n =
  "_" <> renderIntegral n
