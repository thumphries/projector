{-| Warnings for things like exhaustivity and shadowing -}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.Warn (
    Warning (..)
  , warnShadowing
  , warnExhaustivity
  ) where


import           Control.Monad.Trans.State (runState, modify')

import qualified Data.DList as D
import           Data.Set (Set)
import qualified Data.Set as S

import           P

import           Projector.Core.Match
import           Projector.Core.Syntax
import           Projector.Core.Type

import           X.Control.Monad.Trans.Either as XE


data Warning l a
  = ShadowedName a Name
  | InexhaustiveCase a [Constructor]
  | Invariant Text
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- -----------------------------------------------------------------------------
-- Shadowed variables

warnShadowing :: Set Name -> Expr l a -> Either [Warning l a] ()
warnShadowing bound expr =
  if null it then pure () else Left it
  where
    it = (reverse . snd) (runState (go bound expr) mempty)
    warn w = modify' (w:)
    go bound' expr' =
      case expr' of
        ELit _ _ ->
          pure ()
        EVar _ _ ->
          pure ()
        EForeign _ _ _ ->
          pure ()
        ELam a x _ e -> do
          when (S.member x bound') (warn (ShadowedName a x))
          go (S.insert x bound') e
        EApp _ f g -> do
          go bound' f
          go bound' g
        ECon _ _ _ es -> do
          traverse_ (go bound') es
        ECase a e pes -> do
          go bound' e
          for_ pes $ \(pat, alt) -> do
            let bnds = patternBinds pat
                shad = S.intersection bnds bound'
            when (shad /= mempty) (for_ (toList shad) (warn . ShadowedName a))
            go (bnds <> bound') alt
        ERec _ _ fes -> do
          traverse_ (go bound') (fmap snd fes)
        EPrj _ e _ ->
          go bound e
        EList _ es -> do
          traverse_ (go bound') es
        EMap _ f g -> do
          go bound' f
          go bound' g
        EHole _ ->
          pure ()

-- -----------------------------------------------------------------------------
-- Pattern exhaustivity

warnExhaustivity :: Ground l => TypeDecls l -> Expr l a -> Either [Warning l a] ()
warnExhaustivity decls =
  void . first D.toList . sequenceEither .
    foldrExpr
      (\e xs -> first D.singleton (checkExhaustivity decls e) : xs)
      (\_ xs -> xs)
      []

checkExhaustivity :: Ground l => TypeDecls l -> Expr l a -> Either (Warning l a) ()
checkExhaustivity decls expr =
  case expr of
    ECase a _ pes ->
      checkPatternExhaustivity a decls (fmap fst pes)
    _ ->
      pure ()

checkPatternExhaustivity :: Ground l => a -> TypeDecls l -> [Pattern a] -> Either (Warning l a) ()
checkPatternExhaustivity a decls pats =
  exhaustiveMatchTree a decls (buildMatchTree pats)

exhaustiveMatchTree :: Ground l => a -> TypeDecls l -> MatchTree -> Either (Warning l a) ()
exhaustiveMatchTree a decls mt@(MatchTree mtt) =
  -- If this tier contains a PVar, stop.
  mcase (matchTier mt) (pure ()) $ \seen -> do
    -- Otherwise, check the Constructor set
    checkSet a decls seen
    -- ... and recurse
    for_ mtt $ \(pat, subtrees) ->
      case pat of
        Con _ ->
          traverse_ (exhaustiveMatchTree a decls) subtrees
        Var _ ->
          pure ()

-- | Grab all the constructors used in this pattern tier.
-- Short-circuits and returns 'Nothing' if we encounter an exhaustive 'Var'.
matchTier :: MatchTree -> Maybe (Set Constructor)
matchTier (MatchTree mt) =
  foldlM
    (\set (pat, _) ->
      case pat of
        Con c ->
          pure (S.insert c set)
        Var _ ->
          Nothing)
    mempty
    mt

checkSet :: a -> TypeDecls l -> Set Constructor -> Either (Warning l a) ()
checkSet a decls seen =
  fromMaybe (Left (Invariant "BUG: Could not determine type (checkSet)")) $ do
    witness <- head seen
    (tn, _ps, _tys) <- lookupConstructor witness decls
    defn <- lookupType tn decls
    let missing = case defn of
          DVariant _ps cts ->
            S.difference (S.fromList (fmap fst cts)) seen
          DRecord _ps _ ->
            S.difference (S.singleton (Constructor (unTypeName tn))) seen
    pure $
      if missing == S.empty
        then pure ()
        else Left (InexhaustiveCase a (toList missing))
