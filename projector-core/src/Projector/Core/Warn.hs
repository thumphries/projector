{-| Warnings for things like exhaustivity and shadowing -}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.Warn (
    Warning (..)
  , warnShadowing
  ) where


import           Control.Monad.Trans.State (runState, modify')

import           Data.Set (Set)
import qualified Data.Set as S

import           P

import           Projector.Core.Syntax


data Warning l a
  = ShadowedName a Name
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

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
        EList _ es -> do
          traverse_ (go bound') es
        EMap _ f g -> do
          go bound' f
          go bound' g
