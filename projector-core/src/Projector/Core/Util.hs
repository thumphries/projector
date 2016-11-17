{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.Util (
    apE
  , listE
  , listE_
  ) where


import qualified Data.DList as D

import           P


-- | A version of 'ap' that accumulates errors.
apE :: Monoid e => Either e (a -> b) -> Either e a -> Either e b
apE l r =
  case (l, r) of
    (Right f, Right a) ->
      pure (f a)
    (Left a, Right _) ->
      Left a
    (Right _, Left b) ->
      Left b
    (Left a, Left b) ->
      Left (a <> b)

-- | Sequence errors from a list of expressions.
listE :: Monoid e => [Either e a] -> Either e [a]
listE =
  fmap D.toList . foldl' (apE . fmap D.snoc) (pure mempty)

listE_ :: Monoid e => [Either e a] -> Either e ()
listE_ =
  void . listE
