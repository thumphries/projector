{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Backend (
    Backend (..)
  , checkModule
  , runPredicates
  ) where


import qualified Data.Map.Strict as M

import           P

import           Projector.Core
import           Projector.Html.Data.Backend
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim

-- -----------------------------------------------------------------------------
-- Per-backend warnings and linting

checkModule :: Backend a e -> Module HtmlType PrimT b -> Either [e] ()
checkModule b m =
  case predModule (predicates b) m of
    PredOk ->
      pure ()
    PredError es ->
      Left es

predModule :: [Predicate e] -> Module HtmlType PrimT b -> PredResult [e]
predModule preds =
  fmap fold . predResults . fmap (runPredicates preds . snd) . M.elems . moduleExprs

runPredicates :: [Predicate e] -> HtmlExpr a -> PredResult [e]
runPredicates preds expr =
  predResults . with preds $ \pred ->
    case pred of
      ExprPredicate p ->
        foldrExpr (runPredicate . p) (const id) PredOk expr
      PatPredicate p ->
        foldrExpr (const id) (runPredicate . p) PredOk expr

runPredicate :: PredResult e -> PredResult e -> PredResult e
runPredicate r k =
  case r of
    PredError _ ->
      r
    PredOk ->
      k
{-# INLINE runPredicate #-}

predResults :: [PredResult e] -> PredResult [e]
predResults =
  foldr (withPredError (:) (:[]) id) PredOk

withPredError :: (a -> b -> c) -> (a -> c) -> (b -> c) -> PredResult a -> PredResult b -> PredResult c
withPredError bin un1 un2 r1 r2 =
  case (r1, r2) of
    (PredError a, PredError b) ->
      PredError (bin a b)
    (PredError a, PredOk) ->
      PredError (un1 a)
    (PredOk, PredError b) ->
      PredError (un2 b)
    (PredOk, PredOk) ->
      PredOk
