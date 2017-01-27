{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Backend (
    Backend
  , BackendT (..)
  , getBackend
  , BackendError (..)
  , renderBackendError
  , haskellBackend
  , purescriptBackend
  , checkModule
  ) where


import qualified Data.Map.Strict as M

import           P

import           Projector.Core
import           Projector.Html.Data.Backend
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim
import qualified Projector.Html.Backend.Haskell as Hs
import qualified Projector.Html.Backend.Purescript as Purs


data BackendError
  = HaskellBackendError Hs.HaskellError
  | PurescriptBackendError Purs.PurescriptError
  deriving (Eq, Ord, Show)

renderBackendError :: BackendError -> Text
renderBackendError e =
  case e of
    HaskellBackendError he ->
      Hs.renderHaskellError he
    PurescriptBackendError pe ->
      Purs.renderPurescriptError pe

getBackend :: BackendT -> Backend a BackendError
getBackend b =
  case b of
    Haskell ->
      fmap HaskellBackendError haskellBackend

    Purescript ->
      fmap PurescriptBackendError purescriptBackend

haskellBackend :: Backend a Hs.HaskellError
haskellBackend =
  Backend {
      renderModule = Hs.renderModule
    , renderExpr = Hs.renderExpr
    , predicates = Hs.predicates
    }

purescriptBackend :: Backend a Purs.PurescriptError
purescriptBackend =
  Backend {
      renderModule = Purs.renderModule
    , renderExpr = Purs.renderExpr
    , predicates = Purs.predicates
    }

-- -----------------------------------------------------------------------------
-- Per-backend warnings and linting

checkModule :: Backend a e -> Module HtmlType a -> Either [e] ()
checkModule b m =
  case predModule (predicates b) m of
    PredOk ->
      pure ()
    PredError es ->
      Left es

predModule :: [Predicate a e] -> Module HtmlType a -> PredResult [e]
predModule preds =
  fmap fold . predResults . fmap (runPredicates preds . snd) . M.elems . moduleExprs

runPredicates :: [Predicate a e] -> HtmlExpr a -> PredResult [e]
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
