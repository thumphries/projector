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
  ) where


import           P

import           Projector.Html.Data.Backend
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

runPredicates :: [Predicate a e] -> Expr l a -> PredResult [e]
runPredicates =
  undefined
