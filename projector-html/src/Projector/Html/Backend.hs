{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Backend (
    Backend
  , BackendT (..)
  , getBackend
  , haskellBackend
  , purescriptBackend
  ) where


import           Projector.Html.Data.Backend
import qualified Projector.Html.Backend.Haskell as Hs
import qualified Projector.Html.Backend.Purescript as Purs


getBackend :: BackendT -> Backend a
getBackend b =
  case b of
    Haskell ->
      haskellBackend

    Purescript ->
      purescriptBackend

haskellBackend :: Backend a
haskellBackend =
  Backend {
      renderModule = Hs.renderModule
    , renderExpr = Hs.renderExpr
    }

purescriptBackend :: Backend a
purescriptBackend =
  Backend {
      renderModule = Purs.renderModule
    , renderExpr = Purs.renderExpr
    }
