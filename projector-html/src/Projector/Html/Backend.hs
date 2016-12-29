{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Backend (
    BackendT (..)
  , getBackend
  , Backend (..)
  , haskellBackend
  , purescriptBackend
  ) where


import           P

import           Projector.Core  (Name)
import           Projector.Html.Backend.Data
import qualified Projector.Html.Backend.Haskell as Hs
import qualified Projector.Html.Backend.Purescript as Purs
import           Projector.Html.Core

import           System.IO  (FilePath)


data BackendT
  = Haskell
  | Purescript
  deriving (Eq, Ord, Show)

getBackend :: BackendT -> Backend a
getBackend b =
  case b of
    Haskell ->
      haskellBackend

    Purescript ->
      purescriptBackend

-- -----------------------------------------------------------------------------

data Backend a = Backend {
    renderModule :: ModuleName -> Module a -> (FilePath, Text)
  , renderExpr :: Name -> HtmlExpr a -> Text
  }

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
