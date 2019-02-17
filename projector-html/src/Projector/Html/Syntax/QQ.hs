{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Syntax.QQ (
    template
  ) where



import qualified Data.Text as T

import           Projector.Core.Prelude

import qualified Prelude

import           Projector.Html.Syntax (templateFromText)

import           Language.Haskell.TH  (Loc (..), location)
import           Language.Haskell.TH.Quote  (QuasiQuoter)

import           X.Language.Haskell.TH  (dataExp, qparse)


template :: QuasiQuoter
template =
  qparse $ \s -> do
    loc <- location
    case templateFromText (loc_filename loc) (T.pack s) of
      Left e ->
        Prelude.error $ "Failed to parse quasi quoter: " <> show e
      Right t ->
        dataExp t
