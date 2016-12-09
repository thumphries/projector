{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Parser.QQ (
    template
  ) where



import qualified Data.Text as T

import           P

import qualified Prelude

import           Projector.Html.Parser  (parse)

import           Language.Haskell.TH  (Loc (..), location)
import           Language.Haskell.TH.Quote  (QuasiQuoter)

import           X.Language.Haskell.TH  (dataExp, qparse)


template :: QuasiQuoter
template =
  qparse $ \s -> do
    loc <- location
    case parse (loc_filename loc) (T.pack s) of
      Left e ->
        Prelude.error $ "Failed to parse quasi quoter: " <> show e
      Right t ->
        dataExp t
