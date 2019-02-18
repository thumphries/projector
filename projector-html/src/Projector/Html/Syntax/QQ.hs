{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Projector.Html.Syntax.QQ (
    template
  ) where


import           Data.Data (Data)
import           Data.Generics (extQ)
import qualified Data.Text as T

import           Projector.Core.Prelude

import qualified Prelude

import           Projector.Html.Syntax (templateFromText)

import           Language.Haskell.TH  (Q, Exp, ExpQ, Loc (..), Lit (..), location, litE, varE, appE)
import           Language.Haskell.TH.Quote  (QuasiQuoter (..), dataToExpQ)

template :: QuasiQuoter
template =
  qparse $ \s -> do
    loc <- location
    case templateFromText (loc_filename loc) (T.pack s) of
      Left e ->
        Prelude.error $ "Failed to parse quasi quoter: " <> show e
      Right t ->
        dataExp t

dataExp :: Data a => a -> Q Exp
dataExp a = dataToExpQ (const Nothing `extQ` textExp) a

textExp :: T.Text -> Maybe ExpQ
textExp = pure . appE (varE 'T.pack) . litE . StringL . T.unpack

qparse :: ([Char] -> Q Exp) -> QuasiQuoter
qparse parse = QuasiQuoter {
    quoteExp    = parse
  , quotePat    = Prelude.error "not able to qq pats"
  , quoteType   = Prelude.error "not able to qq types"
  , quoteDec    = Prelude.error "not able to qq decs"
  }
