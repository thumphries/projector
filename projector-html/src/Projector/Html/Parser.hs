{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Parser (
    parse
  , ParseError (..)
  ) where


import           P

import           Projector.Html.Data.Position
import           Projector.Html.Data.Template
import           Projector.Html.Data.Token

import           System.IO  (FilePath)

import qualified Text.Megaparsec as P


data ParseError
  = ParseError (P.ParseError (P.Token [Positioned Token]) P.Dec)

parse :: FilePath -> [Positioned Token] -> Either ParseError (Template Range)
parse file =
  first ParseError . P.runParser (template <* P.eof) file

-- -----------------------------------------------------------------------------

type Parser = P.Parsec P.Dec [Positioned Token]

template :: Parser (Template Range)
template =
  undefined
