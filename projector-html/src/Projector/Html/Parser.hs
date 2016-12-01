{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Projector.Html.Parser (
    parse
  , ParseError (..)
  ) where


import qualified Data.Set as S
import qualified Data.Text as T

import           P

import           Projector.Html.Data.Position
import           Projector.Html.Data.Template
import           Projector.Html.Data.Token

import           System.IO  (FilePath)

import qualified Text.Megaparsec as P


newtype ParseError = ParseError {
    unParseError :: (P.ParseError (Positioned Token) ParseErrorComponent)
  } deriving (Show)

parse :: FilePath -> [Positioned Token] -> Either ParseError (Template Range)
parse file =
  first ParseError . P.runParser (template <* P.eof) file . TokenStream

-- -----------------------------------------------------------------------------

type Parser = P.Parsec ParseErrorComponent TokenStream

newtype TokenStream = TokenStream {
    unTokenStream :: [Positioned Token]
  } deriving (Show, Eq, Ord)

instance P.Stream TokenStream where
  type Token TokenStream = Positioned Token
  {-# INLINE uncons #-}
  uncons ts =
    case unTokenStream ts of
      [] ->
        Nothing
      (x:xs) ->
        Just (x, TokenStream xs)
  {-# INLINE updatePos #-}
  updatePos _ _ _ (_ :@ (Range p1 p2)) =
    (positionPos p1, positionPos p2)

positionPos :: Position -> P.SourcePos
positionPos (Position x y f) =
  P.SourcePos f (P.unsafePos (fromIntegral x)) (P.unsafePos (fromIntegral y))
{-# INLINE positionPos #-}


data ParseErrorComponent
  = ParseRuleError Text
  | ParseIndentError Ordering P.Pos P.Pos
  deriving (Eq, Ord, Show)

instance P.ErrorComponent ParseErrorComponent where
  representFail =
    ParseRuleError . T.pack
  representIndentation =
    ParseIndentError

failWith :: ParseErrorComponent -> Parser a
failWith err =
  P.failure S.empty S.empty (S.singleton err)

-- -----------------------------------------------------------------------------

template :: Parser (Template Range)
template =
  undefined
