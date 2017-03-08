{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Syntax.Lexer.Layout (
    layout
  ) where


import qualified Data.List as L

import           P

import           Projector.Html.Data.Position
import           Projector.Html.Syntax.Token


layout :: [Positioned Token] -> [Positioned Token]
layout =
  applyLayout mempty

-- -----------------------------------------------------------------------------

data Scope =
    ExplicitParen
  | ExplicitBrace
  | ExplicitSquare
  deriving (Eq, Ord, Show)

close :: Scope -> [Scope] -> ([Token], [Scope])
close to ps =
  (fmap closeScope (L.takeWhile (/= to) ps <> [to]), drop 1 (L.dropWhile (/= to) ps))

closeScope :: Scope -> Token
closeScope p =
  case p of
    ExplicitParen ->
      ExprRParen
    ExplicitBrace ->
      ExprEnd
    ExplicitSquare ->
      ExprListEnd

applyLayout :: [Scope] -> [Positioned Token] -> [Positioned Token]

applyLayout ps (x@(ExprLParen :@ _) : xs) =
  x : applyLayout (ExplicitParen : ps) xs

applyLayout (ExplicitParen : ps) (x@(ExprRParen :@ _) : xs) =
  x : applyLayout ps xs

applyLayout ps (x@(ExprListStart :@ _) : xs) =
  x : applyLayout (ExplicitSquare : ps) xs

applyLayout (ExplicitSquare : ps) (x@(ExprListEnd :@ _) : xs) =
  x : applyLayout ps xs

applyLayout ps (x@(ExprStart :@ _) : xs) =
  x : applyLayout (ExplicitBrace : ps) xs

applyLayout ps (x@(ExprEnd :@ a) : xs) =
  let (closes, paren) = close ExplicitBrace ps in
  fmap (:@ a) closes <> (x : applyLayout paren xs)

applyLayout ps (x:xs) =
  x : applyLayout ps xs

applyLayout _ [] =
  []
