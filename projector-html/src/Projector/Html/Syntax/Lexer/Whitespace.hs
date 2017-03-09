{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Syntax.Lexer.Whitespace (
    deindent
  ) where


import           P

import           Projector.Html.Data.Position
import           Projector.Html.Syntax.Token


-- | Eliminates unimportant leading whitespace, creating indent/dedent tokens.
deindent :: [Positioned Token] -> [Positioned Token]
deindent =
  deindent'' []

newtype IndentLevel = IndentLevel {
    _unIndentLevel :: Int
  } deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------

deindent'' :: [IndentLevel] -> [Positioned Token] -> [Positioned Token]
deindent'' il (Whitespace x :@ b : xs) =
  newline il b x xs
deindent'' il xs =
  deindent' il xs

deindent' :: [IndentLevel] -> [Positioned Token] -> [Positioned Token]

deindent' il (n@(Newline :@ _) : (Whitespace x :@ b) : xs) =
  n : newline il b x xs

deindent' il (n@(Newline :@ _) : xs@(_ :@ b : _)) =
  n : newline il b 0 xs

deindent' il (x : xs) =
  x : deindent' il xs

deindent' _ [] =
  []

-- -----------------------------------------------------------------------------


-- | Given a new indent level, insert indent or dedent tokens
-- accordingly, then continue with deindent.
newline :: [IndentLevel] -> Range -> Int -> [Positioned Token] -> [Positioned Token]

newline iis@(IndentLevel i : is) a x xs
  | i == x =
    deindent' iis xs

  | i > x =
    (Dedent :@ a) : newline is a x xs

  | otherwise {- i < x -} =
    (Indent (x - i) :@ a) : deindent' (IndentLevel x : iis) xs

newline [] a x xs
  | x == 0 =
    deindent' [] xs

  | otherwise =
    (Indent x :@ a) : deindent' [IndentLevel x] xs
