{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Machinator (
  -- * Versioning
    MachinatorVersion (..)
  , Versioned (..)
  -- * Errors
  , MachinatorError (..)
  , renderMachinatorError
  -- * Datatypes
  , DefinitionFile (..)
  , Definition (..)
  , DefinitionFileGraph (..)
  , buildFileGraph
  -- * Syntax
  , parseDefinitionFile
  , Pretty.ppDefinitionFile
  ) where


import           Projector.Html.Machinator.Data.Definition (DefinitionFile (..), Definition (..), DefinitionFileGraph (..))
import           Projector.Html.Machinator.Data.Version (MachinatorVersion (..), Versioned (..))
import           Projector.Html.Machinator.Graph (buildFileGraph)
import qualified Projector.Html.Machinator.Lexer as Lexer
import qualified Projector.Html.Machinator.Parser as Parser
import qualified Projector.Html.Machinator.Pretty as Pretty

import           Projector.Core.Prelude

import           System.IO (FilePath)


data MachinatorError
  = MParseError Parser.ParseError
  | MLexError Lexer.LexError
  deriving (Eq, Ord, Show)

renderMachinatorError :: MachinatorError -> Text
renderMachinatorError me =
  case me of
    MParseError pe ->
      "Parse error: " <> Parser.renderParseError pe
    MLexError le ->
      "Lexical error: " <> Lexer.renderLexError le


-- | Lex and parse a definition file from a 'Text' value.
--
-- The 'FilePath' is for error reporting only.
parseDefinitionFile :: FilePath -> Text -> Either MachinatorError (Versioned DefinitionFile)
parseDefinitionFile file t =
  first MLexError (Lexer.lexVersioned file t) >>= first MParseError . Parser.parseDefinitionFile file
