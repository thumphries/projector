{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Projector.Html.Machinator.Parser (
    parseDefinitionFile
  , ParseError (..)
  , renderParseError
  ) where


import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as S
import qualified Data.Text as T

import           Projector.Html.Machinator.Data.Definition
import           Projector.Html.Machinator.Data.Position
import           Projector.Html.Machinator.Data.Token as MT
import           Projector.Html.Machinator.Data.Version

import           Projector.Core.Prelude

import           System.IO (FilePath)

import qualified Text.Megaparsec as M


data ParseError = ParseError Text
  deriving (Eq, Ord, Show)

renderParseError :: ParseError -> Text
renderParseError e =
  case e of
    ParseError t ->
      t

-- | Pure parser for definition files.
--
-- The 'FilePath' is for error reporting.
parseDefinitionFile :: FilePath -> Versioned [Positioned Token] -> Either ParseError (Versioned DefinitionFile)
parseDefinitionFile file (Versioned v ts) =
  first (ParseError . T.pack . show) (M.runParser (parseVersioned file v <* M.eof) file (TokenStream ts))


-- -----------------------------------------------------------------------------

type Parser = M.Parsec ParseErrorComponent TokenStream

newtype TokenStream = TokenStream {
    unTokenStream :: [Positioned Token]
  } deriving (Eq, Ord, Show)

instance M.Stream TokenStream where
  type Token TokenStream = Positioned MT.Token

  {-# INLINE uncons #-}
  uncons ts =
    case unTokenStream ts of
      [] -> Nothing
      (pt : rest) -> Just (pt, TokenStream rest)

  {-# INLINE updatePos #-}
  updatePos _ _ l (_ :@ b) =
    (l, positionPos (rangeStart b))

data ParseErrorComponent
  = ParseFail Text
  | ParseBadIndent Ordering M.Pos M.Pos
  | FeatureGuard MachinatorVersion MachinatorFeature
  deriving (Eq, Ord, Show)

instance M.ErrorComponent ParseErrorComponent where
  representFail = ParseFail . T.pack
  representIndentation = ParseBadIndent

parseError :: ParseErrorComponent -> Parser a
parseError e =
  M.failure S.empty S.empty (S.singleton e)

-- -----------------------------------------------------------------------------

{-

-- machinator @ v1

data Foo = Bar String | Baz String

data Bap = Bip Foo

record Quux = {
    a : Foo
  , b : Bap
  , c : Quux
  }

record Quib = {
    a : Foo
  , b : Bap
  , c : Quib
  }

-}

parseVersioned :: FilePath -> MachinatorVersion -> Parser (Versioned DefinitionFile)
parseVersioned file v = do
  ds <- many (definition v)
  pure (Versioned v (DefinitionFile file ds))

definition :: MachinatorVersion -> Parser Definition
definition v =
      record v
  <|> variant v


variant :: MachinatorVersion -> Parser Definition
variant v = do
  hasFeature v HasVariants
  M.try (token TData)
  x <- ident
  token TEquals
  cs <- sepBy1 (alternative v) (token TChoice)
  pure (Definition x (Variant cs))

alternative :: MachinatorVersion -> Parser (Name, [Type])
alternative v = do
  name <- ident
  ts <- many (types v)
  pure (name, ts)

record :: MachinatorVersion -> Parser Definition
record v = do
  hasFeature v HasRecords
  M.try (token TRecord)
  x <- ident
  token TEquals
  token TLBrace
  fts <- sepBy (recordField v) (token TComma)
  token TRBrace
  pure (Definition x (Record fts))

recordField :: MachinatorVersion -> Parser (Name, Type)
recordField v = do
  name <- ident <|> dataAsIdent <|> recordAsIdent
  token TTypeSig
  ty <- types v
  pure (name, ty)

types :: MachinatorVersion -> Parser Type
types v = do
  optionalParens (types' v)

types' :: MachinatorVersion -> Parser Type
types' v = do
  x <- ident
  case x of
    Name "List" ->
      hasFeature v HasLists *> (ListT <$> types v)
    Name "Maybe" ->
      hasFeature v HasMaybe *> (MaybeT <$> types v)
    Name "Either" ->
      hasFeature v HasEither *> (EitherT <$> types v <*> types v)
    _ ->
      case groundFromName x of
        Just t ->
          case t of
            StringT ->
              hasFeature v HasStrings *> pure (GroundT t)
            BoolT ->
              hasFeature v HasBools *> pure (GroundT t)
        Nothing ->
          pure (Variable x)

parens :: Parser a -> Parser a
parens p =
  M.between (M.try (token TLParen)) (token TRParen) p

optionalParens :: Parser a -> Parser a
optionalParens p =
  M.try (parens p) <|> p

-- -----------------------------------------------------------------------------

hasFeature :: MachinatorVersion -> MachinatorFeature -> Parser ()
hasFeature v f =
  if featureEnabled v f then pure () else parseError (FeatureGuard v f)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy m sep =
  M.try (fmap toList (sepBy1 m sep)) <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser (NonEmpty a)
sepBy1 m sep = do
  a <- m
  bs <- many (M.try sep *> m)
  pure (a :| bs)

ident :: Parser Name
ident = do
  TIdent x <- satisfy (\case TIdent _ -> True; _ -> False)
  pure (Name x)

recordAsIdent :: Parser Name
recordAsIdent = do
  TRecord <- satisfy (\case TRecord -> True; _ -> False)
  pure (Name MT.recordKeyword)

dataAsIdent :: Parser Name
dataAsIdent = do
  TData <- satisfy (\case TData -> True; _ -> False)
  pure (Name MT.dataKeyword)

token :: Token -> Parser ()
token t =
  satisfy (== t) *> pure ()

satisfy :: (Token -> Bool) -> Parser Token
satisfy f = M.token testToken Nothing
  where
    testToken x@(a :@ _) =
      if f a
        then Right a
        else Left (S.singleton (M.Tokens (x:|[])), S.empty, S.empty)

positionPos :: Position -> M.SourcePos
positionPos (Position line col file) =
  M.SourcePos file (M.unsafePos (fromIntegral line)) (M.unsafePos (fromIntegral col))
