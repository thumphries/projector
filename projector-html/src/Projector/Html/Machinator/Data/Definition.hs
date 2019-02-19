{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Machinator.Data.Definition (
  -- * Serialisation
    DefinitionFile (..)
  , Definition (..)
  , DefinitionFileGraph (..)
  -- * Datatype types
  , Name (..)
  , Type (..)
  , Ground (..)
  , groundToName
  , groundFromName
  , DataType (..)
  -- * Traversals etc
  , free
  ) where


import           Data.Data (Data, Typeable)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import qualified Data.Set as S

import           GHC.Generics (Generic)

import           Projector.Core.Prelude

import           System.IO  (FilePath)


-- | A set of type definitions from a given file.
data DefinitionFile
  = DefinitionFile FilePath [Definition]
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | A single data definition.
data Definition = Definition {
     defName :: Name
   , defType :: DataType
   } deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | The module graph.
-- Maps each file to the other files it depends on.
newtype DefinitionFileGraph = DefinitionFileGraph {
    unDefinitionFileGraph :: Map FilePath (Set FilePath)
  } deriving (Eq, Ord, Show, Monoid, Data, Typeable, Generic)


-- -----------------------------------------------------------------------------

-- | The name of a type.
newtype Name = Name {
    unName :: Text
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Types.
data Type
  = Variable Name
  | GroundT Ground
  | ListT Type
  | MaybeT Type
  | EitherT Type Type
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Ground types, e.g. platform primitives.
data Ground
  = StringT
  | BoolT
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Obtain the stringy form for a ground type.
groundToName :: Ground -> Name
groundToName g =
  case g of
    StringT ->
      Name "String"
    BoolT ->
      Name "Bool"

-- | Obtain the ground type for a stringy name.
groundFromName :: Alternative f => Name -> f Ground
groundFromName n =
  case unName n of
    "String" ->
      pure StringT
    "Bool" ->
      pure BoolT
    _ ->
      empty

-- | Declarable datatypes, e.g. sums or records.
data DataType
  = Variant (NonEmpty (Name, [Type]))
  | Record [(Name, Type)]
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- -----------------------------------------------------------------------------

free :: DataType -> Set Name
free d =
  case d of
    Variant nts ->
      fold . with nts $ \(_, ts) ->
        S.fromList . join . with ts $ freeInType
    Record fts ->
      S.fromList . join . with fts $ \(_, t) -> (freeInType t)

freeInType :: Type -> [Name]
freeInType t =
  case t of
    Variable n ->
      pure n
    GroundT _ ->
      empty
    ListT lt ->
      freeInType lt
    MaybeT a ->
      freeInType a
    EitherT a b ->
      freeInType a <> freeInType b
