-- | Some shorthand for constructing Haskell98-ish TH syntax trees.
-- This is a defense mechanism against upstream template-haskell churn.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Projector.Html.Backend.Haskell.TH (
  -- * Declarations
    data_
  , Deriving
  , TypeParams
  -- ** Constructors
  , normalC
  , normalC_
  , normalC_'
  -- * Types
  , conT
  , appT
  , arrowT
  , arrowT_
  -- * Expressions
  , litE
  , varE
  , conE
  , lamE
  , appE
  , applyE
  , caseE
  -- * Patterns
  , varP
  , conP
  -- * Matches
  , match
  , match_
  -- * Literals
  , stringL
  , stringL_
  -- * Names
  , TH.mkName
  , mkName_
  ) where


import qualified Data.Text as T

import           Language.Haskell.TH (Dec (..), Con (..), Type (..), Name, Exp (..), Match (..), Pat (..), Lit (..))
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as S

import           P


-- Everything below is as of template-haskell-2.10
-- Upgraders will need to CPP everything in this module, likely.

-- -----------------------------------------------------------------------------
-- Declarations

data_ :: Name -> TypeParams -> [Con] -> Deriving -> Dec
data_ n ps cs ds =
  DataD [] n (fmap TH.PlainTV ps) cs ds

-- Maybe newtype these?
type Deriving = [Name]
type TypeParams = [Name]

-- -----------------------------------------------------------------------------
-- Constructors

normalC :: Name -> [(S.Strict, Type)] -> Con
normalC =
  NormalC

-- | A regular constructor, with nonstrict arguments.
normalC_ :: Name -> [Type] -> Con
normalC_ n =
  normalC n . fmap (S.NotStrict,)

-- | A regular constructor, with strict arguments.
normalC_' :: Name -> [Type] -> Con
normalC_' n =
  normalC n . fmap (S.IsStrict,)

-- -----------------------------------------------------------------------------
-- Types

conT :: Name -> Type
conT =
  ConT

appT :: Type -> Type -> Type
appT =
  AppT

arrowT :: Type
arrowT =
  ArrowT

arrowT_ :: Type -> Type -> Type
arrowT_ t =
  appT (appT arrowT t)

-- -----------------------------------------------------------------------------
-- Expressions

litE :: Lit -> Exp
litE =
  LitE

varE :: Name -> Exp
varE =
  VarE

conE :: Name -> Exp
conE =
  ConE

lamE :: [Pat] -> Exp -> Exp
lamE =
  LamE

appE :: Exp -> Exp -> Exp
appE =
  AppE

-- | Left-biased function application.
applyE :: Exp -> [Exp] -> Exp
applyE =
  foldl' appE

caseE :: Exp -> [Match] -> Exp
caseE =
  CaseE

-- -----------------------------------------------------------------------------
-- Patterns

varP :: Name -> Pat
varP =
  VarP

conP :: Name -> [Pat] -> Pat
conP =
  ConP

-- -----------------------------------------------------------------------------
-- Matches

-- | Construct a pattern match with no where clause.
match :: Pat -> TH.Body -> Match
match p b =
  Match p b []

-- | Construct an unguarded pattern match with no where clause.
match_ :: Pat -> Exp -> Match
match_ p e =
  match p (TH.NormalB e)

-- -----------------------------------------------------------------------------
-- Literals

stringL :: [Char] -> Lit
stringL =
  StringL

stringL_ :: Text -> Lit
stringL_ =
  stringL . T.unpack

-- -----------------------------------------------------------------------------
-- Names

mkName_ :: Text -> TH.Name
mkName_ =
  TH.mkName . T.unpack
