{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Projector.Core.Syntax (
    Expr (..)
  , Name (..)
  , Pattern (..)
  -- * Smart/lazy constructors
  , lam
  , lam_
  , var_
  -- * pattern constructors
  , pvar_
  , pcon_
  ) where


import           P

import           Projector.Core.Type (Type (..), Ground (..), Constructor (..))


-- | The type of Projector expressions.
--
-- The first type parameter, 'l', refers to the type of literal. This is
-- invariant. Literals must have a 'Ground' instance.
data Expr l
  = ELit (Value l)
  | EVar Name
  | ELam Name (Type l) (Expr l)
  | EApp (Expr l) (Expr l)
  | ECon Constructor (Type l) [Expr l]
  | ECase (Expr l) [(Pattern, Expr l)]
  | EList [Expr l]

deriving instance (Eq l, Eq (Value l)) => Eq (Expr l)
deriving instance (Show l, Show (Value l)) => Show (Expr l)
deriving instance (Ord l, Ord (Value l)) => Ord (Expr l)

newtype Name = Name { unName :: Text }
  deriving (Eq, Ord, Show)

-- | Pattern matching. Note that these are necessarily recursive.
data Pattern
  = PVar Name
  | PCon Constructor [Pattern]
  deriving (Eq, Ord, Show)


lam :: Name -> Type l -> Expr l -> Expr l
lam n ty =
  ELam n ty

-- lazy
lam_ :: Text -> Type l -> Expr l -> Expr l
lam_ n =
  lam (Name n)

var_ :: Text -> Expr l
var_ =
  EVar . Name

pvar_ :: Text -> Pattern
pvar_ =
  PVar . Name

pcon_ :: Text -> [Pattern] -> Pattern
pcon_ =
  PCon . Constructor
