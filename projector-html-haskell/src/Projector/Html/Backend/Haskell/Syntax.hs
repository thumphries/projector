{-| Some shorthand for constructing Haskell98-ish TH syntax trees.
    This is a defense mechanism against upstream template-haskell churn.
    The idea is to provide a stable bare-minimum interface protected by CPP,
    and use lenses to set/get the unstable fields. -}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Projector.Html.Backend.Haskell.Syntax (
  -- * Declarations
    data_
  , val_
  , fun_
  , sig
  -- ** Constructors
  , normalC
  , normalC_
  , normalC_'
  , recC
  , recC_
  , recC_'
  -- *** Strictness annotations
  , isStrict
  , notStrict
  -- * Types
  , conT
  , appT
  , arrowT
  , arrowT_
  , listT
  , listT_
  -- * Expressions
  , litE
  , varE
  , conE
  , recConE
  , lamE
  , appE
  , applyE
  , caseE
  , listE
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


import           Data.Char (Char)
import           Data.Foldable (foldl')
import           Data.Function ((.))
import           Data.Functor (Functor(..))
#if MIN_VERSION_template_haskell(2,11,0)
import           Data.Maybe (Maybe(..))
#endif
import           Data.Text (Text)
import qualified Data.Text as T

import           Language.Haskell.TH (Dec (..), Con (..), Type (..), Name, Exp (..), Match (..), Pat (..), Lit (..))
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as S


-- -----------------------------------------------------------------------------
-- Declarations

-- | Declare a simple datatype.
data_ ::
     Name -- ^ The declared type's name
  -> [Name] -- ^ Optional list of type parameters
  -> [Con] -- ^ Constructors
  -> Dec
data_ n ps cs =
#if MIN_VERSION_template_haskell(2,11,0)
  DataD [] n (fmap TH.PlainTV ps) Nothing cs []
#else
  DataD [] n (fmap TH.PlainTV ps) cs []
#endif

-- | Declare a simple function.
fun_ :: Name -> [Pat] -> Exp -> Dec
fun_ n ps e =
  FunD n [TH.Clause ps (TH.NormalB e) []]

-- | Declare a simple value.
val_ :: Pat -> Exp -> Dec
val_ p e =
  ValD p (TH.NormalB e) []

-- | A simple type signature declaration.
sig :: Name -> Type -> Dec
sig =
  SigD

-- -----------------------------------------------------------------------------
-- Constructors


-- | A regular constructor, with strict or nonstrict arguments.
-- Use this with 'isStrict' and 'notStrict', as the 'Strict' datatype
-- is a moving target.
normalC :: Name -> [(S.Strict, Type)] -> Con
normalC =
  NormalC

-- | A regular constructor, with nonstrict arguments.
normalC_ :: Name -> [Type] -> Con
normalC_ n =
  normalC n . fmap (notStrict,)

-- | A regular constructor, with strict arguments.
normalC_' :: Name -> [Type] -> Con
normalC_' n =
  normalC n . fmap (isStrict,)

-- | A record constructor, with strict or nonstrict fields.
-- Use this with 'isStrict' and 'notStrict', as the 'Strict' datatype
-- is a moving target.
recC :: Name -> [(Name, S.Strict, Type)] -> Con
recC =
  RecC

-- | A record constructor, with nonstrict fields.
recC_ :: Name -> [(Name, Type)] -> Con
recC_ n =
  recC n . fmap (\(fn, t) -> (fn, notStrict, t))

-- | A record constructor, with strict fields.
recC_' :: Name -> [(Name, Type)] -> Con
recC_' n =
  recC n . fmap (\(fn, t) -> (fn, isStrict, t))

-- | A strict strictness annotation.
isStrict :: S.Strict
#if MIN_VERSION_template_haskell(2,11,0)
isStrict = S.Bang S.NoSourceUnpackedness S.SourceStrict
#else
isStrict = S.IsStrict
#endif

-- | A nonstrict strictness annotation.
notStrict :: S.Strict
#if MIN_VERSION_template_haskell(2,11,0)
notStrict = S.Bang S.NoSourceUnpackedness S.NoSourceStrictness
#else
notStrict = S.NotStrict
#endif

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

listT :: Type
listT =
  ListT

listT_ :: Type -> Type
listT_ =
  appT listT

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

recConE :: Name -> [(Name, Exp)] -> Exp
recConE =
  RecConE

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

listE :: [Exp] -> Exp
listE =
  ListE

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
