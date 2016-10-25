{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Projector.Core.Check (
  -- * User interface
    typeCheck
  , TypeError (..)
  -- * Guts
  , Check (..)
  , typeError
  , typeCheck'
  , checkPair
  , checkList
  -- * Reusable stuff
  , apE
  , pairE
  ) where


import qualified Bound as B
import qualified Bound.Name as B

import           Data.DList (DList)
import qualified Data.DList as D

import           P

import           Projector.Core.Syntax (Expr (..))
import           Projector.Core.Type (Type (..), Ground (..), typeOf)


data TypeError l a
  = Mismatch (Type l) (Type l)
  | ExpectedArrow (Type l) (Type l)
  | FreeVariable a
  deriving (Eq, Show)

typeCheck ::
     Ground l
  => Expr l n a
  -> Either [TypeError l a] (Type l)
typeCheck =
  first D.toList . unCheck . typeCheck' . fmap Unknown


-- -----------------------------------------------------------------------------

newtype Check l n a = Check {
    unCheck :: Either (DList (TypeError l n)) a
  } deriving (Functor, Applicative, Monad)

data Annotated l a
  = Unknown a
  | Known (Type l)
  deriving (Eq, Show, Functor)

-- Mark all uses of the immediately-bound variable ('B') with its type.
annotate :: Type l -> B.Var (B.Name n ()) (Annotated l a) -> Annotated l a
annotate ty e =
  case e of
    B.B _ ->
      Known ty

    B.F a ->
      a

-- As we've got an explicitly-typed calculus, typechecking is
-- straightforward and syntax-directed. All we have to do is propagate
-- our type annotations around the tree and use (==) in the right spots.
typeCheck' ::
     Ground l
  => Expr l n (Annotated l a)
  -> Check l a (Type l)
typeCheck' expr =
  case expr of
    ELit v ->
      pure (TLit (typeOf v))

    EVar (Known ty) ->
      pure ty

    EVar (Unknown a) ->
      typeError (FreeVariable a)

    ELam ty e -> do
      tb <- typeCheck' (fmap (annotate ty) (B.fromScope e))
      pure (TArrow ty tb)

    EApp a b -> do
      typs <- checkPair a b
      case typs of
        (TArrow c d, e) ->
          if c == e then pure d else typeError (Mismatch c e)
        (c, d) ->
          typeError (ExpectedArrow d c)


typeError :: TypeError l a -> Check l a b
typeError =
  Check . Left . D.singleton

-- Check a list of 'Expr', using 'apE' to accumulate the errors.
checkList :: Ground l => [Expr l n (Annotated l a)] -> Check l a [Type l]
checkList =
  -- This could be written with foldl' and DList if laziness / space
  -- became important.
  Check . foldr fun (pure [])
  where
    fun ::
         Ground l
      => Expr l n (Annotated l a)
      -> Either (DList (TypeError l a)) [Type l]
      -> Either (DList (TypeError l a)) [Type l]
    fun l r =
      (unCheck $ fmap (:) (typeCheck' l)) `apE` r

-- Check a pair of 'Expr', using 'apE' to accumulate the errors.
checkPair ::
     Ground l
  => Expr l n (Annotated l a)
  -> Expr l n (Annotated l a)
  -> Check l a (Type l, Type l)
checkPair a =
  Check . (pairE `on` (unCheck . typeCheck')) a


-- -----------------------------------------------------------------------------

pairE :: Monoid e => Either e b -> Either e c -> Either e (b, c)
pairE l r =
  apE (fmap (,) l) r

-- A version of 'ap' that accumulates errors.
-- Useful when expressions do not relate to one another at all.
apE :: Monoid e => Either e (a -> b) -> Either e a -> Either e b
apE l r =
  case (l, r) of
    (Right f, Right a) ->
      pure (f a)
    (Left a, Right _) ->
      Left a
    (Right _, Left b) ->
      Left b
    (Left a, Left b) ->
      Left (a <> b)
