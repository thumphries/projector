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


import           Data.DList (DList)
import qualified Data.DList as D
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           P

import           Projector.Core.Syntax (Expr (..), Name (..))
import           Projector.Core.Type (Type (..), Ground (..), typeOf)


data TypeError l
  = Mismatch (Type l) (Type l)
  | ExpectedArrow (Type l) (Type l)
  | FreeVariable Name
  deriving (Eq, Show)

typeCheck ::
     Ground l
  => Expr l
  -> Either [TypeError l] (Type l)
typeCheck =
  first D.toList . unCheck . typeCheck' mempty


-- -----------------------------------------------------------------------------

newtype Check l a = Check {
    unCheck :: Either (DList (TypeError l)) a
  } deriving (Functor, Applicative, Monad)

-- typing context
newtype Ctx l = Ctx { unCtx :: Map Name (Type l) }

instance Monoid (Ctx l) where
  mempty = Ctx mempty
  mappend (Ctx a) (Ctx b) = Ctx (mappend a b)

cextend :: Name -> Type l -> Ctx l -> Ctx l
cextend n t =
  Ctx . M.insert n t . unCtx

clookup :: Name -> Ctx l -> Maybe (Type l)
clookup n =
  M.lookup n . unCtx

-- As we've got an explicitly-typed calculus, typechecking is
-- straightforward and syntax-directed. All we have to do is propagate
-- our type annotations around the tree and use (==) in the right spots.
typeCheck' ::
     Ground l
  => Ctx l
  -> Expr l
  -> Check l (Type l)
typeCheck' ctx expr =
  case expr of
    ELit v ->
      pure (TLit (typeOf v))

    EVar n ->
      case clookup n ctx of
        Just t ->
          pure t
        Nothing ->
          typeError (FreeVariable n)

    ELam n ta e -> do
      tb <- typeCheck' (cextend n ta ctx) e
      pure (TArrow ta tb)

    EApp a b -> do
      typs <- checkPair ctx a b
      case typs of
        (TArrow c d, e) ->
          if c == e then pure d else typeError (Mismatch c e)
        (c, d) ->
          typeError (ExpectedArrow d c)


typeError :: TypeError l -> Check l a
typeError =
  Check . Left . D.singleton

-- Check a list of 'Expr', using 'apE' to accumulate the errors.
checkList :: Ground l => Ctx l -> [Expr l] -> Check l [Type l]
checkList ctx =
  -- This could be written with foldl' and DList if laziness / space
  -- became important.
  Check . foldr (fun ctx) (pure [])
  where
    fun ::
         Ground l
      => Ctx l
      -> Expr l
      -> Either (DList (TypeError l)) [Type l]
      -> Either (DList (TypeError l)) [Type l]
    fun ctx' l r =
      (unCheck $ fmap (:) (typeCheck' ctx' l)) `apE` r

-- Check a pair of 'Expr', using 'apE' to accumulate the errors.
checkPair ::
     Ground l
  => Ctx l
  -> Expr l
  -> Expr l
  -> Check l (Type l, Type l)
checkPair ctx a =
  Check . (pairE `on` (unCheck . typeCheck' ctx)) a


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
