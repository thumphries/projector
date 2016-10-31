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
  , listC
  , pairC
  , apC
  -- * Reusable stuff
  , apE
  ) where


import           Data.DList (DList)
import qualified Data.DList as D
import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           P

import           Projector.Core.Syntax (Expr (..), Name (..), Pattern (..))
import           Projector.Core.Type (Type (..), Ground (..), typeOf)


data TypeError l
  = Mismatch (Type l) (Type l)
  | ExpectedArrow (Type l) (Type l)
  | FreeVariable Name
  | BadPattern (Type l) Pattern
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

    ECon c ty es ->
      case ty of
        TVariant _ cs -> do
          -- Look up constructor name
          ts <- maybe (typeError (BadConstructorName c ty)) pure (L.lookup c cs)
          -- Check arity
          unless (length ts == length es) (typeError (BadConstructorArity c ty (length es)))
          -- Typecheck all bnds against expected
          _ <- listC . with (L.zip ts es) $ \(t1, e) -> do
            t2 <- typeCheck' ctx e
            unless (t1 == t2) (typeError (Mismatch t1 t2))
          pure ty

        _ ->
          typeError (BadConstructorName c ty)

    ECase e pes -> do
      ty <- typeCheck' ctx e
      tzs <- listC $ fmap (uncurry (checkPattern ctx ty)) pes
      -- whole list needs to be equal
      undefined

-- | Check a pattern fits the type it is supposed to match,
-- then check its associated branch (if the pattern makes sense)
checkPattern :: Ground l => Ctx l -> Type l -> Pattern -> Expr l -> Check l (Type l)
checkPattern ctx ty pat expr = do
  ctx' <- checkPattern' ctx ty pat
  typeCheck' ctx' expr

checkPattern' :: Ground l => Ctx l -> Type l -> Pattern -> Check l (Ctx l)
checkPattern' ctx ty pat =
  case (ty, pat) of
    (t, PVar x) ->
      pure (cextend x t ctx)

    (TVariant n cs, PCon c pats) -> do
      -- find the constructor in the type
      ts <- maybe (typeError (BadPattern ty pat)) pure (L.lookup c cs)
      -- check the lists are the same length
      unless (length ts == length pats) (typeError (BadPattern ty pat))
      -- Check all recursive pats against type list
      foldM (\ctx' (t', p') -> checkPattern' ctx' t' p') ctx (L.zip ts pats)

    _ ->
      typeError (BadPattern ty pat)

typeError :: TypeError l -> Check l a
typeError =
  Check . Left . D.singleton

checkList ctx = listC . fmap (typeCheck' ctx)

-- Check a pair of 'Expr', using 'apE' to accumulate the errors.
checkPair ::
     Ground l
  => Ctx l
  -> Expr l
  -> Expr l
  -> Check l (Type l, Type l)
checkPair ctx =
  pairC `on` (typeCheck' ctx)

-- -----------------------------------------------------------------------------

-- | Sequence errors from a list of 'Check'.
listC :: [Check l a] -> Check l [a]
listC =
  fmap D.toList . foldl' (apC . fmap D.snoc) (pure mempty)

-- | Sequence errors from two 'Check' functions.
pairC :: Check l a -> Check l b -> Check l (a, b)
pairC l r =
  apC (fmap (,) l) r

-- | 'apE' lifted to 'Check'.
apC :: Check l (a -> b) -> Check l a -> Check l b
apC l r =
  Check $ apE (unCheck l) (unCheck r)

-- -----------------------------------------------------------------------------

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
