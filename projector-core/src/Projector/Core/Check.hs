{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
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

import           Projector.Core.Syntax
import           Projector.Core.Type


data TypeError l a
  = Mismatch (Type l) (Type l) a
  | CouldNotUnify [(Type l, a)] a
  | ExpectedArrow (Type l) (Type l) a
  | FreeVariable Name a
  | FreeTypeVariable TypeName a
  | BadConstructorName Constructor TypeName (Decl l) a
  | BadConstructorArity Constructor (Decl l) Int a
  | BadPatternArity Constructor (Type l) Int Int a
  | BadPatternConstructor Constructor (Type l) a
  | NonExhaustiveCase (Expr l a) (Type l) a

deriving instance (Eq l, Eq (Value l), Eq a) => Eq (TypeError l a)
deriving instance (Show l, Show (Value l), Show a) => Show (TypeError l a)
deriving instance (Ord l, Ord (Value l), Ord a) => Ord (TypeError l a)


typeCheck ::
     Ground l
  => TypeDecls l
  -> Expr l a
  -> Either [TypeError l a] (Type l)
typeCheck c =
  first D.toList . unCheck . typeCheck' c mempty


-- -----------------------------------------------------------------------------

newtype Check l a b = Check {
    unCheck :: Either (DList (TypeError l a)) b
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
  => TypeDecls l
  -> Ctx l
  -> Expr l a
  -> Check l a (Type l)
typeCheck' tc ctx expr =
  case expr of
    ELit _ v ->
      pure (TLit (typeOf v))

    EVar a n ->
      case clookup n ctx of
        Just t ->
          pure t
        Nothing ->
          typeError (FreeVariable n a)

    ELam _ n ta e -> do
      tb <- typeCheck' tc (cextend n ta ctx) e
      pure (TArrow ta tb)

    EApp _ f g -> do
      typs <- checkPair tc ctx f g
      case typs of
        (TArrow c d, e) ->
          if c == e then pure d else typeError (Mismatch c e (extractAnnotation g))
        (c, d) ->
          typeError (ExpectedArrow d c (extractAnnotation f))

    ECon a c tn es ->
      case lookupType tn tc of
        Just ty@(DVariant cs) -> do
          -- Look up constructor name
          ts <- maybe (typeError (BadConstructorName c tn ty a)) pure (L.lookup c cs)
          -- Check arity
          unless (length ts == length es) (typeError (BadConstructorArity c ty (length es) a))
          -- Typecheck all bnds against expected
          _ <- listC . with (L.zip ts es) $ \(t1, e) -> do
            t2 <- typeCheck' tc ctx e
            unless (t1 == t2) (typeError (Mismatch t1 t2 (extractAnnotation e)))
          pure (TVar tn)

        Nothing ->
          typeError (FreeTypeVariable tn a)

    ECase a e pes -> do
      ty <- typeCheck' tc ctx e
      let tzs = fmap fun pes
          fun (pat, ex) = fmap (,a) (checkPattern tc ctx ty pat ex)
      unifyList a (typeError (NonExhaustiveCase expr ty a)) tzs

    EList _a ty es -> do
      _ <- listC . with es $ \e -> do
        t <- typeCheck' tc ctx e
        when (t /= ty) (typeError (Mismatch ty t (extractAnnotation e)))
      pure (TList ty)

    EForeign _ _ ty -> do
      pure ty

-- | Check a pattern fits the type it is supposed to match,
-- then check its associated branch (if the pattern makes sense)
checkPattern ::
     Ground l
  => TypeDecls l
  -> Ctx l
  -> Type l
  -> Pattern a
  -> Expr l a
  -> Check l a (Type l)
checkPattern tc ctx ty pat expr = do
  ctx' <- checkPattern' tc ctx ty pat
  typeCheck' tc ctx' expr

checkPattern' ::
     Ground l
  => TypeDecls l
  -> Ctx l
  -> Type l
  -> Pattern a
  -> Check l a (Ctx l)
checkPattern' tc ctx ty pat =
  case pat of
    PVar _ x ->
      pure (cextend x ty ctx)

    PCon a c pats ->
      case ty of
        TVar tn ->
          case lookupType tn tc of
            Just (DVariant cs) -> do
              -- find the constructor in the type
              ts <- maybe (typeError (BadPatternConstructor c ty a)) pure (L.lookup c cs)
              -- check the lists are the same length
              unless (length ts == length pats) (typeError (BadPatternArity c ty (length ts) (length pats) a))
              -- Check all recursive pats against type list
              foldM (\ctx' (t', p') -> checkPattern' tc ctx' t' p') ctx (L.zip ts pats)
            Nothing ->
              typeError (FreeTypeVariable tn a)
        _ ->
          typeError (BadPatternConstructor c ty a)

unifyList :: Ground l => a -> Check l a (Type l) -> [Check l a (Type l, a)] -> Check l a (Type l)
unifyList a none es = do
  tzs <- listC es
  case L.nubBy ((==) `on` fst) tzs of
    (x,_):[] ->
      pure x
    [] ->
      none
    xs ->
      typeError (CouldNotUnify xs a)

typeError :: TypeError l a -> Check l a b
typeError =
  Check . Left . D.singleton

-- Check a pair of 'Expr', using 'apE' to accumulate the errors.
checkPair ::
     Ground l
  => TypeDecls l
  -> Ctx l
  -> Expr l a
  -> Expr l a
  -> Check l a (Type l, Type l)
checkPair tc ctx =
  pairC `on` (typeCheck' tc ctx)

-- -----------------------------------------------------------------------------

-- | Sequence errors from a list of 'Check'.
listC :: [Check l a b] -> Check l a [b]
listC =
  fmap D.toList . foldl' (apC . fmap D.snoc) (pure mempty)

-- | Sequence errors from two 'Check' functions.
pairC :: Check l a b -> Check l a c -> Check l a (b, c)
pairC l r =
  apC (fmap (,) l) r

-- | 'apE' lifted to 'Check'.
apC :: Check l a (b -> c) -> Check l a b -> Check l a c
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

