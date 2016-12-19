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
  , typeTree
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


typeCheck :: Ground l => TypeDecls l -> Expr l a -> Either [TypeError l a] (Type l)
typeCheck decls =
  fmap extractType . typeTree decls

typeTree ::
     Ground l
  => TypeDecls l
  -> Expr l a
  -> Either [TypeError l a] (Expr l (Type l, a))
typeTree c =
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
  -> Check l a (Expr l (Type l, a))
typeCheck' tc ctx expr =
  case expr of
    ELit a v ->
      pure (ELit (TLit (typeOf v), a) v)

    EVar a n ->
      case clookup n ctx of
        Just t ->
          pure (EVar (t, a) n)
        Nothing ->
          typeError (FreeVariable n a)

    ELam a n ta e -> do
      e' <- typeCheck' tc (cextend n ta ctx) e
      let tb = extractType e'
      pure (ELam (TArrow ta tb, a) n ta e')

    EApp a f g -> do
      (f', g') <- checkPair tc ctx f g
      let tf = extractType f'
          tg = extractType g'
      ty <- case tf of
        TArrow c d ->
          if c == tg then pure d else typeError (Mismatch c tg (extractAnnotation g))
        c ->
          typeError (ExpectedArrow tg c (extractAnnotation f))
      pure (EApp (ty, a) f' g')

    ECon a c tn es ->
      case lookupType tn tc of
        Just ty@(DVariant cs) -> do
          -- Look up constructor name
          ts <- maybe (typeError (BadConstructorName c tn ty a)) pure (L.lookup c cs)
          -- Check arity
          unless (length ts == length es) (typeError (BadConstructorArity c ty (length es) a))
          -- Typecheck all bnds against expected
          es' <- listC . with (L.zip ts es) $ \(t1, e) -> do
            e' <- typeCheck' tc ctx e
            let t2 = extractType e'
            unless (t1 == t2) (typeError (Mismatch t1 t2 (extractAnnotation e)))
            pure e'
          pure (ECon (TVar tn, a) c tn es')

        Nothing ->
          typeError (FreeTypeVariable tn a)

    ECase a e pes -> do
      e' <- typeCheck' tc ctx e
      let te = extractType e'
      pes' <- listC (fmap (uncurry (checkPattern tc ctx te)) pes)
      tb <- unifyList a (typeError (NonExhaustiveCase expr te a)) (fmap (extractAnnotation . snd) pes')
      pure (ECase (tb, a) e' pes')

    EList a ty es -> do
      es' <- listC . with es $ \e -> do
        e' <- typeCheck' tc ctx e
        let t = extractType e'
        when (t /= ty) (typeError (Mismatch ty t (extractAnnotation e)))
        pure e'
      pure (EList (TList ty, a) ty es')

    EForeign a n ty -> do
      pure (EForeign (ty, a) n ty)

-- | Check a pattern fits the type it is supposed to match,
-- then check its associated branch (if the pattern makes sense)
checkPattern ::
     Ground l
  => TypeDecls l
  -> Ctx l
  -> Type l
  -> Pattern
  -> Expr l a
  -> Check l a (Pattern, Expr l (Type l, a))
checkPattern tc ctx ty pat expr = do
  let a = extractAnnotation expr
  ctx' <- checkPattern' a tc ctx ty pat
  expr' <- typeCheck' tc ctx' expr
  pure (pat, expr')

checkPattern' ::
     Ground l
  => a
  -> TypeDecls l
  -> Ctx l
  -> Type l
  -> Pattern
  -> Check l a (Ctx l)
checkPattern' a tc ctx ty pat =
  case pat of
    PVar x ->
      pure (cextend x ty ctx)

    PCon c pats ->
      case ty of
        TVar tn ->
          case lookupType tn tc of
            Just (DVariant cs) -> do
              -- find the constructor in the type
              ts <- maybe (typeError (BadPatternConstructor c ty a)) pure (L.lookup c cs)
              -- check the lists are the same length
              unless (length ts == length pats) (typeError (BadPatternArity c ty (length ts) (length pats) a))
              -- Check all recursive pats against type list
              foldM (\ctx' (t', p') -> checkPattern' a tc ctx' t' p') ctx (L.zip ts pats)
            Nothing ->
              typeError (FreeTypeVariable tn a)
        _ ->
          typeError (BadPatternConstructor c ty a)

unifyList :: Ground l => a -> Check l a (Type l) -> [(Type l, a)] -> Check l a (Type l)
unifyList a none es = do
  case L.nubBy ((==) `on` fst) es of
    (x, _):[] ->
      pure x
    [] ->
      none
    xs ->
      typeError (CouldNotUnify xs a)

extractType :: Expr l (Type l, a) -> Type l
extractType =
  fst . extractAnnotation

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
  -> Check l a (Expr l (Type l, a), Expr l (Type l, a))
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

