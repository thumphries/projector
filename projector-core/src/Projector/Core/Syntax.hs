{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Projector.Core.Syntax (
    Expr (..)
  , extractAnnotation
  , setAnnotation
  , Name (..)
  , Pattern (..)
  , extractPatternAnnotation
  -- * Smart/lazy constructors
  , lit
  , lam
  , lam_
  , lam__
  , var
  , var_
  , app
  , case_
  , con
  , con_
  , rec_
  , rec__
  , prj
  , prj_
  , list
  , foreign_
  , foreign_'
  -- ** pattern constructors
  , pvar
  , pvar_
  , pcon
  , pcon_
  -- * AST traversals
  , foldFree
  , gatherFree
  , patternBinds
  , mapGround
  , foldlExprM
  , foldlExpr
  , foldlPatternM
  , foldrExprM
  , foldrExpr
  , foldrPatternM
  ) where


import           Control.Monad.Trans.Cont (cont, runCont)

import           Data.Set (Set)
import qualified Data.Set as S

import           P

import           Projector.Core.Type


-- | The type of Projector expressions.
--
-- The first type parameter, 'l', refers to the type of literal. This is
-- invariant. Literals must have a 'Ground' instance.
--
-- The second type parameter, 'a', refers to the type of annotation,
-- e.g. source location or '()'.
data Expr l a
  = ELit a (Value l)
  | EVar a Name
  | ELam a Name (Maybe (Type l)) (Expr l a)
  | EApp a (Expr l a) (Expr l a)
  | ECon a Constructor TypeName [Expr l a]
  | ECase a (Expr l a) [(Pattern a, Expr l a)]
  | ERec a TypeName [(FieldName, Expr l a)]
  | EPrj a (Expr l a) FieldName
  | EList a [Expr l a]
  | EMap a (Expr l a) (Expr l a)
  | EForeign a Name (Type l)
  deriving (Functor, Foldable, Traversable)

deriving instance (Ground l, Eq a) => Eq (Expr l a)
deriving instance (Ground l, Show a) => Show (Expr l a)
deriving instance (Ground l, Ord a) => Ord (Expr l a)

extractAnnotation :: Expr l a -> a
extractAnnotation e =
  case e of
    ELit a _ ->
      a
    EVar a _ ->
      a
    ELam a _ _ _ ->
      a
    EApp a _ _ ->
      a
    ECon a _ _ _ ->
      a
    ERec a _ _ ->
      a
    EPrj a _ _ ->
      a
    ECase a _ _ ->
      a
    EList a _ ->
      a
    EMap a _ _ ->
      a
    EForeign a _ _ ->
      a
{-# INLINE extractAnnotation #-}

-- Set the top-level annotation.
setAnnotation :: a -> Expr l a -> Expr l a
setAnnotation a e =
  case e of
    ELit _ b ->
      ELit a b
    EVar _ b ->
      EVar a b
    ELam _ b c d ->
      ELam a b c d
    EApp _ b c ->
      EApp a b c
    ECon _ b c d ->
      ECon a b c d
    ECase _ b c ->
      ECase a b c
    ERec _ b c ->
      ERec a b c
    EPrj _ b c ->
      EPrj a b c
    EList _ b ->
      EList a b
    EMap _ b c ->
      EMap a b c
    EForeign _ b c ->
      EForeign a b c
{-# INLINE setAnnotation #-}

newtype Name = Name { unName :: Text }
  deriving (Eq, Ord, Show)

-- | Pattern matching. Note that these are necessarily recursive.
data Pattern a
  = PVar a Name
  | PCon a Constructor [Pattern a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

extractPatternAnnotation :: Pattern a -> a
extractPatternAnnotation p =
  case p of
    PVar a _ ->
      a
    PCon a _ _ ->
      a
{-# INLINE extractPatternAnnotation #-}

-- lazy exprs
lit :: Value l -> Expr l ()
lit =
  ELit ()

lam :: Name -> Maybe (Type l) -> Expr l () -> Expr l ()
lam =
  ELam ()

lam_ :: Text -> Maybe (Type l) -> Expr l () -> Expr l ()
lam_ n =
  lam (Name n)

lam__ :: Text -> Expr l () -> Expr l ()
lam__ n =
  lam_ n Nothing

var :: Name -> Expr l ()
var =
  EVar ()

var_ :: Text -> Expr l ()
var_ t =
  var (Name t)

app :: Expr l () -> Expr l () -> Expr l ()
app =
  EApp ()

case_ :: Expr l () -> [(Pattern (), Expr l ())] -> Expr l ()
case_ =
  ECase ()

con :: Constructor -> TypeName -> [Expr l ()] -> Expr l ()
con =
  ECon ()

con_ :: Text -> Text -> [Expr l ()] -> Expr l ()
con_ c t =
  con (Constructor c) (TypeName t)

rec_ :: TypeName -> [(FieldName, Expr l ())] -> Expr l ()
rec_ tn fes =
  ERec () tn fes

rec__ :: Text -> [(Text, Expr l ())] -> Expr l ()
rec__ tn fes =
  rec_ (TypeName tn) (fmap (first FieldName) fes)

prj :: Expr l () -> FieldName -> Expr l ()
prj =
  EPrj ()

prj_ :: Expr l () -> Text -> Expr l ()
prj_ e t =
  prj e (FieldName t)

list :: [Expr l ()] -> Expr l ()
list =
 EList ()

foreign_ :: Name -> Type l -> Expr l ()
foreign_ =
  EForeign ()

foreign_' :: Text -> Type l -> Expr l ()
foreign_' =
  foreign_ . Name

-- lazy pats
pvar :: Name -> Pattern ()
pvar =
  PVar ()

pvar_ :: Text -> Pattern ()
pvar_ =
  pvar . Name

pcon :: Constructor -> [Pattern ()] -> Pattern ()
pcon =
  PCon ()

pcon_ :: Text -> [Pattern ()] -> Pattern ()
pcon_ =
  pcon . Constructor

-- | Strict fold over free variables, including foreign definitions.
foldFree :: (b -> Name -> b) -> b -> Expr l a -> b
foldFree f acc expr =
  go f expr mempty acc
  where
    go f' expr' bound acc' =
      case expr' of
        ELit _ _ ->
          acc'

        EVar _ x ->
          if (S.member x bound) then acc' else f' acc' $! x

        ELam _ n _ body ->
          go f' body (S.insert n $! bound) acc'

        EApp _ a b ->
          go f' b bound $! go f' a bound acc'

        ECon _ _ _ es ->
          foldl' (\a e -> go f' e bound a) acc' es

        ECase _ e pes ->
          let patBinds bnd pat =
                case pat of
                  PVar _ x ->
                    S.insert x $! bnd
                  PCon _ _ pats ->
                    foldl' patBinds bnd pats
          in foldl' (\a (p, ee) -> go f' ee (patBinds bound p) a) (go f' e bound acc') $! pes

        ERec _ _ fes ->
          foldl' (\a (_fn, e) -> go f' e bound a) acc' fes

        EPrj _ e _ ->
          go f' e bound acc'

        EList _ es ->
          foldl' (\a e -> go f' e bound a) acc' es

        EMap _ a b ->
          go f' b bound $! go f' a bound acc'

        EForeign _ x _ ->
          if (S.member x bound) then acc' else f' acc' $! x

-- | Gather all free variables in an expression.
gatherFree :: Expr l a -> Set Name
gatherFree =
  foldFree (flip S.insert) mempty

-- | Gather all names bound by a pattern.
patternBinds :: Pattern a -> Set Name
patternBinds pat =
  case pat of
    PVar _ x ->
      S.singleton x
    PCon _ _ pats ->
      foldl' (<>) mempty (fmap patternBinds pats)

-- | Migrate to a different set of ground types.
mapGround ::
     Ground l
  => Ground m
  => (l -> m)
  -> (Value l -> Value m)
  -> Expr l a
  -> Expr m a
mapGround tmap vmap expr =
  case expr of
    ELit a v ->
      ELit a (vmap v)

    EVar a n ->
      EVar a n

    ELam a n t e ->
      ELam a n (fmap (mapGroundType tmap) t) (mapGround tmap vmap e)

    EApp a f g ->
      EApp a (mapGround tmap vmap f) (mapGround tmap vmap g)

    ECon a c tn es ->
      ECon a c tn (fmap (mapGround tmap vmap) es)

    ECase a e pes ->
      ECase a (mapGround tmap vmap e) (fmap (fmap (mapGround tmap vmap)) pes)

    ERec a tn fes ->
      ERec a tn (fmap (fmap (mapGround tmap vmap)) fes)

    EPrj a e f ->
      EPrj a (mapGround tmap vmap e) f

    EList a es ->
      EList a (fmap (mapGround tmap vmap) es)

    EMap a f g ->
      EMap a (mapGround tmap vmap f) (mapGround tmap vmap g)

    EForeign a n t ->
      EForeign a n (mapGroundType tmap t)

-- | Bottom-up monadic fold.
foldrExprM ::
     Monad m
  => (Expr l a -> b -> m b) -- ^ eliminator for exprs
  -> (Pattern a -> b -> m b) -- ^ eliminator for patterns
  -> b
  -> Expr l a
  -> m b
foldrExprM fx fp acc expr =
  case expr of
    ELit _ _ ->
      fx expr acc
    EVar _ _ ->
      fx expr acc
    ELam _ _ _ e -> do
      acc' <- foldrExprM fx fp acc e
      fx expr acc'
    EApp _ i j -> do
      acc' <- foldrExprM fx fp acc j
      acc'' <- foldrExprM fx fp acc' i
      fx expr acc''
    ECon _ _ _ es -> do
      acc' <- foldrM (flip (foldrExprM fx fp)) acc es
      fx expr acc'
    ECase _ e pes -> do
      acc' <-
        foldrM
          (\(pat, ex) a -> foldrExprM fx fp a ex >>= \a' -> foldrPatternM fp a' pat)
          acc
          pes
      acc'' <- foldrExprM fx fp acc' e
      fx expr acc''
    ERec _ _ fes -> do
      acc' <- foldrM (\a b -> foldrExprM fx fp b (snd a)) acc fes
      fx expr acc'
    EPrj _ e _ -> do
      acc' <- foldrExprM fx fp acc e
      fx expr acc'
    EList _ es -> do
      acc' <- foldrM (flip (foldrExprM fx fp)) acc es
      fx expr acc'
    EMap _ i j -> do
      acc' <- foldrExprM fx fp acc j
      acc'' <- foldrExprM fx fp acc' i
      fx expr acc''
    EForeign _ _ _ ->
      fx expr acc

-- | Bottom-up monadic fold over a pattern.
foldrPatternM :: Monad m => (Pattern a -> b -> m b) -> b -> Pattern a -> m b
foldrPatternM fp acc p =
  case p of
    PVar _ _ ->
      fp p acc

    PCon _ _ ps -> do
      acc' <- foldrM (flip (foldrPatternM fp)) acc ps
      fp p acc'

-- | Bottom-up strict fold.
foldrExpr :: (Expr l a -> b -> b) -> (Pattern a -> b -> b) -> b -> Expr l a -> b
foldrExpr fx fp acc expr =
  runCont
    (foldrExprM
       (\e a -> cont (\k -> k (fx e a)))
       (\p a -> cont (\k -> k (fp p a)))
       acc
       expr)
    id

-- | Top-down monadic fold.
foldlExprM ::
     Monad m
  => (b -> Expr l a -> m b)
  -> (b -> Pattern a -> m b)
  -> b
  -> Expr l a
  -> m b
foldlExprM fx fp acc expr =
  case expr of
    ELit _ _ ->
      fx acc expr
    EVar _ _ ->
      fx acc expr
    EForeign _ _ _ ->
      fx acc expr
    ELam _ _ _ e -> do
      acc' <- fx acc expr
      foldlExprM fx fp acc' e
    EApp _ i j -> do
      acc' <- fx acc expr
      acc'' <- foldlExprM fx fp acc' i
      foldlExprM fx fp acc'' j
    ECon _ _ _ es -> do
      acc' <- fx acc expr
      foldM (foldlExprM fx fp) acc' es
    ECase _ e pes -> do
      acc' <- fx acc expr
      acc'' <- foldlExprM fx fp acc' e
      foldlM
        (\a (pat, ex) -> foldlPatternM fp a pat >>= \a' -> foldlExprM fx fp a' ex)
        acc''
        pes
    ERec _ _ fes -> do
      acc' <- fx acc expr
      foldM (foldlExprM fx fp) acc' (fmap snd fes)
    EPrj _ e _ -> do
      acc' <- fx acc expr
      foldlExprM fx fp acc' e
    EList _ es -> do
      acc' <- fx acc expr
      foldM (foldlExprM fx fp) acc' es
    EMap _ i j -> do
      acc' <- fx acc expr
      acc'' <- foldlExprM fx fp acc' i
      foldlExprM fx fp acc'' j

-- | Top-down monadic fold of a pattern.
foldlPatternM :: Monad m => (b -> Pattern a -> m b) -> b -> Pattern a -> m b
foldlPatternM fp acc p =
  case p of
    PVar _ _ ->
      fp acc p

    PCon _ _ ps -> do
      acc' <- fp acc p
      foldlM (foldlPatternM fp) acc' ps

-- | Top-down strict fold.
foldlExpr :: (b -> Expr l a -> b) -> (b -> Pattern a -> b) -> b -> Expr l a -> b
foldlExpr fx fp acc =
  flip runCont id .
  foldlExprM
    (\a e -> cont (\k -> k (fx a e)))
    (\a p -> cont (\k -> k (fp a p)))
    acc
