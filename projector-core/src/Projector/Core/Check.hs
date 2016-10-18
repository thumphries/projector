{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.Check (
  -- * User interface
    typeCheck
  , TypeError (..)
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
  first D.toList . typeCheck' . fmap Unknown

data Annotated l a
  = Unknown a
  | Known (Type l)
  deriving (Eq, Show, Functor)

annotate :: Type l -> B.Var (B.Name n ()) (Annotated l a) -> Annotated l a
annotate ty e =
  case e of
    B.B _ ->
      Known ty

    B.F a ->
      a

typeCheck' ::
     Ground l
  => Expr l n (Annotated l a)
  -> Either (DList (TypeError l a)) (Type l)
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

    EApp a b ->
      let
        ta = typeCheck' a
        tb = typeCheck' b
      in
        case econcat ta tb of
          Left e ->
            Left e
          Right (TArrow c d, e) ->
            if c == e then pure d else typeError (Mismatch c e)
          Right (c, d) ->
            typeError (ExpectedArrow d c)

typeError :: TypeError l a -> Either (DList (TypeError l a)) b
typeError =
  Left . D.singleton

-- hmm this is basically accvalidation from Data.Validation
-- so let's move over to that once we know what's going on
econcat :: Monoid m => Either m b -> Either m c -> Either m (b, c)
econcat l r =
  case (l, r) of
    (Left e1, Left e2) ->
      Left (e1 <> e2)

    (Left e1, _) ->
      Left e1

    (Right _, Left e1) ->
      Left e1

    (Right a, Right b) ->
      Right (a, b)
