{-# LANGUAGE NoImplicitPrelude #-}
module Projector.Core.Prelude.EitherT (
    EitherT
  , Except.ExceptT
  , newEitherT
  , runEitherT
  , eitherT
  , mapEitherT
  , bimapEitherT
  , firstEitherT
  , secondEitherT
  , left
  , hoistEither
  , eitherErrors
  , sequenceEither
  , sequenceEitherT
  ) where

import           Control.Applicative (Applicative (..))
import           Control.Applicative.Lift (Lift (..), Errors, runErrors)
import           Control.Monad (Monad (..), (>>=))
import           Control.Monad.Trans.Class (MonadTrans (..))
import qualified Control.Monad.Trans.Except as Except
import           Data.Monoid (Monoid(..))

import           Data.Either (Either (..), either)
import           Data.Function ((.), id)
import           Data.Functor (Functor (..))
import           Data.Functor.Constant (Constant (..))
import           Data.Traversable (Traversable(..))

type EitherT =
  Except.ExceptT

newEitherT :: f (Either e a) -> EitherT e f a
newEitherT =
  Except.ExceptT

runEitherT :: EitherT e f a -> f (Either e a)
runEitherT =
  Except.runExceptT

eitherT :: Monad m => (x -> m b) -> (a -> m b) -> EitherT x m a -> m b
eitherT f g m =
  runEitherT m >>= either f g

left :: Applicative f => e -> EitherT e f a
left =
  newEitherT . pure . Left

mapEitherT :: (m (Either x a) -> n (Either y b)) -> EitherT x m a -> EitherT y n b
mapEitherT f =
  newEitherT . f . runEitherT

bimapEitherT :: Functor m => (x -> y) -> (a -> b) -> EitherT x m a -> EitherT y m b
bimapEitherT f g =
  mapEitherT (fmap (either (Left . f) (Right . g)))

firstEitherT :: Functor m => (x -> y) -> EitherT x m a -> EitherT y m a
firstEitherT f =
  bimapEitherT f id

secondEitherT :: Functor m => (a -> b) -> EitherT x m a -> EitherT x m b
secondEitherT f =
  bimapEitherT id f

hoistEither :: Monad m => Either x a -> EitherT x m a
hoistEither =
  newEitherT . return

eitherErrors :: Either e a -> Errors e a
eitherErrors e =
  case e of
    Left es ->
      Other (Constant es)
    Right a ->
      Pure a

sequenceEither :: (Monoid x, Traversable t) => t (Either x a) -> Either x (t a)
sequenceEither =
  runErrors . traverse eitherErrors

sequenceEitherT :: (Monad m, Monoid x, Traversable t) => t (EitherT x m a) -> EitherT x m (t a)
sequenceEitherT es = do
  es' <- lift (mapM runEitherT es)
  hoistEither (sequenceEither es')
