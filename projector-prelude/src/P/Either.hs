module P.Either (
    maybeToLeft
  , maybeToRight
  , leftToMaybe
  , rightToMaybe
  , ecase
  , flipEither
  , sequenceEither
  , sequenceExceptT
  ) where


import           Control.Applicative.Lift (Lift (..), Errors, runErrors)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)

import           Data.Functor.Constant (Constant (..))
import           Data.Traversable (Traversable (..))


maybeToLeft :: r -> Maybe l -> Either l r
maybeToLeft r = maybe (Right r) Left

maybeToRight :: l -> Maybe r -> Either l r
maybeToRight l = maybe (Left l) Right

leftToMaybe :: Either l r -> Maybe l
leftToMaybe = either Just (const Nothing)

rightToMaybe :: Either l r -> Maybe r
rightToMaybe = either (const Nothing) Just

ecase :: Either l r -> (l -> b) -> (r -> b) -> b
ecase e l = flip (either l) e

flipEither :: Either a b -> Either b a
flipEither = either Right Left

-- | Lift an 'Either' into 'Errors'.
eitherErrors :: Either e a -> Errors e a
eitherErrors e =
  case e of
    Left es ->
      Other (Constant es)

    Right a ->
      Pure a
{-# INLINE eitherErrors #-}

-- | Like 'sequence', but folding/accumulating all errors in case of a 'Left'.
sequenceEither :: (Monoid x, Traversable t) => t (Either x a) -> Either x (t a)
sequenceEither =
  runErrors . traverse eitherErrors
{-# INLINE sequenceEither #-}

-- | Evaluate each action in sequence, accumulating all errors in case of a failure.
-- Note that this means each action will be run independently, regardless of failure.
sequenceExceptT ::
     (Monad m, Monoid x, Traversable t)
  => t (ExceptT x m a)
  -> ExceptT x m (t a)
sequenceExceptT es = do
  es' <- lift (mapM runExceptT es)
  ExceptT . return $ sequenceEither es'
{-# INLINE sequenceExceptT #-}
