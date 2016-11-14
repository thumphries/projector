{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Core.Termination (
    TerminationError (..)
  , positivityCheck
  ) where


import           Data.Map.Strict as M

import           P

import           Projector.Core.Type
import           Projector.Core.Util (listE_)


data TerminationError l
  = PositivityCheckFailed (Type l) (Type l)
  deriving (Eq, Ord, Show)

data Polarity = Pos | Neg

pflip :: Polarity -> Polarity
pflip p =
  case p of
    Pos -> Neg
    Neg -> Pos

-- | Ensure datatypes are only used recursively in positive position.
positivityCheck :: Ground l => TypeContext l -> Either [TerminationError l] ()
positivityCheck tc =
  let ctx = M.toList (unTypeContext tc)
  in listE_ (fmap (uncurry (positivityCheck' Pos tc)) ctx)

positivityCheck' ::
     Ground l
  => Polarity
  -> TypeContext l
  -> TypeName
  -> Type l
  -> Either [TerminationError l] ()
positivityCheck' pol tc tn ty =
  positivityCheck'' pol tc tn ty ty

positivityCheck'' ::
     Ground l
  => Polarity
  -> TypeContext l
  -> TypeName
  -> Type l
  -> Type l
  -> Either [TerminationError l] ()
positivityCheck'' pol tc tn ty have =
  let cont =
        case have of
          TArrow l r -> do
            positivityCheck'' (pflip pol) tc tn ty l
            positivityCheck'' pol tc tn ty r

          TVariant _ cts ->
            listE_ (mconcat (fmap (snd . (fmap (fmap (positivityCheck'' pol tc tn ty)))) cts))

          TLit _ ->
            pure ()

          TVar _ ->
            pure ()

  in case pol of
       Neg ->
         if | have == TVar tn -> terminationError (PositivityCheckFailed have ty)
            | have == ty -> terminationError (PositivityCheckFailed have ty)
            | otherwise -> cont
       Pos ->
         cont

terminationError :: TerminationError l -> Either [TerminationError l] a
terminationError =
  Left . (:[])
