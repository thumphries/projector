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
  = PositivityCheckFailed (Type l) TypeName
  deriving (Eq, Ord, Show)

data Polarity = Pos | Neg

pflip :: Polarity -> Polarity
pflip p =
  case p of
    Pos -> Neg
    Neg -> Pos

-- | Ensure datatypes are only used recursively in positive position.
positivityCheck :: Ground l => TypeDecls l -> Either [TerminationError l] ()
positivityCheck tc =
  let ctx = M.toList (unTypeDecls tc)
  in listE_ (fmap (uncurry (positivityCheck' Pos tc)) ctx)

positivityCheck' ::
     Ground l
  => Polarity
  -> TypeDecls l
  -> TypeName
  -> Decl l
  -> Either [TerminationError l] ()
positivityCheck' pol tc tn ty =
  case ty of
    DVariant cts ->
      listE_ (mconcat (fmap (snd . (fmap (fmap (positivityCheck'' pol tc tn)))) cts))

positivityCheck'' ::
     Ground l
  => Polarity
  -> TypeDecls l
  -> TypeName
  -> Type l
  -> Either [TerminationError l] ()
positivityCheck'' pol tc tn have =
  let cont =
        case have of
          TArrow l r -> do
            -- Polarity flips whenever we go to the left of an Arrow.
            positivityCheck'' (pflip pol) tc tn l
            positivityCheck'' pol tc tn r

          TLit _ ->
            pure ()

          TList lt ->
            positivityCheck'' pol tc tn lt

          TVar n ->
            case lookupType n tc of
              Just td ->
                positivityCheck' pol tc tn td

              Nothing ->
                -- Free type variable. Won't pass typecheck, but not our problem.
                pure ()

  in case pol of
       Neg ->
         if have == TVar tn
           then terminationError (PositivityCheckFailed have tn)
           else cont
       Pos ->
         if have == TVar tn
           then pure ()
           else cont

terminationError :: TerminationError l -> Either [TerminationError l] a
terminationError =
  Left . (:[])
