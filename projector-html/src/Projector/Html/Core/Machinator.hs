{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Core.Machinator (
    machinatorDecls
  , fromMachinator
  ) where


import           Machinator.Core (Definition (..))
import qualified Machinator.Core.Data.Definition as MC

import           P

import           Projector.Core
import qualified Projector.Html.Core.Prim as Prim
import           Projector.Html.Data.Prim


-- | Convert a set of Machinator definitions to a Projector
-- declaration set.
machinatorDecls :: Foldable f => f Definition -> HtmlDecls
machinatorDecls =
  foldl' (\decls def -> uncurry declareType (fromMachinator def) decls) mempty
{-# SPECIALIZE machinatorDecls :: [Definition] -> HtmlDecls #-}

-- | Convert a Machinator definition to a Projector definition.
fromMachinator :: Definition -> (TypeName, HtmlDecl)
fromMachinator (Definition (MC.Name n) dt) =
  (TypeName n, fromMachinatorDT dt)

fromMachinatorDT :: MC.DataType -> HtmlDecl
fromMachinatorDT dt =
  case dt of
    MC.Variant nts ->
      DVariant . toList . with nts $ \(MC.Name n, ts) ->
        (Constructor n, fmap fromMachinatorT ts)
    MC.Record fts ->
      DRecord . with fts $ \(MC.Name n, t) ->
        (FieldName n, fromMachinatorT t)

fromMachinatorT :: MC.Type -> HtmlType
fromMachinatorT mt =
  case mt of
    MC.Variable (MC.Name n) ->
      TVar (TypeName n)
    MC.GroundT g ->
      fromMachinatorGT g
    MC.ListT t ->
      TList (fromMachinatorT t)

fromMachinatorGT :: MC.Ground -> HtmlType
fromMachinatorGT g =
  case g of
    MC.StringT ->
      TLit TString
    MC.BoolT ->
      TVar Prim.nBool
