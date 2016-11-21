{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Backend.Data (
    ModuleName (..)
  , Module (..)
  ) where


import           P

import           Projector.Core  (Name)
import           Projector.Html.Core.Prim (HtmlDecls, HtmlExpr, HtmlType)


newtype ModuleName = ModuleName { unModuleName :: Text }
  deriving (Eq, Ord, Show)

data Module = Module {
    moduleTypes :: HtmlDecls
  , moduleExprs :: [(Name, HtmlType, HtmlExpr)]
  } deriving (Eq, Ord, Show)
