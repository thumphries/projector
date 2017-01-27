{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Backend.Haskell.Rewrite (
    rewriteModule
  , rewriteExpr
  , rules
  ) where


import           P

import           Projector.Core
import           Projector.Html.Core.Library
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim


rewriteModule :: ModuleName -> Module HtmlType PrimT a -> (ModuleName, Module HtmlType PrimT a)
rewriteModule mn (Module tys imports exprs) =
  let exprs' = fmap (\(ty, e) -> (ty, rewriteFix rules e)) exprs
  in (mn, Module tys imports exprs')

rewriteExpr :: Expr PrimT a -> Expr PrimT a
rewriteExpr =
  rewrite rules

-- TODO these rules can operate on the fully typed AST if we need it
-- TODO oh god, this all goes to hell if people shadow the runtime names
--      (we better make this hard or illegal)

-- * Erase all evidence of the HtmlNode type, which doesn't exist at runtime.
--   Each gets converted to Hydrant's Html type.
-- * Projector's HTML type becomes a monoidal fold of Hydrant's Html type.
rules :: [RewriteRule PrimT a]
rules =
  fmap Rewrite [
      (\case ECon a (Constructor "Plain") _ [x] ->
               pure (apply (textNode a) [x])
             _ ->
               empty)
    , (\case ECon a (Constructor "Whitespace") _ _ ->
               pure (apply (textNode a) [(ELit a (VString " "))])
             _ ->
               empty)
    , (\case ECon a (Constructor "Element") _ [tag, attrs, body] ->
               pure (apply (parentNode a) [tag, attrs, body])
             _ ->
               empty)
    , (\case ECon a (Constructor "VoidElement") _ [tag, attrs] ->
               pure (apply (voidNode a) [tag, attrs])
             _ ->
               empty)
    , (\case ECon a (Constructor "Comment") _ [str] ->
               pure (apply (comment a) [str])
             _ ->
               empty)
    , (\case ECon _ (Constructor "Nested") _ [html] ->
               pure html
             _ ->
               empty)
    , (\case ECon a (Constructor "Html") _ [nodes] ->
               pure (apply (foldHtml a) [nodes])
             _ ->
               empty)
    ]

textNode :: a -> Expr PrimT a
textNode a =
  EForeign a (Name "textNode") (TArrow (TLit TString) tHtml)

parentNode :: a -> Expr PrimT a
parentNode a =
  EForeign a (Name "parentNode") (TArrow tTag (TArrow (TList tAttribute) (TArrow tHtml tHtml)))

voidNode :: a -> Expr PrimT a
voidNode a =
  EForeign a (Name "voidNode") (TArrow tTag (TArrow (TList tAttribute) tHtml))

comment :: a -> Expr PrimT a
comment a =
  EForeign a (Name "comment") (TArrow (TLit TString) tHtml)

foldHtml :: a -> Expr PrimT a
foldHtml a =
  EForeign a (Name "foldHtml") (TArrow (TList tHtml) tHtml)

-- build an application chain
apply :: Expr PrimT a -> [Expr PrimT a] -> Expr PrimT a
apply f =
  foldl' (EApp (extractAnnotation f)) f
