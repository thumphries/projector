{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Projector.Html.Backend.Haskell.Rewrite (
    rewriteModule
  , rewriteExpr
  , rules
  ) where


import qualified Control.Monad.Trans.State as State

import           P

import           Projector.Core
import           Projector.Html.Backend.Rewrite (globalRules)
import qualified Projector.Html.Core.Library as CL
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim


rewriteModule :: ModuleName -> Module HtmlType PrimT a -> (ModuleName, Module HtmlType PrimT a)
rewriteModule mn (Module tys imports exprs) =
  let exprs' = fmap (\(ModuleExpr ty e) -> ModuleExpr ty (rewriteFix (globalRules <> rules) e)) exprs
  in (mn, Module tys imports exprs')

rewriteExpr :: Expr PrimT a -> Expr PrimT a
rewriteExpr =
  rewrite (globalRules <> rules)

-- TODO these rules can operate on the fully typed AST if we need it
-- TODO oh god, this all goes to hell if people shadow the runtime names
--      (we better make this hard or illegal)

-- * Erase all evidence of the Html type, which doesn't exist at runtime.
--   Each gets converted to Projector.Html.Runtime's Html type.
-- * Projector's HTML type becomes a monoidal fold of Projector.Html.Runtime's Html type.
rules :: [RewriteRule PrimT a]
rules =
  fmap Rewrite [
      -- Replace HTML model with Projector.Html.Runtime functions.
      -- These rules are important for correctness - won't work without these.
      (\case ECon a (Constructor "Plain") _ [x] ->
               pure (apply (textNode a) [x])
             _ ->
               empty)
    , (\case ECon a (Constructor "Raw") _ [x] ->
               pure (apply (rawTextNode a) [x])
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
    , (\case ECon a (Constructor "Nested") _ [nodes] ->
               pure (apply (foldHtml a) [nodes])
             _ ->
               empty)

      -- Qualify imports for runtime functions and constructors.
    , (\case EForeign a (Name "append") ty ->
               pure (EForeign a (Name "Projector.Html.Runtime.append") ty)
             _ ->
               empty)
    , (\case EForeign a (Name "concat") ty ->
               pure (EForeign a (Name "Projector.Html.Runtime.concat") ty)
             _ ->
               empty)
    , (\case EForeign a (Name "fold") ty ->
               pure (EForeign a (Name "Projector.Html.Runtime.fold") ty)
             _ ->
               empty)
    , (\case EForeign a (Name "isEmpty") ty ->
               pure (EForeign a (Name "Projector.Html.Runtime.isEmpty") ty)
             _ ->
               empty)
    , (\case ECon a c tn es ->
               ECon a
                 <$> qualifyConstructor c
                 <*> pure tn
                 <*> pure es
             _ ->
               empty)
    , (\case ECase a e ps ->
               let
                 go p =
                   case p of
                     PVar pa n ->
                       pure $ PVar pa n
                     PCon pa c ps' ->
                       PCon pa
                         -- Keep track of any time we need to qualify and return the full Just ECase later
                         <$> (maybe (pure c) (\c' -> State.put True >> pure c') . qualifyConstructor) c
                         <*> mapM go ps'
                 (ec, updated) =
                   flip State.runState False $
                     ECase a e <$> mapM (\(p, e') -> fmap (flip (,) e') . go $ p) ps
               in
                 valueOrEmpty updated ec
             _ ->
               empty)

      -- These rules are just optimisations.
      -- foldHtml of a singleton: id
    , (\case EApp _ (EForeign _ (Name "Projector.Html.Runtime.foldHtml") _) (EList _ [x]) ->
               pure x
             _ ->
               empty)
      -- concat of a singleton: id
    , (\case EApp _ (EForeign _ (Name "Projector.Html.Runtime.concat") _) (EList _ [x]) ->
               pure x
             _ ->
               empty)
      -- adjacent raw plaintext nodes can be merged
    , (\case EApp a fh@(EForeign _ (Name "Projector.Html.Runtime.foldHtml") _) (EList b nodes) -> do
               nodes' <- foldRaw nodes
               pure (EApp a fh (EList b nodes'))
             _ ->
               empty)

      -- TODO
      -- adjacent plaintext nodes can be merged
      -- hoist nested foldHtmls up to the top level
      -- rewrite away redundant isEmpty
      -- rewrite away redundant fold
    ]


qualifyConstructor :: Constructor -> Maybe Constructor
qualifyConstructor c =
  case c of
    Constructor "Tag" ->
      pure $ Constructor "Projector.Html.Runtime.Tag"
    Constructor "Attribute" ->
      pure $ Constructor "Projector.Html.Runtime.Attribute"
    Constructor "AttributeKey" ->
      pure $ Constructor "Projector.Html.Runtime.AttributeKey"
    Constructor "AttributeValue" ->
      pure $ Constructor "Projector.Html.Runtime.AttributeValue"
    Constructor "True" ->
      pure $ Constructor "Projector.Html.Runtime.True"
    Constructor "False" ->
      pure $ Constructor "Projector.Html.Runtime.False"
    _ ->
      empty

textNode :: a -> Expr PrimT a
textNode a =
  EForeign a (Name "Projector.Html.Runtime.textNode") (TArrow (TLit TString) CL.tHtml)

rawTextNode :: a -> Expr PrimT a
rawTextNode a =
  EForeign a (Name "Projector.Html.Runtime.textNodeUnescaped") (TArrow (TLit TString) CL.tHtml)

parentNode :: a -> Expr PrimT a
parentNode a =
  EForeign a (Name "Projector.Html.Runtime.parentNode") (TArrow CL.tTag (TArrow (TList CL.tAttribute) (TArrow CL.tHtml CL.tHtml)))

voidNode :: a -> Expr PrimT a
voidNode a =
  EForeign a (Name "Projector.Html.Runtime.voidNode") (TArrow CL.tTag (TArrow (TList CL.tAttribute) CL.tHtml))

comment :: a -> Expr PrimT a
comment a =
  EForeign a (Name "Projector.Html.Runtime.comment") (TArrow (TLit TString) CL.tHtml)

foldHtml :: a -> Expr PrimT a
foldHtml a =
  EForeign a (Name "Projector.Html.Runtime.foldHtml") (TArrow (TList CL.tHtml) CL.tHtml)

-- build an application chain
apply :: Expr PrimT a -> [Expr PrimT a] -> Expr PrimT a
apply f =
  foldl' (EApp (extractAnnotation f)) f

pattern RawTextNode a b c t =
  EApp a
    (EForeign b (Name "Projector.Html.Runtime.textNodeUnescaped") (TArrow (TLit TString) (TVar (TypeName "Html"))))
    (ELit c (VString t))

foldRaw :: [HtmlExpr a] -> Maybe [HtmlExpr a]
foldRaw exprs =
  if length (go exprs) == length exprs then empty else pure (go exprs)
  where
    go es =
      case es of
        [] ->
          []
        (RawTextNode a b c t1 : RawTextNode _ _ _ t2 : xs) ->
          go (RawTextNode a b c (t1 <> t2) : xs)
        (x:xs) ->
          x : go xs
