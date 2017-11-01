{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Projector.Html.Backend.Purescript.Rewrite (
    rewriteModule
  , rewriteExpr
  , rules
  ) where


import qualified Control.Monad.Trans.State as State

import qualified Data.Text as T

import           P

import           Projector.Core
import qualified Projector.Core as Core
import qualified Projector.Html.Backend.Rewrite as Rewrite
import qualified Projector.Html.Core.Library as CL
import           Projector.Html.Data.Module
import           Projector.Html.Data.Prim


rewriteModule :: ModuleName -> Module HtmlType PrimT a -> (ModuleName, Module HtmlType PrimT a)
rewriteModule mn (Module tys imports exprs) =
  let exprs' = fmap (\(ModuleExpr ty e) -> ModuleExpr ty (rewriteExpr (Just mn) e)) exprs
  in (mn, Module tys imports exprs')

rewriteExpr :: Maybe ModuleName -> Expr PrimT a -> Expr PrimT a
rewriteExpr mn =
  Core.rewriteFix (rules mn) . Core.rewriteFix Rewrite.globalRules

-- * Drop all comments, they don't make sense in virtual-dom world
-- * Replace constructors with DOMLike typeclass methods
rules :: Maybe ModuleName -> [RewriteRule PrimT a]
rules mmn =
  fmap Rewrite [
      -- Replace HTML model with DOMLike functions.
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
               pure (apply (parentNode a) [tag, attrs, EList a [body]])
             _ ->
               empty)
    , (\case ECon a (Constructor "VoidElement") _ [tag, attrs] ->
               pure (apply (voidNode a) [tag, attrs])
             _ ->
               empty)

    -- Erase HTML comments, they have no meaning at runtime
    , (\case ECon a (Constructor "Comment") _ _ ->
               pure (blank a)
             _ ->
               empty)

    , (\case ECon a (Constructor "Nested") _ [nodes] ->
               pure (apply (foldHtml a) [nodes])
             _ ->
               empty)

    -- Strip constructors, these become text nodes.
    , (\case ECon _ (Constructor "Tag") _ [t] ->
               pure t
             _ ->
               empty)
    , (\case ECon _ (Constructor "AttributeKey") _ [k] ->
               pure k
             _ ->
               empty)
    , (\case ECon _ (Constructor "AttributeValue") _ [v] ->
               pure v
             _ ->
               empty)
    -- Replace with runtime function.
    , (\case ECon a (Constructor "Attribute") _ [k, v] ->
               pure (apply (attr a) [k, v])
             _ ->
               empty)

    -- Bool constructors
    , (\case ECon a (Constructor "True") b k ->
               pure (ECon a (Constructor "true") b k)
             _ ->
               empty)
    , (\case ECon a (Constructor "False") b k ->
               pure (ECon a (Constructor "false") b k)
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

    -- Confusingly, for Purescript, we must de-qualify local definitions.
    , (\case EVar a n -> do
               mn <- mmn
               nn <- disqualify mn n
               pure (EVar a nn)
             _ ->
               empty)

    -- Mutate all the constructors in pattern matches.
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
                     PWildcard pa ->
                       pure $ PWildcard pa
                 (ec, updated) =
                   flip State.runState False $
                     ECase a e <$> mapM (\(p, e') -> fmap (flip (,) e') . go $ p) ps
               in
                 valueOrEmpty updated ec
             _ ->
               empty)

      -- These rules are just optimisations.
      -- fold of a singleton: id
    , (\case EApp _ (EForeign _ (Name "Projector.Html.Runtime.Pux.fold") _) (EList _ [x]) ->
               pure x
             _ ->
               empty)
      -- concat of a singleton: id
    , (\case EApp _ (EForeign _ (Name "Projector.Html.Runtime.concat") _) (EList _ [x]) ->
               pure x
             _ ->
               empty)
      -- adjacent raw plaintext nodes can be merged
    , (\case EApp a fh@(EForeign _ (Name "Projector.Html.Runtime.Pux.fold") _) (EList b nodes) -> do
               nodes' <- foldRaw nodes
               pure (EApp a fh (EList b nodes'))
             _ ->
               empty)

    ]

qualifyConstructor :: Constructor -> Maybe Constructor
qualifyConstructor c =
  case c of
    Constructor "True" ->
      pure $ Constructor "true"
    Constructor "False" ->
      pure $ Constructor "false"
    _ ->
      empty

disqualify :: ModuleName -> Name -> Maybe Name
disqualify (ModuleName mn) (Name n) = do
  let (modl, varr) = T.breakOnEnd "." n
  guard (modl == (mn <> "."))
  pure (Name varr)

-- build an application chain
apply :: Expr PrimT a -> [Expr PrimT a] -> Expr PrimT a
apply f =
  foldl' (EApp (extractAnnotation f)) f

blank :: a -> Expr PrimT a
blank a =
  EForeign a (Name "Projector.Html.Runtime.Pux.blank") CL.tHtml

textNode :: a -> Expr PrimT a
textNode a =
  EForeign a (Name "Projector.Html.Runtime.Pux.text") (TArrow (TLit TString) CL.tHtml)

rawTextNode :: a -> Expr PrimT a
rawTextNode a =
  EForeign a (Name "Projector.Html.Runtime.Pux.textUnescaped") (TArrow (TLit TString) CL.tHtml)

parentNode :: a -> Expr PrimT a
parentNode a =
  EForeign a
    (Name "Projector.Html.Runtime.Pux.parent")
    (TArrow CL.tTag (TArrow (TList CL.tAttribute) (TArrow CL.tHtml CL.tHtml)))

voidNode :: a -> Expr PrimT a
voidNode a =
  EForeign a
    (Name "Projector.Html.Runtime.Pux.void")
    (TArrow CL.tTag (TArrow (TList CL.tAttribute) CL.tHtml))

attr :: a -> Expr PrimT a
attr a =
  EForeign a
    (Name "Projector.Html.Runtime.Pux.attr")
    (TArrow CL.tAttributeKey (TArrow CL.tAttributeValue CL.tAttribute))

foldHtml :: a -> Expr PrimT a
foldHtml a =
  EForeign a (Name "Projector.Html.Runtime.Pux.fold") (TArrow (TList CL.tHtml) CL.tHtml)

pattern RawTextNode :: a -> a -> a -> Text -> HtmlExpr a
pattern RawTextNode a b c t =
  EApp a
    (EForeign b (Name "Projector.Html.Runtime.Pux.textUnescaped") (TArrow (TLit TString) (TVar (TypeName "Html"))))
    (ELit c (VString t))

-- Combine adjacent raw text nodes.
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
