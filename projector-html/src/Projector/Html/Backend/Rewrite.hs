{- | Global (backend-independent) rewrite rules. These should be type-preserving. -}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Projector.Html.Backend.Rewrite (
    globalRules
  ) where


import           P

import           Projector.Core
import           Projector.Html.Data.Prim


globalRules :: [RewriteRule PrimT a]
globalRules =
  fmap Rewrite [
      -- adjacent plaintext nodes - fold together
      (\case ECon a (Constructor "Nested") ty [EList b nodes] -> do
               nodes' <- foldRaw nodes
               pure (ECon a (Constructor "Nested") ty [EList b nodes'])
             _ ->
               empty)

      -- concat of an empty list - empty string
    , (\case EApp a Concat (EList _ []) ->
               pure (ELit a (VString ""))
             _ ->
               empty)
      -- concat of a singleton - id
    , (\case EApp _ Concat (EList _ [x]) ->
               pure x
             _ ->
               empty)
      -- adjacent strings in a concat - fold together
    , (\case EApp a fun@Concat (EList b nodes) -> do
               nodes' <- foldStrings nodes
               pure (EApp a fun (EList b nodes'))
             _ ->
               empty)
    ]

pattern Concat <- (EForeign _ (Name "concat") _)


-- Fold together raw text nodes
pattern RawString a b t = ECon a (Constructor "Raw") (TypeName "Html") [ELit b (VString t)]

foldRaw :: [HtmlExpr a] -> Maybe [HtmlExpr a]
foldRaw exprs =
  if length (go exprs) == length exprs then empty else pure (go exprs)
  where
    go es =
      case es of
        [] ->
          []
        (RawString a _ t1 : RawString b _ t2 : xs) ->
          go (RawString a b (t1 <> t2) : xs)
        (x:xs) ->
          x : go xs

-- concat strings together
pattern String a t = ELit a (VString t)

foldStrings :: [HtmlExpr a] -> Maybe [HtmlExpr a]
foldStrings exprs =
  if length (go exprs) == length exprs then empty else pure (go exprs)
  where
    go es =
      case es of
        [] ->
          []
        (String a t1 : String _ t2 : xs) ->
          go (String a (t1 <> t2) : xs)
        (x:xs) ->
          x : go xs
