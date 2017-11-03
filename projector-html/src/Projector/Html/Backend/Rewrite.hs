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

      -- rules for concat
    , (\case EApp a fun@Concat (EList b nodes) ->
               case nodes of
                 [] ->
                   pure (EmptyString a)
                 [x] ->
                   pure x
                 _x -> do
                   nodes' <- foldStrings nodes
                   pure (EApp a fun (EList b nodes'))
             _ ->
               empty)

      -- rule for append
    , (\case EApp a (EApp _ Append (String _ s1)) (String _ s2) ->
               pure (String a (s1 <> s2))
             _ ->
               empty)

      -- rules for list fold
    , (\case EApp a fun@Fold el@(EList b nodes) ->
               case nodes of
                 [] ->
                   pure el
                 [x] ->
                   pure x
                 xs -> do
                   nodes' <- foldLists xs
                   pure (EApp a fun (EList b nodes'))
             _ ->
               empty)
      -- rules for isEmpty
    , (\case EApp a IsEmpty (EList _ nodes) ->
               case nodes of
                 [] ->
                   pure (BTrue a)
                 _x ->
                   pure (BFalse a)
             _ ->
               empty)
    ]

pattern Append :: Expr l a
pattern Append <- (EForeign _ (Name "append") _)

pattern Concat :: Expr l a
pattern Concat <- (EForeign _ (Name "concat") _)

pattern Fold :: Expr l a
pattern Fold <- (EForeign _ (Name "fold") _)

pattern IsEmpty :: Expr l a
pattern IsEmpty <- (EForeign _ (Name "isEmpty") _)

pattern BTrue :: a -> Expr l a
pattern BTrue a = (ECon a (Constructor "True") (TypeName "Bool") [])

pattern BFalse :: a -> Expr l a
pattern BFalse a = (ECon a (Constructor "False") (TypeName "Bool") [])

pattern EmptyString :: a -> HtmlExpr a
pattern EmptyString a = (ELit a (VString ""))

pattern String :: a -> Text -> HtmlExpr a
pattern String a t = ELit a (VString t)

pattern RawString :: a -> a -> Text -> HtmlExpr a
pattern RawString a b t = ECon a (Constructor "Raw") (TypeName "Html") [ELit b (VString t)]

-- Fold together raw text nodes
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

-- fold lists together
foldLists :: [HtmlExpr a] -> Maybe [HtmlExpr a]
foldLists exprs =
  if length (go exprs) == length exprs then empty else pure (go exprs)
  where
    go es =
      case es of
        [] ->
          []
        (EList a l1 : EList _ l2 : xs) ->
          go (EList a (l1 <> l2) : xs)
        (x:xs) ->
          x : go xs
