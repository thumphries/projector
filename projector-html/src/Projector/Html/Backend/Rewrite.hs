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
      -- concat of a singleton - id
    , (\case EApp _ (EForeign _ (Name "concat") _) (EList _ [x]) ->
               pure x
             _ ->
               empty)
    ]


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
