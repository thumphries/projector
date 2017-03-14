{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Projector.Core.Check where


import           Disorder.Core
import           Disorder.Jack

import           P

import           Projector.Core.Check
import           Projector.Core.Syntax
import           Projector.Core.Type

import           Test.Projector.Core.Arbitrary

import           Text.Show.Pretty (ppShow)


prop_welltyped =
  gamble genWellTypedTestExpr' $ \(ty, ctx, e) ->
    typeCheck ctx e === pure ty

prop_welltyped_shrink =
  jackShrinkProp 5 genWellTypedTestExpr' $ \(ty, ctx, e) ->
    typeCheck ctx e === pure ty

prop_welltyped_letrec =
  gamble genWellTypedTestLetrec $ \(ctx, mexps) ->
    let etypes = fmap fst mexps
        exprs = fmap snd mexps
        atypes = fmap (fmap (fst . extractAnnotation)) (typeCheckAll ctx exprs)
    in atypes === pure etypes

prop_illtyped =
  gamble genIllTypedTestExpr' $ \(ctx, e) ->
    property
      (case typeCheck ctx e of
         Left _ ->
           property True
         Right ty ->
           counterexample (ppShow ty) (property False))

prop_illtyped_shrink =
  jackShrinkProp 5 genIllTypedTestExpr' $ \(ctx, e) ->
    property (isLeft (typeCheck ctx e))

-- these are disabled until we can represent type schemes
-- (sometimes functions will simplify into id, which we can't type)

-- prop_nf_consistent =
--   gamble genWellTypedTestExpr' $ \(ty, ctx, e) ->
--     typeCheck ctx (nf mempty e) === pure ty
--
-- prop_whnf_consistent =
--   gamble genWellTypedTestExpr' $ \(ty, ctx, e) ->
--     typeCheck ctx (whnf mempty e) === pure ty

-- -----------------------------------------------------------------------------
-- Unit tests

-- these should go away when the ill typed generator is cleaned up a bit
-- (was having problems with ensuring there's a record in scope)

nRecord :: TypeName
nRecord =
  TypeName "Record"

tRecord :: Decl TestLitT
tRecord =
  DRecord [
      (FieldName "foo", TLit TBool)
    , (FieldName "bar", TLit TString)
    , (FieldName "baz", TLit TInt)
    ]

dRecord :: TypeDecls TestLitT
dRecord =
  declareType nRecord tRecord mempty

prop_record_unit_empty =
  once $
    first (const ()) (typeCheck dRecord expr)
    ===
    Left ()
  where
    expr = ERec () nRecord [
      ]

prop_record_unit_missing =
  once $
    typeCheck dRecord expr
    ===
    Left [ MissingRecordField nRecord (FieldName "bar") (TLit TString, ()) () ]
  where
    expr = ERec () nRecord [
        (FieldName "foo", lit (VBool True))
      , (FieldName "baz", lit (VInt 43))
      ]

prop_record_unit_complete =
  once $
    typeCheck dRecord expr
    ===
    Right (TVar nRecord)
  where
    expr = ERec () nRecord [
        (FieldName "bar", lit (VString "bar!"))
      , (FieldName "foo", lit (VBool True))
      , (FieldName "baz", lit (VInt 43))
      ]

prop_record_unit_extra =
  once $
    typeCheck dRecord expr
    ===
    Left [ ExtraRecordField nRecord (FieldName "quux") (TLit TBool, ()) () ]
  where
    expr = ERec () nRecord [
        (FieldName "bar", lit (VString "bar!"))
      , (FieldName "foo", lit (VBool True))
      , (FieldName "baz", lit (VInt 43))
      , (FieldName "quux", lit (VBool False))
      ]

prop_record_unit_duplicate =
  once $
    typeCheck dRecord expr
    ===
    Left [ DuplicateRecordFields nRecord [ FieldName "bar" ] () ]
  where
    expr = ERec () nRecord [
        (FieldName "bar", lit (VString "bar!"))
      , (FieldName "foo", lit (VBool True))
      , (FieldName "baz", lit (VInt 43))
      , (FieldName "bar", lit (VString "bar!"))
      ]

prop_record_unit_prj_extra =
  once $
    typeCheck dRecord rexpr
    ===
    Left [ ExtraRecordField nRecord (FieldName "quux") (TVar (TypeName "b"), ()) () ]
  where
    rexpr =
      EApp ()
        (ELam () (Name "x") (Just (TLit TBool)) (EVar () (Name "x")))
        (EPrj ()
          (ERec () nRecord [
              (FieldName "bar", lit (VString "bar!"))
            , (FieldName "foo", lit (VBool True))
            , (FieldName "baz", lit (VInt 43))
            ])
          (FieldName "quux"))



return []
tests = $disorderCheckEnvAll TestRunNormal
