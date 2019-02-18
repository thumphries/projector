{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Projector.Core.Check where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           Hedgehog

import           Projector.Core.Prelude

import           Projector.Core.Check
import           Projector.Core.Syntax
import           Projector.Core.Type

import           System.IO (IO)

import           Test.Projector.Core.Gen

import           Text.Show.Pretty (ppShow)


prop_welltyped :: Property
prop_welltyped =
  property $ do
    (ty, ctx, e) <- forAll genWellTypedTestExpr'
    typeCheck ctx e === pure ty

prop_welltyped_letrec :: Property
prop_welltyped_letrec =
  property $ do
    (ctx, mexps) <- forAll genWellTypedTestLetrec
    let etypes = fmap fst mexps
        exprs = fmap snd mexps
        atypes = fmap (fmap (fst . extractAnnotation)) (typeCheckAll ctx exprs)
    atypes === pure etypes

prop_illtyped :: Property
prop_illtyped =
  property $ do
    (ctx, e) <- forAll genIllTypedTestExpr'
    case typeCheck ctx e of
      Left _ ->
        success
      Right ty -> do
        annotate (ppShow ty)
        failure

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
  DRecord [] [
      (FieldName "foo", TLit TBool)
    , (FieldName "bar", TLit TString)
    , (FieldName "baz", TLit TInt)
    ]

dRecord :: TypeDecls TestLitT
dRecord =
  declareType nRecord tRecord mempty

prop_record_unit_empty :: Property
prop_record_unit_empty =
  once $
    first (const ()) (typeCheck dRecord expr)
    ===
    Left ()
  where
    expr = ERec () nRecord [
      ]

prop_record_unit_missing :: Property
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

prop_record_unit_complete :: Property
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

prop_record_unit_extra :: Property
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

prop_record_unit_duplicate :: Property
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

prop_record_unit_prj_extra :: Property
prop_record_unit_prj_extra =
  once $
    typeCheck dRecord rexpr
    ===
    Left [ Annotated () (ExtraRecordField nRecord (FieldName "quux") (TVar (TypeName "b"), ()) ()) ]
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

-- 'id' typechecks on its own
prop_forall_unit_id :: Property
prop_forall_unit_id =
  once $
    typeCheck mempty expr
    ===
    Right
      (Type
         (TForallF
            [ TypeName { unTypeName = "a" } ]
            (Type
               (TArrowF
                  (Type (TVarF TypeName { unTypeName = "a" }))
                  (Type (TVarF TypeName { unTypeName = "a" }))))))
  where
    expr :: Expr TestLitT ()
    expr =
      ELam () (Name "x") Nothing (EVar () (Name "x"))

-- 'id' is usable below
prop_forall_unit_id_app :: Property
prop_forall_unit_id_app =
  once $
    typeCheckIncremental mempty known exprs
    ===
    Right
      (M.fromList
         [ ( Name { unName = "id_bool" }
           , EApp
               ( Type (TLitF TBool) , () )
               (EVar
                  ( Type (TArrowF (Type (TLitF TBool)) (Type (TLitF TBool))) , () )
                  Name { unName = "id" })
               (ELit ( Type (TLitF TBool) , () ) (VBool True))
           )
         , ( Name { unName = "id_int" }
           , EApp
               ( Type (TLitF TInt) , () )
               (EVar
                  ( Type (TArrowF (Type (TLitF TInt)) (Type (TLitF TInt))) , () )
                  Name { unName = "id" })
               (ELit ( Type (TLitF TInt) , () ) (VInt 44))
           )
         ])
  where
    known =
      M.fromList [
          (Name "id", (TForall [TypeName "b"] (TArrow (TVar (TypeName "b")) (TVar (TypeName "b"))), ()))
        ]
    exprs =
      M.fromList [
          (Name "id_bool", EApp () (var_ "id") (lit (VBool True)))
        , (Name "id_int", EApp () (var_ "id") (lit (VInt 44)))
        ]

-- 'id' gets monomorphised within its own declaration scope in absence of explicit constraint
prop_forall_unit_id_app_mono :: Property
prop_forall_unit_id_app_mono =
  once $
    typeCheckAll mempty exprs
    ===
    Left [
        Annotated ()
          (UnificationError (Type (TLitF TInt), ()) (Type (TLitF TBool), ()))
      ]
  where
    exprs :: Map Name (Expr TestLitT ())
    exprs =
      M.fromList [
          (Name "id", ELam () (Name "x") Nothing (EVar () (Name "x")))
        , (Name "id_bool", EApp () (var_ "id") (lit (VBool True)))
        , (Name "id_int", EApp () (var_ "id") (lit (VInt 44)))
        ]

-- 'id' with explicit user-provided type annotation
-- FIXME missing some code to handle this case - want to generalize
{-
--prop_forall_unit_id_explicit =
  once $
    typeCheckIncremental mempty known exprs
    ===
    Left []
  where
    known =
      M.fromList [
          (Name "id", (TForall [TypeName "b"] (TArrow (TVar (TypeName "b")) (TVar (TypeName "b"))), ()))
        ]
    exprs =
      M.fromList [
          (Name "id", ELam () (Name "x") Nothing (EVar () (Name "x")))
        , (Name "id_bool", EApp () (var_ "id") (lit (VBool True)))
        , (Name "id_int", EApp () (var_ "id") (lit (VInt 44)))
        ]
-}

-- 'id' with explicit user-provided type annotation that is too general
prop_forall_unit_id_explicit_neg :: Property
prop_forall_unit_id_explicit_neg =
  once $
    typeCheckIncremental mempty known exprs
    ===
    Left
      [ UnificationError
          ( Type
              (TForallF
                 [ TypeName { unTypeName = "b" } ]
                 (Type
                    (TArrowF
                       (Type (TVarF TypeName { unTypeName = "b" }))
                       (Type (TVarF TypeName { unTypeName = "b" })))))
          , ()
          )
          ( Type
              (TArrowF
                 (Type (TLitF TBool)) (Type (TVarF TypeName { unTypeName = "a" })))
          , ()
          )
      ]
  where
    known =
      M.fromList [
          (Name "id", (TForall [TypeName "b"] (TArrow (TVar (TypeName "b")) (TVar (TypeName "b"))), ()))
        ]
    exprs =
      M.fromList [
          (Name "id", ELam () (Name "x") (Just (TLit TBool)) (EVar () (Name "x")))
        ]

-- -----------------------------------------------------------------------------
-- maybe

prop_maybe_unit_just :: Property
prop_maybe_unit_just =
  once $
    typeCheck maybeDecls expr
    ===
    Right (TApp (TVar (TypeName "Maybe")) (TLit TBool))
  where
    expr =
      ECon () (Constructor "Just") (TypeName "Maybe") [ELit () (VBool True)]

prop_maybe_unit_nothing :: Property
prop_maybe_unit_nothing =
  once $
    typeCheck maybeDecls expr
    ===
    Right (TForall [TypeName "a"] (TApp (TVar (TypeName "Maybe")) (TVar (TypeName "a"))))
  where
    expr :: Expr TestLitT ()
    expr =
      ECon () (Constructor "Nothing") (TypeName "Maybe") []

prop_maybe_unit_case :: Property
prop_maybe_unit_case =
  once $
    typeCheck maybeDecls expr
    ===
    Right (TLit TBool)
  where
    expr =
      ECase ()
        (ECon () (Constructor "Just") (TypeName "Maybe") [ELit () (VBool True)])
        [
          (PCon () (Constructor "Just") [PVar () (Name "x")], EVar () (Name "x"))
        , (PCon () (Constructor "Nothing") [], ELit () (VBool False))
        ]


maybeDecls :: TypeDecls TestLitT
maybeDecls =
  TypeDecls $ M.fromList [
     (TypeName "Maybe", DVariant [TypeName "a"] [
         (Constructor "Just", [TVar (TypeName "a")])
       , (Constructor "Nothing", [])
       ])
   ]

-- -----------------------------------------------------------------------------
prop_either_unit_right :: Property
prop_either_unit_right =
  once $
    typeCheck eitherDecls expr
    ===
    Right (TForall [TypeName "a"] (TApp (TApp (TVar (TypeName "Either")) (TVar (TypeName "a"))) (TLit TString)))
  where
    expr =
      ECon () (Constructor "Right") (TypeName "Either") [ELit () (VString "foobar")]

prop_either_unit_left :: Property
prop_either_unit_left =
  once $
    typeCheck eitherDecls expr
    ===
    Right (TForall [TypeName "a"] (TApp (TApp (TVar (TypeName "Either")) (TLit TBool)) (TVar (TypeName "a"))))
  where
    expr =
      ECon () (Constructor "Left") (TypeName "Either") [ELit () (VBool True)]

prop_either_unit_case :: Property
prop_either_unit_case =
  once $
    typeCheck eitherDecls expr
    ===
    Right (TLit TBool)
  where
    expr =
      ECase ()
        (ECon () (Constructor "Right") (TypeName "Either") [ELit () (VBool True)])
        [
          (PCon () (Constructor "Left") [PVar () (Name "x")], EVar () (Name "x"))
        , (PCon () (Constructor "Right") [PVar () (Name "x")], ELit () (VBool False))
        ]

eitherDecls :: TypeDecls TestLitT
eitherDecls =
  TypeDecls $ M.fromList [
      (TypeName "Either", DVariant [TypeName "a", TypeName "b"] [
            (Constructor "Left", [TVar (TypeName "a")])
          , (Constructor "Right", [TVar (TypeName "b")])
          ])
    ]

-- -----------------------------------------------------------------------------
-- quantified record

prop_blob_unit_rec :: Property
prop_blob_unit_rec =
  once $
    typeCheck blobDecls expr
    ===
    Right (TApp (TVar (TypeName "Blob")) (TLit TBool))
  where
    expr =
      ERec () (TypeName "Blob") [
          (FieldName "value", ELit () (VBool False))
        ]

prop_blob_unit_prj :: Property
prop_blob_unit_prj =
  once $
    typeCheck blobDecls expr
    ===
    Right (TLit TBool)
  where
    expr =
      EPrj ()
        (ERec () (TypeName "Blob") [
            (FieldName "value", ELit () (VBool False))
          ])
        (FieldName "value")

blobDecls :: TypeDecls TestLitT
blobDecls =
  TypeDecls $ M.fromList [
      (TypeName "Blob", DRecord [TypeName "a"] [
          (FieldName "value", TVar (TypeName "a"))
        ])
    ]

once :: PropertyT IO () -> Property
once =
  withTests 1 . property

tests :: IO Bool
tests =
  checkParallel $$(discover)
