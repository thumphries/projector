{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Projector.Html.Backend.Purescript (
    renderModule
  , renderExpr
  ) where


import           Data.Functor.Identity  (Identity, runIdentity)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           P

import           Projector.Core

import           Projector.Html.Backend.Data
import           Projector.Html.Core.Prim as Prim

import           System.IO  (FilePath)

import           Text.PrettyPrint.Annotated.Leijen  (Doc, (<+>), (</>), (<$$>))
import qualified Text.PrettyPrint.Annotated.Leijen as WL


-- -----------------------------------------------------------------------------

renderModule :: ModuleName -> Module a -> (FilePath, Text)
renderModule mn@(ModuleName n) m =
  let modName = T.unwords ["module", n, "where"]
      imports = fmap (uncurry genImport) (M.toList (moduleImports m))
      decls = fmap prettyUndecorated (genModule m)
  in (genFileName mn, T.unlines $ mconcat [
         [modName]
       , imports
       , decls
       ])

renderExpr :: Name -> HtmlExpr a -> Text
renderExpr n =
  prettyUndecorated . genExpDec n

genModule :: Module a -> [Doc a]
genModule (Module ts _ es) =
     genTypeDecs ts
  <> (mconcat . with (M.toList es) $ \(n, (ty, e)) ->
       [genTypeSig n ty, genExpDec n e])

genImport :: ModuleName -> Imports -> Text
genImport (ModuleName n) imports =
  T.unwords [
      "import"
    , n
    , case imports of
        OpenImport ->
          T.empty
        OnlyImport quals ->
          "(" <> T.intercalate ", " (fmap unName quals) <> ")"
    ]

genFileName :: ModuleName -> FilePath
genFileName (ModuleName n) =
  T.unpack (T.replace "." "/" n)

-- -----------------------------------------------------------------------------

genTypeDecs :: HtmlDecls -> [Doc a]
genTypeDecs =
  fmap (uncurry genTypeDec) . M.toList . unTypeDecls

genTypeDec :: TypeName -> HtmlDecl -> Doc a
genTypeDec (TypeName n) ty =
  case ty of
    DVariant cts ->
      WL.hang 2
        (text "data" <+> text n <$$> text "="
          WL.<> (foldl'
                  (<+>)
                  WL.empty
                  (WL.punctuate (WL.linebreak WL.<> text "|") (fmap (uncurry genCon) cts))))

genCon :: Constructor -> [HtmlType] -> Doc a
genCon (Constructor c) ts =
  WL.hang 2 (text c WL.<> foldl' (<+>) WL.empty (fmap genType ts))

genType :: HtmlType -> Doc a
genType ty =
  case ty of
    TLit l ->
      text (ppGroundType l)

    TVar (TypeName n) ->
      text n

    TArrow t1 t2 ->
      WL.parens (genType t1 <+> text "->" <+> genType t2)

    TList t ->
      WL.parens (text "Array" <+> genType t)

genTypeSig :: Name -> HtmlType -> Doc a
genTypeSig (Name n) ty =
  WL.hang 2 (text n <+> "::" <+> genType ty)

genExpDec :: Name -> HtmlExpr a -> Doc a
genExpDec (Name n) expr =
  WL.hang 2 (text n <+> text "=" <$$> genExp expr)

genExp :: HtmlExpr a -> Doc a
genExp expr =
  case expr of
    ELit a v ->
      WL.annotate a (genLit v)

    EVar a (Name x) ->
      WL.annotate a (text x)

    ELam a (Name n) _ body ->
      WL.annotate a (WL.hang 2 (WL.parens (text ("\\" <> n <> " -> ") <$$> genExp body)))

    EApp a fun arg ->
      WL.annotate a (WL.hang 2 (WL.parens (genExp fun </> genExp arg)))

    ECon a (Constructor c) _ es ->
      WL.annotate a (WL.nest 2 (WL.parens (text c <+> WL.fillSep (fmap genExp es))))

    ECase a f bs ->
      WL.annotate a
       (WL.hang 2
         (WL.parens
                ((text "case" <+> genExp f <+> text "of")
           <$$> (foldr (\(p, g) doc -> WL.hang 2 (genMatch p g) <$$> doc)
                      WL.empty
                      bs))))

    EList a _ es ->
      WL.annotate a (WL.hang 2 (WL.list (fmap genExp es)))

    EForeign a (Name n) _ ->
      WL.annotate a (text n)

genMatch :: Pattern -> HtmlExpr a -> Doc a
genMatch p e =
  WL.hang 2 ((genPat p WL.<> text " ->") <$$> genExp e)

genPat :: Pattern -> Doc a
genPat p =
  case p of
    PVar (Name n) ->
      text n
    PCon (Constructor n) ps ->
      WL.parens (text n <+> WL.hsep (fmap genPat ps))

genLit :: Value PrimT -> Doc a
genLit v =
  case v of
    VString x ->
      WL.dquotes (text x)

-- -----------------------------------------------------------------------------

text :: Text -> Doc a
text =
  WL.string . T.unpack

pretty :: Doc a -> WL.SimpleDoc a
pretty =
  WL.renderPretty 0.4 100

prettyDecorated :: (a -> Text) -> (a -> Text) -> Doc a -> Text
prettyDecorated start end =
  runIdentity . WL.displayDecoratedA str (pure . start) (pure . end) . pretty
  where
    str :: [Char] -> Identity Text
    str = pure . T.pack

prettyUndecorated :: Doc a -> Text
prettyUndecorated =
  prettyDecorated (const mempty) (const mempty)
