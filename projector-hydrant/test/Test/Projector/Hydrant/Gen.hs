{-# LANGUAGE OverloadedStrings #-}
module Test.Projector.Hydrant.Gen where


import           Data.Foldable (fold)
import           Data.Monoid ((<>))
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text as T

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Projector.Hydrant

-- structured representation for testing purposes
data TagTree
  = TagNode Tag [Attribute] [TagTree]
  | TagVoidNode Tag [Attribute]
  | TagText Text
  | Doctype Text
  | Comment Text
  deriving (Eq, Show)

genTagTree :: Gen TagTree
genTagTree =
  Gen.sized (genTagTree' . fromIntegral)

genTagTree' :: Int -> Gen TagTree
genTagTree' k
  | k <= 2 = Gen.choice [genTxt, genComment, genDoctype]
  | k <= 10 = Gen.choice [genVoid k, genNode k]
  | otherwise = genNode k
  where
    genAttr = Attribute <$> genAttributeKey <*> genAttributeValue
    genTxt = fmap TagText genUtf81
    genComment = fmap Comment genValidComment
    genDoctype = fmap Doctype genValidDoctype
    genVoid x =
      TagVoidNode
        <$> genTag
        <*> Gen.list (Range.singleton (max 0 (x - 1))) genAttr
    genNode x = do
      t <- genTag
      nats <- Gen.int (Range.linear 0 (max 0 (x-1)))
      attrs <- Gen.list (Range.singleton nats) genAttr
      let x' = max 0 (x - nats - 1)
      (TagNode t attrs . merge) <$> subtree x' []

subtree :: Int -> [TagTree] -> Gen [TagTree]
subtree 0 acc = pure acc
subtree k acc = do
  j <- Gen.int (Range.linear 1 k)
  t <- genTagTree' j
  subtree (k - j) (t:acc)

merge :: [TagTree] -> [TagTree]
merge tt =
  case tt of
    (TagText a : TagText b : ttt) ->
      merge (TagText (a <> b) : ttt)
    (a : bs) ->
      a : merge bs
    [] ->
      []

genTag :: Gen Tag
genTag =
  fmap Tag (Gen.element muppets)

genAttributeKey :: Gen AttributeKey
genAttributeKey =
  fmap AttributeKey (Gen.element simpsons)

genAttributeValue :: Gen AttributeValue
genAttributeValue =
  fmap AttributeValue $ Gen.choice [
      Gen.element viruses
    , genUtf81
    ]

tagTreeHtml :: TagTree -> Html
tagTreeHtml (TagNode t a ts) =
  parentNode t a (foldMap tagTreeHtml ts)
tagTreeHtml (TagVoidNode t a) =
  voidNode t a
tagTreeHtml (TagText t) =
  textNode t
tagTreeHtml (Doctype t) =
  doctype t
tagTreeHtml (Comment t) =
  comment t

genHtml :: Gen Html
genHtml =
  fmap tagTreeHtml genTagTree

genUtf81 :: Gen Text
genUtf81 =
  Gen.text (Range.linear 1 100) Gen.unicode

-- just shrink from both sides
shrinkText :: Text -> [Text]
shrinkText t
  | T.length t <= 1 = []
  | T.length t == 2 =
      case T.unpack t of
        (a:b:[]) ->
          [T.singleton a, T.singleton b]
        _ ->
          []
  | otherwise =
      let (heads, tails) = (T.drop 1 t, T.dropEnd 1 t) in
      fold [[heads, tails], shrinkText heads, shrinkText tails]

-- From W3C HTML 5 Recommendation Section 8.1.6:
--
-- Comments must start with the four character sequence U+003C
-- LESS-THAN SIGN, U+0021 EXCLAMATION MARK, U+002D HYPHEN-MINUS,
-- U+002D HYPHEN-MINUS (<!--). Following this sequence, the comment
-- may have text, with the additional restriction that the text must
-- not start with a single ">" (U+003E) character, nor start with a
-- U+002D HYPHEN-MINUS character (-) followed by a ">" (U+003E)
-- character, nor contain two consecutive U+002D HYPHEN-MINUS
-- characters (--), nor end with a U+002D HYPHEN-MINUS character
-- (-). Finally, the comment must be ended by the three character
-- sequence U+002D HYPHEN-MINUS, U+002D HYPHEN-MINUS, U+003E
-- GREATER-THAN SIGN (-->).
genValidComment :: Gen Text
genValidComment =
  fmap (T.replace "\v" "" . T.replace "-" "\\-") . flip Gen.filter genUtf81 $ \t ->
    and [
        T.take 1 t /= ">"
      , T.take 2 t /= "->"
      , T.takeEnd 1 t /= "-"
      ]

genValidDoctype :: Gen Text
genValidDoctype =
  fmap (T.filter (/= '"') . T.filter (/= '>') . T.filter (/= '\'')) . flip Gen.filter genValidComment $ \t ->
    and [
        T.takeEnd 1 t /= "/"
      ]

simpsons :: IsString a => [a]
simpsons = [
    "homer"
  , "marge"
  , "maggie"
  , "lisa"
  , "bart"
  , "flanders"
  , "moe"
  , "barney"
  ]

muppets :: IsString a => [a]
muppets = [
    "kermit"
  , "gonzo"
  , "fozzy"
  , "chef"
  , "statler"
  , "waldorf"
  , "beaker"
  , "animal"
  ]

viruses :: IsString a => [a]
viruses = [
    "rotavirus"
  , "smallpox"
  , "norovirus"
  , "chickenpox"
  , "camelpox"
  , "dengue"
  , "echovirus"
  , "equine morbillivirus"
  , "gou virus"
  , "measles"
  , "monkeypox"
  ]
