# hydrant

Very simple markup combinators for constructing HTML.

- No typeclasses
- Few intermediate datatypes
- Nothing but Text, Builders and newtypes
- No knowledge of any HTML standards
- Tag, attribute, tree construction combinators
- Entity escaping functions
- No pretty-printing, as injecting whitespace alters the rendering semantics
  - Feel free to use one as a post-processor though
  - Maybe hydrant could provide one as an optional thing

## Usage

Hydrant is not meant to be a particularly nice DSL, just a simple
one. If you want to construct some dirt-simple HTML in a small
application, or render HTML from a templating language, you're in the
right place.

```haskell
module HydrantDemo where

import Data.Foldable
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Hydrant
import System.IO (FilePath)

data Foo
  = FooPara Text [Foo]
  | FooImg FilePath

view :: Foo -> Html
view f =
  case f of
    FooPara t foos ->
      parentNode (Tag "p") [] (textNode t <> fold (fmap view foos))
    FooImg fp ->
      voidNode (Tag "img") [
          Attribute (AttributeKey "src") (AttributeValue (T.pack fp))
        ]

renderFooHtml :: Foo -> LT.Text
renderFooHtml =
  toLazyText . view
```
