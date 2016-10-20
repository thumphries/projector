# projector-html

A HTML frontend for projector.

The goal here is to provide a frontend that looks like a regular
templating language, but desugars to typed HTML plus projector
expressions. A valid template, when applied, should produce a
structured HTML document (and not a string).

`projector-html` will include
- Lexer and parser for our Template syntax
- Code to elaborate this syntax into Projector core
- HTML types in Projector
- Projector functions (helpers) to ease working with said types

## Types

Just a sketch.

We can model most of the DOM as a rose tree, with tags and attributes
on every node. However, the
[HTML spec](https://html.spec.whatwg.org/#elements-2) is a little more specific.

We also need to distinguish between the various parts of an
`Attribute`, as users may want to splice in just a key, just a value,
or a whole pair.

Comments will need special handling, as they should not be escaped,
unlike plaintext.

```haskell
data Html
  = Element Tag [Attribute] [Html]        -- Most elements
  | VoidElement Tag [Attribute]           -- Self-closing tags
  | Comment Text                          -- <!-- abc -->
  | Plain Text                            -- flarghletargle

data Tag
  = Tag Text                              -- e.g. Tag "div"

data Attribute
  = Attribute AttributeKey AttributeValue -- e.g. id=123

data AttributeKey
  = AttributeKey Text

data AttributeValue
  = AttributeValue Text
  -- might need to support integers here
```


### Event handlers

A supplementary goal is to allow annotation of templates with event
handler stubs, such that we can write Pux or Elm-style interactive
programs and use our templates directly.

The Haskell backend should just erase or ignore the stubs.

Couple of approaches are possible here:
- Construct these stubs as data and interpret the resulting structure
- The language compiles to expressions in some monad transformer,
  provide such foreign primitive functions
- AST annotations that we can statically analyze
- Pass them in as abstract lambdas, and erase them for the Haskell edition

## Syntax

Just a sketch.

- Jumps between markup mode and expression mode
  - Only the lexer knows about this
- Markup mode uses forced indentation
  - Eliminates a lot of pointless whitespace nodes
- Projector expressions are always delimited by { }
- Tags must be closed/matched within visible scope
- Record projection using . notation, `abc.def`
- Explicit type annotations, syntax likely to change

Some examples of templates and how they'd desugar below.

---

```html
\ value : Text ->
<html>
  { text value }
</html>
```

```haskell
\v -> Element (Tag "html") [] [Plain v]
```

---

```html
\ key : AttributeKey
  value : AttributeValue
  attribute :: Attribute ->
<div { key }="literal" id={ value } { attribute }>
  abcdefg
</div>
```

```haskell
\k v a ->
  Element (Tag "div") [
      Attribute k (AttributeValue "literal")
	, Attribute (AttributeKey "id") v
	, a
	] [Plain "abcdefg"]
```

---

Here, the first and last newlines are ignored, as is the indent level.

```html
\ foo : Html
  bar : Text ->
<html>
  { foo }
  { text bar }
</html>
```

```haskell
\f ->
  \b ->
    Element (Tag "html" [] [
        f
	  , Plain "\n"
	  , Plain b
      ]
```

---

We also support this kind of quick inline definition.

```html
\ foo : Html
  bar : Text ->
<i>{ foo }{ text bar }</i>
```

```haskell
\f ->
  \b ->
     Element (Tag "i" [] [
         f
	   , Plain b
       ]
```

---

String concatenation into Attributes will likely be necessary. This
is something we'd bolt onto the grammar sometime after the POC.

```html
<div id="abcdef{ ghi jkl mno }ghijklmno">
```

```haskell
\ ghi jkl mno ->
  Element (Tag "div" [(Attribute (AttributeKey "id") (AttributeValue $ "abcdef" <> (ghi jkl mno) <> "ghijklmno"))] [])
```

ignore the infix operators etc.
