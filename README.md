# Projector

Projector is a templating language that aims to be typed and total.

## Types

Projector should support all of the constructs permitted in
[Machinator](https://github.com/ambiata/machinator). At the very
least, this means we need inductive sums, records, and higher-kinded
types.

Ground types should be configurable. The core language should not be
tied to any particular frontend or backend.

## Foundations

- The core calculus should be strongly normalizing
  - System F / HM with records?
- The template syntax permits only function application, variable
  substitution, and case statements.
- Other templates (partials) are represented as lambda abstractions;
  the user can apply them as any other function.
- Custom functions are defined in Haskell in terms of the core calculus.

### Type inference

Templates and custom functions will have explicit type annotations.
There will be no lambda exposed to the user. We may not need
inference. Stick closely to HM, though, in case we eventually do need
it.

## Dynamics

In comes a rose tree of expressions, out comes a rose tree of
values. We combine the tree according to some user-supplied fold.

We can reduce as far as we like at compile time, and apply rewrite
rules to simplify the expression tree.

## Frontend syntax

(This is a strawman, truly terrible. The frontend will be one of the last
things built.)

- Write your templates in a direct style, referring to things by name.
- Template syntax should permit only function application,
  substitution, and case statements.
- Data declarations are parsed by Machinator and provided to the core
  language.
- Type annotations for templates: ??? open question. likewise, can
  templates have arity greater than 1? should input be bound to `this`?
- Figuring out case syntax is pretty hard; the below is not acceptable


```handlebars
<html>
  <div id={{ attr this.nameid }}>
    {{# case this.content of
	      Foo a b -> b
          Bar c d -> { some kind of template block here {{ renderInt d }} }
    }}
  </div>
</html>
```

... where the types look like this:

```haskell
data Input = Input {
    nameid :: Text
  , content :: FooBar
  }

data FooBar
  = Foo Int Text
  | Bar Int Int
```

... and we have defined the following custom functions:

```haskell
attr :: Text -> AttributeValue
renderInt :: Int -> Text
```

... with quite some handwaving.

## Backends

The types annotated are also available in each target language through
Machinator.

A compiled template should be a simple function from the input
datatype to the result type, e.g. `FooBar -> Html`.

As an intermediate step, we might coerce each type into some generic
form and write an interpreter over that. It would be sound modulo
bugs. We could also write a typesafe renderer using the SOP encoding
from `generics-sop`, as a nice sanity check that is probably also
quite fast.

However, since we have the types present in the target language, we
should be able to produce well-typed projections in that language,
i.e. ordinary functions. Stretch goal.

### Correct HTML frontend + Pux/React backend

The main endgame is to build a system from Projector that produces
well-formed HTML, with both Pux and Haskell backends.

In particular, we should be able to perform correct substitutions for
Attributes (keys, values, or key/value pairs), produce only balanced
tags, etc. We should be able to spit out a Pux or React function, as
well as the stringy representation.

## Conceptual reviewers

- Tim H
- Jacob
- ??? you?
