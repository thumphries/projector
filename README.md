# Projector

Projector is a templating language that aims to be typed and total.

## Types

The type system should support
- Inductive sums / variants
- Records
- List and Maybe
- A configurable set of ground types

The core language should not be tied to any particular frontend or
backend.

Anything expressible in
[Machinator](https://github.com/ambiata/machinator) should have a
straightforward equivalent in Projector's core.

## Foundations

- The core calculus should be strongly normalizing
  - System F / HM with records
  - Maybe even STLC if we can get away with it
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

## Frontend sketch

(This is a strawman, truly terrible. The frontend will be one of the last
things built.)

- Write your templates in a direct style, referring to things by name.
- Template syntax should permit only function application,
  substitution, and case statements.
- Data declarations are parsed by Machinator and provided to the core
  language.
- Type annotations for templates: ??? open question. likewise, can
  templates have arity greater than 1? should input be bound to `this`?

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
