# Projector

This repository contains a few interrelated projects.

## projector-core

projector-core is an implementation of a typed lambda calculus. It is
based on the total fragment of the Damas-Hindley-Milner calculus, with
the following features:

- Lambda abstraction, application
- Damas-Milner polymorphism (TODO)
- A configurable set of ground/primitive types
- Variants (sum types)
- Records (product types) (TODO)
- Damas-Milner type inference
  - The record system is quite simplistic. Records require some
    hinting! (TODO)
- Termination checker (TODO)

The `projector-core` package includes the syntax and type definitions,
the type checker, a simplifier (normaliser), and Jack generators for
well-typed and ill-typed terms.

## projector-html

projector-html is a typed templating language that produces
well-formed HTML, featuring:

- A surface syntax that desugars into typed projector-core for
  checking and simplification
- A collection of platform-specific backends, which transform the
  simplified projector-core terms into (somewhat) idiomatic native
  code

The objective here is to provide as much actionable feedback as
possible to the frontend developer mechanically, while removing a few
classes of error from production. This eases code review and reduces
deployment risk. If we can produce reusable and efficient code in the
process, that is nice too.

Specifically, the following should no longer be possible:
- Runtime rendering errors
  - Unbound variables will throw type errors
  - Nonsensical expressions will throw type errors
- Infinite loops while rendering

A well-formed template, when rendered, should be a finite
(terminating) projection from the platform's version of the data to
the target platform's notion of HTML.

Backends for `projector-html` aim to provide reusable code in the
target language, such that we can at any point just check in the
generated code and walk away from this tool entirely. For example, a
backend targeting `Hydrant` would render each template accepting a
`Foo` as `Foo -> Html`.

### projector-html-runtime-hs

This is a Haskell Hydrant runtime for `projector-html`.

### projector-html-runtime-purs

This is a Purescript Pux runtime for `projector-html`.

## Conceptual reviewers

- Tim H
- Charles
- Jacob
