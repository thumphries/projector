#########
Projector
#########

Projector is a typed, total [#total]_ HTML templating language.

Projector templates can be typechecked, statically analyzed, partially
evaluated, optimised via rewrite rules, and compiled to any sensible
DOM representation.

Projector was designed to enable progressive enhancement, but is also
useful as a general-purpose type-safe templating system. It provides a
number of static guarantees and a fast feedback cycle, while also
decoupling your views from your current choice of framework.

It is currently in a pre-release [#prerelease]_ state, but has been
used extensively in production by a team made up of both designers and
engineers.

.. contents:: **Table of Contents**
  :backlinks: none

********
Features
********

- Rich HTML model
- Sum types, records, typed literals
- Type inference
- Typed holes
- Simplification via partial evaluation and rewrite rules
- Compiles to a variety of backends
- Interpreter (Haskell-embedded)

*********
Use cases
*********

General-purpose templating
==========================

Projector can be used wherever you need to write HTML.

Progressive enhancement
=======================

Projector templates can be run natively on both client and server.

Portability
===========

Write your views in Projector and compile them to any supported
platform. When a new frontend framework comes out, write a new backend
and migrate straight to it. If Projector is not your jam, just check
in the generated code and walk away.

Performance
===========

A typed, total representation enables a range of compile-time
optimisations, including partial evaluation and rewriting of algebraic
identities.

Note that this claim is mostly hot air. No real benchmarking has been
done. Normalisation and rewriting does have a significant effect on
the size of the generated code, however.

********
Examples
********

Projector turns your data into DOM. A typical Projector workflow has
two steps:

- Model your data
- Write functions

The simplest template looks much like plain HTML::

  <p>Hello, world!</p>

If you want to say hello to multiple people and don't feel like
writing a template each time, your template can take arguments::

  \ name : String -> Html =
  <p>Hello, {{name}}!</p>

We are not limited to `String`! Let's write some datatypes::

  data Greeting =
      Hello Name
    | Goodbye

  record Name = {
      is : String
    }

... and use them in a more complex template::

  \ greet : Greeting -> Html =
  <div>
    { case greet of
        Hello name ->
          <p>Hello, {{ name.is }}!</p>
        Goodbye ->
          <p>Goodbye!</p>
    }
  </div>

We can write simple combinators, too::

  \ col1 : Html -> col2 : Html -> Html =
  <div class="col-1">
    { col1 }
  </div>
  <div class="col-2">
    { col2 }
  </div>

********
Backends
********

The Projector backend scene is currently a little bit
limited. Currently we support only Haskell (via Hydrant) and
Purescript (only tested with Pux).

However, supporting additional languages / frameworks is fairly easy,
as long as that language can represent functions and a consistent
representation for all the datatypes Projector supports.

The existing backends run over the simplified and typechecked
AST. Thus far, they have all been a handful of rewrite rules, a couple
of heuristics, and a syntax-directed function producing a new AST.

Haskell
=======

Hydrant
-------

Hydrant is a small HTML library of very limited scope, originally
designed to better support Projector.

Purescript
==========

Note that the Purescript backends all require a second round of
typechecking. This is not ideal. It would be smarter to just generate
the JS directly using some optimising intermediate representation.
However, a second typechecker is a rather helpful counterbalance when
generating code!

Prospective backends
====================

Projector could and should support a variety of backend targets.

Volunteer labour would be helpful! If you think you could take on such
a project, please do get in touch. It would require deep knowledge of
the target platform and a willingness to get knee deep in an
unfamiliar compiler.

**********************
Implementation details
**********************

Projector is little more than a collection of well-understood
techniques glued together and made to do templating.

- Lambda calculus
- Heeren-style implementation of Hindley-Damas-Milner type inference
- Lennart Augustsson's trick for neat type errors
- Homespun record system
- Extremely naive beta/eta reduction
- Extremely naive rewrite engine
- Syntax-directed translation to various backends
- Extensive property-based testing

The core calculus is fairly generic and could be used for other
purposes, though you may run into some cut corners.

*********
Blemishes
*********

- The language is being grown on an as-needed basis. Thus, your
  workflow may not be supported.
- Computation has been pushed out of the templating language wherever
  possible, along with side effects. The expression language is rather
  underpowered.
- The set of literals and the type language are somewhat
  arbitrary.
- The type declaration language currently lives in an external codebase.
- The language has no notion of module, and no let construct. These
  are frequent feature requests that will hopefully be addressed soon.
- There aren't yet enough backends.
- Projector feels a lot like Haskell, because it was designed by
  Haskell developers. Little usability testing has been done.
- Codebase is slowly transitioning from an in-house project to an
  open-source one suitable for use by third parties. This will take time!
- Most of the Projector code in existence is proprietary. Hopefully
  this will change!

*********
Footnotes
*********

.. [#total] The core calculus itself is not total. Projector relies on
  a series of syntactic restrictions to ensure users write total
  programs. We believe a termination checker could be bolted on fairly
  easily.
.. [#prerelease] Much of Projector's design is ad-hoc. Many choices
  were made to reduce scope and expedite an MVP. The featureset and
  syntax may change dramatically in ways that are not guaranteed to be
  backwards-compatible. Treat this release as a technology preview.
.. [#higherordertypes] The core calculus supports parameterised types
  just fine. Syntactic restrictions prevent users from creating their
  own. This restriction may be lifted at some point.


.. image:: https://circleci.com/gh/thumphries/projector.svg?style=svg
    :target: https://circleci.com/gh/thumphries/projector
