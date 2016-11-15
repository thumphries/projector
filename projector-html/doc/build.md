# Build system sketch

- User types come from Machinator specs
  - They are converted into Haskell by Machinator
  - We convert them into Projector types also
- projector-html primitives (e.g. Text, Int, stringConcat)
  - Defined in a Haskell module
  - Included as ground types and foreign functions in Projector's context
- projector-html library code (e.g. Html, escapeText, ...)
  - Are defined in Projector AST/Decl format
  - Optionally defined in Haskell (if they'll still exist after rewriting)
  - Included as bound types and bound functions in Projector's context
- Templates are parsed and converted into Projector expressions (elaboration)
- All expressions are
  - Typechecked
  - Normalised, i.e. reduced as far as we can go statically
  - Rewritten, i.e. substitute foreign functions all over the place, simplify strings, ...
  - Normalised again, in case we've created new simplification opportunities
  - Converted into Haskell expressions
  - Declared in some Haskell module

## Flow chart

```
+-------------------------+                                 +---------------------------+
| machinator declarations +---------------------------------> Haskell type declarations |
+-----------+-------------+                                 +------------^--------------+
            |                                                            |
            |                                                            |
            |                    +-----------------+        +------------+--------------+
            +--------------------> projector types |        | projector-html primitives |
                                 +--------+--------+        +------------+--------------+
                                          |                              |
                                          +------------------------------+
                                                                         |
    +---------------+           +-----------------------+          +-----v-----+
    |  templates    +-----------> projector expressions +----------> typecheck |
    +---------------+           +-----------^-----------+          +-----+-----+
                                            |                            |
                                            |                            |
                                            |                      +-----v-----+
                                            |                      | normalise |
                                            |                      +-----+-----+
                               +------------+------------+               |
                               |  projector-html library |               |
                               +-------------------------+        +------v--------+
                                                                  | rewrite rules |
                                                                  +------+--------+
                                                                         |
                                                                         |
                                                                   +-----v-----+
                                                                   | normalise |
                                                                   +-----+-----+
                                                                         |
                                                                         |
                                                                         |
                                                                         |
                                                                         |
                                                                         |
                                                                         |
                                                           +-------------v-------------+
                                                           | Haskell expr declarations |
                                                           +---------------------------+
```
