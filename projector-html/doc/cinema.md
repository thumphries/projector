# cinema

`cinema` will be the build tool for `projector-html`.

For v1, in goes:
- A set of datatype definitions (`.mcn` - Machinator format)
- A set of templates (`.prj` - `projector-html` format)
- A module prefix
- A backend
- Output directory

... and out comes
- Datatype declarations for the definitions, under `prefix <.> Data`
  (or whatever is reasonable for the chosen backend). One module per
  directory.
- Function declarations for the templates, under `prefix <.>
  Html`. One module per directory.
- (optional) A flurry of errors in human or machine-readable format

It should be a well-behaved unix tool for the most part, including
usage conventions etc.

### v0 usage examples

- If no arguments are supplied, display usage (do not do work)
- If no backend is supplied, perform type checking and then stop.
- Default to human-readable errors to stderr
- It is important that we do our own glob expansion, can't trust shells

```
cinema --backend haskell --prefix "Bikeshed.Projector.NWO" --data '**/*.mcn' --templates '**/*.prj' -o "dist/build/Bikeshed/Projector/NWO/"
```

```
cinema -b purescript -p "Bikeshed.Foo" -d '**/*.mcn' -t '**/*.prj' -o "dist/purs/build/Bikeshed/Foo"
```

### v1 features

#### machine-readable errors

provide a flag that makes all output machine-readable (JSON probably)

#### caching / reuse

`cinema` should cache typechecking/elaboration results and timestamps
for all the files it touches, such that it does the least amount of
work possible when rebuilding. Stash this in .cinema/cache or
something. Invalidate cache on a failed read.

The half-second dev loop is our goal here.

Likewise, specifying one backend per invocation means we're wasting a
lot of effort rechecking everything.
