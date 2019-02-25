# Projector Command Line

There are a few command line tools for projector at the moment, and
all are probably a work in progress. The `projector` executable is
the closest to a project ready tool for building tools, and is used
in anger by a number of projector users.

### Usage

Long form.
```
projector \
  --templates template-dir \    # directory containing projector (.prj) and machinator (.mcn) files.
  --examples examples-dir \     # directory containing projector (.prj) files requiring no arguments.
  --haskell-output gen \        # directory to generate haskell output (from --templates).
  --html-output html \          # directory to generate html examples (from --examples).
  --module-prefix Project \     # specified module prefix for haskell code.
  --watch                       # run the compilation in a loop, watching for file changes.
```

Short form.
```
projector \
  -t template-dir \             # directory containing projector (.prj) and machinator (.mcn) files.
  -e examples-dir \             # directory containing projector (.prj) files requiring no arguments.
  -o gen \                      # directory to generate haskell output (from --templates).
  -h html \                     # directory to generate html examples (from --examples).
  -m Project \                  # specified module prefix for haskell code.
  -w                            # run the compilation in a loop, watching for file changes.
```

### Conventions

The projector cli uses the following conventions.


Given a template + data types files:

```
examples-dir/examples/form.prj
template-dir/components/form.prj
template-dir/components/form.mcn
```


Running projector with:
```
projector -t template-dir -e examples-dir -o gen -h examples -m Project
```

Templates refer to names using paths so there is no tranlsation:
```
components/form
```

Haskell code gen generates a module name (with specified prefix) and camel case:
```
Project.Components.Form.componentsForm
```

Data types are generated in a `.Data` suffixed module:
```
Project.Components.Form.Data
```

### Development Loops

If working on html/styling of components, it is recommended that you create an example
with no arguments that can be turned straight into html. See `test/examples/example.prj`
for an example.

Once you have example html generating, you can just serve the files up. An example of
a caddy configuration that does this:
```
localhost:30080
browse
log stdout
```

If you need to specify a path to css assets or similar, you can add a rewrite rule:
```
rewrite /assets/.* /static/{path}
```

If you are doing haskell development you can use ghci and ctrl+r, or if you want
to have automatic reloading you can run `ghcid` with `Rapid.restart`. To do this
in the main module that starts your web server:

```
import qualified Rapid

_update :: IO ()
_update = do
  Rapid.rapid 0 $ \r ->
    Rapid.restart r ("server" :: [Char]) run -- Where run starts your server.
```

Then run `ghcid` with something like:

```
ghcid -- -T Main._update
```
