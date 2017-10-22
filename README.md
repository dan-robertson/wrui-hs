# WebRender UI Haskell bindings

This contains some Haskell bindings
for [WebRender UI](https://github.com/dan-robertson/wrui). There are
some low-level bindings which just give you access to the api in
`Graphics.Rendering.WebRender` and there are is a (slightly) higher-level
interface for laying out boxes in `Graphics.UI.WebRender.Layout`.

Performance seems perfectly fine although the interface makes no
effort to be particularly performant. I suspect this could be improved
with a little care.

# Building

Build by doing

    $ stack build

You will need stack installed. This expects to find `libwrui.so` in
`/usr/lib64/`. One can change this by modifying `stack.yaml` on the
line that starts with `extra-lib-dirs:`.

## Sample applications

There are three simple example applications in `app/`:

* `wilson`—A program which generates and draws random mazes using
  Wilson's algorithm for random spanning trees. Namely it goes on
  random walks erasing any loops. This only uses the lower level api.
  Optionally takes the size of the (square) maze as a parameter
  (default 100)
* `words`—A stress-test for laying out text. Give it a file with a load
  of words in it (or by default it will try `/usr/share/dict/words`)
  and it will draw lines of random text with various alignments and
  wrapping. Click to change the justification being used.
* `calc`—A very simple RPN calculator.

Run one by doing `$ stack exec APPNAME`.
