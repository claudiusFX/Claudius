# Claudius - A fantasy retro computer library.

Claudius started out trying to be a functional library that works like a fantasy console system like [TIC-80](https://tic80.com)or [PICO-8](https://www.lexaloffle.com/pico-8.php): A way to do some retro-style demo graphics progeramming but in OCaml rather than in LUA. In its current form it doesn't do nearly as much as those fantasy consoles, instead just concentrating on enabling you to make graphical demos, and lacks things like audio, expressive input support, sprite editors and so forth. But if your goal is to enter something like [Tiny Code Christmas](https://tcc.lovebyte.party) or [Genuary](https://genuary.art), then Claudius is designed for that use case.

# Docs

There are [odoc](https://github.com/ocaml/odoc) documentation for most of Claudius. You can build that documentation with:

```
$ dune build @doc
$ open _build/default/_doc/_html/index.html
 ```
 Or you can use whatever browser directly to open that index file.

 # Requirements

 Claudius has been tested under macOS, Linux, and Windows via WSL, and requires that you have [SDL](https://www.libsdl.org) 2 installed.

 It requires OCaml 5 or newer ([see here for installation instructions](https://ocaml.org/releases/5.3.0#installation-instructions)), and relies on [tsdl](https://github.com/dbuenzli/tsdl) for talking to SDL, and [ounit2](https://opam.ocaml.org/packages/ounit2/) for unit tests.
