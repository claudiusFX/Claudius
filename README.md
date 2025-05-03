# Claudius - A fantasy retro computer library.

Claudius started out trying to be a functional library that works like a fantasy console system like [TIC-80](https://tic80.com)or [PICO-8](https://www.lexaloffle.com/pico-8.php): A way to do some retro-style demo graphics progeramming but in OCaml rather than in LUA. In its current form it doesn't do nearly as much as those fantasy consoles, instead just concentrating on enabling you to make graphical demos, and lacks things like audio, expressive input support, sprite editors and so forth. But if your goal is to enter something like [Tiny Code Christmas](https://tcc.lovebyte.party) or [Genuary](https://genuary.art), then Claudius is designed for that use case.

## Credits

Sincere thanks from Claudius team to all those who have contributed or made suggestions - Claudius is made infinitely better for having diversity of influences!

Claudius uses [Tamzen font](https://github.com/sunaku/tamzen-font) as the default text font.


# Using Claudius

Claudius is a library for OCaml to do retro-style graphics, and so you need to create a new project that uses Cladius. But because Claudius isn't currently in Opam, you'll need to add it into your project using one of the two methods:

## Using Claudius

You should use dune to pin Claudius by adding it to the `dune-project` file for your repository - you can find an [example project here](https://github.com/gridbugs/hello-claudius). Basically you need to do two things:

Add the following to your dune-project file:

```
(pin
 (url "git+https://github.com/claudiusFX/Claudius.git")
 (package
  (name claudius)))
```

Then run:

```shell
$ dune pkg lock
```

## Developing Claudius

If you're working on Claudius itself, then life is a bit easier using a vendor directory to add a version you can edit and commit to:

```shell
$ dune init proj myprogram
$ cd myprogram
$ mkdir vendor
$ cd vendor
$ echo "(vendored_dirs *)" > dune
$ git clone https://github.com/claidiusFX/claudius.git
$ cd ..
$ git submodule update --init --recursive
```

# Docs

There are [odoc](https://github.com/ocaml/odoc) documentation for most of Claudius. You can build that documentation with:

```shell
$ dune build @doc
$ open _build/default/_doc/_html/index.html
 ```
 Or you can use whatever browser directly to open that index file.

 # Requirements

 Claudius has been tested under macOS, Linux, and Windows via WSL, and requires that you have [SDL](https://www.libsdl.org) 2 installed.

 It requires OCaml 5 or newer ([see here for installation instructions](https://ocaml.org/releases/5.3.0#installation-instructions)), and relies on [tsdl](https://github.com/dbuenzli/tsdl) for talking to SDL, and [ounit2](https://opam.ocaml.org/packages/ounit2/) for unit tests.

# Troubleshooting

Some users running programs built with Claudius on Ubuntu via WSL may experience a segmentation fault causing the SDL window to crash. It can be fixed with adding the following environment variable before running your program. In your terminal enter the following commands:

```shell
$ export LIBGL_ALWAYS_SOFTWARE=1
$ dune exec myprogram
 ```
If you are using bash, you can add the above environment variable to your bashrc file:

```shell
$ echo 'export LIBGL_ALWAYS_SOFTWARE=1' >> ~/.bashrc
$ source ~/.bashrc
$ dune exec myprogram
 ```
