# Claudius - A fantasy retro computer library.

Claudius started out trying to be a functional library that works like a fantasy console system like [TIC-80](https://tic80.com) or [PICO-8](https://www.lexaloffle.com/pico-8.php): A way to do some retro-style demo graphics programming but in OCaml rather than in LUA. In its current form it doesn't do nearly as much as those fantasy consoles, instead just concentrates on enabling you to make graphical demos, and lacks things like audio, expressive input support, sprite editors and so forth. But if your goal is to enter something like [Tiny Code Christmas](https://tcc.lovebyte.party) or [Genuary](https://genuary.art), then Claudius is designed for that use case.

## Credits

Sincere thanks from Claudius team to all those who have contributed or made suggestions - Claudius is made infinitely better for having diversity of influences!

Claudius uses [Tamzen font](https://github.com/sunaku/tamzen-font) as the default text font.

# Docs

There are [odoc](https://github.com/ocaml/odoc) documentation for most of Claudius. You can find an online version [on the Claudius website](https://claudiusfx.org/claudius/index.html).

There is also a large range of [example programs using Claudius](https://github.com/claudiusFX/claudius-examples/) that you are encourage to experiment with: try running them and then changing the code to see what else you can get them to do!

# Using Claudius

Claudius is a library for OCaml to do retro-style graphics, and so you need to create a new project that uses Cladius. But because Claudius isn't currently in Opam, you'll need to add it into your project using one of the two methods:

## Using Claudius

Claudius is [available via opam](https://opam.ocaml.org/packages/claudius/), and so you should be able to install it by simply running:

```shell
$ opam install cladius
```

And then once that is installed, you can add it as a dependancy to your project in your dune file, like this:

```
(executable
 (public_name my_program)
 (name main)
 (libraries claudius))
```

To see examples of how Claudius is used and learn how it works, we recommend you checkout the [examples library](https://github.com/claudiusFX/claudius-examples), run those, and then try editing the examples to make them do different things!

## Standard keys

Mostly Claudius doesn't have any interaction points beyond those you provide, but there are a few:

* F1 - Show debug overlay
* F2 - Save a screenshot to a GIF
* F3 - Save an animation to a GIF

## Developing Claudius

If you want to make open-source contributions to Claudius, you are welcome to do so. For that you will need to use the below approach

If you're working on Claudius itself, then life is a bit easier using a vendor directory to add a version you can edit and commit to:

```shell
$ dune init proj myprogram
$ cd myprogram
$ mkdir vendor
$ cd vendor
$ echo "(vendored_dirs *)" > dune
$ git clone https://github.com/claudiusFX/Claudius.git
$ cd ..
$ git submodule update --init --recursive
```

You can build that documentation with:

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
