(lang dune 3.17)

(name claudius)

(generate_opam_files true)

(source
 (github mdales/claudius))

(authors "Michael Dales <michael@digitalflapjack.com>")

(maintainers "Michael Dales <michael@digitalflapjack.com>")

(license ICS)

(documentation https://url/to/documentation)

(pin
  (url "git+https://github.com/claudiusFX/ocaml-gif.git")
  (package (name giflib)))

(package
 (name claudius)
 (synopsis "A retro computer library")
 (description "A longer description")
 (depends ocaml dune tsdl ounit2 odoc giflib crunch)
 (tags
  (graphics rendering paletted)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
