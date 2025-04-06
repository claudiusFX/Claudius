(lang dune 3.12)

(name claudius)

(generate_opam_files true)

(source
 (github mdales/claudius))

(authors "Michael Dales <michael@digitalflapjack.com>")

(maintainers "Michael Dales <michael@digitalflapjack.com>")

(license ICS)

(documentation https://url/to/documentation)

(package
 (name claudius)
 (synopsis "A retro computer library.")
 (description "A longer description")
 (depends ocaml dune tsdl ounit2 odoc giflib)
 (tags
  (graphics rendering paletted)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
