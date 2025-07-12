(lang dune 3.17)

(name claudius)

(generate_opam_files true)

(source
 (github claudiusFX/claudius))

(authors "3pleX-dev <afuadajoe@gmail.com>" "Adenuga Israel Abimbola <israelabimmbolaa@gmail.com>" "Michael Dales <michael@digitalflapjack.com>" "Seed Lu <dr.seed.lu@gmail.com>" "Shreya Pawaskar <shreya.pawaskar.main@gmail.com>" "Vanisha1606 <vanishag1606@gmail.com>" "Wah Vanessa <wahvanessa22@gmail.com>")

(maintainers "Michael Dales <michael@digitalflapjack.com>" "Shreya Pawaskar <shreya.pawaskar.main@gmail.com>")

(license ICS)

(documentation https://github.com/claudiusFX/claudius)

(package
 (name claudius)
 (synopsis "A retro-style graphics library")
 (description "An functional style retro-graphics library for OCaml for building generative art, demos, and games.")
 (depends ocaml dune tsdl ounit2 odoc giflib crunch ocamlformat)
 (tags
  (graphics rendering paletted)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
