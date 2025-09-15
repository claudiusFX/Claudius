(lang dune 3.18)

(name claudius)

(generate_opam_files true)

(source
 (github claudiusFX/claudius))

(authors "3pleX-dev <afuadajoe@gmail.com>" "Adenuga Israel Abimbola <israelabimmbolaa@gmail.com>" "Michael Dales <michael@digitalflapjack.com>" "Seed Lu <dr.seed.lu@gmail.com>" "Shreya Pawaskar <shreya.pawaskar.main@gmail.com>" "Vanisha1606 <vanishag1606@gmail.com>" "Wah Vanessa <wahvanessa22@gmail.com>")

(maintainers "Michael Dales <michael@digitalflapjack.com>" "Shreya Pawaskar <shreya.pawaskar.main@gmail.com>")

(maintenance_intent "(latest)")

(license ISC)

(documentation https://github.com/claudiusFX/claudius)

(package
 (name claudius)
 (synopsis "A retro-style graphics library")
 (description "A functional style retro-graphics library for OCaml for building generative art, demos, and games.")
 (depends (ocaml (>= 5.1)) dune (tsdl (>= 1.1.0)) (ounit2 :with-test) (odoc :with-doc) (giflib (>= 1.0.3)) (imagelib (>= 20221222)) (crunch (>= 4.0.0)) (hsluv (>= 0.1.0)) (ocamlformat (and (>= 0.27.0) :with-dev-setup )))
 (tags
  (graphics rendering paletted)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
