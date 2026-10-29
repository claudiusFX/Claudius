(lang dune 3.18)

(name claudius)

(generate_opam_files true)

(source
 (github claudiusFX/claudius))

(authors "3pleX-dev" "Adenuga Israel Abimbola" "Michael Dales" "Patrick Ferris" "Seed Lu" "Shreya Pawaskar" "Vanisha1606" "Wah Vanessa")

(maintainers "Michael Dales <michael@digitalflapjack.com>")

(maintenance_intent "(latest)")

(license ISC)

(package
 (name claudius)
 (documentation https://github.com/claudiusFX/claudius)
 (synopsis "A retro-style graphics library")
 (description "A functional style retro-graphics library for OCaml for building generative art, demos, and games. This is the core feature library, must be used with either the SDL or Javascript backends.")
 (depends
    (ocaml (>= 5.1))
    dune
    (ounit2 :with-test)
    (odoc :with-doc)
    (giflib (>= 1.1.0))
    (imagelib (>= 20221222))
    (crunch (>= 4.0.0))
    (hsluv (>= 0.1.0))
    (ocamlformat (and (>= 0.29.0) :with-dev-setup ))
  )
 (tags
  (graphics rendering paletted)))

(package
 (name claudius_sdl)
 (documentation https://github.com/claudiusFX/claudius)
 (synopsis "A Claudius implementation for SDL")
 (description "A functional style retro-graphics library for OCaml for building generative art, demos, and games.")
 (depends
    (ocaml (>= 5.1))
    dune
    (ounit2 :with-test)
    (odoc :with-doc)
    (ocamlformat (and (>= 0.29.0) :with-dev-setup ))
    (claudius (>= 2.0))
   )
 (tags
  (graphics rendering paletted)))

(package
   (name claudius_js)
   (documentation https://github.com/claudiusFX/claudius)
   (synopsis "A Claudius implementation for Browsers")
   (description "A functional style retro-graphics library for OCaml for building generative art, demos, and games.")
   (depends
        (ocaml (>= 5.1))
        dune
        (js_of_ocaml (>= 6.5.0))
        js_of_ocaml-ppx
        (odoc :with-doc)
        (ocamlformat (and (>= 0.29.0) :with-dev-setup ))
        (claudius (>= 2.0))
   )
   (tags
    (graphics rendering paletted))
)