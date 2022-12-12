(lang dune 3.3)

(name OCADml)

(generate_opam_files true)

(source
 (github OCADml/OCADml))

(documentation https://OCADml.github.io/OCADml)

(license GPL-2.0-or-later)

(authors "Geoff deRosenroll<geoffderosenroll@gmail.com")

(maintainers "Geoff deRosenroll<geoffderosenroll@gmail.com")

(package
 (name OCADml)
 (synopsis "Types and functions for building CAD packages in OCaml")
 (description "Types and functions for building CAD packages in OCaml")
 (tags
  ("OCADml" "CAD"))
 (depends
  (ocaml
   (>= 4.13.0))
  (cairo2
   (>= 0.6.2))))
