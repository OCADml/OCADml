(lang dune 3.6)

(name OCADml)

(using directory-targets 0.1)

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
   (>= 4.14.0))
  dune
  (gg
   (>= 1.0.0))
  (alcotest :with-test))
 (depopts cairo2)
 (conflicts
  (cairo2
   (< 0.6.2))))
