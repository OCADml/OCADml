(lang dune 3.3)

(using directory-targets 0.1)

(cram enable)

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
 (synopsis "Types and utilities for building CAD packages in OCaml.")
 (description "Types and utilities for building CAD packages in OCaml.")
 (depends
  (ocaml
   (>= 4.08.0))
  (cairo2
   (>= 0.6.2))))
