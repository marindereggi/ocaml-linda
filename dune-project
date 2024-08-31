(lang dune 3.16)

(name linda)

(generate_opam_files true)

(source
 (github marindereggi/ocaml-linda))

(authors "Marin Gazvoda de Reggi")

(maintainers "Marin Gazvoda de Reggi")

(license GPL-3.0)

; (documentation https://url/to/documentation)

(package
 (name linda)
 (synopsis "Tuple Space-Based Parallel Programming in OCaml")
 (description
  "This library provides a tuple space-based parallel programming model in OCaml.")
 (depends ocaml dune)
 (tags
  (linda tuple-space parallel-programming ocaml)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
