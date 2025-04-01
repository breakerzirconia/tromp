(lang dune 3.17)

(name tromp)

(version 0.1.0)

(generate_opam_files true)

(source
 (github breakerzirconia/tromp))

(authors "Constantine Ter-Matevosian <terkot1010@gmail.com>")

(maintainers "Constantine Ter-Matevosian <terkot1010@gmail.com>")

(license MIT)

; (documentation https://url/to/documentation)

(package
 (name tromp)
 (synopsis "A tool for generating Tromp diagrams from lambda expressions")
;  (description "A longer description")
 (depends ocaml)
 (tags
  ("lambda calculus" "tromp diagrams")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
