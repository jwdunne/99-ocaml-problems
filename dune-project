(lang dune 3.12)

(name ocaml_problems)

(generate_opam_files true)

(source
 (github jwdunne/99-ocaml-problems))

(authors "James Dunne")

(license MIT)

(package
 (name ocaml_problems)
 (synopsis "Solutions for https://v2.ocaml.org/learn/tutorials/99problems.html")
 (depends ocaml dune)
 (tags
  (topics ocaml ninety-nine-problems)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
