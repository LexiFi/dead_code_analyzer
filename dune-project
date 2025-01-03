(lang dune 3.16)

(generate_opam_files true)

(version 0.9)

(maintainers "Alain Frisch <alain.frisch@lexifi.com>")

(authors
 "Alain Frisch <alain.frisch@lexifi.com>"
 "Corentin De Souza <de.souza.corentin@gmail.com>")

(source
 (github LexiFi/dead_code_analyzer))

(package
 (name dead_code_analyzer)
 (synopsis "Dead code analyzer for OCaml")
 (license MIT)
 (depends
  (ocaml (>= 5))
  (compiler-libs.common (>= 1.0.7)))
)
