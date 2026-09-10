(lang dune 3.20)

(name dead_code_analyzer)

(generate_opam_files true)

(version 1.3.0)

(maintainers
 "Alain Frisch <alain.frisch@lexifi.com>"
 "Corentin De Souza <corentin@fantazio.eu>")

(authors
 "Alain Frisch <alain.frisch@lexifi.com>"
 "Corentin De Souza <corentin@fantazio.eu>")

(source
 (github LexiFi/dead_code_analyzer))

(package
 (name dead_code_analyzer)
 (synopsis "Dead code analyzer for OCaml")
 (license MIT)
 (depends
  (ocaml (and (>= 4.14) (< 5.6)))
  cppo
 )
)
