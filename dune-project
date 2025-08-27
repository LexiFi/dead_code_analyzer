(lang dune 3.16)

(name dead_code_analyzer)

(generate_opam_files true)

(version 1.0.0)

(maintainers
 "Alain Frisch <alain.frisch@lexifi.com>"
 "Corentin De Souza <de.souza.corentin@gmail.com>")

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
  (ocaml (and (>= 5.2) (< 5.3)))
 )
)
