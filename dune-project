(lang dune 3.20)

(name aoc2025)

(generate_opam_files true)

(source
 (github possessedc0bra/advent-of-code))

(authors "Yannick Koller")

(maintainers "Yannick Koller")

(license LICENSE)

(documentation <https://url/to/documentation>)

(package
 (name aoc2025)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (ocaml
   (>= 5.3))
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat :with-dev-setup)
  domainslib
  ppx_deriving
  re
  pcre
  mtime
  (alcotest :with-test))
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at <https://dune.readthedocs.io/en/stable/reference/dune-project/index.html>
