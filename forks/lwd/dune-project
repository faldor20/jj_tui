(lang dune 3.5)

(generate_opam_files true)

(formatting
 (enabled_for dune))

(name lwd)

(source
 (github let-def/lwd))

(license MIT)

(authors "Frédéric Bour")

(maintainers "fred@tarides.com")

(package
 (name lwd)
 (synopsis "Lightweight reactive documents")
 (documentation "https://let-def.github.io/lwd/doc")
 (depends
  dune
  seq
  (ocaml
   (>= "5.2"))
  (qtest :with-test)
  (qcheck :with-test)))

(package
 (name lwd_picos)
 (synopsis "Lightweight reactive documents with Picos backend")
 (allow_empty)
 (depends
  lwd
  (picos (>= "0.6.0"))
  (picos_std (>= "0.6.0"))))