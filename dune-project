(lang dune 3.9)

(name jj_tui)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name jj_tui)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune eio-project notty stdio nottui lwd base eio_main angstrom ppx_expect)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
