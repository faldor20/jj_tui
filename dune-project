(lang dune 3.12)

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
 (depends 
  lwd
  ocaml
  dune
  stdio
  nottui
  base 
  eio_main 
  angstrom 
  ppx_expect 
  ppx_jane
  eio-process

  (picos_std (= 0.5.0))
  (picos_io (= 0.5.0))
 ;;for notty
 uutf
 )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
