(lang dune 3.12)

(name jj_tui)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(pin
 (url "git+https://github.com/faldor20/ppx_record_updater.git")
 (package
  (name ppx_record_updater)))

(package
 (name jj_tui)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  lwd
  lwd_picos
  ocaml
  dune
  stdio
  nottui
  base
  angstrom
  ppx_expect
  ppx_jane
  (picos_std
   (= 0.5.0))
  (picos_io
   (= 0.5.0))
  ;;for notty
  uutf
  yojson
  ppx_record_updater)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
