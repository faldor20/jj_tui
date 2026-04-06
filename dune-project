(lang dune 3.20)

(name jj_tui)

; Keep opam metadata generated from the same dependency source that Dune pkg
; consumes, so Nix, opam, and plain Dune builds all describe the same project.
(generate_opam_files true)

(source
 (github faldor20/jj_tui))

(authors "Eli Dowling")

(maintainers "eli.jambu@gmail.com")

(license MIT)

(documentation "https://github.com/faldor20/jj_tui")

; The repo vendors patched forks of these libraries. Pin them so Dune package
; management resolves the same sources the build already uses locally.
(pin
 (url "./forks/notty")
 (package
  (name notty)
  (version "0.3.0")))

(pin
 (url "./forks/lwd")
 (package
  (name lwd)
  (version "0.1.0"))
 (package
  (name lwd_picos)
  (version "0.1.0")))

(pin
 (url "./forks/nottui")
 (package
  (name nottui)
  (version "dev"))
 (package
  (name nottui_picos)
  (version "dev")))

(pin
 (url "git+https://github.com/faldor20/ppx_record_updater.git")
 (package
  (name ppx_record_updater)))

(package
 (name jj_tui)
 (synopsis "Terminal UI for the Jujutsu version control system")
 (description "A terminal UI for Jujutsu built in OCaml with Notty and Nottui.")
 (depends
  (ocaml (>= "5.1"))
  angstrom
  base
  bigstringaf
  logs
  logs-ppx
  lwd
  lwd_picos
  notty
  nottui
  nottui_picos
  ppx_deriving
  ppx_deriving_yaml
  ppx_deriving_yojson
  ppx_expect
  ppx_jane
  ppx_record_updater
  re
  signal
  spawn
  stdio
  uutf
  yaml
  yojson
  (picos_std (= "0.6.0"))
  (picos_io (= "0.6.0"))
  (picos_mux (= "0.6.0")))
 (tags
  (topics "terminal" "tui" "jujutsu" "jj" "vcs")))
