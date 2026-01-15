# Technology Stack

**Analysis Date:** 2026-01-15

## Languages

**Primary:**
- OCaml 5.2+ - All application code (`dune-project`)

**Secondary:**
- None

## Runtime

**Environment:**
- OCaml 5.2+ native compilation
- OPAM package manager

**Package Manager:**
- OPAM (inferred from `.opam` files)
- Lockfile: Not detected

## Frameworks

**Core:**
- Nottui (forked) - TUI framework built on Notty and Lwd (`forks/nottui/dune-project`)
- Notty 0.3.0 (forked) - Declarative terminal rendering library (`forks/notty/dune-project`)
- Lwd (forked) - Lightweight reactive documents for UI (`forks/lwd/dune-project`)

**Testing:**
- ppx_expect - Inline snapshot testing (`jj_tui/lib/dune`)

**Build/Dev:**
- Dune 3.12+ - Build system (`dune-project`)
- OCamlformat (janestreet profile) - Code formatting (`.ocamlformat`)
- Nix flakes - Development environment (`flake.nix`)

## Key Dependencies

**Critical:**
- picos 0.5.0+ - Structured concurrency with multicore support (`dune-project`)
- picos_io 0.5.0+ - I/O operations for Picos (`dune-project`)
- yojson - JSON parsing for JJ output (`jj_tui/lib/jj_json.ml`)
- yaml - YAML configuration loading (`jj_tui/lib/config.ml`)
- angstrom - Parser combinators (`jj_tui/lib/dune`)

**Infrastructure:**
- re - Regular expressions (`jj_tui/lib/dune`)
- logs - Structured logging framework (`jj_tui/lib/logging.ml`)
- signal 0.4.0+ - Signal handling (`forks/nottui/dune-project`)
- base - Jane Street Base library (`jj_tui/lib/dune`)
- stdio - Standard I/O (`jj_tui/lib/dune`)

## Configuration

**Environment:**
- YAML-based user configuration: `$XDG_CONFIG_HOME/jj_tui/config.yaml` (Linux) or `~/Library/Preferences/jj_tui/` (macOS)
- Key bindings: Configurable via YAML (`jj_tui/lib/config.ml`)

**Build:**
- Dune build system with static linking (`jj_tui/bin/dune`)
- Version generated from git at build time (`jj_tui/bin/dune:34-42`)
- PPX preprocessing for deriving, logging, and testing (`jj_tui/lib/dune`)

## Platform Requirements

**Development:**
- Any platform with OCaml 5.2+ support
- Nix for reproducible builds (`flake.nix`)

**Production:**
- Distributed as native executable
- Multi-platform static builds: x86_64-linux, aarch64-linux, aarch64-darwin, x86_64-darwin

---

*Stack analysis: 2026-01-15*
*Update after major dependency changes*
