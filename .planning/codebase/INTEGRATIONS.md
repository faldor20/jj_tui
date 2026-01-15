# External Integrations

**Analysis Date:** 2026-01-15

## APIs & External Services

**Jujutsu VCS (Primary Integration):**
- JJ CLI subprocess execution - `jj_tui/bin/jj_process.ml`
  - Integration method: Direct CLI invocation via `Picos_io.Unix`
  - Auth: Inherits from user's jj configuration
  - Commands: log, commit, rebase, bookmark, squash, split, git operations
  - Template system: Custom JJ templates for JSON output (`jj_tui/lib/jj_json.ml`)

**JSON/YAML Parsing:**
- Yojson - `jj_tui/lib/jj_json.ml` (parses JSONL from `jj log --template`)
- Yaml - `jj_tui/lib/config.ml` (user configuration)

## Data Storage

**Databases:**
- None - Stateless TUI application

**File Storage:**
- Configuration: `$XDG_CONFIG_HOME/jj_tui/config.yaml` (Linux) or `~/Library/Preferences/jj_tui/` (macOS)
- Logs: `$XDG_STATE_HOME/jj_tui/` (Linux) or `~/Library/Logs/jj_tui/` (macOS)
  - Log rotation: Keeps 20 most recent log files (`jj_tui/lib/logging.ml`)

**Caching:**
- None

## Authentication & Identity

**Auth Provider:**
- None - Relies on JJ CLI authentication

## Monitoring & Observability

**Error Tracking:**
- None

**Analytics:**
- None

**Logs:**
- File-based logging via `logs` library - `jj_tui/lib/logging.ml`
  - Location: Platform-specific (see File Storage above)
  - Format: Timestamped structured logs
  - Level: Debug by default

## CI/CD & Deployment

**Hosting:**
- GitHub repository - Source distribution

**CI Pipeline:**
- GitHub Actions - `.github/workflows/build-nix.yml`
  - Multi-platform builds via Nix
  - Platforms: x86_64-linux, aarch64-linux, aarch64-darwin, x86_64-darwin

## Environment Configuration

**Development:**
- Required: OCaml 5.2+, Dune 3.12+
- Optional: Nix for reproducible builds
- JJ CLI must be installed and on PATH

**Production:**
- JJ CLI required on PATH
- Optional YAML config at standard locations
- No network dependencies

## Webhooks & Callbacks

**Incoming:**
- None

**Outgoing:**
- None

## Forked Dependencies (Customized)

**Local forks maintained in `forks/`:**
- Nottui - `forks/nottui/` (Picos async integration, custom modifications)
- Notty - `forks/notty/` (Unicode graphics support)
- Lwd - `forks/lwd/` (OCaml 5.2+ compatibility)

**External forks (pinned):**
- ppx_record_updater - `git+https://github.com/faldor20/ppx_record_updater.git`
- picos - `github:ocaml-multicore/picos`

---

*Integration audit: 2026-01-15*
*Update when adding/removing external services*
