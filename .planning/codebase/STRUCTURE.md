# Codebase Structure

**Analysis Date:** 2026-01-15

## Directory Layout

```
jj_tui/
├── bin/                   # Executable and main UI components
├── lib/                   # Reusable library components
├── test/                  # Test suite
├── forks/                 # Forked dependencies (nottui, notty, lwd)
├── dune-project           # Project metadata
├── jj_tui.opam           # OPAM package manifest
└── README.md              # User documentation
```

## Directory Purposes

**bin/**
- Purpose: Executable entry point and UI orchestration
- Contains: Main binary, views, commands, process execution
- Key files:
  - `main.ml` (88 lines) - Entry point with Picos domain initialization
  - `jj_ui.ml` (261 lines) - Main UI functor orchestrating views
  - `graph_view.ml` - Commit graph visualization
  - `file_view.ml` - File explorer and diff pane
  - `show_view.ml` (187 lines) - Details/preview pane
  - `jj_commands.ml` (512 lines) - Command execution framework
  - `graph_commands.ml` (552 lines) - Graph view command definitions
  - `file_commands.ml` - File view command definitions
  - `jj_process.ml` (375 lines) - JJ CLI process execution
  - `jj_widgets.ml` (213 lines) - JJ-specific widgets
  - `global_vars.ml` - Reactive state store (Lwd.var-based)
  - `global_funcs.ml` - State manipulation utilities
- Subdirectories: None

**lib/**
- Purpose: Reusable library components
- Contains: Core logic, parsing, rendering, configuration
- Key files:
  - `jj_json.ml` - JJ log output parsing (JSONL)
  - `render_jj_graph.ml` (1,117 lines) - Lane-based graph rendering algorithm
  - `render_jj_graph_tests.ml` (848 lines) - Golden tests for graph rendering
  - `commit_render.ml` - Commit content rendering with syntax highlighting
  - `commit_render_tests.ml` (189 lines) - Commit render tests
  - `process_wrappers.ml` (336 lines) - JJ output parsing wrappers
  - `widgets_citty.ml` - Custom widget implementations
  - `config.ml` (54 lines) - YAML config loading
  - `key_map.ml` (405 lines) - Keybinding system
  - `key.ml` (85 lines) - Key type definitions
  - `logging.ml` - Structured logging framework
  - `ansiReverse.ml` (337 lines) - ANSI escape code handling
  - `ansiReverseTests.ml` (298 lines) - ANSI parsing tests
  - `jj_json_tests.ml` (290 lines) - JSON parsing tests
  - `process.ml` - Basic process execution interface
  - `outputParsing.ml` - JJ output parsing utilities
  - `util.ml` (102 lines) - General utilities
  - `os.ml` - OS detection
- Subdirectories:
  - `key_map/` - Additional key map implementations
  - `widgets/` - Archived widget implementations
  - `test/` - Library-level test fixtures

**test/**
- Purpose: Test suite
- Contains: Test files and fixtures
- Key files:
  - `lib/ansi.ml` - ANSI integration tests

**forks/**
- Purpose: Forked dependencies maintained locally
- Contains: nottui, notty, lwd subdirectories
- Subdirectories:
  - `nottui/` - Forked TUI framework with Picos integration
  - `notty/` - Forked terminal rendering library
  - `lwd/` - Forked reactive document framework

## Key File Locations

**Entry Points:**
- `jj_tui/bin/main.ml` - CLI entry point (4 Picos domains, TUI loop)
- `jj_tui/bin/jj_ui.ml` - UI orchestrator

**Configuration:**
- `.ocamlformat` - OCamlformat config (janestreet profile)
- `dune-project` - Dune project metadata
- `jj_tui/lib/dune` - Library build configuration
- `jj_tui/bin/dune` - Executable build configuration

**Core Logic:**
- `jj_tui/lib/render_jj_graph.ml` - Graph rendering algorithm (1,117 lines)
- `jj_tui/bin/jj_commands.ml` - Command execution framework (512 lines)
- `jj_tui/bin/graph_commands.ml` - Graph commands (552 lines)
- `jj_tui/bin/jj_process.ml` - Process execution (375 lines)

**Testing:**
- `jj_tui/lib/*_tests.ml` - Inline ppx_expect tests
- `jj_tui/test/lib/` - Separate test library

**Documentation:**
- `README.md` - User-facing documentation
- `design.md` - Implementation notes

## Naming Conventions

**Files:**
- snake_case for all files (e.g., `graph_view.ml`, `jj_widgets.ml`, `commit_render.ml`)
- Test files: `*_tests.ml` suffix (e.g., `jj_json_tests.ml`, `render_jj_graph_tests.ml`)

**Directories:**
- snake_case for all directories
- Plural names for collections: `forks/`, `test/`, `widgets/`

**Special Patterns:**
- `*.ml` - OCaml implementation files
- `*.mli` - OCaml interface files (rarely used in this project)
- `dune` - Build configuration files in each directory

## Where to Add New Code

**New View:**
- Primary code: `jj_tui/bin/{name}_view.ml`
- Commands: `jj_tui/bin/{name}_commands.ml`
- Register in: `jj_tui/bin/jj_ui.ml`

**New Widget:**
- Implementation: `jj_tui/lib/widgets_citty.ml`
- JJ-specific: `jj_tui/bin/jj_widgets.ml`
- Tests: Co-located `*_tests.ml` file

**New Parsing Logic:**
- Implementation: `jj_tui/lib/{name}.ml`
- Tests: `jj_tui/lib/{name}_tests.ml` with ppx_expect

**Utilities:**
- Shared helpers: `jj_tui/lib/util.ml`
- Lwd operators: `jj_tui/lib/util.ml`

## Special Directories

**forks/**
- Purpose: Local forks of dependencies with custom patches
- Source: Git submodules or vendored code
- Committed: Yes (source of truth)

**test/**
- Purpose: Test suite separate from inline tests
- Source: Hand-written test cases
- Committed: Yes

---

*Structure analysis: 2026-01-15*
*Update when directory structure changes*
