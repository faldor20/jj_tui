# Architecture

**Analysis Date:** 2026-01-15

## Pattern Overview

**Overall:** Functional Reactive TUI Application with Structured Concurrency

**Key Characteristics:**
- Single native executable with reactive UI
- Functor-based modular architecture
- Structured concurrency via Picos multi-domain parallelism
- Reactive data flow using Lwd

## Layers

**UI/Presentation Layer:**
- Purpose: Terminal rendering and user interaction
- Contains: View implementations, widget composition, rendering logic
- Key modules:
  - `jj_tui/bin/jj_ui.ml` - Main UI orchestrator
  - `jj_tui/bin/graph_view.ml` - Commit graph visualization
  - `jj_tui/bin/file_view.ml` - File explorer and modification pane
  - `jj_tui/bin/show_view.ml` - Details/preview pane
  - `jj_tui/lib/commit_render.ml` - Commit rendering with syntax highlighting
- Depends on: Notty, Nottui, Lwd, global state
- Used by: Main entry point

**Command/Input Layer:**
- Purpose: Handle user input and map to actions
- Contains: Command definitions, keybinding system, input handlers
- Key modules:
  - `jj_tui/bin/graph_commands.ml` - Graph view commands (552 lines)
  - `jj_tui/bin/file_commands.ml` - File view commands
  - `jj_tui/bin/jj_commands.ml` - Core command execution framework (512 lines)
  - `jj_tui/lib/key_map.ml` - Flexible keybinding system (405 lines)
- Depends on: Global state, JJ process layer
- Used by: View implementations

**State Management Layer:**
- Purpose: Reactive application state
- Contains: Lwd.var-based state store
- Key modules:
  - `jj_tui/bin/global_vars.ml` - Central reactive state (ui_state_t, graph state)
  - `jj_tui/bin/global_funcs.ml` - State manipulation utilities
- Depends on: Lwd reactive primitives
- Used by: All layers

**Process Execution Layer:**
- Purpose: Execute JJ CLI and parse output
- Contains: Process spawning, output parsing, JJ integration
- Key modules:
  - `jj_tui/bin/jj_process.ml` - Mutex-protected JJ CLI execution (375 lines)
  - `jj_tui/lib/process_wrappers.ml` - JJ output parsing (336 lines)
  - `jj_tui/lib/jj_json.ml` - JSON/JSONL parsing from jj log
- Depends on: Picos_io for async I/O
- Used by: Command layer

**Data Processing Layer:**
- Purpose: Parse and render JJ data structures
- Contains: Graph rendering algorithm, ANSI parsing
- Key modules:
  - `jj_tui/lib/render_jj_graph.ml` - Lane-based graph rendering (1,117 lines)
  - `jj_tui/lib/ansiReverse.ml` - ANSI escape code handling (337 lines)
  - `jj_tui/lib/outputParsing.ml` - JJ output utilities
- Depends on: Parsing libraries (angstrom, re)
- Used by: UI layer

**Configuration Layer:**
- Purpose: Load and manage user preferences
- Contains: Config loading, logging setup
- Key modules:
  - `jj_tui/lib/config.ml` - YAML config loading
  - `jj_tui/lib/logging.ml` - Structured logging
  - `jj_tui/lib/os.ml` - OS detection
- Depends on: Yaml, logs libraries
- Used by: All layers

## Data Flow

**User Input Flow:**

1. User presses key in terminal
2. `jj_tui/bin/main.ml` - Nottui event loop captures input
3. `jj_tui/bin/jj_ui.ml` - UI forwards event to active view
4. Command lookup via `key_map` registry
5. Command builder in `graph_commands.ml` or `file_commands.ml` constructs command variant
6. `jj_tui/bin/jj_commands.ml` - `run_command` executes command
7. `jj_tui/bin/jj_process.ml` - JJ CLI execution (mutex-protected)
8. Output parsing via `jj_json.ml` or `process_wrappers.ml`
9. State update via `Lwd.set` in `global_vars.ml`
10. Reactive re-render: `render_jj_graph.ml` + `commit_render.ml`
11. Display to terminal via Notty

**Command Execution Lifecycle:**

```
Key press → Command resolution → Command variant construction →
Async execution (Cmd_async) or Synchronous (Cmd) →
JJ process spawn → Output capture → State update → UI re-render
```

**State Management:**
- All state stored as `Lwd.var` reactive variables
- Changes trigger automatic UI updates
- Global mutex prevents concurrent JJ CLI access

## Key Abstractions

**Functors (Dependency Injection):**
- Purpose: Modular architecture with testability
- Pattern: `module Make (Vars : Global_vars.Vars)`
- Examples:
  - `Jj_ui.Make(Vars)` - `jj_tui/bin/jj_ui.ml`
  - `Graph_view.Make(Vars)`, `File_view.Make(Vars)` - View implementations
  - `Jj_process.Make(Vars)` - Process execution
  - `Graph_commands.Make(Vars)`, `File_commands.Make(Vars)` - Command definitions

**Command Variants:**
- Purpose: Flexible command system supporting various execution patterns
- Examples: `Cmd`, `Cmd_r`, `Dynamic_r`, `PromptThen`, `Selection_prompt`, `Cmd_async`, `SubCmd`
- Pattern: Variant types with lazy evaluation and composition

**Reactive Variables (Lwd.var):**
- Purpose: State management with automatic propagation
- Examples: `hovered_revision`, `graph_revs`, `ui_state`, `term_width_height`
- Pattern: `let$` syntax sugar for reactive bindings

**Process Safety:**
- Purpose: Prevent concurrent JJ CLI access
- Pattern: Global mutex `Jj_process.access_lock` in `jj_tui/bin/jj_process.ml`
- Concurrency: Picos structured concurrency with Flock task management

## Entry Points

**Main Executable:**
- Location: `jj_tui/bin/main.ml` (88 lines)
- Triggers: User runs `jj_tui` command
- Responsibilities: Initialize 4 Picos domains, setup logging, launch TUI event loop

**UI Orchestrator:**
- Location: `jj_tui/bin/jj_ui.ml` (261 lines)
- Triggers: Called by main after initialization
- Responsibilities: Bind views together, forward events, manage popups

## Error Handling

**Strategy:** Exception-based with Result types for parsing

**Patterns:**
- Process errors: Captured and logged, UI shows error messages
- Parse errors: Result types propagate failures to UI
- Unsafe operations: Some intentional `failwith` for programmer errors

## Cross-Cutting Concerns

**Logging:**
- File-based logging via `logs` library - `jj_tui/lib/logging.ml`
- Mutex-protected for thread safety
- Platform-specific log directories

**Validation:**
- YAML schema validation for config - `jj_tui/lib/config.ml`
- Key binding validation - `jj_tui/lib/key.ml`

**Concurrency:**
- Picos structured concurrency - 4 domains launched in `jj_tui/bin/main.ml`
- Global mutex for JJ CLI access - `jj_tui/bin/jj_process.ml`

---

*Architecture analysis: 2026-01-15*
*Update when major patterns change*
