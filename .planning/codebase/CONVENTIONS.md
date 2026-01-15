# Coding Conventions

**Analysis Date:** 2026-01-15

## Naming Patterns

**Files:**
- snake_case for all source files (`commit_render.ml`, `jj_json.ml`, `render_jj_graph.ml`)
- Test files: `*_tests.ml` suffix (`commit_render_tests.ml`, `jj_json_tests.ml`)
- Some camelCase preserved in library names (`ansiReverse.ml`, `outputParsing.ml`)

**Functions:**
- snake_case for all functions (`render_commit_content`, `graph_node_attr`, `parse_jj_log_output`)
- No special prefix for async functions
- Event handlers: descriptive names (`forward_events`, `handle_input`)

**Variables:**
- snake_case for variables (`hovered_revision`, `graph_revs`, `active_files`)
- Constants: lowercase with underscores (`pad_amount = 2`)
- Lwd reactive vars: descriptive names without special suffix

**Types:**
- snake_case for type names (`ui_state_t`, `jj_commit`, `jj_author`, `command_variant`)
- Variant constructors: PascalCase (`Cmd`, `Cmd_r`, `PromptThen`, `Selection_prompt`)
- Record fields: snake_case (`working_copy`, `is_preview`, `change_id`)

## Code Style

**Formatting:**
- OCamlformat with janestreet profile (`.ocamlformat`)
- 2 space indentation
- `let-binding-spacing=double-semicolon` - double semicolon termination
- `space-around-records=true`, `space-around-lists=true`, `space-around-arrays=true`
- `dock-collection-brackets=true` - collections start on new lines
- `break-cases=nested` - match statements on multiple lines

**Linting:**
- No explicit linting tool configured
- Warnings enabled via dune (`-w A-4-42-44-45-48-66`)

## Import Organization

**Order:**
1. External packages (Notty, Nottui, Lwd, Base, etc.)
2. Local library modules (Jj_tui, Util, etc.)
3. Functor modules (Make(Vars))

**Patterns:**
- `open` statements at top of file or module
- Selective opens: `open! Util` (force open even if shadowing)
- Infix operators from Lwd: `open Lwd_infix`

**Path Aliases:**
- None - uses explicit module paths

## Error Handling

**Patterns:**
- Result types for parsing: `Result.t` with `Ok`/`Error`
- Exceptions for programmer errors: `failwith` with descriptive messages
- Graceful degradation: `Option.value ~default:...` for missing config

**Error Types:**
- Parsing: Return `Result.t` with `Error (``Msg msg)`
- Runtime: `failwith` for invariant violations
- Unsafe operations: Deliberate `*_exn` suffix (e.g., `key_of_string_exn`)

## Logging

**Framework:**
- `logs` library with custom file reporter (`jj_tui/lib/logging.ml`)
- Levels: debug, info, warn, error

**Patterns:**
- PPX syntax: `[%log info "message %s" arg]`
- Structured logging with format strings
- File-based logging to platform-specific directories

## Comments

**When to Comment:**
- Module-level documentation: `(** ... *)` with markdown
- Function documentation: Single-line `(** ... *)` before definitions
- Inline explanations: `(* ... *)` for non-obvious logic
- Algorithm explanations: Multi-line doc comments

**OCamldoc:**
- Used for public APIs and complex functions
- Format: `(** Description *)` before declarations
- Markdown formatting supported in doc comments

**TODO Comments:**
- Format: `(* TODO: description *)` or inline
- Examples found in: `jj_tui/bin/global_funcs.ml`, `jj_tui/bin/graph_view.ml`

## Function Design

**Size:**
- Generally under 50 lines per function
- Large functions: Graph rendering algorithm (~100+ lines acceptable)
- Extract helpers for complex logic

**Parameters:**
- Named parameters: `~prefix:string ~rest:string`
- Optional parameters: `?(working_copy = false)`
- Labeled arguments preferred for clarity

**Return Values:**
- Explicit `Result.t` for operations that can fail
- Option types for nullable values
- Direct values for infallible operations

## Module Design

**Exports:**
- Library modules auto-exported from `lib/` directory
- Explicit module signatures rarely used (prefer `.ml` only)
- Functor pattern: `module Make (Vars : Global_vars.Vars) = struct ... end`

**Functors:**
- Used extensively for dependency injection
- Pattern: Most major modules parameterized over `Vars` signature
- Examples: `Jj_ui.Make(Vars)`, `Graph_view.Make(Vars)`, `Jj_process.Make(Vars)`

**Patterns:**
- Double semicolon termination: All top-level let bindings end with `;;`
- PPX deriving: Heavy use of `[@@deriving yojson, yaml, show, eq]`
- Custom operators: Lwd operators in `util.ml` (`<-$`, `$->`, `|>$`, etc.)

---

*Convention analysis: 2026-01-15*
*Update when patterns change*
