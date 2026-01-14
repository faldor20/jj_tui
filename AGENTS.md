# AGENTS.md - Coding Agent Guidelines for jj_tui

> A terminal UI for the Jujutsu version control system, built in OCaml with Notty/Nottui.

## Project Structure

```
jj_tui/
├── bin/           # Main executable and UI components
│   ├── main.ml    # Entry point
│   ├── jj_ui.ml   # Main UI orchestration
│   ├── graph_view.ml, graph_commands.ml  # Commit graph UI
│   ├── file_view.ml, file_commands.ml    # File diff UI
│   └── global_vars.ml                    # Shared state
├── lib/           # Core library (jj_tui)
│   ├── ansiReverse.ml     # ANSI escape parsing
│   ├── render_jj_graph.ml # Commit graph rendering
│   ├── config.ml, key_map.ml  # Configuration
│   └── *_tests.ml         # Inline tests
├── test/lib/      # Additional test library (jj_tui_test)
└── forks/         # Vendored dependencies (notty, nottui, lwd)
```

## Build Commands

**Requires Nix** - This project uses Nix for dependency management.

```bash
# Enter development shell (required first)
nix develop

# Build
dune build

# Build and watch
dune build --watch

# Run the application
dune exec jj_tui

# Run all tests
dune runtest

# Run tests for specific library
dune runtest -p jj_tui

# Run tests and show output
dune runtest --force

# Format code
dune fmt
# or
ocamlformat -i <file.ml>

# Check formatting without applying
dune fmt --preview
```



### Running Individual Tests

Tests use `ppx_expect` inline tests. To run tests in a specific file:

```bash
# Run inline tests for jj_tui library
dune runtest jj_tui/lib

# Run inline tests for test library
dune runtest jj_tui/test/lib

# Promote expect test changes (update golden output)
dune promote
```
## Key things to remember:
- Run `dune build` if there are type errors, often these can be caused by changes across files not being picked up which requires a rebuild 
- If you are getting a type error and can't fix it: Records with the same fields names can cause weird type inference issues, once it's infered it won't change.
  You need to either:
  - locally open the module with the type you want,
  - or do Module.( myrecord.field)
  - or explicitly annotate the type 

## Code Style Guidelines

### OCamlformat Configuration

Uses `profile = janestreet` with customizations. Key settings:
- `let-binding-spacing = double-semicolon` - End let bindings with `;;`
- `break-cases = nested` - Multi-line match statements
- `if-then-else = keyword-first` - Align `then`/`else`
- `space-around-records/lists/arrays = true` - Spacing for trailing commas

### Import/Open Patterns

```ocaml
(* Module-level opens at top of file *)
open Lwd_infix
open Notty
open Nottui
open Jj_tui
open! Jj_tui.Util  (* open! for shadowing *)

(* Functor-based module creation *)
module Make (Vars : Global_vars.Vars) = struct
  open Vars
  module Process = Jj_process.Make (Vars)
  open Process
  (* ... *)
end
```

### Naming Conventions

- **Functions/values**: `snake_case` - `get_hovered_rev`, `parse_escape_seq`
- **Types**: `snake_case` - `ui_state_t`, `rev_id`
- **Modules**: `PascalCase` - `Internal`, `Parser`, `Key_Map`
- **Type parameters**: `'a`, `'acc`, `'b`
- **Record fields**: `snake_case` with semicolons
- **Variant constructors**: `PascalCase` - `Unique`, `Duplicate`, `Apply`

### Type Definitions

```ocaml
(* Record types - use semicolons before fields *)
type t = {
    key_map : Key_map.key_config [@updater]
  ; single_pane_width_threshold : int
  ; max_commits : int
}
[@@deriving yaml, record_updater ~derive:yaml]

(* Variant types *)
type 'a maybe_unique =
  | Unique of 'a
  | Duplicate of 'a
```

### Error Handling

```ocaml
(* Prefer Result for parsing/fallible operations *)
let of_string remap =
  match remap with
  | "up" -> Ok (`Arrow `Up)
  | _ -> Error (`Msg ("Invalid remap: " ^ remap))

(* Use Option for optional values *)
Sys.getenv_opt "XDG_CONFIG_HOME" |> Option.value ~default:"~/.config"

(* Exception handling for I/O *)
try
  let ic = open_in config_file in
  (* ... *)
with
| Sys_error _ -> default_config
| ex -> [%log warn "Error: %s" (Printexc.to_string ex)]; default_config
```

### Lwd Operators (Reactive UI)

```ocaml
let ( <-$ ) f v = Lwd.map ~f (Lwd.get v)
let ( $-> ) v f = Lwd.map ~f (Lwd.get v)
let ( let$$ ) v f = Lwd.map ~f (Lwd.get v)
let ( |>$ ) v f = Lwd.map ~f v
let ( >> ) f g x = g (f x)   (* Compose left-to-right *)
let ( << ) f g x = f (g x)   (* Compose right-to-left *)

(* Usage *)
let$ root = root in
root |> Nottui.Ui.event_filter (...)
```

### Logging

Uses `logs-ppx` with custom timestamp wrapper:

```ocaml
open Jj_tui.Logging

[%log info "Loading config..."]
[%log warn "Error parsing config: %s" msg]
[%log debug "Old logs cleaned up"]
```

### Testing (ppx_expect)

```ocaml
let%expect_test "test_name" =
  let result = some_function () in
  print_endline result;
  [%expect {|
    expected output here
  |}]
;;
```

### Documentation Comments

```ocaml
(** Module-level documentation *)

(** Function documentation - concise, one line preferred *)
let get_unique_id maybe_unique_rev = ...

(**
   Multi-line documentation for complex functions.
   Explains algorithm or non-obvious behavior.
*)
```

## Key Patterns

### Functor-Based Dependency Injection

```ocaml
module Make (Vars : Global_vars.Vars) = struct
  (* Access Vars.* throughout the module *)
end
```

### UI State Management

Global state in `Global_vars.Vars` using `Lwd.var`:

```ocaml
type ui_state_t = {
    view : [`Main | `Cmd_I of cmd_args | ...] Lwd.var
  ; hovered_revision : string maybe_unique Lwd.var
  (* ... *)
}
```

### Command Handling

Commands defined in `graph_commands.ml` / `file_commands.ml`:

```ocaml
type command =
  | Cmd of string list
  | Cmd_async of string * string list
  | Dynamic of (unit -> command)
  | Selection_prompt of (...)
```

## Dependencies

Key libraries:
- **nottui** / **nottui_picos**: Terminal UI framework (forked)
- **lwd** / **lwd_picos**: Reactive programming (forked)
- **notty**: Terminal rendering (forked)
- **angstrom**: Parser combinators
- **picos**: Multicore/async runtime
- **ppx_expect**: Inline testing
- **ppx_deriving_yaml/yojson**: Serialization

## Common Pitfalls

1. **End `let` bindings with `;;`** - Required by ocamlformat config
2. **Use Lwd operators** - Don't call `Lwd.get` directly in render functions
3. **Functor pattern** - Most modules require `Make(Vars)` instantiation
4. **Vendored forks** - Don't modify files in `forks/` unless necessary
5. **Nix required** - Build system not set up for pure opam/dune
