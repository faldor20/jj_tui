# Plan: Integrate New Graph Renderer into jj_tui

## Overview

Replace the current approach (parsing jj's ASCII graph output with regex) with the new OCaml-native graph renderer (`render_jj_graph.ml`). This enables:
1. Full control over graph rendering
2. Ability to insert "preview" nodes before/after any node for move/rebase previews
3. Cleaner separation between data fetching and rendering

## Architecture

```
Current Flow:
  jj log (ASCII graph + markers) → regex parse → display rows

New Flow:
  jj log --no-graph --color never (JSON) → parse JSON → build node list → render_jj_graph → display
                                                              ↓
                                                    [insert preview nodes here]
```

## Design Decisions

- **Node type**: Flat record with `is_preview` flag (not a variant)
- **Styling**: Custom styling (to be decided later, not matching jj)
- **Elided revisions**: Render as `~` with one-line gap, matching jj's format

---

## Phase 1: JSON Data Layer

**Goal**: Create a module to fetch and parse jj log output as JSON.

### Task 1.1: Create `jj_tui/lib/jj_json.ml`

New module for JSON types and parsing.

**Types to define:**

```ocaml
type jj_author = {
    email : string
  ; timestamp : string
}
[@@deriving yojson]

type jj_commit = {
    commit_id : string
  ; parents : string list
  ; change_id : string
  ; description : string
  ; working_copy : bool
  ; immutable : bool
  ; wip : bool
  ; hidden : bool
  ; divergent : bool
  ; empty : bool
  ; bookmarks : string list
  ; author : jj_author
}
[@@deriving yojson]
```

**Functions to implement:**

```ocaml
(** The jj template that produces JSONL output *)
val json_log_template : string

(** Parse JSONL (one JSON object per line) from jj log output *)
val parse_jj_log_output : string -> (jj_commit list, string) result

(** Convert list of jj_commit to render_jj_graph.node list.
    Uses two-pass approach: create nodes, then link parents. *)
val commits_to_nodes : jj_commit list -> Render_jj_graph.node list
```

**JSON template string:**

```
'{' 
  ++ '"commit_id":' ++ json(commit_id)
  ++ ',"parents":[' ++ parents.map(|c| json(c.commit_id())).join(",") ++ ']'
  ++ ',"change_id":' ++ json(change_id)
  ++ ',"description":' ++ json(description)
  ++ ',"working_copy":' ++ json(current_working_copy)
  ++ ',"immutable":' ++ json(immutable)
  ++ ',"wip":' ++ json(description.first_line().starts_with("wip:"))
  ++ ',"hidden":' ++ json(hidden)
  ++ ',"divergent":' ++ json(divergent)
  ++ ',"empty":' ++ json(empty)
  ++ ',"bookmarks":[' ++ bookmarks.map(|b| json(b.name())).join(",") ++ ']'
  ++ ',"author":{"email":' ++ json(author.email()) ++ ',"timestamp":' ++ json(author.timestamp()) ++ '}'
  ++ '}
'
```

### Task 1.2: Update `jj_tui/lib/dune`

Add `jj_json` module and ensure `yojson` dependency is available (already in project for tests).

### Task 1.3: Write tests for `jj_json.ml`

Create `jj_tui/lib/jj_json_tests.ml` with:
- Test parsing valid JSONL
- Test handling missing parents gracefully (root commits)
- Test node linking produces correct parent references

---

## Phase 2: Extend Graph Renderer

**Goal**: Enhance `render_jj_graph.ml` to support richer output needed for UI integration.

### Task 2.1: Extend `node` type in `render_jj_graph.ml`

Add fields needed for display and preview functionality:

```ocaml
type node = {
    (* Existing fields *)
    parents : node list
  ; creation_time : int64
  ; working_copy : bool
  ; immutable : bool
  ; wip : bool
  ; change_id : string
  ; commit_id : string
    (* New fields for display *)
  ; description : string
  ; bookmarks : string list
  ; author_email : string
  ; author_timestamp : string
  ; empty : bool
  ; hidden : bool
  ; divergent : bool
    (* Preview support *)
  ; is_preview : bool
}
```

**Note**: Update `jj_json.ml` conversion function to populate all fields.

### Task 2.2: Add elided revision support

Add a way to represent elided sections in the graph:

```ocaml
(** Special node type for elided revisions *)
val make_elided_node : unit -> node

(** Check if a node represents an elided section *)
val is_elided : node -> bool
```

Elided nodes should render as:
```
~
```
(tilde followed by blank line, matching jj's format)

### Task 2.3: Create structured output type

Instead of returning strings, return structured data for UI integration:

```ocaml
type graph_row_output = {
    graph_chars : string        (* The graph prefix like "○ " or "├─╮" *)
  ; node : node                 (* The node this row represents *)
  ; row_type : row_type         (* What kind of row this is *)
}

and row_type =
  | NodeRow                     (* The main row with the node glyph *)
  | LinkRow                     (* Merge/fork connector lines *)
  | PadRow                      (* Padding/continuation lines *)
  | TermRow                     (* Termination lines with ~ *)

(** Render nodes to structured output for UI integration *)
val render_nodes_structured : 
  state -> 
  node list -> 
  info_lines:(node -> int) ->   (* How many content lines per node *)
  graph_row_output list
```

### Task 2.4: Update existing tests

Ensure all existing golden tests in `render_jj_graph_tests.ml` still pass after type changes.

---

## Phase 3: Process Layer Integration

**Goal**: Add new functions to `process_wrappers.ml` for JSON-based graph fetching.

### Task 3.1: Add `get_graph_json` function

```ocaml
(** Fetch graph data as JSON and parse into commits *)
val get_graph_json : 
  ?revset:string -> 
  int ->                        (* limit *)
  Jj_json.jj_commit list

(** Fetch and convert to renderer nodes *)
val get_graph_nodes :
  ?revset:string ->
  int ->
  (Render_jj_graph.node list * Global_vars.rev_id array)
```

**Implementation notes:**
- Call `jj log --no-graph --color never -T <json_template> --limit <n> [revset]`
- Parse JSONL output
- Convert to nodes
- Extract rev_ids for selection tracking

### Task 3.2: Keep old `graph_and_revs` working

Don't remove the old function yet - keep it for fallback/comparison during development.

---

## Phase 4: Graph View Integration

**Goal**: Update `graph_view.ml` to use the new renderer.

### Task 4.1: Create `render_commit_content` function

Render the text content for a node (styling TBD):

```ocaml
val render_commit_content : Render_jj_graph.node -> Notty.image
```

Basic implementation (styling to be refined later):
- Show change_id (first 8 chars)
- Show author email
- Show timestamp
- Show description first line (or "(no description set)")
- Show bookmarks if any
- Different appearance for working_copy, immutable, empty, preview nodes

### Task 4.2: Create `render_graph_row` function

Combine graph prefix with content:

```ocaml
val render_graph_row : 
  Render_jj_graph.graph_row_output -> 
  render_content:(Render_jj_graph.node -> Notty.image) ->
  Notty.image
```

### Task 4.3: Update `graph_view` function

Replace the current flow:

```ocaml
(* OLD *)
let graph, rev_ids = graph_and_revs ?revset max_commits () in
(* process graph which is array of `Selectable string | `Filler string *)

(* NEW *)
let nodes, rev_ids = get_graph_nodes ?revset max_commits in
let rendered_rows = Render_jj_graph.render_nodes_structured ... nodes in
(* Convert to list items, distinguishing Selectable from Filler based on row_type *)
```

### Task 4.4: Handle elided revisions in UI

Elided nodes should appear as `Filler` items (non-selectable) in the list widget.

---

## Phase 5: Preview Node Support

**Goal**: Enable inserting preview nodes for rebase/move visualization.

### Task 5.1: Add preview insertion functions

In `render_jj_graph.ml` or a new `graph_preview.ml`:

```ocaml
(** Create a preview node *)
val make_preview_node : 
  label:string -> 
  ?target_commit_id:string ->
  unit -> 
  node

(** Insert a preview node after the specified commit *)
val insert_preview_after : 
  nodes:node list -> 
  after_commit_id:string -> 
  preview:node -> 
  node list

(** Insert a preview node before the specified commit *)
val insert_preview_before :
  nodes:node list ->
  before_commit_id:string ->
  preview:node ->
  node list
```

### Task 5.2: Visual distinction for preview nodes

In `render_commit_content`, handle `is_preview = true` nodes:
- Use different glyph (e.g., `◇` or `?`)
- Use distinct styling (e.g., dim, italic)
- Show preview label instead of commit info

### Task 5.3: Integration with commands

Wire up preview display for commands like:
- Rebase preview: Show where commits would land
- Move preview: Show destination

(Specific command integration TBD based on UX decisions)

---

## Files Summary

| File | Action | Description |
|------|--------|-------------|
| `lib/jj_json.ml` | **Create** | JSON types, parsing, node conversion |
| `lib/jj_json_tests.ml` | **Create** | Unit tests for JSON parsing |
| `lib/render_jj_graph.ml` | **Modify** | Extend node type, add structured output |
| `lib/render_jj_graph_tests.ml` | **Modify** | Update tests for new node fields |
| `lib/process_wrappers.ml` | **Modify** | Add JSON-based graph fetching |
| `bin/graph_view.ml` | **Modify** | Use new renderer, add content rendering |
| `lib/dune` | **Modify** | Add jj_json module |

---

## Implementation Order

1. **Phase 1**: Create `jj_json.ml` - can be tested independently
2. **Phase 2**: Extend `render_jj_graph.ml` - update types and add structured output
3. **Phase 3**: Update `process_wrappers.ml` - add JSON fetching
4. **Phase 4**: Update `graph_view.ml` - integrate everything
5. **Phase 5**: Add preview node support

Each phase should be testable independently before moving to the next.

---

## Testing Strategy

1. **Unit tests**: JSON parsing, node conversion, graph rendering
2. **Golden tests**: Existing tests in `render_jj_graph_tests.ml` should continue passing
3. **Integration test**: Run the TUI and verify graph displays correctly
4. **Manual testing**: Compare output visually with `jj log` output

---

## Open Questions (for later)

1. **Styling**: What colors/styles to use for different node types?
2. **Preview UX**: How should preview nodes be triggered and displayed?
3. **Performance**: Is JSON parsing fast enough for large repos? (Probably fine with limits)
