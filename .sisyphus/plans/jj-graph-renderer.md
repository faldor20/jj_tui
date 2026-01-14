# JJ Graph Renderer Implementation Plan

**Created**: 2026-01-15
**Status**: Ready for implementation

---

## Summary

Implement a lane-based DAG graph renderer in OCaml that exactly matches the Rust reference implementation from Meta/Sapling. The renderer produces both string output (for tests) and Notty UI output (for TUI integration).

---

## Non-negotiables

- **Do not change golden test outputs** in `jj_tui/lib/render_jj_graph_tests.ml`
- **Preserve the public API that tests compile against**:
  - `type node = { parents : node list; creation_time : int64; working_copy : bool; immutable : bool; wip : bool; change_id : string; commit_id : string }`
  - `type state = { depth : int; columns : _ array; pending_joins : _ list }`
  - `render_nodes_to_string : ?info_rows:(node -> int) -> state -> node list -> string`

---

## Key Risk / Design Constraint

The Rust reference renderer's box drawing output uses **2 chars per column cell** (`"│ "`, `"╭─"`, etc). The OCaml golden tests encode a **different spacing convention**.

**Strategy**:
- Port the **topology → intermediate representation** (columns + link flags) 1:1 from Rust
- Implement OCaml box-drawing formatter that produces **exactly the OCaml golden output format**

---

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      API Layer                               │
│  render_nodes_to_string : state -> node list -> string      │
│  render_nodes_to_ui : state -> node list -> Notty.image     │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                   GraphRowRenderer                           │
│  - Maintains column state                                    │
│  - Assigns nodes to columns                                  │
│  - Produces GraphRow records                                 │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                  BoxDrawingRenderer                          │
│  - Converts GraphRow → string/image                          │
│  - Selects glyphs based on LinkLine flags                    │
│  - Handles pad lines, term lines                             │
└─────────────────────────────────────────────────────────────┘
```

---

## Tasks

### Task 1: Lock down existing compilation surface

**Goal**: Ensure tests compile unchanged

**Actions**:
- Leave `node` and `state` types in `render_jj_graph.ml` as tests expect
- `state.depth`, `state.pending_joins` may become unused but must exist
- `state.columns` may be repurposed as renderer's column state

**Deliverable**: `render_jj_graph_tests.ml` compiles unchanged

---

### Task 2: Implement internal type equivalents

**Goal**: Create OCaml versions of Rust types

**Types to add** (can be nested modules in `render_jj_graph.ml`):

```ocaml
(* Column state *)
type 'a column = Empty | Blocked | Reserved of 'a | Ancestor of 'a | Parent of 'a

(* Ancestor specification for parents *)
type ancestor = AncestorOf of node | ParentOf of node | Anonymous
(* Note: Anonymous treated as direct for is_direct() like Rust *)

(* Row element types *)
type node_line_entry = NL_Blank | NL_Ancestor | NL_Parent | NL_Node
type pad_line_entry = PL_Blank | PL_Ancestor | PL_Parent

(* LinkLine as int bitset *)
module LinkLine : sig
  type t = int
  val empty : t
  val ( lor ) : t -> t -> t
  val intersects : t -> t -> bool
  
  (* Bit constants *)
  val horiz_parent : t      (* 0x0001 *)
  val horiz_ancestor : t    (* 0x0002 *)
  val vert_parent : t       (* 0x0004 *)
  val vert_ancestor : t     (* 0x0008 *)
  val left_fork_parent : t  (* 0x0010 *)
  val left_fork_ancestor : t (* 0x0020 *)
  val right_fork_parent : t (* 0x0040 *)
  val right_fork_ancestor : t (* 0x0080 *)
  val left_merge_parent : t (* 0x0100 *)
  val left_merge_ancestor : t (* 0x0200 *)
  val right_merge_parent : t (* 0x0400 *)
  val right_merge_ancestor : t (* 0x0800 *)
  val child : t             (* 0x1000 *)
  
  (* Compound flags *)
  val horizontal : t
  val vertical : t
  val left_fork : t
  val right_fork : t
  val left_merge : t
  val right_merge : t
  val any_merge : t
  val any_fork : t
end

(* Intermediate row representation *)
type graph_row = {
  row_node : node
; glyph : Uchar.t
; message : string  (* empty for now, ready for future text *)
; merge : bool
; node_line : node_line_entry array
; link_line : LinkLine.t array option
; term_line : bool array option
; pad_lines : pad_line_entry array
}
```

**Deliverable**: Compiles, no formatting yet

---

### Task 3: Column utility functions

**Goal**: Implement Rust `ColumnsExt` trait equivalent

**Functions**:
```ocaml
val column_matches : node column -> node -> bool
val column_variant : _ column -> int  (* for merge priority *)
val column_merge : node column -> node column -> node column

val columns_find : node column array -> node -> int option
val columns_first_empty : node column array -> int option
val columns_find_empty : node column array -> prefer:int -> int option
val columns_new_empty : node column array ref -> int
val columns_reset : node column array ref -> unit
  (* Blocked → Empty, trim trailing Empty *)
```

**Priority order for merge**: Parent(4) > Ancestor(3) > Reserved(2) > Blocked(1) > Empty(0)

**Deliverable**: Unit correctness by inspection; used by renderer

---

### Task 4: GraphRowRenderer.next_row algorithm

**Goal**: Translate Rust core algorithm to OCaml

**Algorithm steps**:

1. **Determine target column for node**:
   - If column already reserved for it, use it
   - Else use first empty or append
   - Clear target to Empty before assigning parents

2. **Initialize row arrays from current columns**:
   - `node_line` from column state (Ancestor/Parent → vertical markers; others blank)
   - `link_line` similarly
   - `term_line` all false
   - `pad_lines` similarly

3. **Assign parent columns**:
   - If parent already has a column, merge into it
   - Else try `find_empty` preferring current node column
   - Else append new column and extend row arrays in sync

4. **Mark anonymous parents as terminations** (`term_line[i] = true`)

5. **Single-parent swap optimization**:
   - If exactly one parent and parent column > node column
   - Swap columns and emit fork/merge link flags

6. **Connect node column to all parent columns**:
   - Compute bounds (min/max ancestor/parent columns)
   - Fill horizontal segments between outer bounds
   - Set left/right merge markers on node col if needed
   - Set fork markers per parent column

7. **Reset columns** (Blocked cleanup + trailing trim)

8. **Filter optional lines** (only keep link_line/term_line if needed)

**Deliverable**: Working topology engine producing stable row structures

---

### Task 5: BoxDrawing formatting (matching OCaml golden outputs)

**Goal**: Convert GraphRow to string matching test expectations exactly

**Glyph mapping** (use existing `P` module constants):
- `P.v` = `│`, `P.h` = `─`
- `P.vr` = `├`, `P.vl` = `┤`, `P.t` = `┬`, `P.b` = `┴`, `P.cross` = `┼`
- Elbows: `P.edl` = `╭`, `P.edr` = `╮`, `P.eul` = `╰`, `P.eur` = `╯`
- `P.ancestor` = `·`, `P.sp` = ` `
- Node glyphs: `P.Node.working_copy` = `@`, `P.Node.normal` = `○`, `P.Node.immutable` = `◆`, `P.Node.wip` = `◌`

**Glyph selection logic** (port from Rust `box_drawing.rs`):
- 14 glyph types: SPACE, HORIZONTAL, PARENT, ANCESTOR, MERGE_LEFT/RIGHT/BOTH, FORK_LEFT/RIGHT/BOTH, JOIN_LEFT/RIGHT/BOTH, TERMINATION
- Complex conditional based on LinkLine flags and `merge` bool

**Important**: Match OCaml golden test spacing, not Rust 2-wide cells

**Also implement**:
- Termination rendering: `│` then `~` rows
- `info_rows` support: insert additional pad rows after certain nodes

**Deliverable**: `render_nodes_to_string` matches all golden tests exactly

---

### Task 6: Notty UI output

**Goal**: Provide `render_nodes_to_ui` for TUI integration

**Approach (correctness-first)**:
- First implementation: call `render_nodes_to_string`, convert to `Notty.image` via `I.string A.empty` with newline splitting
- Later optimization (optional): render cell-wise with `I.uchar` and `I.hcat/I.vcat`

**Signature**:
```ocaml
val render_nodes_to_ui : ?info_rows:(node -> int) -> state -> node list -> Notty.image
```

**Deliverable**: UI renderer visually matching string output

---

### Task 7: Additional tests (additive only)

**Goal**: Better coverage without modifying existing golden outputs

**New test cases**:
- Linear chain (no merges, only `│` lines)
- Single-parent swap scenario
- Explicit anonymous termination behavior (`~`)
- Multi-parent/octopus merge patterns

**Smoke test for UI**:
- Ensure `render_nodes_to_ui` produces non-empty image for simple graph

**Deliverable**: Improved test coverage

---

## Verification Checklist

- [ ] `dune build` succeeds
- [ ] `dune runtest` passes all tests in `jj_tui/lib/`
- [ ] No changes to existing `%expect` blocks
- [ ] `render_nodes_to_string` matches golden outputs exactly
- [ ] `render_nodes_to_ui` exists and returns usable `Notty.image`

---

## File Changes

| File | Action |
|------|--------|
| `jj_tui/lib/render_jj_graph.ml` | Extend with full implementation (~500 LOC) |
| `jj_tui/lib/render_jj_graph_tests.ml` | **No changes** (golden tests) |

---

## Future Work (out of scope for this plan)

- Node description text rendering
- ANSI color support for graph lines
- Performance optimization for large graphs

---

## Reference Materials

- Rust source: `docs/renderdagsrc.md` (column.rs, renderer.rs, box_drawing.rs)
- Test data: `test/jj_log.json`
- Existing glyphs: `P` module in `render_jj_graph.ml`
