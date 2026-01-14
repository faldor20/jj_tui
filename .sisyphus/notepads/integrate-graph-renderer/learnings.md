# Learnings - Task 2.1: Extend node type

## Task Completed: 2026-01-15

### What Was Done
Extended the `node` type in `render_jj_graph.ml` with 8 new fields to support richer display information and preview functionality.

### Files Modified
1. `/home/eli/Code/ocaml/jj_tui/jj_tui/lib/render_jj_graph.ml` - Extended node type definition
2. `/home/eli/Code/ocaml/jj_tui/jj_tui/lib/render_jj_graph_tests.ml` - Updated 14 node creation sites in tests
3. `/home/eli/Code/ocaml/jj_tui/jj_tui/lib/jj_json.ml` - Updated commits_to_nodes to populate new fields

### New Fields Added
```ocaml
; description : string
; bookmarks : string list
; author_email : string
; author_timestamp : string
; empty : bool
; hidden : bool
; divergent : bool
; is_preview : bool
```

### Key Patterns Observed

#### 1. Type Extension Strategy
- Extended the record type first
- Let the compiler identify all creation sites that need updating
- Updated each site systematically
- Ran `dune build` frequently to catch errors early

#### 2. Test Data Defaults
For test nodes, used sensible defaults:
- `description = "test commit"`
- `bookmarks = []`
- `author_email = "test@example.com"`
- `author_timestamp = "2024-01-01T00:00:00Z"`
- `empty = false`
- `hidden = false`
- `divergent = false`
- `is_preview = false`

#### 3. Real Data Population (jj_json.ml)
Mapped from jj_commit fields:
- `description = jj_commit.description`
- `bookmarks = jj_commit.bookmarks`
- `author_email = jj_commit.author.email`
- `author_timestamp = jj_commit.author.timestamp`
- `empty = jj_commit.empty`
- `hidden = jj_commit.hidden`
- `divergent = jj_commit.divergent`
- `is_preview = false` (always false for real commits, will be true for preview nodes)

#### 4. LSP Behavior
- LSP showed errors during incremental updates (expected)
- Errors cleared after running `dune build`
- Final LSP diagnostics were clean after all changes

### Verification Results
✅ `dune build` - SUCCESS (only warnings, no errors)
✅ `dune runtest` - SUCCESS (all tests pass)
✅ LSP diagnostics - CLEAN (no errors in modified files)

### Impact on Existing Code
- No changes to graph rendering logic
- No changes to test expectations (graph output unchanged)
- All existing tests continue to pass
- Type extension is backward compatible (only adds fields)

### Next Steps
This completes Task 2.1. The node type now has all fields needed for:
- Display functionality (description, bookmarks, author info)
- Preview support (is_preview flag)
- Additional metadata (empty, hidden, divergent)

Ready for Task 2.2: Update graph rendering to use new fields.

---

# Learnings - Task 2.2: Add elided revision support

## Task Completed: 2026-01-15

### What Was Done
Added infrastructure for elided revisions - special nodes representing skipped commits in the graph display (shown as `~` in jj's output).

### Files Modified
1. `/home/eli/Code/ocaml/jj_tui/jj_tui/lib/render_jj_graph.ml` - Added elided node functions
2. `/home/eli/Code/ocaml/jj_tui/jj_tui/lib/render_jj_graph_tests.ml` - Added 3 tests for elided nodes

### Functions Added
```ocaml
(** Special marker for elided nodes *)
let elided_marker = "~ELIDED~"

(** Create a special node representing an elided section *)
let make_elided_node () : node

(** Check if a node represents an elided section *)
let is_elided (n : node) : bool
```

### Implementation Details

#### Elided Node Marker
- Used special string `"~ELIDED~"` as marker in both `commit_id` and `change_id` fields
- This makes elided nodes easily identifiable and prevents confusion with real commits
- Marker is unlikely to collide with actual commit IDs

#### Elided Node Properties
- `parents = []` - Elided nodes don't track parent relationships
- `creation_time = Int64.zero` - No meaningful timestamp
- `working_copy = false`, `immutable = false`, `wip = false` - Not a real commit
- `commit_id = elided_marker`, `change_id = elided_marker` - Special marker
- `description = "(elided revisions)"` - Human-readable description
- `bookmarks = []` - No bookmarks
- `author_email = ""`, `author_timestamp = ""` - No author info
- `empty = false` - Not technically empty
- `hidden = true` - Conceptually hidden (elided means skipped)
- `divergent = false`, `is_preview = false` - Not divergent or preview

#### Detection Function
- `is_elided` checks if `commit_id` equals `elided_marker`
- Simple and efficient O(1) check
- Works because elided_marker is unique and won't appear in real commits

### Tests Added
1. **make_elided_node** - Verifies elided node creation with correct marker and properties
2. **is_elided_true** - Verifies `is_elided` returns true for elided nodes
3. **is_elided_false** - Verifies `is_elided` returns false for normal nodes

### Verification Results
✅ `dune build` - SUCCESS (only warnings, no errors)
✅ `dune runtest` - SUCCESS (all tests pass, including 3 new elided node tests)
✅ LSP diagnostics - CLEAN (no errors in modified files)

### Key Patterns Observed

#### 1. Special Node Pattern
- Used a special marker string to identify elided nodes
- Alternative approaches considered:
  - Variant type: Would require changing node type everywhere
  - Optional field: Would add complexity to all node handling
  - Special marker: Simple, backward compatible, easy to check

#### 2. Test Coverage
- Tested creation (make_elided_node)
- Tested positive detection (is_elided on elided node)
- Tested negative detection (is_elided on normal node)
- This covers all code paths for the new functions

#### 3. No .mli File
- `render_jj_graph.ml` has no corresponding `.mli` file
- All functions are automatically exported
- No need to update module signature

### Impact on Existing Code
- No changes to existing functions or types
- No changes to graph rendering logic (that's a later task)
- All existing tests continue to pass
- New functions are additive only

### Next Steps
This completes Task 2.2. The infrastructure for elided nodes is now in place:
- Functions to create elided nodes
- Functions to identify elided nodes
- Tests to verify behavior

Ready for Task 2.3: Create structured output type for UI integration.

### Notes for Future Tasks
- Elided nodes should render as `~` followed by blank line (per plan line 156-160)
- Actual rendering logic will be implemented in a later task
- The `hidden = true` property may be useful for filtering or special rendering

---

# Learnings - Task 2.3: Create structured output types

## Task Completed: 2026-01-15

### What Was Done
Added structured output types (`row_type` and `graph_row_output`) and implemented `render_nodes_structured` function to return structured data instead of plain strings for UI integration.

### Files Modified
1. `/home/eli/Code/ocaml/jj_tui/jj_tui/lib/render_jj_graph.ml` - Added types and function
2. `/home/eli/Code/ocaml/jj_tui/jj_tui/lib/render_jj_graph_tests.ml` - Added 2 tests

### Types Added
```ocaml
type row_type =
  | NodeRow  (** The main row with the node glyph *)
  | LinkRow  (** Merge/fork connector lines *)
  | PadRow   (** Padding/continuation lines *)
  | TermRow  (** Termination lines with ~ *)

type graph_row_output = {
    graph_chars : string  (** The graph prefix like "○ " or "├─╮" *)
  ; node : node           (** The node this row represents *)
  ; row_type : row_type   (** What kind of row this is *)
}
```

### Function Added
```ocaml
val render_nodes_structured : 
  ?info_lines:(node -> int) ->
  state -> 
  node list -> 
  graph_row_output list
```

### Implementation Details

#### Row Type Classification
Created `classify_row_type` helper function that detects row type based on content:
- **NodeRow**: Contains node glyphs (○, @, ◌, ◆)
- **TermRow**: Contains termination marker (~)
- **LinkRow**: Contains merge/fork characters (├, ╮, ╯, ╰, ┬, ┴, ┼)
- **PadRow**: Everything else (vertical lines, spaces)

Used `Str.search_forward` with `Str.regexp_string` to search for UTF-8 characters in strings, since `String.contains` only works with single-byte chars.

#### Structured Rendering Function
`render_nodes_structured` mirrors the logic of `render_nodes_to_string` but:
1. Builds a list of `graph_row_output` records instead of concatenating strings
2. Classifies each line using `classify_row_type`
3. Associates each line with its corresponding node
4. Returns the list in correct order (reversed at the end since we build it backwards)

The function handles:
- Extra pad lines from previous rows
- Node lines (main commit row)
- Link lines (merge/fork connectors)
- Term lines (two lines for termination)
- Info lines (additional content lines per node)
- Final extra pad line

### Tests Added
1. **render_nodes_structured_simple** - Tests basic 2-node graph with info_lines
   - Verifies correct number of rows
   - Verifies each row has correct type, node, and graph_chars
   
2. **render_nodes_structured_row_types** - Tests complex merge graph
   - Verifies row type classification (NodeRow, LinkRow, PadRow)
   - Counts each type to ensure correct classification
   - Tests the merge topology from existing golden test

### Verification Results
✅ `dune build` - SUCCESS (only warnings, no errors)
✅ `dune runtest` - SUCCESS (all tests pass, including 2 new structured output tests)
✅ LSP diagnostics - CLEAN (no errors in modified files)

### Key Patterns Observed

#### 1. UTF-8 String Searching
OCaml's `String.contains` only works with single-byte characters. For UTF-8 glyphs, used:
```ocaml
let contains_str s substr =
  try
    let _ = Str.search_forward (Str.regexp_string substr) s 0 in
    true
  with Not_found -> false
```

#### 2. Mirroring Existing Logic
The `render_nodes_structured` function closely mirrors `render_nodes_to_string`:
- Same structure and flow
- Same handling of extra_pad_line_ref
- Same rendering of node_line, link_line, term_line, pad_lines
- Only difference: builds structured list instead of string buffer

This ensures consistency and makes it easy to verify correctness by comparing outputs.

#### 3. Backward Compatibility
- Kept existing `render_nodes_to_string` function unchanged
- New function is additive only
- All existing tests continue to pass
- No breaking changes to public API

#### 4. Test Expectations
- Initial test expectations needed adjustment (dune promote)
- Final pad line is not included when empty (expected behavior)
- Tests verify both structure (row count, types) and content (graph_chars)

### Impact on Existing Code
- No changes to existing functions
- No changes to graph rendering logic
- All existing tests continue to pass
- New types and function are additive only

### Next Steps
This completes Task 2.3. The structured output infrastructure is now in place:
- Types to represent different row types
- Function to generate structured output
- Tests to verify behavior

Ready for Task 2.4: Update existing tests (if needed).

### Notes for Future Tasks
- The `graph_row_output` type can be extended with additional fields if needed
- The `classify_row_type` function could be made more sophisticated if needed
- The UI can now consume structured output and style different row types differently
- Each row is associated with its node, enabling hover/selection features

---

# Learnings - Task 3.1: Add JSON-based graph fetching functions

## Task Completed: 2026-01-15

### What Was Done
Added two new functions to `process_wrappers.ml` for JSON-based graph fetching using the jj_json module.

### Files Modified
1. `/home/eli/Code/ocaml/jj_tui/jj_tui/lib/process_wrappers.ml` - Added get_graph_json and get_graph_nodes

### Functions Added
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
  (Render_jj_graph.node list * string maybe_unique array)
```

### Implementation Details

#### get_graph_json Function
Executes `jj log --no-graph --color never -T <json_template> --limit <n> [revset]` and parses the JSONL output:
- Builds command args list with JSON template from `Jj_json.json_log_template`
- Handles optional revset parameter by appending `-r <revset>` to args
- Calls `jj_no_log` to execute the command (no snapshot needed for read-only operation)
- Parses output using `Jj_json.parse_jj_log_output`
- Returns list of `Jj_json.jj_commit` or fails with descriptive error message

#### get_graph_nodes Function
Converts commits to renderer nodes and extracts rev_ids for selection tracking:
- Calls `get_graph_json` to fetch commits
- Converts to nodes using `Jj_json.commits_to_nodes`
- Extracts rev_ids as `string maybe_unique array`:
  - `Duplicate commit_id` if commit is divergent or hidden
  - `Unique change_id` otherwise
- Returns tuple of `(node list, string maybe_unique array)`

### Key Patterns Observed

#### 1. Command Execution Pattern
Followed the same pattern as `graph_and_revs`:
- Build args list incrementally
- Handle optional revset with pattern matching
- Use `jj_no_log` for execution (no snapshot for read-only commands)
- No async/promise needed for simple synchronous operations

#### 2. Rev_id Type Clarification
The plan mentioned `Global_vars.rev_id array` but the actual type is `string maybe_unique array`:
- `rev_id` is defined in `Process` module as a record with change_id, commit_id, divergent
- The UI layer uses `string maybe_unique` where the string is either change_id or commit_id
- `maybe_unique` is a variant: `Unique of 'a | Duplicate of 'a`
- Divergent or hidden commits use `Duplicate commit_id`
- Normal commits use `Unique change_id`

This matches the pattern in the existing `find_selectable_from_graph` function (lines 80-82).

#### 3. Error Handling
Used simple `failwith` for JSON parsing errors since:
- This is a critical error that should stop execution
- The error message includes the parse error details
- Matches the error handling pattern in the codebase
- Alternative would be to return Result type, but that's not the pattern here

#### 4. Functor Pattern
Functions added inside the `Make` functor:
- Have access to `jj_no_log` from the Process parameter
- Follow the same structure as other functions in the module
- No need to pass Process explicitly

### Verification Results
✅ `dune build` - SUCCESS (only warnings, no errors)
✅ LSP diagnostics - CLEAN (no errors in process_wrappers.ml)

### Impact on Existing Code
- No changes to existing functions
- `graph_and_revs` remains unchanged (kept for backward compatibility)
- New functions are additive only
- No breaking changes to public API

### Next Steps
This completes Task 3.1. The JSON-based graph fetching functions are now in place:
- `get_graph_json` fetches and parses JSON commits
- `get_graph_nodes` converts to renderer nodes with rev_ids
- Both functions ready for integration in graph view

Ready for Phase 4: Graph View Integration.

### Notes for Future Tasks
- The new functions use the same command execution pattern as existing code
- Rev_ids are extracted in the same format as the old graph_and_revs function
- The functions can be used as drop-in replacements once the UI is updated
- Consider adding tests for these functions in a future task
