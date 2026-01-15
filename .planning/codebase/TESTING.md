# Testing Patterns

**Analysis Date:** 2026-01-15

## Test Framework

**Runner:**
- ppx_expect - OCaml inline snapshot testing
- Config: `(inline_tests)` in `jj_tui/lib/dune`

**Assertion Library:**
- ppx_expect built-in: `[%expect {| ... |}]` blocks
- Custom test helpers for rendering

**Run Commands:**
```bash
dune test                          # Run all tests
dune runtest                       # Run tests (alias)
dune test jj_tui/lib              # Run library tests only
dune promote                       # Accept test output changes
```

## Test File Organization

**Location:**
- Co-located with source: `jj_tui/lib/*_tests.ml`
- Separate test directory: `jj_tui/test/lib/`

**Naming:**
- Pattern: `{module}_tests.ml` (e.g., `jj_json_tests.ml`, `commit_render_tests.ml`)
- Integration tests: `jj_tui/test/lib/ansi.ml`

**Structure:**
```
jj_tui/lib/
  commit_render.ml
  commit_render_tests.ml         # Co-located tests
  jj_json.ml
  jj_json_tests.ml               # Co-located tests (290 lines)
  render_jj_graph.ml
  render_jj_graph_tests.ml       # Golden tests (848 lines)
  ansiReverse.ml
  ansiReverseTests.ml            # Co-located tests (298 lines)
```

## Test Structure

**Suite Organization:**
```ocaml
let%expect_test "test_name" =
  (* Arrange *)
  let node = make_test_node ... in

  (* Act *)
  let result = render_commit_content node in

  (* Assert *)
  render_and_print result;
  [%expect {|
    Expected output here
  |}]
;;
```

**Patterns:**
- `let%expect_test` for each test case
- Helper functions for test data creation
- Direct output comparison via expect blocks
- Golden tests for graph rendering (exact glyph output)

## Mocking

**Framework:**
- No explicit mocking library
- Test data created via helper functions

**Patterns:**
```ocaml
(* Factory pattern for test data *)
let make_test_node
      ?(working_copy = false)
      ?(immutable = false)
      ()
  = {
    commit_id = "test123";
    working_copy;
    immutable;
    ...
  }
```

**What to Mock:**
- JJ output: Create sample JSON/JSONL strings
- ANSI sequences: Hand-crafted escape code strings
- UI state: Construct test state records

**What NOT to Mock:**
- Rendering functions: Test actual Notty output
- Parsing logic: Test real angstrom parsers
- Pure functions: Test directly

## Fixtures and Factories

**Test Data:**
```ocaml
(* Factory functions in test files *)
let make_test_commit ?(id = "abc123") () =
  {
    commit_id = id;
    parents = [];
    change_id = "xyz789";
    description = "Test commit";
    ...
  }

(* Example from jj_json_tests.ml *)
let sample_jsonl = {|
{"commit_id":"abc123","parents":[]}
{"commit_id":"def456","parents":["abc123"]}
|}
```

**Location:**
- Factory functions: Defined in test files near usage
- Shared fixtures: Inline strings in test files
- Test data: `jj_tui/test/` directory

## Coverage

**Requirements:**
- No enforced coverage target
- Coverage tracked for awareness
- Focus on critical paths: parsing, rendering, graph algorithm

**Configuration:**
- No coverage tool configured in dune
- Manual coverage via test count

**View Coverage:**
- Not available - no coverage tool integration

## Test Types

**Unit Tests:**
- Scope: Single function/module in isolation
- Examples: `jj_json_tests.ml` (JSON parsing), `key.ml` tests
- Speed: Fast (< 1s per test file)

**Golden Tests:**
- Scope: Exact output verification for graph rendering
- Examples: `render_jj_graph_tests.ml` (848 lines)
- Philosophy: "Prefer updating algorithm to match golden outputs"
- Use: Regression detection for complex visual output

**Integration Tests:**
- Scope: Multiple modules together
- Examples: `jj_tui/test/lib/ansi.ml` (ANSI parsing + rendering)
- Setup: Real JJ output strings

## Common Patterns

**Expect Test Pattern:**
```ocaml
let%expect_test "parse_valid_jsonl" =
  let input = {|{"commit_id":"abc123"}|} in
  (match parse_jj_log_output input with
   | Ok commits ->
     Printf.printf "Parsed %d commits\n" (List.length commits)
   | Error msg ->
     Printf.printf "Error: %s\n" msg);
  [%expect {|
    Parsed 1 commits
  |}]
;;
```

**Error Testing:**
```ocaml
let%expect_test "parse_invalid_json" =
  let input = {|invalid json|} in
  (match parse_jj_log_output input with
   | Ok _ -> print_endline "Unexpected success"
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {|
    Error: JSON parse failed
  |}]
;;
```

**Rendering Test Pattern:**
```ocaml
(* Helper to convert Notty images to strings *)
let image_to_string img =
  let buf = Buffer.create 256 in
  let w, h = Notty.I.(width img, height img) in
  Notty.Render.to_buffer buf Notty.Cap.dumb (0, 0) (w, h) img;
  Buffer.contents buf
;;

let%expect_test "render_commit" =
  let node = make_test_node () in
  let img = render_commit_content node in
  print_endline (image_to_string img);
  [%expect {| ... |}]
;;
```

**Snapshot Testing:**
- All tests use expect blocks as snapshots
- Workflow: Run tests, review diffs, `dune promote` to accept changes

## Test Coverage Summary

**Well-tested modules:**
- `jj_json.ml` - 290 lines of tests, 45 test cases
- `render_jj_graph.ml` - 848 lines of golden tests
- `ansiReverse.ml` - 298 lines of ANSI parsing tests
- `commit_render.ml` - 189 lines of rendering tests

**Untested modules:**
- `jj_commands.ml` - No tests for command execution
- `graph_commands.ml` - No tests for command definitions
- `jj_process.ml` - No tests for process execution
- `jj_ui.ml` - No tests for UI orchestration
- `global_funcs.ml` - No tests for state updates

---

*Testing analysis: 2026-01-15*
*Update when test patterns change*
