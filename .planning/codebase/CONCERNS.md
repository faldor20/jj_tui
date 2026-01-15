# Codebase Concerns

**Analysis Date:** 2026-01-15

## Tech Debt

**Rendering delay (performance):**
- Issue: UI always renders "at least one frame behind" user input
- Files: `jj_tui/bin/global_funcs.ml:46` (TODO comment)
- Why: Current reactive variable update pattern introduces frame delay
- Impact: Slight UI lag, user perceives delayed response
- Fix approach: Mailbox processor pattern to trigger updates without Lwd.var delay

**Double rendering requirement:**
- Issue: Some UI updates require double render pass for correct display
- Files: `jj_tui/bin/graph_view.ml:137` (TODO comment)
- Why: Dependency on layout calculations from previous frame
- Impact: Slight performance overhead, extra frame delay
- Fix approach: Refactor rendering to compute layout in single pass

**DOP list duplication:**
- Issue: "Stop using dop last twice" - code duplication in list processing
- Files: `jj_tui/bin/global_funcs.ml:61` (TODO comment)
- Why: Quick implementation without refactoring
- Impact: Redundant computation, harder to maintain
- Fix approach: Extract shared logic into helper function

**Commented-out code:**
- Issue: Large blocks of old implementation left in comments
- Files: `jj_tui/bin/jj_process.ml:57-110` (picos_process old implementation)
- Why: Historical reference during refactoring
- Impact: Code clutter, confusion for new contributors
- Fix approach: Remove commented code, rely on git history

## Known Bugs

**No known critical bugs documented**
- No explicit bug tracking in code comments
- No FIXME comments found
- TODO items represent planned improvements, not bugs

## Security Considerations

**Shell command execution (low risk):**
- Risk: JJ CLI executed with user-provided arguments
- Files: `jj_tui/bin/jj_process.ml`
- Current mitigation: Arguments come from UI state, not direct user input
- Recommendations: Input validation already adequate for TUI context

**Build-time shell substitution:**
- Risk: Shell command in dune build rule with environment variable
- Files: `jj_tui/bin/dune:40-42` (GIT_DESCRIBE substitution)
- Current mitigation: Build-time only, not runtime
- Recommendations: Low priority - build environment trusted

**Unsafe operations:**
- Risk: `unsafe_color_of_int`, `unsafe_style_of_int` could crash on invalid input
- Files: `jj_tui/lib/ansiReverse.ml:9-11,14-15,142,164`
- Current mitigation: Input validated by ANSI parser before unsafe ops
- Recommendations: Add defensive checks or document preconditions

## Performance Bottlenecks

**Graph rendering complexity:**
- Problem: Graph rendering algorithm is computationally intensive
- Files: `jj_tui/lib/render_jj_graph.ml` (1,117 lines)
- Measurement: No profiling data available
- Cause: Lane-based algorithm with complex state tracking
- Improvement path: Profile to identify hotspots, consider caching rendered glyphs

**String concatenation in loops:**
- Problem: Inefficient string building during graph construction
- Files: `jj_tui/lib/process_wrappers.ml:161`
- Cause: Using `^` operator instead of Buffer
- Improvement path: Replace with `Buffer.add_string` for better performance

**ANSI parsing on every render:**
- Problem: Complex regex parsing runs on each frame
- Files: `jj_tui/lib/ansiReverse.ml`
- Cause: No caching of parsed ANSI sequences
- Improvement path: Cache parsed output keyed by input string

## Fragile Areas

**Unsafe list access (high risk):**
- Why fragile: `List.hd`, `List.nth` crash on empty/short lists
- Files:
  - `jj_tui/bin/graph_view.ml:68,87` - unchecked `List.nth`
  - `jj_tui/lib/render_jj_graph.ml:525,1092` - `List.hd` without empty check
  - Multiple test files use `List.hd` (acceptable in tests)
- Common failures: Runtime exceptions on empty lists
- Safe modification: Use pattern matching or `List.nth_opt`
- Test coverage: No tests for edge cases (empty lists)

**Error escalation with failwith:**
- Why fragile: Parsing errors converted to exceptions instead of Result propagation
- Files:
  - `jj_tui/lib/jj_json.ml:88` - failwith on JSON parse error
  - `jj_tui/lib/process_wrappers.ml:63,72` - failwith for divergent parsing
  - `jj_tui/lib/util.ml:54` - failwith for unicode validation
- Common failures: Crashes on malformed JJ output
- Safe modification: Return `Result.t` throughout parsing pipeline
- Test coverage: Limited error case testing

**Mutable state in rendering:**
- Why fragile: Multiple refs in graph rendering create complex state dependencies
- Files: `jj_tui/lib/render_jj_graph.ml` (refs: `need_link_line`, `columns`, `result`, etc.)
- Common failures: State inconsistencies if render order changes
- Safe modification: Refactor to pure functional style with explicit state passing
- Test coverage: Golden tests catch output changes but not state bugs

## Scaling Limits

**No explicit scaling limits identified**
- Application is a local TUI tool, not a service
- Performance scales with JJ repository size
- Memory usage grows with commit graph size

## Dependencies at Risk

**ppx_record_updater (unmaintained fork):**
- Risk: Pinned to personal fork `git+https://github.com/faldor20/ppx_record_updater.git`
- Files: `dune-project`
- Impact: Custom record update syntax used throughout (`[@updater]` attribute)
- Migration plan: Monitor upstream, consider inlining if abandoned

**Forked UI libraries:**
- Risk: Maintaining forks of nottui, notty, lwd
- Files: `forks/nottui/`, `forks/notty/`, `forks/lwd/`
- Impact: Need to track upstream changes, apply patches manually
- Migration plan: Contribute patches upstream, reduce divergence

## Missing Critical Features

**No persistent state:**
- Problem: No session persistence across restarts
- Current workaround: Users restart from scratch each time
- Blocks: Resuming work, saving UI preferences beyond config
- Implementation complexity: Medium (add state serialization)

**No undo/redo:**
- Problem: JJ operations cannot be undone from TUI
- Current workaround: Use JJ CLI `jj undo` manually
- Blocks: Safe experimentation with rebases/squashes
- Implementation complexity: Medium (track command history)

## Test Coverage Gaps

**Command execution untested:**
- What's not tested: Core command execution logic in `jj_commands.ml` (512 lines)
- Risk: Commands could fail silently, incorrect JJ CLI invocation
- Priority: High
- Difficulty to test: Medium (need mock JJ CLI or integration tests)

**Process execution untested:**
- What's not tested: JJ process spawning in `jj_process.ml` (375 lines)
- Risk: Process management bugs, I/O handling failures
- Priority: High
- Difficulty to test: Medium (need process mocking)

**UI orchestration untested:**
- What's not tested: Main UI logic in `jj_ui.ml` (261 lines)
- Risk: Event routing bugs, view integration issues
- Priority: Medium
- Difficulty to test: High (requires TUI testing framework)

**State management untested:**
- What's not tested: State updates in `global_funcs.ml`
- Risk: State inconsistencies, race conditions
- Priority: Medium
- Difficulty to test: Medium (unit tests for state transitions)

---

*Concerns audit: 2026-01-15*
*Update as issues are fixed or new ones discovered*
