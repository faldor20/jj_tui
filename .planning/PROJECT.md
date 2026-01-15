# jj_tui - Interactive Rebase Preview

## What This Is

A terminal UI for Jujutsu (jj) version control that provides visual, interactive rebase previews. Users can see exactly how their commit graph will look after a rebase before executing the operation, making rebasing safer and more intuitive.

## Core Value

Visual rebase preview with live graph updates - users must be able to configure a rebase interactively, see the resulting graph structure with preview nodes, and execute only when they're confident the result is correct.

## Requirements

### Validated

<!-- Existing capabilities confirmed from codebase -->

- ✓ TUI commit graph visualization with Nottui/Notty/Lwd - existing
- ✓ JJ CLI subprocess execution and output parsing - existing
- ✓ Reactive UI state management via Lwd.var - existing
- ✓ Multi-node selection with space key - existing
- ✓ Graph navigation (up/down/scroll) - existing
- ✓ Custom graph renderer (static display working) - existing
- ✓ Rebase command variants (single/children/branch) - existing
- ✓ Command keybinding system with mode support - existing
- ✓ Functor-based modular architecture - existing

### Active

<!-- Current scope: Interactive rebase preview feature -->

- [ ] Rebase mode activation (press 'r' on hovered commit)
- [ ] Destination selection (space-select target nodes while in rebase mode)
- [ ] Source mode toggle (press 's': single/children/branch)
- [ ] Destination mode toggle (press 'd': insert-before/insert-after/add-after)
- [ ] Preview node insertion into graph data structure
- [ ] Preview node rendering (distinct color + symbol)
- [ ] Dynamic graph re-rendering on mode/selection changes
- [ ] Cycle detection (prevent commit being ancestor + descendant)
- [ ] UI mode indicators (show current source and destination modes)
- [ ] Rebase execution on confirmation (press 'y')
- [ ] Cancel and exit rebase mode (press 'esc')

### Out of Scope

- Multiple simultaneous rebases (treating multi-select as separate operations) - complexity not needed for MVP
- Other preview modes (squash, split, parallelize) - future enhancement
- Performance optimization for large graphs - premature until rebase preview works
- Undo/redo within rebase preview mode - JJ has `jj undo` at CLI level
- Persistent rebase state across sessions - ephemeral mode is sufficient

## Context

**Codebase Architecture:**
- OCaml 5.2+ functional reactive TUI
- Nottui/Notty for terminal rendering, Lwd for reactive state
- Picos for structured concurrency
- Custom graph renderer in `render_jj_graph.ml` (1,117 lines, lane-based algorithm)
- Command system in `graph_commands.ml` with 547 lines of existing commands
- Global reactive state in `global_vars.ml` (ui_state_t, graph state)

**Graph Rendering:**
- Custom renderer implemented and working for static display
- Renders commit graph with Unicode box-drawing glyphs
- Lane-based algorithm handles node positioning and parent-child relationships
- Golden tests verify exact glyph output

**Rebase Command Structure:**
- Three source modes: single revision (`-r`), with descendants (`-s`), bookmark (`-b`)
- Destination specified via `-d` flag
- Insert modes: `--insert-before`, `--insert-after`, or default (add as parent)
- Existing commands at lines 312-342 in `graph_commands.ml`

**Technical Debt to Navigate:**
- Frame rendering delay (TODO in `global_funcs.ml:46`)
- Unsafe list access patterns in graph rendering
- No tests for command execution logic

**Known Constraints:**
- Must integrate with existing JJ CLI (no direct repo access)
- Graph structure from JJ must be augmented, not replaced
- Preview nodes need clear visual distinction to avoid confusion

## Constraints

- **Language**: OCaml 5.2+ - existing codebase
- **TUI Framework**: Nottui/Notty/Lwd - forked versions maintained in `forks/`
- **VCS Integration**: JJ CLI subprocess execution only - no direct libgit2/jj library access
- **Architecture**: Functor-based with reactive state - must maintain existing patterns
- **Build System**: Dune 3.12+ - established build configuration
- **Testing**: ppx_expect inline tests - existing test infrastructure

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| Custom graph renderer over jj's ASCII output | Need dynamic preview node insertion and re-rendering | ✓ Good - static rendering complete |
| Hovered node as rebase source, space-select for destinations | Consistent with existing selection model, clear intent | — Pending |
| Single rebase at a time (ignore multi-select) | Simpler state management, clearer preview | — Pending |
| Mode toggles with 'd'/'s' keys | Quick iteration without prompts, shows all options | — Pending |
| Distinct preview node rendering (color + symbol) | Must be unmistakably different from real commits | — Pending |
| Cycle detection before preview | Prevent invalid graph states, better UX than error on execute | — Pending |

---
*Last updated: 2026-01-15 after initialization*
