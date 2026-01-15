# Changes to Match Original JJ Rendering Format

## Goal
Match the original `jj log` output format which displays commit information on two lines:

**Original jj format:**
```
@  ztooztwk eli.jambu@gmail.com 2026-01-15 14:05:59 235795c5
│  (empty) (no description set)
```

## Changes Made

### 1. Updated `render_commit_content` in `graph_view.ml` (lines 19-79)

**Previous format (single line):**
```
change_id author_name timestamp (bookmarks) description
```

**New format (two lines):**
```
Line 1: change_id email timestamp [bookmarks] commit_id_short
Line 2: (empty) description
```

**Key changes:**
- Show **full email** instead of extracting name before `@`
- Add **commit_id_short** (8 characters) at end of line 1
- Move **description** to line 2
- Add **(empty)** prefix when `node.empty` is true
- Remove parentheses around bookmarks, show as space-separated list
- Use `I.vcat` to create two-line image

### 2. Updated `render_graph_row` in `graph_view.ml` (lines 81-109)

**Problem:** When content is multi-line, the graph character only appears on the first line with `I.hcat [ graph_img; content_img ]`.

**Solution:** Detect multi-line content and manually create graph continuation for subsequent lines:

```ocaml
if content_height > 1 then
  (* Replace node glyphs (○, @, ◌, ◆) with vertical bar │ *)
  let graph_continuation = replace_node_glyphs_with_bar row.graph_chars in
  (* Create each line with appropriate graph prefix *)
  let lines = List.init content_height (fun i ->
    let line_img = I.vcrop i 1 content_img in
    if i = 0 then I.hcat [ graph_img; line_img ]
    else I.hcat [ graph_continuation; line_img ]
  ) in
  I.vcat lines
else
  I.hcat [ graph_img; content_img ]
```

**How it works:**
1. Check if content height > 1
2. Create `graph_continuation` by replacing all node glyphs with `│`
3. For each line:
   - Line 0: Use original `graph_chars` (contains node glyph)
   - Line 1+: Use `graph_continuation` (node glyph replaced with `│`)
4. Vertically stack all lines

### 3. Color Scheme

**Line 1:**
- `change_id`: cyan (bold cyan if working_copy, yellow if empty, lightmagenta if immutable)
- `email`: dim white
- `timestamp`: dim white  
- `bookmarks`: bold green
- `commit_id_short`: dim cyan

**Line 2:**
- `description`: white (dim if empty/preview, lightyellow if wip)

## Examples

### Simple commit (empty)
```
@  ztooztwk eli.jambu@gmail.com 2026-01-15 14:05:59 235795c5
│  (empty) (no description set)
```

### Commit with description
```
○  smqmznlq eli.jambu@gmail.com 2026-01-15 01:40:23 09a9f33f
│  Add new feature
```

### Commit with bookmarks
```
◆  noszsqtm eli.jambu@gmail.com 2025-11-22 00:26:06 main master 35b532af
│  remove aarch64 linux because it doesn't seem to work
```

### Complex graph
```
│ ○  nkwwwlnw eli.jambu@gmail.com 2026-01-15 00:30:16 89abd641
│ │  rewrite
```

## Testing

- ✅ `dune build` - compiles successfully
- ✅ `dune runtest` - all existing tests pass
- ✅ Multi-line rendering works correctly
- ✅ Graph continuation characters display properly

## Files Modified

1. **`jj_tui/bin/graph_view.ml`**
   - `render_commit_content` (lines 19-79): Two-line format
   - `render_graph_row` (lines 81-109): Multi-line graph handling

## No Breaking Changes

- All existing functionality preserved
- Tests pass without modification
- Only visual formatting changed
