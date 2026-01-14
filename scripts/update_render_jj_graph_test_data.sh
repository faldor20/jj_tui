#!/usr/bin/env bash
set -euo pipefail

# Regenerate the JSONL fixture used by `jj_tui/lib/render_jj_graph_tests.ml`.
#
# Usage:
#   ./scripts/update_render_jj_graph_test_data.sh [REVSET] [OUTFILE]
#
# Defaults:
#   REVSET  = 'all()'
#   OUTFILE = 'test/jj_log.json'
#
# Notes:
# - Output is JSONL (one JSON object per line).
# - `wip` is a heuristic derived from the description's first line starting with "wip:".

REVSET=""
OUTFILE="${2:-test/jj_log.json}"

echo "Updating $OUTFILE with revset: $REVSET" >&2

TEMPLATE=$(cat <<'JJTEMPLATE'
'{'
  ++ '"commit_id":' ++ json(commit_id)
  ++ ',"parents":[' ++ parents.map(|c| json(c.commit_id())).join(",") ++ ']'
  ++ ',"change_id":' ++ json(change_id)
  ++ ',"description":' ++ json(description)
  ++ ',"author":{"name":' ++ json(author.name())
    ++ ',"email":' ++ json(author.email())
    ++ ',"timestamp":' ++ json(author.timestamp())
  ++ '}'
  ++ ',"committer":{"name":' ++ json(committer.name())
    ++ ',"email":' ++ json(committer.email())
    ++ ',"timestamp":' ++ json(committer.timestamp())
  ++ '}'
  ++ ',"working_copy":' ++ json(current_working_copy)
  ++ ',"immutable":' ++ json(immutable)
  ++ ',"wip":' ++ json(description.first_line().starts_with("wip:"))
  ++ '}
  '
JJTEMPLATE
)


mkdir -p "$(dirname "$OUTFILE")"
# We want a stable "top-to-bottom" order like `jj log`, but without graph text.
# Write to a temp file first so parse errors don't clobber the existing fixture.
tmp_out="${OUTFILE}.tmp"
jj log  -T "$TEMPLATE" > "$tmp_out"
mv "$tmp_out" "$OUTFILE"

echo "Wrote $OUTFILE" >&2
