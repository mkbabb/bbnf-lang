#!/bin/sh
# sk-v27 LW3.TREE-1 falsifier (RULINGS 2026-11-12d TREE-WRITE): a test run must not dirty the tree.
# Usage: tree_write_check.sh <bl worktree> <log dir>
# From a tree whose two paths are clean, runs the two tests that wrote tracked files
# (`gen_ts_parser`, `ts_node_execute`) and reads `git status --porcelain` over those paths.
# Prints `rc=<nextest rc> dirty=<n>`; exit 0 iff rc = 0 and dirty = 0.
# Afterwards it puts the two paths back to HEAD, so the next read starts clean.
set -u
BL=$1; L=$2; mkdir -p "$L"
P1=crates/core/benches/ts/generated_json.mjs
P2=docs/tranches/AZ-IV/audit/W5-node-execute.txt
export CARGO_TARGET_DIR=${CARGO_TARGET_DIR:-$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang}
cd "$BL" || exit 2
echo "bl=$(git rev-parse HEAD) tree=$(git rev-parse 'HEAD^{tree}')" > "$L/heads.txt"
git diff --quiet HEAD -- crates/core/tests/gen_ts_parser.rs crates/core/tests/ts_node_execute.rs \
  || echo "worktree edits to the two tests (a mutant read): $(git diff --stat HEAD -- crates/core/tests | tail -1)" >> "$L/heads.txt"
pre=$(git status --porcelain -- "$P1" "$P2")
if [ -n "$pre" ]; then echo "refused: the two paths are dirty before the run: $pre"; exit 2; fi
cargo nextest run -p bbnf --locked --no-fail-fast --test gen_ts_parser --test ts_node_execute > "$L/nextest.log" 2>&1
rc=$?
git status --porcelain -- "$P1" "$P2" > "$L/porcelain.txt"
dirty=$(grep -c . "$L/porcelain.txt")
echo "rc=$rc dirty=$dirty $(grep -E '^ +Summary' "$L/nextest.log" | tail -1 | sed 's/^ *//')" | tee "$L/result.txt"
cat "$L/porcelain.txt"
[ "$dirty" -eq 0 ] || git checkout HEAD -- "$P1" "$P2"
[ "$rc" -eq 0 ] && [ "$dirty" -eq 0 ]
