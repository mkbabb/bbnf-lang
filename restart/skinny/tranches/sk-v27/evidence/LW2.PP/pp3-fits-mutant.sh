#!/bin/sh
# sk-v27 LW2.PP-3 falsifier: a renderer with an off-by-one in `fits` must be
# caught by at least one law in tests/laws.rs.
# Usage (under guarded): pp3-fits-mutant.sh <pprint-worktree>
# The mutant turns the group decision `col + flat + trailing > w` into `>= w`
# (a group that fits exactly is broken). render.rs is restored afterwards.
# Exit 0 iff at least one law test fails.
set -u
WT=$1; R=$WT/rust; S=$(mktemp -d "$HOME/.dev-logs/bbnf-lockstep/lw2/s8-pp/pp3mut.XXXXXX")
cp "$R/src/render.rs" "$S/render.rs"
restore() { cp "$S/render.rs" "$R/src/render.rs"; rm -rf "$S"; }
trap restore EXIT
perl -0pi -e 's/(h\.trailing as u64\s*)> self\.max_width as u64/$1>= self.max_width as u64/' "$R/src/render.rs"
grep -q '>= self.max_width as u64' "$R/src/render.rs" || { echo "mutant not applied"; exit 2; }
( cd "$R" && cargo test --release --test laws 2>&1 ) > "$S/out.log"; rc=$?
grep -E '^test |test result|panicked' "$S/out.log" | head -24
echo "cargo test rc=$rc"
if grep -q '^test .* FAILED$' "$S/out.log"; then echo "MUTANT CAUGHT"; exit 0; fi
echo "MUTANT NOT CAUGHT"; exit 1
