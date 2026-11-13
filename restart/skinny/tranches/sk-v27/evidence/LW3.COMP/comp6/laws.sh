#!/bin/sh
# sk-v27 LW3.COMP-6 lattice-law falsifier (RULINGS 2026-11-13a COMP6-Q): every LatticeDomain
# of bbnf-ir (Inline, Bool, CharSet, Dispatch, Type) and the Bitset control satisfy the
# join-semilattice laws over 10^5 generated cases (`passes::lattice_laws`).
# Usage: laws.sh <bl worktree> <log dir>. Prints one line per law test and the laws broken;
# exit 0 iff every one of the 7 law tests passes.
set -u
BL=$1; L=$2; mkdir -p "$L"
export CARGO_TARGET_DIR=${CARGO_TARGET_DIR:-$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang}
cd "$BL" || exit 2
echo "bl=$(git rev-parse HEAD) tree=$(git rev-parse 'HEAD^{tree}')" > "$L/heads.txt"
git diff --quiet HEAD -- crates/ir/src || echo "worktree edits under crates/ir/src (a mutant read)" >> "$L/heads.txt"
cargo nextest run -p bbnf-ir --lib --locked --no-fail-fast --retries 0 \
  -E 'test(lattice) | test(/csp_contract::inline_domain/)' > "$L/nextest.log" 2>&1
rc=$?
grep -E '^ +(PASS|FAIL) ' "$L/nextest.log" | sed -E 's/^ +//; s/\[[^]]*\] \([^)]*\) //' | sort > "$L/tests.txt"
grep -oE 'law: "[^"]*"' "$L/nextest.log" | sort | uniq -c > "$L/laws-broken.txt"
pass=$(grep -c '^PASS' "$L/tests.txt"); fail=$(grep -c '^FAIL' "$L/tests.txt")
echo "rc=$rc pass=$pass fail=$fail" | tee "$L/result.txt"; cat "$L/tests.txt" "$L/laws-broken.txt"
[ "$rc" -eq 0 ] && [ "$pass" -eq 7 ] && [ "$fail" -eq 0 ]
