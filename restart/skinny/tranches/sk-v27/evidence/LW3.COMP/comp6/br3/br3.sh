#!/bin/sh
# sk-v27 LW3.COMP-6-BR3 named born-RED gate (RULINGS 2026-11-13b COMP6-T; cure unit LW5.COMP-1).
# Usage: br3.sh <bl worktree> <log dir>. Places br3_type_law.rs as a bbnf-ir test for one run,
# removes it, prints `rc= result=`. Exit 0 iff the law passes (GREEN = cured). RED today.
set -u
BL=$1; L=$2; mkdir -p "$L"
export CARGO_TARGET_DIR=${CARGO_TARGET_DIR:-$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang}
cd "$BL" || exit 2
T=crates/ir/tests/br3_type_law.rs
[ -e "$T" ] && { echo "refused: $T exists"; exit 2; }
echo "bl=$(git rev-parse HEAD) tree=$(git rev-parse 'HEAD^{tree}')" > "$L/heads.txt"
cp "$(dirname "$0")/br3_type_law.rs" "$T"
cargo nextest run -p bbnf-ir --locked --retries 0 --test br3_type_law > "$L/nextest.log" 2>&1; rc=$?
rm -f "$T"
msg=$(grep -oE 'of 100000 cases: [^"]*' "$L/nextest.log" | head -1)
echo "rc=$rc ${msg:-passed}" | tee "$L/result.txt"
[ "$rc" -eq 0 ]
