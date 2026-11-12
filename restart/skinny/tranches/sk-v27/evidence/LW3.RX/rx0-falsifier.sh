#!/usr/bin/env bash
# sk-v27 LW3.RX-0 falsifier: parse-that's rust/regex carries no BBNF vocabulary.
#
# Usage: rx0-falsifier.sh <parse-that checkout> <log dir> <label>
#   grep: `git grep -niE bbnf -- rust/regex` over the working tree, no
#         exclusion; GREEN needs 0 lines (17 at LW3 entry).
#   law:  the oracle tooling still runs: rust/regex/tests/to_source.rs law 1,
#         then oracle.mjs on the smoke corpus and on the printed corpus, then
#         oracle/law.mjs (law 2), as LW2.RX-3 read it.
# Exit 0 only when the grep reads 0 and both laws pass.
set -u
repo=$1; logs=$2; label=$3
mkdir -p "$logs"
guard=~/.claude/guard/guarded
export CARGO_TARGET_DIR=${CARGO_TARGET_DIR:-$HOME/.dev-logs/bbnf-lockstep/lw3/target-parse-that}
cd "$repo" || exit 2
sha=$(git rev-parse --short=9 HEAD)
"$guard" --light -- git grep -niE bbnf -- rust/regex >"$logs/$label-grep.log" 2>&1
lines=$(grep -vc '^\[guarded\]' "$logs/$label-grep.log")
cd rust || exit 2
"$guard" --timeout 60 -- cargo test -p parse-that-regex --locked --test to_source round_trip -- --nocapture >"$logs/$label-law1.log" 2>&1
law1=$?
law2=skipped
if [ "$law1" = 0 ]; then
  RX3_PRINTED="$logs/$label-printed.json" "$guard" --timeout 30 -- cargo test -p parse-that-regex --locked --test to_source print_smoke_corpus >"$logs/$label-print.log" 2>&1
  "$guard" --light --timeout 10 -- node regex/oracle/oracle.mjs --corpus regex/oracle/smoke.json --out "$logs/$label-oracle" >"$logs/$label-oracle-src.log" 2>&1
  "$guard" --light --timeout 10 -- node regex/oracle/oracle.mjs --corpus "$logs/$label-printed.json" --out "$logs/$label-oracle" >"$logs/$label-oracle-printed.log" 2>&1
  "$guard" --light -- node regex/oracle/law.mjs "$logs/$label-oracle/sk-v27-LW1-smoke-answers.json" "$logs/$label-oracle/sk-v27-LW2-RX3-printed-answers.json" >"$logs/$label-law2.log" 2>&1
  law2=$?
fi
verdict=RED
[ "$lines" = 0 ] && [ "$law1" = 0 ] && [ "$law2" = 0 ] && verdict=GREEN
echo "rx0 $label @ $sha: grep bbnf lines=$lines; law1 rc=$law1; law2 rc=$law2 -> $verdict"
grep -v '^\[guarded\]' "$logs/$label-grep.log" | head -20
[ -f "$logs/$label-law2.log" ] && grep -E '^rx3 law' "$logs/$label-law2.log"
[ "$verdict" = GREEN ]
