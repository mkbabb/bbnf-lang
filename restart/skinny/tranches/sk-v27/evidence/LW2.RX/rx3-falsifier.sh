#!/usr/bin/env bash
# sk-v27 LW2.RX-3 falsifier: Hir::to_source as the parser's round-trip oracle.
#
# Usage: rx3-falsifier.sh <parse-that checkout> <log dir> <label>
#   law 1: rust/regex/tests/to_source.rs — parse(hir.to_source(flags)) == hir
#          for every accepted pattern among the 171 leaves and the smoke corpus;
#   law 2: V8 and JSC give the same answer for the source and the printed text
#          on every smoke input (oracle.mjs twice, then oracle/law.mjs);
#   filed: parse_that's generated_utf8_test (born-RED, ignored) read with
#          --include-ignored; it must fail on the Span::as_str panic.
# GREEN (exit 0) when law 1 and law 2 pass. The filed born-RED is reported.
set -u
repo=$1; logs=$2; label=$3
mkdir -p "$logs"
guard=~/.claude/guard/guarded
export CARGO_TARGET_DIR=${CARGO_TARGET_DIR:-$HOME/.dev-logs/bbnf-lockstep/lw2/s7-rx/target-pt}
cd "$repo/rust" || exit 2
sha=$(git rev-parse --short=7 HEAD)
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
"$guard" --timeout 30 -- cargo test -p parse_that --locked --test regex generated_utf8 -- --include-ignored >"$logs/$label-filed.log" 2>&1
filed_rc=$?
filed_reason=$(grep -o 'Span::as_str: invalid bounds [0-9.]* for src len [0-9]*' "$logs/$label-filed.log" | head -1)
verdict=RED
[ "$law1" = 0 ] && [ "$law2" = 0 ] && verdict=GREEN
echo "rx3 $label @ $sha: law1 rc=$law1; law2 rc=$law2; filed born-RED rc=$filed_rc (${filed_reason:-no panic}) -> $verdict"
grep -E 'rx3 round trip|error\[E[0-9]+\]|broken' "$logs/$label-law1.log" | head -5
[ -f "$logs/$label-law2.log" ] && grep -vE '^\[guarded\]' "$logs/$label-law2.log" | head -20
[ "$verdict" = GREEN ]
