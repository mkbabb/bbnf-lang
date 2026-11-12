#!/usr/bin/env bash
# sk-v27 LW3.RX-1d falsifier, parse-that half: u/v code-point semantics.
#
# Usage: rx1d-falsifier.sh <parse-that checkout> <log dir> <label> <corpus dir>
#   leaves: tests/oracle_leaf_corpus.rs -- 0 ENGINE leaves among the 133
#           DFA-tier leaves, each case classed by rule (ENCODING-EXCLUDED,
#           AGREE, DIALECT-EXCLUDED, ENGINE), and every DIALECT leaf linted
#           RX-NOU;
#   gen:    examples/leaf_oracle compare on the generated corpus
#           (<corpus dir>/gen-corpus.json, its V8 + JSC answers with the
#           leaf's flags and read with `u`) -- 0 ENGINE cases;
#   utf8:   parse_that tests/regex/generated_utf8_test.rs read
#           --include-ignored, GREEN only when its #[ignore] is gone;
#   lint:   tests/lint_nou.rs (the engine's general lint id RX-NOU).
set -u
repo=$1; logs=$2; label=$3; corpus=$4
mkdir -p "$logs"
guard=~/.claude/guard/guarded
# Under one outer `guarded` run (GUARD_INNER=1) the steps run in its tree.
if [ -n "${GUARD_INNER:-}" ]; then guard=$(cd "$(dirname "$0")" && pwd)/inner-run.sh; fi
. ~/.dev-logs/bbnf-lockstep/lw3/s4-rx/env-pt.sh
cd "$repo" || exit 2
sha=$(git rev-parse --short=9 HEAD)
ignored=$(grep -B1 'fn generated_parser_reads_a_non_ascii_class_member' rust/parse_that/tests/regex/generated_utf8_test.rs | grep -cE '^\s*#\[ignore')
cd rust || exit 2
"$guard" --timeout 60 -- cargo test -p parse-that-regex --locked --test oracle_leaf_corpus -- --nocapture >"$logs/$label-leaves.log" 2>&1
leaves=$?
"$guard" --timeout 60 -- cargo run --locked -q -p parse-that-regex --example leaf_oracle -- compare \
  "$corpus/gen-corpus.json" "$corpus/gen-answers.json" "$corpus/gen-answers-u.json" >"$logs/$label-gen.log" 2>&1
gen=$?
"$guard" --timeout 60 -- cargo test -p parse_that --locked --test regex generated_utf8 -- --include-ignored >"$logs/$label-utf8.log" 2>&1
utf8=$?
"$guard" --timeout 60 -- cargo test -p parse-that-regex --locked --test lint_nou >"$logs/$label-lint.log" 2>&1
lint=$?
verdict=RED
[ "$leaves" = 0 ] && [ "$gen" = 0 ] && [ "$utf8" = 0 ] && [ "$ignored" = 0 ] && [ "$lint" = 0 ] && verdict=GREEN
echo "rx1d $label @ $sha: leaves rc=$leaves; gen rc=$gen; utf8 rc=$utf8 (ignore attrs $ignored); lint rc=$lint -> $verdict"
grep -hE '^oracle leaf corpus|^ENGINE |^RX-NOU|panicked at|ENGINE leaves no unit' "$logs/$label-leaves.log" | head -16
grep -hE '^generated corpus|^ENGINE' "$logs/$label-gen.log" | head -6
grep -hE '^test .*(FAILED|ok)$|panicked at|must read' "$logs/$label-utf8.log" | head -4
grep -hE '^test result|^error' "$logs/$label-lint.log" | head -2
[ "$verdict" = GREEN ]
