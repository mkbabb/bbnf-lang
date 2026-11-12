#!/usr/bin/env bash
# sk-v27 LW3.RX-3 falsifier (parse-that): ECMAScript case canonicalization
# from generated UCD tables at the pinned Unicode version (OR-6).
#
# Usage: rx3-falsifier.sh <parse-that checkout> <log dir> <label> <ucd dir> <fold corpus dir>
#   born:   born_red_lw1 kelvin_folds_under_iu, long_s_folds_under_iu,
#           latin1_folds_under_i_and_iu (read --include-ignored, GREEN only
#           when their #[ignore] is gone) and kelvin_does_not_fold_without_u;
#   tables: tools/ucd/gen_tables.py over the pinned UCD inputs (it refuses
#           any input whose sha256 differs) reproduces src/case_tables.rs
#           byte for byte; tests/casefold_tables.rs;
#   pin:    node's Unicode version is the tables' (17.0);
#   fold:   examples/leaf_oracle fold-compare on <fold corpus dir>/fold-corpus.json
#           and its V8 + JSC answers -- 0 differing cases.
set -u
repo=$1; logs=$2; label=$3; ucd=$4; fold=$5
mkdir -p "$logs"
guard=~/.claude/guard/guarded
# Under one outer `guarded` run (GUARD_INNER=1) the steps run in its tree.
if [ -n "${GUARD_INNER:-}" ]; then guard=$(cd "$(dirname "$0")" && pwd)/inner-run.sh; fi
. ~/.dev-logs/bbnf-lockstep/lw3/s4-rx/env-pt.sh
cd "$repo" || exit 2
sha=$(git rev-parse --short=9 HEAD)
f=rust/regex/tests/born_red_lw1.rs
ignored=0
for t in kelvin_folds_under_iu long_s_folds_under_iu latin1_folds_under_i_and_iu; do
  n=$(grep -B1 "fn $t" "$f" | grep -cE '^\s*#\[ignore')
  ignored=$((ignored + n))
done
cd rust/regex || exit 2
"$guard" --timeout 60 -- cargo test -p parse-that-regex --locked --test born_red_lw1 -- --include-ignored folds_under kelvin_does_not_fold >"$logs/$label-born.log" 2>&1
born=$?
gen=$logs/$label-case_tables.rs
python3 -I tools/ucd/gen_tables.py "$ucd" "$gen" >"$logs/$label-tables.log" 2>&1 && cmp "$gen" src/case_tables.rs >>"$logs/$label-tables.log" 2>&1
tables=$?
"$guard" --timeout 60 -- cargo test -p parse-that-regex --locked --test casefold_tables >"$logs/$label-casefold.log" 2>&1
casefold=$?
uv=$(node -p 'process.versions.unicode')
pin=1; [ "$uv" = "17.0" ] && grep -q 'UNICODE_VERSION: &str = "17.0.0"' src/case_tables.rs 2>/dev/null && pin=0
"$guard" --timeout 60 -- cargo run --locked -q -p parse-that-regex --example leaf_oracle -- fold-compare \
  "$fold/fold-corpus.json" "$fold/fold-answers.json" >"$logs/$label-fold.log" 2>&1
foldrc=$?
verdict=RED
[ "$born" = 0 ] && [ "$ignored" = 0 ] && [ "$tables" = 0 ] && [ "$casefold" = 0 ] && [ "$pin" = 0 ] && [ "$foldrc" = 0 ] && verdict=GREEN
echo "rx3 $label @ $sha: born rc=$born (ignore attrs $ignored); tables rc=$tables; casefold rc=$casefold; pin rc=$pin (node unicode $uv); fold rc=$foldrc -> $verdict"
grep -hE '^test .*(FAILED|ok)$|left:|right:|^error' "$logs/$label-born.log" | head -12
tail -2 "$logs/$label-tables.log"
grep -hE '^test result|^error' "$logs/$label-casefold.log" | head -2
grep -hE '^fold corpus|^DIFFER|error' "$logs/$label-fold.log" | head -6
[ "$verdict" = GREEN ]
