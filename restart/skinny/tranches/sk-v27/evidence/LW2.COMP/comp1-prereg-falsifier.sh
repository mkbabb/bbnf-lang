#!/usr/bin/env bash
# LW2.COMP-1 pre-registration falsifier (S5 COMP). Usage: comp1-prereg-falsifier.sh <bbnf-lang tree> <log>
# GREEN (rc 0) when: bench/PREREG.md registers the two root trials' 14 cells and the digest test
# passes; S11's harness refuses an unregistered cell id (mutant) and a registered cell on the
# wrong matrix, before running anything; RED (rc 1) otherwise.
set -u
T=$1; LOG=$2; : > "$LOG"
say() { echo "$*" | tee -a "$LOG"; }
red=0
cd "$T" || exit 2
n=$(grep -cE '^\| C-(EG|CSP)-[A-Z0-9-]+ \| F-TRIAL-(EG|CSP) \|' bench/PREREG.md)
say "registered trial cells: $n (want 14)"; [ "$n" = 14 ] || red=1
python3 -m unittest bench/test_prereg.py >>"$LOG" 2>&1; rc=$?; say "test_prereg rc=$rc"; [ $rc = 0 ] || red=1
OUT=$(mktemp -d)
for i in 1 2; do
  python3 bench/cb-b0/cbb0.py run --cell C-EG-MUTANT --label C-EG-MUTANT --matrix PORTABLE --reps 1 --out "$OUT" -- true >>"$LOG" 2>&1; rc=$?
  ref=$(grep -c "cbb0: refused: cell C-EG-MUTANT is not registered" "$LOG")
  say "mutant (unregistered cell) read $i: rc=$rc refusals-so-far=$ref"; { [ $rc = 2 ] && [ "$ref" = "$i" ]; } || red=1
done
python3 bench/cb-b0/cbb0.py run --cell C-EG-PARSE-TWITTER-P --label C-EG-PARSE-TWITTER-P --matrix NATIVE --reps 1 --out "$OUT" -- true >>"$LOG" 2>&1; rc=$?
grep -q "registered for PORTABLE, not NATIVE" "$LOG"; m=$?
say "registered cell on the wrong matrix: rc=$rc refused=$([ $m = 0 ] && echo yes || echo no)"; { [ $rc = 2 ] && [ $m = 0 ]; } || red=1
n_records=$(find "$OUT" -name '*.json' | wc -l | tr -d ' ')
say "records written by refused runs: $n_records (want 0)"; [ "$n_records" = 0 ] || red=1
rm -rf "$OUT"
say "PREREG.md sha256: $(shasum -a 256 bench/PREREG.md | cut -d' ' -f1)"
say "VERDICT: $([ $red = 0 ] && echo GREEN || echo RED)"
exit $red
