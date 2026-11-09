#!/bin/sh
# LW1.COMP-1b checker falsifier for item forms and MOVE-by-absence
# (LW2 S14 ADJUDICATOR, sustaining S12 finding 2).
#
# Usage: fates-form-mutants.sh <checker.sh> <commit> <scratch-dir>
#
# Exports <commit>'s crates/, PLAN.md and the census evidence into
# <scratch-dir>, then re-adds the DELETEd `calculate_acyclic_deps_scc` under
# each `pub` item form the audit counts. Every form must be refused (refused >= 1).
# The control tree (no mutant) must read refused=0. In gate mode
# (FATES_AT=gate) the two simd-scan MOVE rows must be refused while
# crates/simd-scan is present, and pass once it is absent.
set -u
CHK=$(cd "$(dirname "$1")" && pwd)/$(basename "$1"); C=$2; S=$3
REPO=$(git rev-parse --show-toplevel)
EV=restart/skinny/tranches/sk-v27/evidence
rm -rf "$S"; mkdir -p "$S"
fresh() { # $1 = tree dir
  mkdir -p "$1"
  git -C "$REPO" archive "$C" crates restart/skinny/tranches/sk-v27/PLAN.md \
    $EV/LW1.COMP/CRATE-CENSUS.tsv $EV/LW2.COMP/ZERO-CALLER-LW1.tsv | tar -x -C "$1"
}
run() { # $1 = label, $2 = tree
  out=$(sh "$CHK" "$2/$EV/LW1.COMP/CRATE-CENSUS.tsv" "$2/$EV/LW2.COMP/ZERO-CALLER-LW1.tsv" "$2" 2>&1)
  echo "$1: $(printf '%s' "$out" | tail -1)"
}
fresh "$S/control"
run "control" "$S/control"
for form in 'pub fn' 'pub const fn' 'pub unsafe fn' 'pub async fn' 'pub const unsafe fn' \
            'pub extern "C" fn' 'pub(crate) fn'; do
  t="$S/m-$(echo "$form" | tr -c 'a-z\n' '-')"; cp -R "$S/control" "$t"
  printf '\n%s calculate_acyclic_deps_scc() {}\n' "$form" >> "$t/crates/core/src/graph/scc.rs"
  run "mutant [$form]" "$t"
done
( export FATES_AT=gate; run "gate, simd-scan present" "$S/control" )
cp -R "$S/control" "$S/gate-moved"; rm -rf "$S/gate-moved/crates/simd-scan"
( export FATES_AT=gate; run "gate, simd-scan absent" "$S/gate-moved" )
