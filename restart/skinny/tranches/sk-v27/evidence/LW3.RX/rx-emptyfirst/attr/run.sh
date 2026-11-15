#!/bin/sh
# RXEF-BENCH attribution (RULINGS 2026-11-15a): one arm. Usage: run.sh <arm-name> <worktree>
# Shared clean of bbnf (dev) first, probe.diff applied, the example copied in, read, both removed.
set -u
arm=$1; W=$2; A=$(cd "$(dirname "$0")" && pwd); L=$HOME/.dev-logs/bbnf-lockstep/lw3/rxef-attr; mkdir -p $L
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
CLEAN=$HOME/Programming/bbnf-lang-wt/lw3-int/restart/skinny/tranches/sk-v27/evidence/LW3.AUDIT/tgt-clean.sh
O=$A/$arm.txt
cd $W || exit 2
echo "arm=$arm head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')" > $O
sh $CLEAN clean $W dev bbnf >> $O 2>&1 || { echo "clean failed" >> $O; exit 3; }
git apply $A/probe.diff || exit 4
cp $A/sheets_attr_probe.rs crates/core/examples/sheets_attr_probe.rs
cargo build -q -p bbnf --example sheets_attr_probe > $L/$arm-build.log 2>&1; echo "build rc=$?" >> $O
sh $CLEAN hash dev bbnf >> $O
X=$CARGO_TARGET_DIR/debug/examples/sheets_attr_probe
echo "== stress.txt" >> $O; $X file crates/core/tests/fixtures/sheets/stress.txt >> $O 2>&1
echo "== nested.txt" >> $O; $X file crates/core/tests/fixtures/sheets/nested.txt >> $O 2>&1
for i in '=LET(x, 1, x)' '=LAMBDA(x, x)' '=LET(f, LAMBDA(x, x), f)'; do echo "== $i" >> $O; $X tree "$i" >> $O 2>&1; done
rm -f crates/core/examples/sheets_attr_probe.rs; git apply -R $A/probe.diff
echo "after dirty=$(git status --short | grep -vc '^??')" >> $O
