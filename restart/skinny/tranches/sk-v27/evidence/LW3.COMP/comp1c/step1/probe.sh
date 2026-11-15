#!/bin/sh
# LW3.COMP-1c step 1: run tree_probe.rs once (copied in, run, removed); output to $1.
set -u
out=$1; W=$HOME/Programming/bbnf-lang-wt/lw3-comp1c; D=$W/restart/skinny/tranches/sk-v27/evidence/LW3.COMP/comp1c/step1
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
cd $W || exit 2
cp $D/tree_probe.rs crates/core/tests/comp1c_tree_probe.rs
cargo clean -p bbnf --profile ax-iter > $out 2>&1
cargo nextest run --cargo-profile ax-iter -p bbnf --test comp1c_tree_probe --no-capture >> $out 2>&1; echo "rc=$?" >> $out
rm -f crates/core/tests/comp1c_tree_probe.rs
