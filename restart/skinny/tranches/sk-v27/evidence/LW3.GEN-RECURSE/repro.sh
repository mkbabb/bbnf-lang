#!/bin/sh
# GEN-RECURSE RED read n (RULINGS 2026-11-14k): copy the repro test in, run it under ax-iter, remove it.
set -u
n=$1; W=$2; L=$HOME/.dev-logs/bbnf-lockstep/lw3/gen-recurse; mkdir -p $L
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
E=$W/restart/skinny/tranches/sk-v27/evidence; cd $W || exit 2
echo "head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')" > $L/red-r$n.log
cp $E/LW3.GEN-RECURSE/gen_recurse_repro.rs crates/core/tests/gen_recurse_repro.rs
cargo nextest run --cargo-profile ax-iter -p bbnf --test gen_recurse_repro --no-fail-fast >> $L/red-r$n.log 2>&1; echo "rc=$?" >> $L/red-r$n.log
rm -f crates/core/tests/gen_recurse_repro.rs
