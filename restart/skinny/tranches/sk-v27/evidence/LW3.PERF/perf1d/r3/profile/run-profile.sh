#!/bin/sh
set -u
L=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/r3/profile
W=$HOME/Programming/bbnf-lang-wt/lw3-s9b-prof
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus
cd $W || exit 2
python3 -I $L/instrument.py crates/core/src/grammar/generated/json.rs > $L/instrument.log 2>&1 || exit 3
cp $L/perf1d_profile.rs crates/core/examples/perf1d_profile.rs
git diff --stat > $L/diag-tree.diff.stat; git diff > $L/diag-tree.diff
cargo run -p bbnf --locked --profile bench --example perf1d_profile > $L/profile.tsv 2> $L/build.log; echo "rc=$?" >> $L/build.log
