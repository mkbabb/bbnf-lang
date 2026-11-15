#!/bin/sh
# PERF1D-B2 gate read at the lane: regen --check, iter-clippy (-D warnings), full nextest.
set -u
L=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/r3
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
export BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus
cd $HOME/Programming/bbnf-lang-wt/lw3-s9b || exit 2
echo "head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')" > $L/gate-head.log
cargo xtask regen --check > $L/gate-regen-check.log 2>&1; echo "rc=$?" >> $L/gate-regen-check.log
cargo iter-clippy > $L/gate-clippy.log 2>&1; echo "rc=$?" >> $L/gate-clippy.log
cargo iter-test --no-fail-fast > $L/gate-nextest.log 2>&1; echo "rc=$?" >> $L/gate-nextest.log
