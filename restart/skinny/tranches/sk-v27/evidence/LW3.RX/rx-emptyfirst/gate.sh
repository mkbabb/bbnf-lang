#!/bin/sh
# LW3.RX-EMPTYFIRST GREEN read n: clean bbnf, falsifier, regen --check, iter-clippy, full nextest.
set -u
n=$1; L=$HOME/.dev-logs/bbnf-lockstep/lw3/rx-emptyfirst/green-r$n; mkdir -p $L
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus BBNF_ORACLE_CACHE=$HOME/.dev-logs/bbnf-lockstep/lw3/s5b-oracle/oracle-cache
cd $HOME/Programming/bbnf-lang-wt/lw3-rx-emptyfirst || exit 2
echo "head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')" > $L/head.log
cargo clean -p bbnf > $L/clean.log 2>&1
cargo nextest run --cargo-profile ax-iter -p bbnf --test rx_emptyfirst --no-fail-fast > $L/falsifier.log 2>&1; echo "rc=$?" >> $L/falsifier.log
cargo xtask regen --check > $L/regen-check.log 2>&1; echo "rc=$?" >> $L/regen-check.log
cargo iter-clippy > $L/clippy.log 2>&1; echo "rc=$?" >> $L/clippy.log
cargo iter-test --no-fail-fast > $L/nextest.log 2>&1; echo "rc=$?" >> $L/nextest.log
