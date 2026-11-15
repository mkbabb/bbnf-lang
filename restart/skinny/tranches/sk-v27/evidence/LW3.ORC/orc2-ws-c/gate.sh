#!/bin/sh
# ORC2-WS-C gate read n at the lane: clean bbnf, regen --check, falsifier, iter-clippy, full nextest, g1 root on ebnf,bnf,math.
set -u
n=$1; L=$HOME/.dev-logs/bbnf-lockstep/lw3/orc2-ws-c/green-r$n; mkdir -p $L
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus BBNF_ORACLE_CACHE=$HOME/.dev-logs/bbnf-lockstep/lw3/s5b-oracle/oracle-cache
cd $HOME/Programming/bbnf-lang-wt/lw3-s5c || exit 2
echo "head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')" > $L/head.log
cargo clean -p bbnf > $L/clean.log 2>&1
cargo xtask regen --check > $L/regen-check.log 2>&1; echo "rc=$?" >> $L/regen-check.log
cargo nextest run --cargo-profile ax-iter -p bbnf --test rb_root_whitespace --run-ignored all --no-fail-fast > $L/falsifier.log 2>&1; echo "rc=$?" >> $L/falsifier.log
cargo iter-clippy > $L/clippy.log 2>&1; echo "rc=$?" >> $L/clippy.log
cargo iter-test --no-fail-fast > $L/nextest.log 2>&1; echo "rc=$?" >> $L/nextest.log
cargo run -q -p bbnf-oracle --bin g1 -- --grammar ebnf,bnf,math --arms root --manifest grammar/MANIFEST > $L/g1.log 2>&1; echo "rc=$?" >> $L/g1.log
