#!/bin/sh
set -u
L=$HOME/.dev-logs/bbnf-lockstep/lw3/orc2-ws-c
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus
cd $HOME/Programming/bbnf-lang-wt/lw3-s5c || exit 2
cargo xtask regen > $L/regen.log 2>&1; echo "rc=$?" >> $L/regen.log
cargo nextest run --cargo-profile ax-iter -p bbnf --test rb_root_whitespace --no-fail-fast > $L/falsifier-try.log 2>&1; echo "rc=$?" >> $L/falsifier-try.log
