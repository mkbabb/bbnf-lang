#!/bin/sh
# ORC2-WS-C RED read n: the born-RED falsifier under --include-ignored at the seat's base, after cargo clean -p bbnf (TGT-1).
set -u
n=$1; L=$HOME/.dev-logs/bbnf-lockstep/lw3/orc2-ws-c
export CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus
cd $HOME/Programming/bbnf-lang-wt/lw3-s5c || exit 2
echo "head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')" > $L/red-r$n.log
cargo clean -p bbnf >> $L/red-r$n.log 2>&1
cargo nextest run --cargo-profile ax-iter -p bbnf --test rb_root_whitespace --run-ignored all --no-fail-fast >> $L/red-r$n.log 2>&1; echo "rc=$?" >> $L/red-r$n.log
