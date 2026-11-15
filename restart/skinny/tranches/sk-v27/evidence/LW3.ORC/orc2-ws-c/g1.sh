#!/bin/sh
# g1 read: $1 = label, rest = g1 args.
set -u
L=$HOME/.dev-logs/bbnf-lockstep/lw3/orc2-ws-c; lab=$1; shift
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus BBNF_ORACLE_CACHE=$HOME/.dev-logs/bbnf-lockstep/lw3/s5b-oracle/oracle-cache
cd $HOME/Programming/bbnf-lang-wt/lw3-s5c || exit 2
{ echo "head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')"; echo "\$ g1 $*"; } > $L/g1-$lab.log
cargo run -q -p bbnf-oracle --bin g1 -- "$@" >> $L/g1-$lab.log 2>&1; echo "rc=$?" >> $L/g1-$lab.log
