#!/bin/sh
# LW3.RX-EMPTYFIRST read: kind (red|green|mutant) n; `cargo clean -p bbnf` (TGT-1), then the falsifier.
set -u
k=$1; n=$2; L=$HOME/.dev-logs/bbnf-lockstep/lw3/rx-emptyfirst; W=${3:-$HOME/Programming/bbnf-lang-wt/lw3-rx-emptyfirst}
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
cd $W || exit 2
echo "head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')" > $L/$k-r$n.log
cargo clean -p bbnf >> $L/$k-r$n.log 2>&1
cargo nextest run --cargo-profile ax-iter -p bbnf --test rx_emptyfirst --no-fail-fast >> $L/$k-r$n.log 2>&1; echo "rc=$?" >> $L/$k-r$n.log
