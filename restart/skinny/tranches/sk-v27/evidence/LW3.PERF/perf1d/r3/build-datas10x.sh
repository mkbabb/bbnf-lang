#!/bin/sh
# Build the data_s 10x example in the base tree (target-base) and the cure lane (target-bbnf-lang); copy both out; remove the copied source.
set -u
L=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/r3
export CARGO_INCREMENTAL=0
for v in base cure; do
  case $v in base) W=$HOME/Programming/bbnf-lang-wt/lw3-s9b-base; T=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/target-base;;
             cure) W=$HOME/Programming/bbnf-lang-wt/lw3-s9b; T=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang;; esac
  cp $L/perf1d_datas10x.rs $W/crates/core/examples/perf1d_datas10x.rs
  (cd $W && CARGO_TARGET_DIR=$T cargo build -p bbnf --locked --profile bench --example perf1d_datas10x > $L/bins/build-datas10x-$v.log 2>&1; echo "rc=$?" >> $L/bins/build-datas10x-$v.log)
  cp $T/release/examples/perf1d_datas10x $L/bins/datas10x-$v
  rm $W/crates/core/examples/perf1d_datas10x.rs
  echo "datas10x-$v head=$(cd $W && git rev-parse HEAD) $(shasum -a 256 $L/bins/datas10x-$v)" >> $L/bins/heads.txt
done
