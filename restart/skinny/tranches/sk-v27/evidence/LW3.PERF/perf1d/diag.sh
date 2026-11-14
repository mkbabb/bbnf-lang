#!/bin/sh
# PERF-1d attribution (diagnostic only, never committed): cure with json.rs's emitted NEON
# first_quote_or_backslash restored (diag) vs cure vs base, json_monolithic, interleaved x3.
set -u
L=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/bench; D=$HOME/Programming/bbnf-lang-wt/lw3-s9b-diag
export BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus
cd $D && CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/target-diag cargo bench -p bbnf --locked --bench json_monolithic --no-run --message-format=json 2>$L/build-diag.err \
 | python3 -I -c 'import json,sys
for l in sys.stdin:
    m=json.loads(l)
    if m.get("reason")=="compiler-artifact" and m.get("executable") and m["target"]["name"]=="json_monolithic": print(m["executable"])' > $L/bin-diag.txt
DX=$(cat $L/bin-diag.txt); BX=$(awk '$1=="json_monolithic"{print $2}' $L/bins-base.txt); CX=$(awk '$1=="json_monolithic"{print $2}' $L/bins-cure.txt)
for r in 4 5 6; do
  (cd $HOME/Programming/bbnf-lang-wt/lw3-s9b-base/crates/core && $BX --bench > $L/json_monolithic-base-r$r.txt 2>&1)
  (cd $HOME/Programming/bbnf-lang-wt/lw3-s9b/crates/core && $CX --bench > $L/json_monolithic-cure-r$r.txt 2>&1)
  (cd $D/crates/core && $DX --bench > $L/diag/json_monolithic-diag-r$r.txt 2>&1)
done
