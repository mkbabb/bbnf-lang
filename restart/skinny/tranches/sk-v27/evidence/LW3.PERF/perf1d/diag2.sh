#!/bin/sh
# PERF-1d attribution 2 (diagnostic only, never committed): cure + a [patch] to local parse-that with
# #[inline(always)] on prim::neon::byte_class_from_eq_set_64 (diag2) vs cure vs base, interleaved x3.
set -u
L=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/bench; D=$HOME/Programming/bbnf-lang-wt/lw3-s9b-diag2
export BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus
cd $D && CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/target-diag2 cargo bench -p bbnf --bench json_monolithic --no-run --message-format=json 2>$L/build-diag2.err \
 | python3 -I -c 'import json,sys
for l in sys.stdin:
    m=json.loads(l)
    if m.get("reason")=="compiler-artifact" and m.get("executable") and m["target"]["name"]=="json_monolithic": print(m["executable"])' > $L/bin-diag2.txt
DX=$(cat $L/bin-diag2.txt); BX=$(awk '$1=="json_monolithic"{print $2}' $L/bins-base.txt); CX=$(awk '$1=="json_monolithic"{print $2}' $L/bins-cure.txt)
for r in 7 8 9; do
  (cd $HOME/Programming/bbnf-lang-wt/lw3-s9b-base/crates/core && $BX --bench > $L/json_monolithic-base-r$r.txt 2>&1)
  (cd $HOME/Programming/bbnf-lang-wt/lw3-s9b/crates/core && $CX --bench > $L/json_monolithic-cure-r$r.txt 2>&1)
  (cd $D/crates/core && $DX --bench > $L/diag2/json_monolithic-diag2-r$r.txt 2>&1)
done
