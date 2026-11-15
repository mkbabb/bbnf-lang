#!/bin/sh
# RX-EMPTYFIRST merge-gate bench (RULINGS 2026-11-14j/k: the PERF-1c paired protocol): build one tree's bench
# executables (css_l4, google_sheets_monolithic, json_monolithic as control) after the shared clean of bbnf in
# the bench profile, and copy them out, so both reads run the same executables. Usage: build-bins.sh <worktree> <base|cure>
set -u
W=$1; V=$2; L=$HOME/.dev-logs/bbnf-lockstep/lw3/rx-emptyfirst/bench; mkdir -p $L/bins
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus
S=$HOME/Programming/bbnf-lang-wt/lw3-rx-emptyfirst/restart/skinny/tranches/sk-v27/evidence/LW3.AUDIT/tgt-clean.sh
cd $W || exit 2
echo "$V head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')" >> $L/bins/heads.txt
sh $S clean $W bench bbnf >> $L/bins/heads.txt 2>&1
cargo bench -p bbnf --locked --bench css_l4 --bench google_sheets_monolithic --bench json_monolithic --no-run --message-format=json 2>$L/bins/build-$V.err \
 | python3 -I -c 'import json,sys
for l in sys.stdin:
    m=json.loads(l)
    if m.get("reason")=="compiler-artifact" and m.get("executable") and m["target"]["name"] in ("css_l4","google_sheets_monolithic","json_monolithic"): print(m["target"]["name"], m["executable"])' > $L/bins/built-$V.txt
: > $L/bins/bins-$V.txt
while read name exe; do cp "$exe" $L/bins/$name-$V && echo "$name $L/bins/$name-$V" >> $L/bins/bins-$V.txt; done < $L/bins/built-$V.txt
shasum -a 256 $L/bins/*-$V >> $L/bins/heads.txt
