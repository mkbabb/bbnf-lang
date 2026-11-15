#!/bin/sh
# PERF-1d merge gate (RULINGS 2026-11-14e): base (785027c44, binaries in target-base) vs the cure lane head,
# json_monolithic and css_l4, 12 rounds interleaved, minima compared.
set -u
L=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/gatebench; mkdir -p $L; B=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/bench
export BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus
cd $HOME/Programming/bbnf-lang-wt/lw3-s9b || exit 2
echo "cure head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')" > $L/heads.txt
CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang cargo bench -p bbnf --locked --bench json_monolithic --bench css_l4 --no-run --message-format=json 2>$L/build-cure.err \
 | python3 -I -c 'import json,sys
for l in sys.stdin:
    m=json.loads(l)
    if m.get("reason")=="compiler-artifact" and m.get("executable") and m["target"]["name"] in ("json_monolithic","css_l4"): print(m["target"]["name"], m["executable"])' > $L/bins-cure.txt
cp $B/bins-base.txt $L/bins-base.txt; echo "base head=785027c44 (binaries $B/bins-base.txt)" >> $L/heads.txt
for r in 01 02 03 04 05 06 07 08 09 10 11 12; do
  for v in base cure; do
    while read name exe; do "$exe" --bench > $L/$name-$v-r$r.txt 2>&1; done < $L/bins-$v.txt
  done
done
