#!/bin/sh
# Build the cure's bench binaries (bench profile) once and copy them out, so both gate reads run the same executables.
set -u
L=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/r3; mkdir -p $L/bins
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
cd $HOME/Programming/bbnf-lang-wt/lw3-s9b || exit 2
echo "cure head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')" > $L/bins/heads.txt
cargo bench -p bbnf --locked --bench json_monolithic --bench css_l4 --no-run --message-format=json 2>$L/bins/build-cure.err \
 | python3 -I -c 'import json,sys
for l in sys.stdin:
    m=json.loads(l)
    if m.get("reason")=="compiler-artifact" and m.get("executable") and m["target"]["name"] in ("json_monolithic","css_l4"): print(m["target"]["name"], m["executable"])' > $L/bins/built-cure.txt
: > $L/bins/bins-cure.txt
while read name exe; do cp "$exe" $L/bins/$name-cure && echo "$name $L/bins/$name-cure" >> $L/bins/bins-cure.txt; done < $L/bins/built-cure.txt
B=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/bench/bins-base.txt; : > $L/bins/bins-base.txt
while read name exe; do cp "$exe" $L/bins/$name-base && echo "$name $L/bins/$name-base" >> $L/bins/bins-base.txt; done < $B
echo "base head=785027c44 (copied from $B)" >> $L/bins/heads.txt
shasum -a 256 $L/bins/*-base $L/bins/*-cure >> $L/bins/heads.txt
