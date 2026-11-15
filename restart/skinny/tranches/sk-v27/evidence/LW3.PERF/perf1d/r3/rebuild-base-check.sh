#!/bin/sh
# Diagnostic: rebuild base 785027c44's bench binaries in target-base with this seat's env; compare with the ninth seat's copies.
set -u
L=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/r3; mkdir -p $L/rebuild
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/target-base
cd $HOME/Programming/bbnf-lang-wt/lw3-s9b-base || exit 2
cargo bench -p bbnf --locked --bench json_monolithic --bench css_l4 --no-run --message-format=json 2>$L/rebuild/build.err \
 | python3 -I -c 'import json,sys
for l in sys.stdin:
    m=json.loads(l)
    if m.get("reason")=="compiler-artifact" and m.get("executable") and m["target"]["name"] in ("json_monolithic","css_l4"): print(m["target"]["name"], m["executable"])' > $L/rebuild/built.txt
while read name exe; do cp "$exe" $L/rebuild/$name-base-rebuilt; done < $L/rebuild/built.txt
shasum -a 256 $L/rebuild/*-base-rebuilt $L/bins/*-base > $L/rebuild/sha.txt
