#!/bin/sh
# PERF-1d bisect under RULINGS 2026-11-14e (diagnostic trees, never committed; parse-that by local [patch]):
#   base = 785027c44 (emitted NEON kernels, pin 6889b29), binaries from bench/bins-base.txt
#   Q    = cure + quote scan routed to parse_that::scan::find_quote_or_backslash (whitespace via byte_class_from_eq_set_64)
#   W    = Q + the emitted NEON whitespace bitmap restored
# json_monolithic and css_l4, 10 rounds interleaved base Q W.
set -u
L=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/bisect; B=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/bench
export BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus
for v in Q W; do
  cd $HOME/Programming/bbnf-lang-wt/lw3-s9b-diag$v || exit 2
  CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/target-diag$v cargo bench -p bbnf --bench json_monolithic --bench css_l4 --no-run --message-format=json 2>$L/build-$v.err \
   | python3 -I -c 'import json,sys
for l in sys.stdin:
    m=json.loads(l)
    if m.get("reason")=="compiler-artifact" and m.get("executable") and m["target"]["name"] in ("json_monolithic","css_l4"): print(m["target"]["name"], m["executable"])' > $L/bins-$v.txt
done
cp $B/bins-base.txt $L/bins-base.txt
for r in 01 02 03 04 05 06 07 08 09 10; do
  for v in base Q W; do
    while read name exe; do "$exe" --bench > $L/$name-$v-r$r.txt 2>&1; done < $L/bins-$v.txt
  done
done
