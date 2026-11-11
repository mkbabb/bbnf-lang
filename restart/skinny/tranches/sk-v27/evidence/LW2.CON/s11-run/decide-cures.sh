#!/bin/bash
set -e
cd ~/Programming/bbnf-lang-wt/lw2-s11-bench/docs/benchmarks/records/LW2-CON-2
DC=../../../../bench/cb-b0/decide.py
for i in 1 2; do
  python3 $DC family C-PERF3-CSS-BOOTSTRAP C-PERF3-CSS-TAILWIND C-PERF3-JSON-TWITTER > F-CURE-PERF3-decision-read$i.json
  python3 $DC family C-PERF3-CSS-BOOTSTRAP C-PERF3-CSS-TAILWIND C-PERF3-JSON-TWITTER --quiet-only > F-CURE-PERF3-decision-quiet-read$i.json
  python3 $DC family C-RX4-CSSL4-BOOTSTRAP C-RX4-CSSL4-TAILWIND C-RX4-PRETTY-BOOTSTRAP > F-CURE-RX4-decision-read$i.json
  python3 $DC family C-RX4-CSSL4-BOOTSTRAP C-RX4-CSSL4-TAILWIND C-RX4-PRETTY-BOOTSTRAP --quiet-only > F-CURE-RX4-decision-quiet-read$i.json
done
for f in F-CURE-PERF3-decision F-CURE-PERF3-decision-quiet F-CURE-RX4-decision F-CURE-RX4-decision-quiet; do cmp $f-read1.json $f-read2.json && echo "$f identical x2"; done
