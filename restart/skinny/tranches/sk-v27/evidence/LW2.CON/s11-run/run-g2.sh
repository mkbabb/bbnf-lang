#!/bin/bash
# S11 BENCH: C-G2-AA then C-G2-SLOW (F-JS), 30 invocations x 4 ABBA blocks each.
set -euo pipefail
R=~/Programming/bbnf-lang-wt/lw2-s11-bench
D=$R/docs/benchmarks/records/LW2-CON-2
T=~/.dev-logs/bbnf-lockstep/corpus-v1/data/json/twitter.json
BP=$R/bench/contest/build-planes/node-default.json
ARM="node $R/bench/contest/arms/node_json_parse.mjs --time 50 $T"
cd $R
export CBB0_WAIT_DEADLINE=$(( $(date +%s) + 3600 ))
for i in $(seq 1 30); do
  python3 bench/cb-b0/pairs.py run --cell C-G2-AA --out $D/C-G2-AA --blocks 4 --a "$ARM" --b "$ARM" \
    --build-a $BP --build-b $BP --note "JSON.parse self vs self, K=50 parses per process" >/dev/null
done
MED=$(python3 - <<PY
import glob,json,statistics
w=[p[s]["wall_ns"] for f in glob.glob("$D/C-G2-AA/*.json") for b in json.load(open(f))["blocks"] for p in b["pairs"] for s in ("a","b")]
print(int(0.15*statistics.median(w)))
PY
)
echo "slow_ns=$MED"
export CBB0_WAIT_DEADLINE=$(( $(date +%s) + 3600 ))
for i in $(seq 1 30); do
  python3 bench/cb-b0/pairs.py run --cell C-G2-SLOW --out $D/C-G2-SLOW --blocks 4 --a "$ARM" --b "$ARM" \
    --slow-a-ns $MED --build-a $BP --build-b $BP --note "arm A slowed by the runner: 15% of the C-G2-AA median wall" >/dev/null
done
echo done
