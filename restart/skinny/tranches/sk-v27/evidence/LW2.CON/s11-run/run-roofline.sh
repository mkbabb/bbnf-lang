#!/bin/bash
set -euo pipefail
R=~/Programming/bbnf-lang-wt/lw2-s11-bench; D=$R/docs/benchmarks/records/LW2-CON-2/roofline; mkdir -p $D
clang -O3 -o ~/.dev-logs/bbnf-lockstep/s11-arms/bin/membw $R/bench/roofline/membw.c
for i in 1 2; do echo "load1=$(sysctl -n vm.loadavg) $(~/.dev-logs/bbnf-lockstep/s11-arms/bin/membw 9)" > $D/membw-read$i.txt; done
cd ~/Programming/parse-that-wt/sk-v27-lw2/rust
export PARSE_THAT_CORPUS_DIR=~/.dev-logs/bbnf-lockstep/corpus-pt-v1/data CARGO_TARGET_DIR=~/.dev-logs/bbnf-lockstep/target-parse-that
for i in 1 2; do echo "# parse-that $(git rev-parse --short HEAD) load1=$(sysctl -n vm.loadavg)" > $D/stage1-neon-read$i.txt; cargo bench -q -p parse-that-simd --bench stage1_throughput >> $D/stage1-neon-read$i.txt 2>&1; done
echo ROOF-DONE
