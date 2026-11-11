#!/bin/bash
# S11 BENCH: F-CURE-PERF3 and F-CURE-RX4 cells, 30 invocations x 4 ABBA blocks (240 pairs) each.
#   run-cure.sh perf3|rx4
set -euo pipefail
R=~/Programming/bbnf-lang-wt/lw2-s11-bench
D=$R/docs/benchmarks/records/LW2-CON-2
B=~/.dev-logs/bbnf-lockstep/s11-arms/bin
cd $R
cell() { # cell-id parent-bin cure-bin kind file K
  : "${CBB0_WAIT_DEADLINE:=$(( $(date +%s) + 3600 ))}"; export CBB0_WAIT_DEADLINE
  for i in $(seq 1 30); do
    python3 bench/cb-b0/pairs.py run --cell $1 --out $D/$1 --blocks 4 --a "$3 $4 $5 $6" --b "$2 $4 $5 $6" \
      --note "arm a = cure, arm b = parent; r = ln(t_cure / t_parent); K=$6 parses per process" >/dev/null
  done
  echo "$1 done"
}
case $1 in
perf3)
  P=~/.dev-logs/bbnf-lockstep/corpus-pt-v1/data
  cell C-PERF3-CSS-BOOTSTRAP $B/perf3-ad0dc8a $B/perf3-92ff470 css $P/css/bootstrap.css 300
  cell C-PERF3-CSS-TAILWIND  $B/perf3-ad0dc8a $B/perf3-92ff470 css $P/css/tailwind-output.css 30
  cell C-PERF3-JSON-TWITTER  $B/perf3-ad0dc8a $B/perf3-92ff470 json $P/json/twitter.json 300 ;;
rx4)
  C=~/.dev-logs/bbnf-lockstep/corpus-v1/data
  cell C-RX4-CSSL4-BOOTSTRAP  $B/rx4-63054dba0 $B/rx4-88b60839f css_l4 $C/css/bootstrap.css 10
  cell C-RX4-CSSL4-TAILWIND   $B/rx4-63054dba0 $B/rx4-88b60839f css_l4 $C/css/tailwind.css 1
  cell C-RX4-PRETTY-BOOTSTRAP $B/rx4-63054dba0 $B/rx4-88b60839f css_pretty $C/css/bootstrap.css 20 ;;
esac
