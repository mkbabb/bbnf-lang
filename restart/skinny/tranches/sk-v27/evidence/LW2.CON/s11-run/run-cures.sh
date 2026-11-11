#!/bin/bash
# One X-7 load-wait budget (60 min) shared across the six cure cells.
set -euo pipefail
L=~/.dev-logs/bbnf-lockstep/lw2/s11-bench
until [ $(wc -l < $L/perf3-digest.txt 2>/dev/null || echo 0) -ge 6 ]; do sleep 10; done
export CBB0_WAIT_DEADLINE=$(( $(date +%s) + 2400 ))
$L/run-cure.sh perf3
$L/run-cure.sh rx4
echo ALL-DONE
