#!/bin/sh
# window.sh <start-epoch> <log>: foreign guarded runs in PT worktrees since start (collision receipt)
awk -v t=$1 -F'\t' '$3>=t && $0 ~ /parse-that/ && $0 !~ /lw3-s6-kernel|s6-kernel\//' ~/.claude/guard/log/zone-runs.tsv > $2
echo "# foreign PT runs in window: $(wc -l < $2 | tr -d ' ')" >> $2
