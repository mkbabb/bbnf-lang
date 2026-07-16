#!/bin/zsh
set -e
PARENT=/private/tmp/claude-504/-Users-mkbabb-Programming-bbnf-lang/67d0543b-e7c7-48da-9c77-0a9635fd4b0d/scratchpad/PROVE-11-work
cd "$PARENT/typescript"
PRI="$PARENT/dist-pristine/parse.js"
MOD="$PARENT/dist-modified/parse.js"
OUT="$PARENT/paired-samples.jsonl"
: > "$OUT"
ROUNDS=${1:-10}
for r in $(seq 1 $ROUNDS); do
  la=$(sysctl -n vm.loadavg | awk '{print $2}')
  P=$(node --expose-gc scripts/measure-prove11.mjs "$PRI")
  M=$(node --expose-gc scripts/measure-prove11.mjs "$MOD")
  echo "{\"round\":$r,\"loadavg\":\"$la\",\"pristine\":$P,\"modified\":$M}" >> "$OUT"
  echo "round $r done (loadavg $la)"
done
echo "ALL ROUNDS DONE"
