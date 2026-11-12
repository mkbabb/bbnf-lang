#!/usr/bin/env bash
# LW3.TOPO-0 falsifier (LW3-ORDER §3 "ENTRY and CI", adapted from the LW1.TOPO-1
# checker as topo0_lw2_check.sh was):
# (1) heads-LW3.json exists and every SHA cited in LW3-ORDER.md, RULINGS.md and
#     the LEDGER rows from the LG-2 eighth-reading row onward resolves against it
#     (topo1_check.py: 0 unresolved; tokens a committed mutant log lists as
#     refused must still be refused);
# (2) in each of bbnf-lang, parse-that and pprint, origin/master is an ancestor
#     of origin/sk-v27-lw3 (x3), and bbnf-lang's sk-v27-lw3 carries the order
#     branch origin/sk-v27-lw3-order (§1: "BL also takes an ordinary merge").
# usage: topo0_lw3_check.sh <tranche-dir> <heads-LW3.json> [extra-record ...]
#   An extra record is read as one more cited-SHA source (the mutant uses it).
set -u
TR=$1; HEADS=$2; shift 2; P=~/Programming; rc=0
SCR=${TOPO0_SCRATCH:-$HOME/.dev-logs/bbnf-lockstep/lw3/s1-entry}; mkdir -p "$SCR"
TAIL="$SCR/ledger-from-lg2-r8.md"
# The LEDGER slice: from the LG-2 eighth-reading row to the end of the file.
awk '/^\| 2026-11-11 \| \*\*LG-2 gate, eighth reading/{on=1} on' "$TR/LEDGER.md" > "$TAIL"
if [ ! -s "$TAIL" ]; then echo "RED"; echo "  LEDGER has no LG-2 eighth-reading row"; rc=1; fi
MUT=()
[ -f "$TR/evidence/LW3.TOPO/topo0-mutant-1.log" ] && MUT=(--mutants "$TR/evidence/LW3.TOPO/topo0-mutant-1.log")
python3 "${TOPO1:-$TR/topo/topo1_check.py}" "$HEADS" ${MUT[@]+"${MUT[@]}"} \
  "$TR/LW3-ORDER.md" "$TR/RULINGS.md" "$TAIL" "$@" || rc=1
for r in pprint parse-that bbnf-lang; do
  git -C $P/$r fetch -q origin
  if git -C $P/$r rev-parse -q --verify origin/sk-v27-lw3 >/dev/null &&
     git -C $P/$r merge-base --is-ancestor origin/master origin/sk-v27-lw3; then
    echo "$r: origin/master ($(git -C $P/$r rev-parse --short=9 origin/master)) ancestor of sk-v27-lw3 ($(git -C $P/$r rev-parse --short=9 origin/sk-v27-lw3)): true"
  else echo "$r: origin/master ancestor of sk-v27-lw3: FALSE"; rc=1; fi
done
if git -C $P/bbnf-lang rev-parse -q --verify origin/sk-v27-lw3 >/dev/null &&
   git -C $P/bbnf-lang merge-base --is-ancestor origin/sk-v27-lw3-order origin/sk-v27-lw3; then
  echo "bbnf-lang: sk-v27-lw3-order ancestor of sk-v27-lw3: true"
else echo "bbnf-lang: sk-v27-lw3-order ancestor of sk-v27-lw3: FALSE"; rc=1; fi
echo "rc=$rc"; exit $rc
