#!/usr/bin/env bash
# LW2.TOPO-0 falsifier: (1) heads-LW2.json exists and every SHA cited in the
# order, RULINGS and LEDGER resolves against it (topo1_check.py, 0 unresolved;
# the falsifier's own mutant tokens must still be refused);
# (2) sk-v27-topo, sk-v27-lw1-gate, sk-v27 and lockstep/merge-skv25 are each
# ancestors of bbnf-lang origin/sk-v27-lw2 (x4); (3) parse-that and pprint
# origin/sk-v27-lw2 exist on their stated bases.
# usage: topo0_lw2_check.sh <tranche-dir> <heads-LW2.json>
set -u
TR=$1; HEADS=$2; BL=~/Programming/bbnf-lang; rc=0
python3 "${TOPO1:-$TR/topo/topo1_check.py}" "$HEADS" --mutants "$TR/evidence/LW2.TOPO/topo0-mutant.log" \
  "$TR/LW2-ORDER.md" "$TR/RULINGS.md" "$TR/LEDGER.md" || rc=1
git -C $BL fetch -q origin
for r in sk-v27-topo sk-v27-lw1-gate sk-v27 lockstep/merge-skv25; do
  if git -C $BL merge-base --is-ancestor origin/$r origin/sk-v27-lw2 2>/dev/null; then echo "ancestor $r: true"; else echo "ancestor $r: FALSE"; rc=1; fi
done
for p in "parse-that 0f6c598" "pprint cf83853"; do set -- $p
  git -C ~/Programming/$1 fetch -q origin
  if git -C ~/Programming/$1 merge-base --is-ancestor $2 origin/sk-v27-lw2 2>/dev/null; then echo "$1 sk-v27-lw2 on $2: true"; else echo "$1 sk-v27-lw2 on $2: FALSE"; rc=1; fi
done
echo "rc=$rc"; exit $rc
