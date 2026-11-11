#!/bin/sh
# LG-2 seventh reading, local rows x2 at the final heads.
set -u
O=~/.dev-logs/bbnf-lockstep/lw2/r7; E=/Users/mkbabb/Programming/bbnf-lang-wt/sk-v27-lw2/restart/skinny/tranches/sk-v27/evidence
BL=~/Programming/bbnf-lang-wt/sk-v27-lw2; PT=~/Programming/parse-that-wt/lw2-r7-head; PPI=~/Programming/pprint-wt/sk-v27-lw2; PP=~/Programming/pprint-wt/lw2-topo-4e-0.4
export CARGO_TARGET_DIR=~/.dev-logs/bbnf-lockstep/target-shared
for n in 1 2; do
  echo "== read $n heads bl=$(git -C $BL rev-parse --short HEAD) pt=$(git -C $PT rev-parse --short HEAD) ppi=$(git -C $PPI rev-parse --short HEAD) pp=$(git -C $PP rev-parse --short HEAD)"
  sh $O/rows.sh $n $BL $PT $PPI
  (cd $PT/rust && cargo test -q -p parse-that-regex --test abi_fingerprint) > $O/r7/lg23-hir-r$n.log 2>&1; echo "LG-2.3 Hir ABI v1 rc=$? $(grep 'test result' $O/r7/lg23-hir-r$n.log | tail -1)"
  sh $E/LW2.TOPO-4e/pins.sh $PP $PT $BL > $O/r7/pins-r$n.log 2>&1; echo "TOPO-4e pins rc=$? $(tail -1 $O/r7/pins-r$n.log)"
  sh $E/LW2.TOPO-4d/semver-probe.sh r7-head-r$n 47${n}10 $PP $PT $BL > $O/r7/semver-r$n.out 2>&1; echo "semver-probe rc=$? $(tail -1 $O/r7/semver-r$n.out)"
  sh $E/LW2.TOPO-4d/topo4c.sh r7-head-r$n 47${n}20 $PP $PPI $PT $BL > $O/r7/topo4c-r$n.out 2>&1; echo "topo4c rc=$? $(tail -1 $O/r7/topo4c-r$n.out)"
done
