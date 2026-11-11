#!/bin/sh
# LG-2 eighth reading, light rows, one read. Usage: rows.sh <n> <bbnf-lang tree> <parse-that tree> <pprint tree>
set -u
N=$1; BL=$2; PT=$3; PP=$4
O=~/.dev-logs/bbnf-lockstep/lw2/r8/r8; mkdir -p $O
for spec in "bbnf-lang $BL" "parse-that $PT" "pprint $PP"; do set -- $spec
  (cd $2 && sh scripts/check-repo-dag.sh) > $O/lg21-dag-$1-r$N.log 2>&1; echo "LG-2.1 $1 rc=$?"
  echo "  gitmodules=$( [ -f $2/.gitmodules ] && echo present || echo absent ) gitlinks=$(git -C $2 ls-files -s | awk '$1==160000' | wc -l | tr -d ' ')"
done
for r in bbnf-lang parse-that pprint; do git -C ~/Programming/$r fetch -q origin; printf "%s r%s af15f63e0-in-master=" $r $N; git -C ~/Programming/$r merge-base --is-ancestor af15f63e0 origin/master 2>/dev/null; echo $?; done > $O/lg22-ancestor-r$N.log
echo "LG-2.2 ancestor: $(tr '\n' ' ' < $O/lg22-ancestor-r$N.log)"
(cd $PP/rust && shasum -a 256 -c ABI-v2.sha256) > $O/lg23-opabi-r$N.log 2>&1; echo "LG-2.3 op.rs ABI v2 rc=$?"
(cd $BL/bench && python3 -I -m pytest -q test_decision_plan.py test_prereg.py) > $O/lg24-r$N.log 2>&1; echo "LG-2.4 plan rc=$? $(tail -1 $O/lg24-r$N.log)"
(cd ~/.dev-logs/bbnf-lockstep/custody && shasum -a 256 -c MANIFEST.sha256) > $O/lg25-custody-r$N.log 2>&1; echo "LG-2.5 custody rc=$? ok=$(grep -c ': OK' $O/lg25-custody-r$N.log)"
for p in "bbnf-lang sk-lineage/local-master af15f63e0" "pprint sk-lineage/pprint-local 2b4d2d486"; do set -- $p
  got=$(git -C ~/Programming/$1 ls-remote origin refs/heads/$2 | cut -c1-9); echo "  preserve $1 $2 $([ "$got" = "$3" ] && echo SAME || echo "MOVED $got")"; done
for spec in "bbnf-lang $BL" "parse-that $PT" "pprint $PP"; do set -- $spec
  printf "%s patch: %s\n" $1 "$(cd $2 && git ls-files '*Cargo.toml' '*.cargo/config.toml' | xargs grep -l '^\[patch' 2>/dev/null | tr '\n' ' ')"; done > $O/lga-patch-r$N.log
echo "LG-a: $(tr '\n' ';' < $O/lga-patch-r$N.log)"
