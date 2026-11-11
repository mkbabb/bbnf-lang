#!/bin/sh
# sk-v27 LW2 LG-2 fifth reading, critic O-1: would the publish set's pprint 0.3.7 / pprint_derive 0.2.3 break
# crates.io consumers that already exist? Each probe is a throwaway crate (never a repository manifest) that
# depends on one published reverse dependency, pinned exactly at its newest non-yanked version, whose
# manifest asks for `pprint = "0.3"`; the probe's own [patch.crates-io] substitutes the 0.3-line tree the
# gate would publish as pprint 0.3.7 (+ pprint_derive 0.2.3), which is what a fresh resolve would pick once
# 0.3.7 is on crates.io. Each probe runs twice: a control without the patch (the consumer against the pprint
# crates.io holds today) and the substituted build. `cargo +nightly check`, since the published consumers'
# own dependency tree (smallbox's `nightly` feature) does not build on stable. A probe whose control fails
# is VOID (it cannot discriminate). GREEN when every substituted build passes; RED when a consumer whose
# control passes breaks under the substitution.
# Usage: semver-probe.sh <tag> <pprint 0.3-line tree>
set -u
TAG=$1; PP03=$2/rust
L=~/.dev-logs/bbnf-lockstep/lw2/r5/topo-4c/semver-$TAG; rm -rf $L; mkdir -p $L
export CARGO_TARGET_DIR=~/.dev-logs/bbnf-lockstep/target-shared
V=$(cd $PP03 && cargo metadata --offline --no-deps --format-version 1 2>/dev/null | python3 -I -c "import sys,json;print(' '.join(p['name']+'='+p['version'] for p in json.load(sys.stdin)['packages']))")
echo "substituted: $V ($(git -C $PP03 rev-parse --short HEAD))" > $L/result.tsv; BAD=0; VOID=0
for spec in "bbnf =0.2.9" "gorgeous =0.1.8" "bbnf-analysis =0.1.1"; do
  set -- $spec; C=$L/$1; mkdir -p $C/src
  K=$L/$1-control; mkdir -p $K/src
  printf '[package]\nname = "semver-probe"\nversion = "0.0.0"\nedition = "2021"\npublish = false\n\n[dependencies]\n%s = "%s"\n\n[workspace]\n' $1 $2 > $K/Cargo.toml
  printf 'fn main() {}\n' > $K/src/main.rs
  (cd $K && cargo +nightly check) > $L/$1-control.log 2>&1; krc=$?
  kpicked=$(grep -h -A1 'name = "pprint"' $K/Cargo.lock 2>/dev/null | grep version | tr -d ' "' | tr '\n' ' ')
  printf "%s %s control\trc=%s\tpprint %s\n" "$1" "$2" "$krc" "$kpicked" >> $L/result.tsv
  printf '[package]\nname = "semver-probe"\nversion = "0.0.0"\nedition = "2021"\npublish = false\n\n[dependencies]\n%s = "%s"\n\n[patch.crates-io]\npprint = { path = "%s" }\npprint_derive = { path = "%s/derive" }\n\n[workspace]\n' $1 $2 $PP03 $PP03 > $C/Cargo.toml
  printf 'fn main() {}\n' > $C/src/main.rs
  (cd $C && cargo +nightly check) > $L/$1.log 2>&1; rc=$?
  picked=$(grep -h -A1 'name = "pprint"' $C/Cargo.lock 2>/dev/null | grep version | tr -d ' "' | tr '\n' ' ')
  nerr=$(grep -c '^error' $L/$1.log)
  printf "%s %s substituted\trc=%s\tpprint %s\terrors=%s\n" "$1" "$2" "$rc" "$picked" "$nerr" >> $L/result.tsv
  if [ $krc -ne 0 ]; then VOID=$((VOID+1)); elif [ $rc -ne 0 ]; then BAD=$((BAD+1)); fi
done
if [ $VOID -gt 0 ]; then echo "semver-probe $TAG: VOID ($VOID controls fail)" | tee -a $L/result.tsv; exit 2
elif [ $BAD -eq 0 ]; then echo "semver-probe $TAG: GREEN" | tee -a $L/result.tsv; exit 0
else echo "semver-probe $TAG: RED ($BAD of 3 existing consumers break)" | tee -a $L/result.tsv; exit 1; fi
