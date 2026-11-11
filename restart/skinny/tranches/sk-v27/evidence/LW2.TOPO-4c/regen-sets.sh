#!/bin/sh
# sk-v27 LW2.TOPO-4c falsifier (critic O-3, and the O-2 copy): `cargo xtask regen --check` holds both
# in-crate copy directories as sets. One read over a bbnf-lang tree, each mutant applied and removed in turn:
#   clean                                   -> rc 0
#   M1 orphan  crates/core/grammar/orphan.bbnf          (no manifest grammar's copy; ships in `bbnf`)     -> rc != 0
#   M2 orphan  crates/bbnf-path/registry/orphan.registry.json  (no listed sidecar; ships in `bbnf-path`) -> rc != 0
#   M3 drift   crates/bbnf-path/registry/json.registry.json, one byte appended (when the copy exists,
#              else it is created, which is M2's case again)                                          -> rc != 0
# GREEN when all four hold. The tree is left as found (each mutant is removed or restored; `git status` checked).
# Usage: regen-sets.sh <tag> <bbnf-lang tree>
set -u
TAG=$1; BL=$2
L=~/.dev-logs/bbnf-lockstep/lw2/r5/topo-4c/regen-$TAG; rm -rf $L; mkdir -p $L
export CARGO_TARGET_DIR=~/.dev-logs/bbnf-lockstep/target-shared
cd $BL || exit 2
before=$(git status --porcelain)
chk() { cargo xtask regen --check > $L/$1.log 2>&1; rc=$?; printf "%s\trc=%s\n" "$1" "$rc" >> $L/result.tsv; return $rc; }
: > $L/result.tsv
chk clean; C=$?
printf 'orphan = "x" ;\n' > crates/core/grammar/orphan.bbnf; chk m1-core-orphan; M1=$?; rm -f crates/core/grammar/orphan.bbnf
mkdir -p crates/bbnf-path/registry
printf '{}\n' > crates/bbnf-path/registry/orphan.registry.json; chk m2-path-orphan; M2=$?; rm -f crates/bbnf-path/registry/orphan.registry.json
J=crates/bbnf-path/registry/json.registry.json
if [ -f $J ]; then cp $J $L/json.bak; printf ' ' >> $J; chk m3-path-drift; M3=$?; cp $L/json.bak $J
else printf '{}\n' > $J; chk m3-path-drift; M3=$?; rm -f $J; fi
rmdir crates/bbnf-path/registry 2>/dev/null
after=$(git status --porcelain)
[ "$before" = "$after" ]; T=$?; printf "tree-unchanged\t%s\n" "$([ $T -eq 0 ] && echo yes || echo NO)" >> $L/result.tsv
if [ $C -eq 0 ] && [ $M1 -ne 0 ] && [ $M2 -ne 0 ] && [ $M3 -ne 0 ] && [ $T -eq 0 ]; then echo "regen-sets $TAG: GREEN" | tee -a $L/result.tsv; exit 0
else echo "regen-sets $TAG: RED" | tee -a $L/result.tsv; exit 1; fi
