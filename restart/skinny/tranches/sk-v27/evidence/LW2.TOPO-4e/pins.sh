#!/bin/sh
# sk-v27 LW2.TOPO-4e falsifier (critic N-1 / ask TOPO-4-A4, after LW2.TOPO-4d): do the git pins the
# three repositories build against name the exact trees the publish set publishes from?
# Every pprint/pprint_derive pin in parse-that and bbnf-lang (each Cargo.toml and Cargo.lock: bbnf-lang
# root, skinny/, wasm/) must resolve to a commit whose git tree equals the pprint publish tree's HEAD
# tree, and every parse-that pin in bbnf-lang to the parse-that publish tree's HEAD tree. Trees, not
# commit ids: a merge commit tree-equal to the lane commit is the same source (ask TOPO-4-A5).
# Then each manifest's lockfile is read with `cargo metadata --locked` (rc 0: lock in step).
# Usage: pins.sh <pprint publish tree> <parse-that tree> <bbnf-lang tree>
# GREEN when every pin matches and every lockfile is in step; RED otherwise.
set -u
PP=$1; PT=$2; BL=$3; bad=0
PPT=$(git -C $PP rev-parse 'HEAD^{tree}'); PTT=$(git -C $PT rev-parse 'HEAD^{tree}')
echo "pprint publish tree $(git -C $PP rev-parse --short HEAD) $(cd $PP/rust && grep -m1 '^version' Cargo.toml)"
echo "parse-that tree $(git -C $PT rev-parse --short HEAD)"
chk() { repo=$1; pat=$2; want=$3; src=$4; mf=$5
  for rev in $(grep -o "$pat?rev=[0-9a-f]*\|$pat\", rev = \"[0-9a-f]*" $mf | grep -o '[0-9a-f]\{40\}' | sort -u); do
    git -C $repo cat-file -e "$rev^{commit}" 2>/dev/null || git -C $repo fetch -q origin "$rev" 2>/dev/null
    t=$(git -C $repo rev-parse "$rev^{tree}" 2>/dev/null)
    if [ "$t" = "$want" ]; then v=ok; else v=MISMATCH; bad=$((bad+1)); fi
    printf "%s\t%s\t%s\t%s\n" "$v" "$src" "$(echo $rev | cut -c1-9)" "${mf#$6/}"
  done; }
for f in $(cd $PT && git ls-files 'rust/*Cargo.toml' 'rust/Cargo.lock'); do chk $PP github.com/mkbabb/pprint $PPT pprint $PT/$f $PT; done
for f in Cargo.toml Cargo.lock skinny/Cargo.toml skinny/Cargo.lock wasm/Cargo.toml wasm/Cargo.lock; do
  chk $PP github.com/mkbabb/pprint $PPT pprint $BL/$f $BL; chk $PT github.com/mkbabb/parse-that $PTT parse-that $BL/$f $BL; done
for d in $PT/rust $BL $BL/skinny $BL/wasm; do
  (cd $d && cargo metadata --locked --format-version 1 >/dev/null 2>&1); rc=$?
  [ $rc -eq 0 ] || bad=$((bad+1)); printf "lock\t%s\trc=%s\n" "$d" "$rc"; done
[ $bad -eq 0 ] && echo "pins: GREEN" || echo "pins: RED ($bad)"
[ $bad -eq 0 ]
