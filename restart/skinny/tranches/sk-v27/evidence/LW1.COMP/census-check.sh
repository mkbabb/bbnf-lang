#!/bin/sh
# LW1.COMP-1 falsifier: every crate manifest (outside restart/, docs/, audit/ archives) in the
# root tree (origin/master c9b93adb4) and the sk-v26 tree (b5fd0ca98), plus each sk-v26
# compiler module, has a census row with a fate and an owning unit.
T=${1:?census tsv}; R=~/Programming/bbnf-lang; miss=0
row() { awk -F'\t' -v t="$1" -v p="$2" '$1==t && $2==p && $7!="" && $8!=""' "$T" 2>/dev/null | head -1; }
for tree in root:c9b93adb4 sk-v26:b5fd0ca98; do
  name=${tree%%:*}; rev=${tree#*:}
  for m in $(git -C $R ls-tree -r --name-only $rev | grep -E '(^|/)Cargo.toml$' | grep -v -E '^(restart|docs|audit)/'); do
    d=$(dirname $m); [ "$d" = . ] && continue
    git -C $R show $rev:$m | grep -q '^\[package\]' || continue
    if [ $name = sk-v26 ] && [ $d != crates/core ]; then
      case $d in skinny/*) p='skinny/crates/*';; *) p='crates/{analysis,bbnf-path,bbnf-path-ts,bootstrap,csp-solver,egraph,egraph-derive,gorgeous,ir,lsp,ser,simd-scan}';; esac
      [ -n "$(row $name "$p")" ] || { echo "MISSING $name $d"; miss=$((miss+1)); }; continue
    fi
    t=$name; case $d in skinny/*) t=skinny;; esac
    [ -n "$(row $t $d)" ] || { echo "MISSING $t $d"; miss=$((miss+1)); }
  done
done
for f in $(git -C $R ls-tree --name-only b5fd0ca98 crates/core/src/compiler/); do
  [ -n "$(row sk-v26 $f)" ] || { echo "MISSING sk-v26 $f"; miss=$((miss+1)); }
done
echo "missing=$miss"; [ $miss -eq 0 ]
