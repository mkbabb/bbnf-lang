#!/bin/bash
# S12 finding 3 falsifier: no live source names an item COMP-1b deleted.
rev=$1
P=(crates/*/src typescript/src wasm xtask)
echo "== git grep -nwE 'parse_with_ir|calculate_acyclic_deps_scc' $rev -- ${P[*]}"
git grep -nwE 'parse_with_ir|calculate_acyclic_deps_scc' "$rev" -- "${P[@]}"
echo "count=$(git grep -nwE 'parse_with_ir|calculate_acyclic_deps_scc' "$rev" -- "${P[@]}" | wc -l | tr -d ' ')"
