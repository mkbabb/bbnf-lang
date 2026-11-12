#!/bin/sh
# LW3.COMP-1b RED tree (S7 SPINE): the mechanical merge of the spine into
# <base>, every conflict taken spine-side (git merge-tree -X theirs), plus the
# anti-drop test registered as a [[test]] target. Prints the unreferenced
# commit's sha. Never moves a branch.
# usage: red-tree.sh <base> <spine> <root_api_present.rs>
set -e
base=$1; spine=$2; t=$3
tree=$(git merge-tree --write-tree -X theirs "$base" "$spine" | head -1)
tmp=$(mktemp -d); export GIT_INDEX_FILE=$tmp/index
git read-tree "$tree"
blob=$(git hash-object -w "$t")
git update-index --add --cacheinfo "100644,$blob,crates/core/tests/root_api_present.rs"
cargo_blob=$( { git show "$tree:crates/core/Cargo.toml"; printf '\n[[test]]\nname = "root_api_present"\npath = "tests/root_api_present.rs"\n'; } | git hash-object -w --stdin)
git update-index --cacheinfo "100644,$cargo_blob,crates/core/Cargo.toml"
t2=$(git write-tree)
printf 'LW3.COMP-1b RED tree: merge-tree -X theirs %s %s, plus root_api_present\n' "$base" "$spine" | git commit-tree "$t2" -p "$base" -p "$spine"
rm -rf "$tmp"
