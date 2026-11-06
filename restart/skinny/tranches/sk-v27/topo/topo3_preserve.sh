#!/usr/bin/env bash
# LW1.TOPO-3 / ruling X-6: commit a dirty tree to a preserve branch through an
# alternate index. The worktree, its real index, its HEAD and its branch are not
# touched; no checkout switch, no stash. Blobs over 5 MiB (release assets) and
# nested repositories are left out of the tree and listed in CUSTODY-PRESERVE.tsv.
# usage: topo3_preserve.sh <name> <worktree> <branch> <custody-dir>
set -euo pipefail
name=$1; wt=$2; branch=$3; C=$4/$name
cd "$wt"
head=$(head -1 "$C/HEAD")
[[ $(git rev-parse HEAD) == "$head" ]] || { echo "HEAD moved since capture" >&2; exit 2; }
export GIT_INDEX_FILE=$C/preserve.index
rm -f "$GIT_INDEX_FILE"
git read-tree "$head"
ex=()
while IFS=$'\t' read -r p _; do ex+=(":(exclude,literal)$p"); done < <(cat "$C/big.tsv" "$C/nested.tsv")
git add -A -- . ${ex[@]+"${ex[@]}"}
{ printf 'kind\tpath\tid\tbytes\twhere\n'
  while IFS=$'\t' read -r p h sz; do printf 'big\t%s\tsha256:%s\t%s\thttps://github.com/mkbabb/bbnf-lang/releases/download/custody-sk-v25/%s\n' "$p" "$h" "$sz" "$h"; done < "$C/big.tsv"
  while IFS=$'\t' read -r p h; do printf 'nested-repo\t%s\tcommit:%s\t-\tits own repository\n' "$p" "$h"; done < "$C/nested.tsv"
} > "$C/CUSTODY-PRESERVE.tsv"
blob=$(git hash-object -w "$C/CUSTODY-PRESERVE.tsv")
git update-index --add --cacheinfo "100644,$blob,CUSTODY-PRESERVE.tsv"
tree=$(git write-tree)
msg="custody(sk-v27 LW1.TOPO-3): preserve the $name dirty tree at ${head:0:9} (ruling X-6)

Made through an alternate index: the checkout, its index and its branch were
not touched. Blobs over 5 MiB and nested repositories are listed in
CUSTODY-PRESERVE.tsv; per-entry fates are ruled at LW7."
commit=$(git commit-tree "$tree" -p "$head" -m "$msg")
git update-ref "refs/heads/$branch" "$commit" 0000000000000000000000000000000000000000
unset GIT_INDEX_FILE
echo "$name $branch $commit tree=$tree"
