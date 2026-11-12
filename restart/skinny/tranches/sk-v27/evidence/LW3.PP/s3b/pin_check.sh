#!/bin/sh
# usage: pin_check.sh <repo> <rev>: every pprint pin (manifests and lockfiles)
# at <rev> names pprint 69d1fe9fa522f18e129fbedea0288a9c19b5bbf9 (LW3.PP-2/PP-3a).
# exit 0 GREEN, 1 RED.
repo=$1; rev=$2; want=69d1fe9fa522f18e129fbedea0288a9c19b5bbf9
cd "$repo" || exit 2
bad=0; n=0
for f in $(git ls-tree -r --name-only "$rev" | grep -E '(^|/)Cargo\.(toml|lock)$' | grep -v '^restart'); do
  for line in $(git show "$rev:$f" | grep -oE 'mkbabb/pprint.{0,30}rev[ ="]+[0-9a-f]{40}' | grep -oE '[0-9a-f]{40}|[0-9a-f]{7,39}$' ); do
    n=$((n+1))
    if [ "$line" != "$want" ]; then echo "RED: $f pins pprint $line"; bad=1; fi
  done
done
[ $n -eq 0 ] && { echo "RED: no pprint pin found"; exit 1; }
[ $bad -eq 0 ] && echo "GREEN: $n pprint pins at $rev, all $want"
exit $bad
