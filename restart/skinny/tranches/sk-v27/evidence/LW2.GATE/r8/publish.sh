#!/bin/sh
# LW2 publish to crates.io, DAG order; each crate must appear in the sparse index (and be downloadable)
# before its dependents publish. Usage: publish.sh <pprint src> <parse-that src> <bbnf-lang src> [first-crate]
set -u
PP=$1/rust; PT=$2/rust; BL=$3; FROM=${4:-}
L=~/.dev-logs/bbnf-lockstep/lw2/r8/publish; mkdir -p $L
export CARGO_TARGET_DIR=~/.dev-logs/bbnf-lockstep/target-shared
idx() { n=$(echo $1 | tr A-Z a-z); case ${#n} in 1) p=1/$n;; 2) p=2/$n;; 3) p=3/$(echo $n|cut -c1)/$n;; *) p=$(echo $n|cut -c1-2)/$(echo $n|cut -c3-4)/$n;; esac
  curl -sf -H 'Cache-Control: no-cache' "https://index.crates.io/$p" | grep -q "\"vers\":\"$2\""; }
skip=1; [ -z "$FROM" ] && skip=0
for spec in "$PT parse-that-egraph-derive" "$PT parse-that-egraph" "$PP/derive pprint_derive" "$PP pprint" \
  "$PT parse-that-simd" "$PT parse-that-regex" "$PT parse_that" "$BL bbnf-ser" "$BL bbnf-ir" "$BL bbnf-path" "$BL bbnf" \
  "$BL bbnf-analysis" "$BL bbnf-lsp" "$BL gorgeous"; do set -- $spec; d=$1; pkg=$2
  [ "$pkg" = "$FROM" ] && skip=0; [ $skip -eq 1 ] && continue
  ver=$(cd $d && cargo metadata --no-deps --format-version 1 2>/dev/null | python3 -I -c "import sys,json;print([p['version'] for p in json.load(sys.stdin)['packages'] if p['name']=='$pkg'][0])")
  rev=$(git -C $d rev-parse --short HEAD)
  if idx $pkg $ver; then echo "$pkg $ver already in index: STOP (never reuse a version)"; exit 3; fi
  (cd $d && cargo publish -p $pkg) > $L/$pkg-$ver.log 2>&1; rc=$?
  printf "%s\t%s\t%s\tpublish rc=%s\n" $pkg $ver $rev $rc | tee -a $L/published.tsv
  [ $rc -eq 0 ] || { tail -20 $L/$pkg-$ver.log; exit $rc; }
  i=0; until idx $pkg $ver; do i=$((i+1)); [ $i -gt 60 ] && { echo "$pkg $ver not in index after 10 min"; exit 4; }; sleep 10; done
  code=$(curl -s -o $L/$pkg-$ver.crate -w '%{http_code}' -L "https://static.crates.io/crates/$pkg/$pkg-$ver.crate")
  sha=$(shasum -a 256 $L/$pkg-$ver.crate | cut -d' ' -f1)
  printf "%s\t%s\tindex=present\tdownload=%s\tserved-sha256=%s\n" $pkg $ver $code $sha | tee -a $L/published.tsv
  [ "$code" = 200 ] || { echo "$pkg $ver not downloadable ($code): STOP"; exit 5; }
done
echo "publish: DONE"
