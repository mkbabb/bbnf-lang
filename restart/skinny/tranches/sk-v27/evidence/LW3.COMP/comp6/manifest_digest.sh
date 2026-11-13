#!/bin/sh
# Driver for manifest_digest.rs (sk-v27 LW3.COMP-6 evidence). Usage: manifest_digest.sh <bl worktree> <out.tsv>
# Builds the tool's test binary once, then runs one MANIFEST entry per process.
set -u
BL=$1; OUT=$2; : > "$OUT"
export CARGO_TARGET_DIR=${CARGO_TARGET_DIR:-$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang}
cd "$BL/crates/core" || exit 2
bin=$(cargo test -p bbnf --locked --test manifest_digest --no-run --message-format=json 2>/dev/null \
  | python3 -I -c 'import json,sys
for l in sys.stdin:
    m=json.loads(l)
    if m.get("reason")=="compiler-artifact" and m["target"]["name"]=="manifest_digest" and m.get("executable"): print(m["executable"])')
[ -x "$bin" ] || { echo "no binary"; exit 2; }
n=$(grep -c . ../../grammar/MANIFEST); i=0
while [ $i -lt "$n" ]; do
  rel=$(grep . ../../grammar/MANIFEST | sed -n "$((i+1))p" | awk '{print $NF}')
  BBNF_MANIFEST_DIGEST_OUT=$OUT BBNF_MANIFEST_ENTRY=$i "$bin" --exact manifest_digest --test-threads 1 > /dev/null 2>&1 \
    || printf '%s\tall\tABORT\n' "$rel" >> "$OUT"
  i=$((i+1))
done
