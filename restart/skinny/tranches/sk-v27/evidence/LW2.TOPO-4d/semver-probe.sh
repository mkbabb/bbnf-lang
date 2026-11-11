#!/bin/sh
# sk-v27 LW2.TOPO-4d falsifier (RULINGS 2026-11-11b, TOPO-4-A2): do the crates already on crates.io
# still build on a fresh resolve once the publish set is published? Generalises LW2.TOPO-4c's
# semver-probe.sh (critic O-1) from the pprint 0.3 tree to the whole publish set.
#  1. The publish set is published, in TOPO-4c's DAG order, into a fresh dry-run store
#     (S16's registry-server.py; `cargo publish --no-verify`: packaging and build verification are
#     topo4c.sh's job, this probe reads only what a resolver would pick).
#  2. union-registry.py serves crates.io as it would read after that publish: every crates.io
#     version plus the store's versions.
#  3. The consumers are read from crates.io itself, so they are the same at every tree: the newest
#     non-yanked version of each publish-set crate that crates.io holds, the newest non-yanked
#     version of each crates.io reverse dependency of the set that is not in the set, and the
#     pprint_derive 0.2 documented example (`#[derive(Pretty)]`, `let doc: Doc = hey.into()`)
#     against the newest pprint crates.io holds, which isolates the derive.
#  4. Each consumer, pinned exactly, is checked twice with `cargo +nightly check` (the published
#     tree's smallbox `nightly` feature does not build on stable): the control against crates.io
#     as it is, and the fresh resolve against the union registry.
# VOID when a control fails (it cannot discriminate); GREEN when every control and every fresh
# resolve passes; RED when a consumer whose control passes breaks on the fresh resolve.
# Usage: semver-probe.sh <tag> <port> <pprint tree the consumers pin> <parse-that> <bbnf-lang>
set -u
TAG=$1; PORT=$2; PP=$3/rust; PT=$4/rust; BL=$5
EV=$(cd "$(dirname "$0")" && pwd)
SRV=$EV/../LW2.GATE/s16-topo4/registry-server.py; NAMES=$EV/../LW2.GATE/s16-topo4/ours.txt
L=~/.dev-logs/bbnf-lockstep/lw2/r6/topo-4d/semver-$TAG; rm -rf $L; mkdir -p $L
export CARGO_TARGET_DIR=~/.dev-logs/bbnf-lockstep/target-shared
export RUSTUP_HOME=${RUSTUP_HOME:-$HOME/.rustup}
export CARGO_REGISTRIES_LW2_TOKEN=dry-run
export CARGO_HOME=~/.dev-logs/bbnf-lockstep/registry/cargo-home-semver-$TAG; rm -rf $CARGO_HOME; mkdir -p $CARGO_HOME
STORE=~/.dev-logs/bbnf-lockstep/registry/store-semver-$TAG; rm -rf $STORE; mkdir -p $STORE
UPORT=$((PORT+1))
python3 -I $SRV $STORE $PORT $NAMES 2> $L/registry.log & SP=$!
python3 -I $EV/union-registry.py $STORE $UPORT 2> $L/union.log & UP=$!
trap 'kill $SP $UP 2>/dev/null; wait $SP $UP 2>/dev/null' EXIT INT TERM
for p in $PORT $UPORT; do for i in 1 2 3 4 5 6 7 8 9 10; do curl -sf http://127.0.0.1:$p/index/config.json >/dev/null && break; sleep 1; done; done
PUBF=$L/publish-config.toml
printf '[registries.lw2]\nindex = "sparse+http://127.0.0.1:%s/index/"\n\n[source.crates-io]\nreplace-with = "lw2"\n' $PORT > $PUBF
UNIF=$L/union-config.toml
printf '[registries.union]\nindex = "sparse+http://127.0.0.1:%s/index/"\n\n[source.crates-io]\nreplace-with = "union"\n' $UPORT > $UNIF
: > $L/published.tsv
pub() { dir=$1; pkg=$2
  ver=$(cd $dir && cargo metadata --offline --no-deps --format-version 1 2>/dev/null | python3 -I -c "import sys,json;print([p['version'] for p in json.load(sys.stdin)['packages'] if p['name']=='$pkg'][0])")
  (cd $dir && cargo publish --no-verify -p $pkg --registry lw2 --config $PUBF) > $L/publish-$pkg-$ver.log 2>&1
  rc=$?; printf "%s\t%s\t%s\trc=%s\n" "$pkg" "$ver" "$(git -C $dir rev-parse --short HEAD)" "$rc" >> $L/published.tsv; return $rc; }
pub $PT parse-that-egraph-derive && pub $PT parse-that-egraph \
 && pub $PP/derive pprint_derive && pub $PP pprint \
 && pub $PT parse-that-simd && pub $PT parse-that-regex && pub $PT parse_that \
 && pub $BL bbnf-ser && pub $BL bbnf-ir && pub $BL bbnf-path && pub $BL bbnf && pub $BL bbnf-analysis && pub $BL bbnf-lsp && pub $BL gorgeous
PRC=$?
if [ $PRC -ne 0 ]; then echo "semver-probe $TAG: VOID (publish rc=$PRC)" | tee $L/result.tsv; exit 2; fi
# The consumers, read from crates.io (same list at every tree).
python3 -I - $NAMES > $L/consumers.tsv <<'PY'
import json, sys, urllib.request
UA = {"User-Agent": "lockstep-dry-run (generic client)"}
def get(u): return json.loads(urllib.request.urlopen(urllib.request.Request(u, headers=UA), timeout=60).read())
def newest(n):
    vs = [v["num"] for v in get(f"https://crates.io/api/v1/crates/{n}")["versions"] if not v["yanked"]]
    return vs[0] if vs else None
ours = [n.strip() for n in open(sys.argv[1]) if n.strip()]
out = {}
for n in ours:
    try: v = newest(n)
    except urllib.error.HTTPError: continue  # a new name: no published consumer to protect
    if v: out[n] = v
    rd = get(f"https://crates.io/api/v1/crates/{n}/reverse_dependencies?per_page=100")
    for vv in rd.get("versions", []):
        if vv["crate"] not in ours and vv["crate"] not in out:
            out[vv["crate"]] = newest(vv["crate"])
for n, v in sorted(out.items()): print(f"{n}\t{v}\tcrate")
if "pprint" in out: print(f"pprint\t{out['pprint']}\tderive-example")
PY
probe() { name=$1; ver=$2; kind=$3; mode=$4; D=$L/$name-$kind-$mode; mkdir -p $D/src
  printf '[package]\nname = "semver-probe"\nversion = "0.0.0"\nedition = "2021"\npublish = false\n\n[dependencies]\n%s = "=%s"\n\n[workspace]\n' $name $ver > $D/Cargo.toml
  if [ $kind = derive-example ]; then
    printf 'use pprint::Doc;\nuse pprint::Pretty;\n#[derive(Pretty)]\nstruct Hey {\n    a: u32,\n    b: u32,\n}\nfn main() {\n    let hey = Hey { a: 1, b: 2 };\n    let doc: Doc = hey.into();\n    let _ = doc;\n}\n' > $D/src/main.rs
  else printf 'fn main() {}\n' > $D/src/main.rs; fi
  if [ $mode = control ]; then (cd $D && cargo +nightly check) < /dev/null > $D.log 2>&1
  else (cd $D && cargo +nightly check --config $UNIF) < /dev/null > $D.log 2>&1; fi
  rc=$?
  picked=$(python3 -I -c "
import re,sys
ours={n.strip() for n in open('$NAMES') if n.strip()}
t=open('$D/Cargo.lock').read() if __import__('os').path.exists('$D/Cargo.lock') else ''
print(' '.join(f'{n}={v}' for n,v in re.findall(r'name = \"([^\"]+)\"\nversion = \"([^\"]+)\"',t) if n in ours) or '-')")
  printf "%s =%s %s\t%s\trc=%s\t%s\n" "$name" "$ver" "$kind" "$mode" "$rc" "$picked" >> $L/result.tsv
  return $rc; }
echo "published: $(cut -f1,2 $L/published.tsv | tr '\t\n' '= ')" > $L/result.tsv
BAD=0; VOID=0
while IFS="$(printf '\t')" read -r name ver kind; do
  probe $name $ver $kind control; krc=$?
  probe $name $ver $kind fresh; rc=$?
  if [ $krc -ne 0 ]; then VOID=$((VOID+1)); elif [ $rc -ne 0 ]; then BAD=$((BAD+1)); fi
done < $L/consumers.tsv
N=$(grep -c . $L/consumers.tsv)
if [ $VOID -gt 0 ]; then echo "semver-probe $TAG: VOID ($VOID of $N controls fail)" | tee -a $L/result.tsv; exit 2
elif [ $BAD -eq 0 ]; then echo "semver-probe $TAG: GREEN ($N of $N consumers build, controls and fresh resolves)" | tee -a $L/result.tsv; exit 0
else echo "semver-probe $TAG: RED ($BAD of $N published consumers break on a fresh resolve)" | tee -a $L/result.tsv; exit 1; fi
