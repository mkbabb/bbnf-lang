#!/bin/sh
# sk-v27 LW2.TOPO-4b falsifier: the TOPO-4 publish dry-run (S16's topo4.sh, same order and registry),
# parameterised by the four source trees, plus the crates.io version read TOPO-4-A names.
# DAG order differs from topo4.sh in one place: bbnf-path precedes bbnf, since bbnf's published manifest
# keeps its versioned dev-dependency on bbnf-path (S16 never reached that step).
# One read: a fresh store and CARGO_HOME; the registry server runs inside this process tree and stops on exit.
# Usage: topo4b.sh <tag> <port> <pprint-0.3-line> <pprint-0.4-line> <parse-that> <bbnf-lang>
# GREEN when every crate publishes (rc 0), the fresh `bbnf` consumer builds with `cargo +stable build`,
# and no published (name, version) is one crates.io already holds (yanked versions included).
set -u
TAG=$1; PORT=$2; PP03=$3/rust; PP04=$4/rust; PT=$5/rust; BL=$6
EV=$(cd "$(dirname "$0")" && pwd)
SRV=$EV/../LW2.GATE/s16-topo4/registry-server.py; NAMES=$EV/../LW2.GATE/s16-topo4/ours.txt
L=~/.dev-logs/bbnf-lockstep/lw2/r5/topo-4b/$TAG; rm -rf $L; mkdir -p $L
# Machine law: one shared target dir. A distinct port per read gives each read its own registry source id,
# so no registry artefact of one read is reused by another.
export CARGO_TARGET_DIR=~/.dev-logs/bbnf-lockstep/target-shared
export RUSTUP_HOME=${RUSTUP_HOME:-$HOME/.rustup}
export CARGO_REGISTRIES_LW2_TOKEN=dry-run
export CARGO_HOME=~/.dev-logs/bbnf-lockstep/registry/cargo-home-$TAG; rm -rf $CARGO_HOME; mkdir -p $CARGO_HOME
STORE=~/.dev-logs/bbnf-lockstep/registry/store-$TAG; rm -rf $STORE; mkdir -p $STORE
python3 -I $SRV $STORE $PORT $NAMES 2> $L/registry.log & SP=$!
trap 'kill $SP 2>/dev/null; wait $SP 2>/dev/null' EXIT INT TERM
for i in 1 2 3 4 5 6 7 8 9 10; do curl -sf http://127.0.0.1:$PORT/index/config.json >/dev/null && break; sleep 1; done
CFGF=$L/registry-config.toml
printf '[registries.lw2]\nindex = "sparse+http://127.0.0.1:%s/index/"\n\n[source.crates-io]\nreplace-with = "lw2"\n' $PORT > $CFGF
CFG="--config $CFGF"
: > $L/order.tsv; : > $L/versions.tsv
pub() { dir=$1; pkg=$2; rev=$(git -C $dir rev-parse --short HEAD)
  ver=$(cd $dir && cargo metadata --offline --no-deps --format-version 1 2>/dev/null | python3 -c "import sys,json;print([p['version'] for p in json.load(sys.stdin)['packages'] if p['name']=='$pkg'][0])")
  printf "%s\t%s\n" "$pkg" "$ver" >> $L/versions.tsv
  (cd $dir && eval cargo publish -p $pkg --registry lw2 $CFG) > $L/$pkg-$ver-$rev.log 2>&1
  rc=$?; printf "%s\t%s\t%s\t%s\trc=%s\n" "$pkg" "$ver" "$rev" "$dir" "$rc" >> $L/order.tsv; return $rc; }
pub $PT parse-that-egraph-derive && pub $PT parse-that-egraph \
 && pub $PP03/derive pprint_derive && pub $PP03 pprint && pub $PP04 pprint \
 && pub $PT parse-that-simd && pub $PT parse-that-regex && pub $PT parse_that \
 && pub $BL bbnf-ser && pub $BL bbnf-ir && pub $BL bbnf-path && pub $BL bbnf && pub $BL bbnf-analysis && pub $BL bbnf-lsp && pub $BL gorgeous
PRC=$?; echo "publish rc=$PRC" >> $L/order.tsv
C=$L/consumer; rm -rf $C; mkdir -p $C/src
printf '[package]\nname = "topo4-consumer"\nversion = "0.0.0"\nedition = "2021"\npublish = false\n\n[dependencies]\nbbnf = "0.2"\n\n[workspace]\n' > $C/Cargo.toml
printf 'fn main() {\n    println!("{}", std::any::type_name::<bbnf::Span<'"'"'static>>());\n}\n' > $C/src/main.rs
(cd $C && eval cargo +stable build $CFG) > $L/consumer-build.log 2>&1; CRC=$?; echo "consumer cargo +stable build rc=$CRC" >> $L/order.tsv
grep -h -A1 'name = "bbnf"' $C/Cargo.lock >> $L/order.tsv 2>/dev/null
# crates.io version read (TOPO-4-A): the whole publish set at the versions these trees declare, read
# from the sparse index (yanked included), whether or not the cascade above reached the crate.
for spec in "$PT parse-that-egraph-derive" "$PT parse-that-egraph" "$PP03/derive pprint_derive" "$PP03 pprint" "$PP04 pprint" \
  "$PT parse-that-simd" "$PT parse-that-regex" "$PT parse_that" "$BL bbnf-ser" "$BL bbnf-ir" "$BL bbnf-path" "$BL bbnf" \
  "$BL bbnf-analysis" "$BL bbnf-lsp" "$BL gorgeous"; do set -- $spec
  (cd $1 && cargo metadata --offline --no-deps --format-version 1 2>/dev/null) | python3 -I -c "
import sys,json,urllib.request
pkg='$2'; v=[p['version'] for p in json.load(sys.stdin)['packages'] if p['name']==pkg][0]
n=pkg.lower(); rel=('1/'+n) if len(n)==1 else ('2/'+n) if len(n)==2 else ('3/'+n[0]+'/'+n) if len(n)==3 else (n[:2]+'/'+n[2:4]+'/'+n)
try:
    body=urllib.request.urlopen(urllib.request.Request('https://index.crates.io/'+rel,headers={'User-Agent':'lockstep-dry-run (generic client)'}),timeout=60).read().decode()
    held={json.loads(l)['vers']:json.loads(l)['yanked'] for l in body.splitlines() if l.strip()}
except urllib.error.HTTPError as e:
    held={} if e.code==404 else None
if held is None: print(f'{pkg}\t{v}\tREAD-ERROR'); sys.exit()
print(f'{pkg}\t{v}\t'+('COLLISION (yanked)' if held.get(v) else 'COLLISION' if v in held else 'free'))
"
done > $L/cratesio-versions.tsv
NC=$(grep -c COLLISION $L/cratesio-versions.tsv); NE=$(grep -c READ-ERROR $L/cratesio-versions.tsv)
echo "crates.io collisions=$NC read-errors=$NE" >> $L/order.tsv
if [ $PRC -eq 0 ] && [ $CRC -eq 0 ] && [ $NC -eq 0 ] && [ $NE -eq 0 ]; then echo "TOPO-4b read $TAG: GREEN" | tee -a $L/order.tsv; exit 0
else echo "TOPO-4b read $TAG: RED" | tee -a $L/order.tsv; exit 1; fi
