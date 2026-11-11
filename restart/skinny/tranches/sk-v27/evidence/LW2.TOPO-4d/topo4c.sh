#!/bin/sh
# sk-v27 LW2.TOPO-4d: LW2.TOPO-4c's dry-run (evidence/LW2.TOPO-4c/topo4c.sh: same DAG order, registry,
# fresh consumers and crates.io read) under RULINGS 2026-11-11b TOPO-4-A2. Three changes:
#  - the publish set holds one pprint, from the tree the consumers pin; pprint 0.4.0-alpha.0 left the set;
#  - the fresh consumers ask for the versions the trees declare (bbnf, bbnf-path), not a fixed "0.2"/"0.1";
#  - the crates.io read adds the version-line rule: for each crate crates.io already holds, its newest
#    non-yanked version is the baseline; the crate breaks when `cargo semver-checks check-release
#    --release-type patch` against that baseline says a new major (0.x: minor) version is required, or,
#    for the proc-macro pprint_derive (no library target to diff), when its 0.2 documented example
#    (`#[derive(Pretty)]`, `let doc: Doc = hey.into()`) against the newest crates.io pprint fails with
#    the tree substituted at the baseline's next patch (control: the same example as published, rc 0).
#    The declared version must equal the rule's version: next minor if it breaks, next patch if not,
#    stepping over any version crates.io holds (yanked included). A crate that does not break but
#    depends on a set crate that does is refused for a manual re-export read (none at either tree).
#  - the pprint integration head's declared version is reconciled: not a version of the publish set,
#    not a pre-release of one, ordered after the published pprint, and not held by crates.io.
# Usage: topo4c.sh <tag> <port> <pprint tree the consumers pin> <pprint integration head> <parse-that> <bbnf-lang>
# GREEN when every crate publishes (rc 0), both fresh consumers build with `cargo +stable build`,
# there are 0 collisions, every line verdict holds, and the integration head is reconciled.
set -u
TAG=$1; PORT=$2; PP03=$3/rust; PPI=$4/rust; PT=$5/rust; BL=$6
EV=$(cd "$(dirname "$0")" && pwd)
SRV=$EV/../LW2.GATE/s16-topo4/registry-server.py; NAMES=$EV/../LW2.GATE/s16-topo4/ours.txt
L=~/.dev-logs/bbnf-lockstep/lw2/r6/topo-4d/topo4c-$TAG; rm -rf $L; mkdir -p $L
export PATH=~/.dev-logs/bbnf-lockstep/tools/bin:$PATH   # cargo-semver-checks 0.51.0 (cargo install --locked --root)
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
meta() { (cd $1 && cargo metadata --offline --no-deps --format-version 1 2>/dev/null) | python3 -I -c "import sys,json;p=[p for p in json.load(sys.stdin)['packages'] if p['name']=='$2'][0];print(p['$3'])"; }
: > $L/order.tsv; : > $L/set.tsv
pub() { dir=$1; pkg=$2; rev=$(git -C $dir rev-parse --short HEAD); ver=$(meta $dir $pkg version)
  printf "%s\t%s\t%s\t%s\n" "$pkg" "$ver" "$dir" "$(meta $dir $pkg manifest_path)" >> $L/set.tsv
  (cd $dir && eval cargo publish -p $pkg --registry lw2 $CFG) > $L/$pkg-$ver-$rev.log 2>&1
  rc=$?; printf "%s\t%s\t%s\t%s\trc=%s\n" "$pkg" "$ver" "$rev" "$dir" "$rc" >> $L/order.tsv; return $rc; }
pub $PT parse-that-egraph-derive && pub $PT parse-that-egraph \
 && pub $PP03/derive pprint_derive && pub $PP03 pprint \
 && pub $PT parse-that-simd && pub $PT parse-that-regex && pub $PT parse_that \
 && pub $BL bbnf-ser && pub $BL bbnf-ir && pub $BL bbnf-path && pub $BL bbnf && pub $BL bbnf-analysis && pub $BL bbnf-lsp && pub $BL gorgeous
PRC=$?; echo "publish rc=$PRC" >> $L/order.tsv
BV=$(meta $BL bbnf version); BPV=$(meta $BL bbnf-path version)
C=$L/consumer; rm -rf $C; mkdir -p $C/src
printf '[package]\nname = "topo4-consumer"\nversion = "0.0.0"\nedition = "2021"\npublish = false\n\n[dependencies]\nbbnf = "%s"\n\n[workspace]\n' $BV > $C/Cargo.toml
printf 'fn main() {\n    println!("{}", std::any::type_name::<bbnf::Span<'"'"'static>>());\n}\n' > $C/src/main.rs
(cd $C && eval cargo +stable build $CFG) > $L/consumer-build.log 2>&1; CRC=$?; echo "consumer (bbnf = \"$BV\") cargo +stable build rc=$CRC" >> $L/order.tsv
grep -h -A1 'name = "bbnf"' $C/Cargo.lock >> $L/order.tsv 2>/dev/null
P=$L/path-consumer; rm -rf $P; mkdir -p $P/src
printf '[package]\nname = "topo4c-path-consumer"\nversion = "0.0.0"\nedition = "2021"\npublish = false\n\n[dependencies]\nbbnf = "%s"\nbbnf-path = "%s"\n\n[workspace]\n' $BV $BPV > $P/Cargo.toml
printf 'use bbnf::path::{Bbnf, Json, TypedPath};\nuse bbnf_path::path;\nfn main() {\n    let j: TypedPath<Json, ()> = path!(Json, "value", "@branch_0");\n    let b: TypedPath<Bbnf, ()> = path!(Bbnf, "element");\n    println!("{} {}", j.len(), b.len());\n}\n' > $P/src/main.rs
(cd $P && eval cargo +stable build $CFG) > $L/path-consumer-build.log 2>&1; PCRC=$?; echo "path consumer (bbnf = \"$BV\", bbnf-path = \"$BPV\") cargo +stable build rc=$PCRC" >> $L/order.tsv
# The version read: the whole set at the versions these trees declare, whether or not the cascade reached it.
for spec in "$PT parse-that-egraph-derive" "$PT parse-that-egraph" "$PP03/derive pprint_derive" "$PP03 pprint" \
  "$PT parse-that-simd" "$PT parse-that-regex" "$PT parse_that" "$BL bbnf-ser" "$BL bbnf-ir" "$BL bbnf-path" "$BL bbnf" \
  "$BL bbnf-analysis" "$BL bbnf-lsp" "$BL gorgeous"; do set -- $spec
  printf "%s\t%s\t%s\t%s\n" "$2" "$(meta $1 $2 version)" "$(meta $1 $2 manifest_path)" "$(meta $1 $2 targets | grep -q proc-macro && echo proc-macro || echo lib)"
done > $L/declared.tsv
# Break verdicts, one per crate with a crates.io baseline.
python3 -I - $L/declared.tsv > $L/baselines.tsv <<'PY'
import json, sys, urllib.request
UA = {"User-Agent": "lockstep-dry-run (generic client)"}
for line in open(sys.argv[1]):
    pkg, v, man, kind = line.rstrip("\n").split("\t")
    n = pkg.lower(); rel = ('1/'+n) if len(n)==1 else ('2/'+n) if len(n)==2 else ('3/'+n[0]+'/'+n) if len(n)==3 else (n[:2]+'/'+n[2:4]+'/'+n)
    try:
        body = urllib.request.urlopen(urllib.request.Request('https://index.crates.io/'+rel, headers=UA), timeout=60).read().decode()
        held = [json.loads(l) for l in body.splitlines() if l.strip()]
    except urllib.error.HTTPError as e:
        if e.code != 404: print(f"{pkg}\t{v}\t{man}\t{kind}\tREAD-ERROR\t-"); continue
        held = []
    def key(s): a = s.split('-')[0].split('.'); return tuple(int(x) for x in a) + ((0,) if '-' in s else (1,))
    live = sorted((h['vers'] for h in held if not h['yanked'] and '-' not in h['vers']), key=key)
    print(f"{pkg}\t{v}\t{man}\t{kind}\t{live[-1] if live else '-'}\t{','.join(h['vers'] for h in held) or '-'}")
PY
: > $L/breaks.tsv
while IFS="$(printf '\t')" read -r pkg v man kind base held; do
  [ "$base" = "-" ] && { printf "%s\t-\tnew-name\n" "$pkg" >> $L/breaks.tsv; continue; }
  if [ "$kind" = lib ]; then
    cargo semver-checks check-release --manifest-path $man --baseline-version $base --release-type patch < /dev/null > $L/semver-checks-$pkg.log 2>&1
    s=$(grep -m1 'Summary' $L/semver-checks-$pkg.log)
    case "$s" in *"new major version"*) verdict=breaks;; *"new minor version"*|*"no semver update required"*) verdict=compatible;; *) verdict=READ-ERROR;; esac
    printf "%s\t%s\t%s\t%s\n" "$pkg" "$base" "$verdict" "$(echo $s | sed 's/^ *//')" >> $L/breaks.tsv
  else
    # proc-macro: the documented example of the baseline line, as published (control) and with the tree at
    # the baseline's next patch substituted (a copy outside the repository; only its version line differs).
    nextp=$(python3 -I -c "a='$base'.split('.');print(f'{a[0]}.{a[1]}.{int(a[2])+1}')")
    T=$L/derive-tree; rm -rf $T; cp -R "$(dirname $man)" $T; rm -rf $T/target
    python3 -I -c "import re;p='$T/Cargo.toml';s=open(p).read();open(p,'w').write(re.sub(r'(?m)^version = \"[^\"]+\"','version = \"$nextp\"',s,count=1))"
    pv=$(awk -F'\t' '$1=="pprint"{print $5}' $L/baselines.tsv)
    for mode in control substituted; do D=$L/derive-$mode; rm -rf $D; mkdir -p $D/src
      printf '[package]\nname = "derive-probe"\nversion = "0.0.0"\nedition = "2021"\npublish = false\n\n[dependencies]\npprint = "=%s"\n' $pv > $D/Cargo.toml
      [ $mode = substituted ] && printf '\n[patch.crates-io]\n%s = { path = "%s" }\n' $pkg $T >> $D/Cargo.toml
      printf '\n[workspace]\n' >> $D/Cargo.toml
      printf 'use pprint::Doc;\nuse pprint::Pretty;\n#[derive(Pretty)]\nstruct Hey {\n    a: u32,\n    b: u32,\n}\nfn main() {\n    let hey = Hey { a: 1, b: 2 };\n    let doc: Doc = hey.into();\n    let _ = doc;\n}\n' > $D/src/main.rs
      (cd $D && cargo +stable check) < /dev/null > $D.log 2>&1; eval "r_$mode=\$?"
    done
    used=$(grep -c "name = \"$pkg\"" $L/derive-substituted/Cargo.lock 2>/dev/null); uv=$(grep -h -A1 "name = \"$pkg\"" $L/derive-substituted/Cargo.lock | grep version | tr -d ' "')
    if [ $r_control -ne 0 ] || [ "$uv" != "version=$nextp" ]; then verdict=READ-ERROR; elif [ $r_substituted -ne 0 ]; then verdict=breaks; else verdict=compatible; fi
    printf "%s\t%s\t%s\tdocumented example vs pprint =%s: control rc=%s, tree at %s substituted rc=%s (%s)\n" "$pkg" "$base" "$verdict" "$pv" "$r_control" "$nextp" "$r_substituted" "$uv" >> $L/breaks.tsv
  fi
done < $L/baselines.tsv
# Expected versions (TOPO-4-A2) and the integration head's reconciliation.
PIV=$(meta $PPI pprint version)
python3 -I - $L/baselines.tsv $L/breaks.tsv $L/set.tsv "$PIV" > $L/cratesio-versions.tsv <<'PY'
import json, sys, re
base = {l.split('\t')[0]: l.rstrip('\n').split('\t') for l in open(sys.argv[1])}
brk = {l.split('\t')[0]: l.rstrip('\n').split('\t') for l in open(sys.argv[2])}
deps = {}
for l in open(sys.argv[3]):
    pkg, v, d, man = l.rstrip('\n').split('\t')
    t = open(man).read(); deps[pkg] = {n for n in base if n != pkg and re.search(r'(?m)^\s*' + re.escape(n) + r'(\.workspace)?\s*=', t)}
piv = sys.argv[4]
def tup(s): return tuple(int(x) for x in s.split('-')[0].split('.'))
for pkg, (p, v, man, kind, b, held) in base.items():
    hv = set() if held == '-' else set(held.split(','))
    coll = 'COLLISION' if v in hv else 'free'
    if b == 'READ-ERROR': print(f"{pkg}\t{v}\tREAD-ERROR"); continue
    verdict = brk[pkg][2]
    if verdict == 'new-name': print(f"{pkg}\t{v}\t{coll}\tline: new name, any free version"); continue
    if verdict == 'READ-ERROR': print(f"{pkg}\t{v}\t{coll}\tLINE READ-ERROR"); continue
    if verdict == 'compatible' and any(brk.get(d, ['', '', ''])[2] == 'breaks' for d in deps[pkg]):
        print(f"{pkg}\t{v}\t{coll}\tLINE REFUSED: compatible by its own API but depends on a breaking set crate (manual re-export read)"); continue
    M, m, pt = tup(b)
    if verdict == 'breaks': e = (M + 1, 0, 0) if M > 0 else (0, m + 1, 0)
    else: e = (M, m, pt + 1)
    while '.'.join(map(str, e)) in hv: e = (e[0], e[1], e[2] + 1)
    exp = '.'.join(map(str, e))
    print(f"{pkg}\t{v}\t{coll}\tline: {verdict} vs {b} -> expected {exp}: {'OK' if v == exp else 'LINE-MISMATCH'}")
pv = base['pprint'][1]; ph = base['pprint'][5]
held = set() if ph == '-' else set(ph.split(','))
ok = piv != pv and piv.split('-')[0] != pv and tup(piv) > tup(pv) and piv not in held
print(f"pprint (integration head)\t{piv}\t{'free' if piv not in held else 'COLLISION'}\treconcile vs published pprint {pv}: {'OK' if ok else 'UNRECONCILED'}")
PY
NC=$(grep -c COLLISION $L/cratesio-versions.tsv); NE=$(grep -c -E 'READ-ERROR|REFUSED' $L/cratesio-versions.tsv)
NL=$(grep -c -E 'LINE-MISMATCH|UNRECONCILED' $L/cratesio-versions.tsv)
echo "crates.io collisions=$NC line-mismatches=$NL read-errors=$NE" >> $L/order.tsv
if [ $PRC -eq 0 ] && [ $CRC -eq 0 ] && [ $PCRC -eq 0 ] && [ $NC -eq 0 ] && [ $NE -eq 0 ] && [ $NL -eq 0 ]; then echo "TOPO-4d topo4c read $TAG: GREEN" | tee -a $L/order.tsv; exit 0
else echo "TOPO-4d topo4c read $TAG: RED" | tee -a $L/order.tsv; exit 1; fi
