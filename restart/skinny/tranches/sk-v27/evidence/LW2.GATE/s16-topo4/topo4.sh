#!/bin/sh
# sk-v27 LW2.TOPO-4 publish dry-run, one read: a fresh store, publish in DAG order, then a fresh consumer of bbnf.
# Usage: topo4.sh <read-number> <port>   (the registry server must be listening on <port> over store read-<n>)
set -u
N=$1; PORT=$2
L=~/.dev-logs/bbnf-lockstep/lw2/s15-freeze/topo4-r$N; mkdir -p $L
export CARGO_TARGET_DIR=~/.dev-logs/bbnf-lockstep/target-topo4-r$N; rm -rf $CARGO_TARGET_DIR
export RUSTUP_HOME=${RUSTUP_HOME:-$HOME/.rustup}
export CARGO_REGISTRIES_LW2_TOKEN=dry-run
# A fresh CARGO_HOME per read: the registry cache is keyed by URL, so a crate a former read (or crates.io
# through the proxy) extracted at the same name and version must never be reused.
export CARGO_HOME=~/.dev-logs/bbnf-lockstep/registry/cargo-home-r$N; rm -rf $CARGO_HOME; mkdir -p $CARGO_HOME
CFGF=$L/registry-config.toml
printf '[registries.lw2]\nindex = "sparse+http://127.0.0.1:%s/index/"\n\n[source.crates-io]\nreplace-with = "lw2"\n' $PORT > $CFGF
CFG="--config $CFGF"
PP03=~/Programming/pprint-wt/lw2-s3-dag/rust          # pprint 0.3.6 @ fe7e9ca: the rev every consumer pins
PP04=~/Programming/pprint-wt/sk-v27-lw2/rust          # pprint 0.4.0-alpha.0 @ b980672: the integration head (A-8 PROVISIONAL)
PT=~/Programming/parse-that-wt/sk-v27-lw2/rust        # parse-that @ 5608b94
BL=~/Programming/bbnf-lang-wt/lw2-s15-freeze          # bbnf-lang @ the S15 lane head
pub() { dir=$1; pkg=$2
  (cd $dir && eval cargo publish -p $pkg --registry lw2 $CFG) > $L/$pkg-$(basename $(dirname $dir))-$(git -C $dir rev-parse --short HEAD).log 2>&1
  rc=$?; printf "%s\t%s\t%s\trc=%s\n" "$pkg" "$(git -C $dir rev-parse --short HEAD)" "$dir" "$rc" >> $L/order.tsv; return $rc; }
: > $L/order.tsv
pub $PT parse-that-egraph-derive && pub $PT parse-that-egraph \
 && pub $PP03/derive pprint_derive && pub $PP03 pprint && pub $PP04 pprint \
 && pub $PT parse-that-simd && pub $PT parse-that-regex && pub $PT parse_that \
 && pub $BL bbnf-ser && pub $BL bbnf-ir && pub $BL bbnf && pub $BL bbnf-path && pub $BL bbnf-analysis && pub $BL bbnf-lsp && pub $BL gorgeous
echo "publish rc=$?" >> $L/order.tsv
C=$L/consumer; rm -rf $C; mkdir -p $C/src
printf '[package]\nname = "topo4-consumer"\nversion = "0.0.0"\nedition = "2021"\npublish = false\n\n[dependencies]\nbbnf = "0.2"\n\n[workspace]\n' > $C/Cargo.toml
printf 'fn main() {\n    println!("{}", std::any::type_name::<bbnf::Span<'"'"'static>>());\n}\n' > $C/src/main.rs
(cd $C && eval cargo +stable build $CFG) > $L/consumer-build.log 2>&1; echo "consumer cargo +stable build rc=$?" >> $L/order.tsv
grep -h -A1 'name = "bbnf"' $C/Cargo.lock >> $L/order.tsv
