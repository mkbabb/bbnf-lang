#!/bin/bash
# sk-v27 LW2.CON-2 (S11 BENCH): build a cure driver once per arm.
#   build.sh perf3|rx4 <commit> <out-bin>
# Makes a detached worktree of the arm's repository at <commit> under ~/.dev-logs/bbnf-lockstep/
# s11-arms/, writes the driver's Cargo.toml from its template, and builds it with one shared
# CARGO_TARGET_DIR per driver per arm (disk rules). The binary is copied to <out-bin>.
set -euo pipefail
kind=$1; rev=$2; out=$3
HERE=$(cd "$(dirname "$0")" && pwd)
ARMS=~/.dev-logs/bbnf-lockstep/s11-arms
case $kind in
  perf3) repo=~/Programming/parse-that; key=PT ;;
  rx4)   repo=~/Programming/bbnf-lang; key=BL ;;
  *) echo "kind perf3|rx4" >&2; exit 2 ;;
esac
wt=$ARMS/$kind-$rev
[ -d "$wt" ] || git -C "$repo" worktree add --detach "$wt" "$rev" >/dev/null
src=$ARMS/drv-$kind-$rev
rm -rf "$src"; mkdir -p "$src"; cp -R "$HERE/$kind/src" "$src/"
sed "s|@$key@|$wt|" "$HERE/$kind/Cargo.toml.in" > "$src/Cargo.toml"
cp "$HERE/../../rust-toolchain.toml" "$src/"
cd "$src"
CARGO_TARGET_DIR=$ARMS/target-$kind-$rev cargo build --release --quiet
cp "$ARMS/target-$kind-$rev/release/$kind-driver" "$out"
git -C "$repo" rev-parse HEAD >/dev/null
echo "$kind $rev -> $out ($(shasum -a 256 "$out" | cut -c1-16)) rustc $(rustc --version | cut -d' ' -f2)"
