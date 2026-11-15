#!/bin/sh
# sk-v27 LW3.AUDIT-CLEAN (RULINGS 2026-11-14k CLEAN-PROFILE): the one clean every gate uses (TGT-1).
#
# `cargo clean -p <pkg>` with no --profile cleans only the dev profile (target/debug). A read under
# `--profile ax-iter` (every `cargo iter-*` alias, `cargo xtask`, `--cargo-profile ax-iter`) keeps its
# library, and with the shared CARGO_TARGET_DIR a workspace member's artifacts are keyed by package and
# workspace-relative path, not by worktree: a stale library built in another worktree (or a mutant's)
# is reused whenever this worktree's sources are older than it. So every gate cleans the touched
# packages in EVERY profile its read uses, and proves the clean: no library of a cleaned package is
# left in that profile's deps, and `hash` prints each library's sha256 and mtime after the build.
#
# Usage:
#   tgt-clean.sh clean <worktree> <profiles,comma-separated> <pkg>...   e.g. clean . dev,ax-iter bbnf gorgeous
#   tgt-clean.sh hash  <profile> <crate_name>...                         e.g. hash ax-iter bbnf gorgeous
#   tgt-clean.sh all   <worktree> <profiles>   (every workspace member, from `cargo metadata`)
# Exit 0 iff every clean removed every library of every named package; prints one row per
# (profile, package): `clean <profile> <pkg> removed=<n> left=<n>`.
set -u
export CARGO_TARGET_DIR=${CARGO_TARGET_DIR:-$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang}
pdir() { case "$1" in dev|test) echo debug;; release|bench) echo release;; *) echo "$1";; esac; }
libs() { # <profile> <crate_name>: every library artifact of the crate in that profile
  ls "$CARGO_TARGET_DIR/$(pdir "$1")/deps/lib$(echo "$2" | tr - _)-"*.rlib \
     "$CARGO_TARGET_DIR/$(pdir "$1")/deps/lib$(echo "$2" | tr - _)-"*.rmeta "$CARGO_TARGET_DIR/$(pdir "$1")/deps/lib$(echo "$2" | tr - _)-"*.dylib 2>/dev/null; }
cmd=$1; shift
case "$cmd" in
hash)
  prof=$1; shift
  for c in "$@"; do
    for f in $(libs "$prof" "$c"); do
      case "$f" in *.rlib|*.dylib)
        echo "hash $prof $c $(shasum -a 256 "$f" | cut -c1-16) mtime=$(stat -f %Sm -t %Y-%m-%dT%H:%M:%S "$f") $(basename "$f")";;
      esac
    done
  done ;;
clean|all)
  wt=$1; profs=$2; shift 2
  cd "$wt" || exit 2
  if [ "$cmd" = all ]; then
    set -- $(cargo metadata --locked --offline --no-deps --format-version 1 | python3 -I -c 'import json,sys; print(" ".join(p["name"] for p in json.load(sys.stdin)["packages"]))')
  fi
  bad=0
  for prof in $(echo "$profs" | tr , ' '); do
    for p in "$@"; do
      out=$(cargo clean -p "$p" --profile "$prof" 2>&1 | tail -1)
      n=$(echo "$out" | grep -oE 'Removed [0-9,]+ files' | grep -oE '[0-9,]+' | tr -d ,)
      lib=$(cargo metadata --locked --offline --no-deps --format-version 1 | python3 -I -c '
import json,sys
p=[x for x in json.load(sys.stdin)["packages"] if x["name"]==sys.argv[1]]
t=[t["name"] for x in p for t in x["targets"] if "lib" in t["kind"] or "proc-macro" in t["kind"] or "rlib" in t["kind"]]
print(t[0] if t else "")' "$p")
      left=0; [ -n "$lib" ] && left=$(libs "$prof" "$lib" | grep -c .)
      echo "clean $prof $p removed=${n:-0} left=$left"
      [ "$left" -eq 0 ] || bad=1
    done
  done
  exit $bad ;;
*) echo "usage: tgt-clean.sh clean|all|hash ..." >&2; exit 2 ;;
esac
