#!/usr/bin/env bash
# LW2.TOPO-2 falsifier (S4 TOOLCHAIN): the LW1.PERF-2 command from a clean clone.
#   usage: s4-topo2-stable-plane.sh <bbnf-lang|parse-that|pprint> <rev> <tag>
# Clones https://github.com/mkbabb/<repo> (credential-free) at <rev>. In the
# cargo workspace, on the pinned stable toolchain (TOOLCHAIN, default 1.99.1:
# the rust-stable row of bench/W0-BENCH-PINS.tsv v2), it runs
#   cargo +$TOOLCHAIN build --release
#   cargo +$TOOLCHAIN test --workspace --locked --no-fail-fast
# after `cargo xtask corpus fetch` where the workspace has that verb (TOPO-6:
# CI fetches the corpus the same way). The config read: every `rustc-wrapper`
# or `-Z` match in .cargo/config.toml must lie between the `nightly alias
# family` BEGIN/END markers. No wrapper and no RUSTFLAGS from the caller.
# Prints one verdict line; exit 0 = GREEN, 1 = RED.
set -u
repo=$1 rev=$2 tag=$3
TOOLCHAIN=${TOOLCHAIN:-1.99.1}
root=${S4_ROOT:-$HOME/.dev-logs/bbnf-lockstep/lw2/S4-TOOLCHAIN}
dir=$root/clones/$repo-$tag
log=$root/topo2-$repo-$tag.log
rm -rf "$dir"; mkdir -p "$root/clones"
{
  echo "# $(date -u +%FT%TZ) repo=$repo rev=$rev toolchain=$TOOLCHAIN load=$(sysctl -n vm.loadavg)"
  GIT_TERMINAL_PROMPT=0 git -c credential.helper= clone -q --no-checkout "https://github.com/mkbabb/$repo" "$dir" &&
    git -C "$dir" checkout -q --detach "$rev" || { echo "VERDICT RED clone-failed"; exit 1; }
  git -C "$dir" log -1 --format='# head %H %s'
  ws=$dir; [ -f "$dir/rust/Cargo.toml" ] && ws=$dir/rust
  export CARGO_TARGET_DIR=$dir/target
  unset RUSTC_WRAPPER RUSTFLAGS CARGO_BUILD_RUSTC_WRAPPER CARGO_ENCODED_RUSTFLAGS
  cd "$ws" || exit 1
  cargo +"$TOOLCHAIN" build --release > "$dir.build.log" 2>&1; b=$?
  grep -E '^error|^  = note|^error\[' "$dir.build.log" | head -12
  echo "# build rc=$b"
  if [ -f corpus/MANIFEST.sha256 ] || [ -f ../corpus/MANIFEST.sha256 ]; then
    cargo +"$TOOLCHAIN" xtask corpus fetch > "$dir.corpus.log" 2>&1; echo "# corpus fetch rc=$?"
  fi
  cargo +"$TOOLCHAIN" test --workspace --locked --no-fail-fast > "$dir.test.log" 2>&1; t=$?
  grep -E '^error(\[|:)' "$dir.test.log" | sort -u | head -12
  grep -E '^test result:' "$dir.test.log" | awk '{p+=$4; f+=$6} END {print "# tests passed=" p " failed=" f}'
  grep -E '^test .* \.\.\. FAILED$' "$dir.test.log" | sed 's/^/# FAILED /' | head -40
  echo "# test rc=$t"
  cfg=$dir/.cargo/config.toml; stray=0
  if [ -f "$cfg" ]; then
    stray=$(awk '/nightly alias family: BEGIN/{f=1} /nightly alias family: END/{f=0;next} !f && /rustc-wrapper|-Z/{print}' "$cfg" | tee /dev/stderr | wc -l | tr -d ' ')
    # Structural read of the same file: every parsed `-Z` value and every
    # `rustc-wrapper` key must sit under alias.nightly-* (a block appended
    # after a [target.*] header parses as target keys; the line read alone
    # cannot see that).
    misplaced=$(python3 - "$cfg" <<'PY'
import sys, tomllib
bad = []
def walk(node, path):
    if isinstance(node, dict):
        for k, v in node.items():
            p = path + [k]
            if k == "rustc-wrapper":
                bad.append(".".join(p))
            walk(v, p)
    elif isinstance(node, list):
        for v in node:
            walk(v, path)
    elif isinstance(node, str) and "-Z" in node:
        if not (len(path) == 2 and path[0] == "alias" and path[1].startswith("nightly-")):
            bad.append(".".join(path))
walk(tomllib.load(open(sys.argv[1], "rb")), [])
for b in sorted(set(bad)):
    print(b, file=sys.stderr)
print(len(set(bad)))
PY
)
    echo "# parsed -Z / rustc-wrapper outside alias.nightly-*: $misplaced"
    stray=$((stray + misplaced))
  fi
  echo "# config matches outside the +nightly alias family: $stray"
  if [ "$b" = 0 ] && [ "$t" = 0 ] && [ "$stray" = 0 ]; then echo "VERDICT GREEN build=$b test=$t stray=$stray"; v=0
  else echo "VERDICT RED build=$b test=$t stray=$stray"; v=1; fi
  mv "$dir.build.log" "$dir.test.log" "$root/" 2>/dev/null; rm -rf "$dir" "$dir.corpus.log"
  exit $v
} 2>&1 | tee "$log"
exit "${PIPESTATUS[0]}"
