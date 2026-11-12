#!/usr/bin/env bash
# sk-v27 LW3.RX-1a falsifier: Route A on master.
#
# Usage: rx1a-falsifier.sh <parse-that checkout> <log dir> <label>
#   corpus: rust/regex/tests/oracle_leaf_corpus.rs reads the 133 DFA-tier
#           leaves of oracle/leaf-corpus.json (21,722 cases, V8+JSC answers)
#           through the engine; GREEN needs >= 121 AGREE with every ENGINE
#           leaf named as owed to LW3.RX-1d;
#   abi:    tests/abi_fingerprint.rs (Hir ABI v1 unchanged);
#   grep:   git grep -niE bbnf -- rust/regex reads 0.
set -u
repo=$1; logs=$2; label=$3
mkdir -p "$logs"
guard=~/.claude/guard/guarded
# Under one outer `guarded` run (GUARD_INNER=1) the steps run in its tree.
if [ -n "${GUARD_INNER:-}" ]; then guard=$(cd "$(dirname "$0")" && pwd)/inner-run.sh; fi
export CARGO_TARGET_DIR=${CARGO_TARGET_DIR:-$HOME/.dev-logs/bbnf-lockstep/lw3/target-parse-that}
cd "$repo" || exit 2
sha=$(git rev-parse --short=9 HEAD)
lines=$("$guard" --light -- git grep -niE bbnf -- rust/regex 2>/dev/null | grep -vc '^\[guarded\]')
cd rust || exit 2
"$guard" --timeout 60 -- cargo test -p parse-that-regex --locked --test oracle_leaf_corpus -- --nocapture >"$logs/$label-corpus.log" 2>&1
corpus=$?
"$guard" --timeout 30 -- cargo test -p parse-that-regex --locked --test abi_fingerprint >"$logs/$label-abi.log" 2>&1
abi=$?
verdict=RED
[ "$corpus" = 0 ] && [ "$abi" = 0 ] && [ "$lines" = 0 ] && verdict=GREEN
echo "rx1a $label @ $sha: corpus rc=$corpus; abi rc=$abi; grep bbnf lines=$lines -> $verdict"
grep -E '^oracle leaf corpus|^ENGINE|panicked|assertion|error(\[|:)' "$logs/$label-corpus.log" | head -40
[ "$verdict" = GREEN ]
