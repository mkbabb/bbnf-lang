#!/usr/bin/env bash
# sk-v27 LW3.RX-1d falsifier, bbnf-lang half: the u/v engine is pinned and
# a regex leaf whose reading differs without `u` is linted BBNF-RX-NOU.
#
# Usage: rx1d-bl-falsifier.sh <bbnf-lang checkout> <log dir> <label>
#   pin:   the parse-that rev the workspace pins carries the engine's lint
#          (rust/regex/src/lint.rs at that rev);
#   lint:  crates/analysis tests/regex_reading.rs (BBNF-RX-NOU, the engine's
#          RX-NOU under the BBNF namespace);
#   regen: `cargo xtask regen --check` (the nine products, 9/9 clean, on the
#          pinned engine).
set -u
repo=$1; logs=$2; label=$3
mkdir -p "$logs"
guard=~/.claude/guard/guarded
# Under one outer `guarded` run (GUARD_INNER=1) the steps run in its tree.
if [ -n "${GUARD_INNER:-}" ]; then guard=$(cd "$(dirname "$0")" && pwd)/inner-run.sh; fi
. ~/.dev-logs/bbnf-lockstep/lw3/s4-rx/env-bl.sh
cd "$repo" || exit 2
sha=$(git rev-parse --short=9 HEAD)
pin=$(grep -oE 'parse_that +=.*rev = "[0-9a-f]+"' Cargo.toml | grep -oE '[0-9a-f]{40}')
git -C ~/Programming/parse-that cat-file -e "$pin:rust/regex/src/lint.rs" 2>/dev/null
pinrc=$?
"$guard" --timeout 60 -- cargo test -p bbnf-analysis --locked --test regex_reading >"$logs/$label-lint.log" 2>&1
lint=$?
"$guard" --timeout 85 -- cargo xtask regen --check >"$logs/$label-regen.log" 2>&1
regen=$?
verdict=RED
[ "$pinrc" = 0 ] && [ "$lint" = 0 ] && [ "$regen" = 0 ] && verdict=GREEN
echo "rx1d-bl $label @ $sha (pt pin ${pin:0:9}): pin has lint rc=$pinrc; lint rc=$lint; regen --check rc=$regen -> $verdict"
grep -hE '^test .*(FAILED|ok)$|panicked at|left:|right:|^error' "$logs/$label-lint.log" | head -6
grep -hE 'regen --check|drift' "$logs/$label-regen.log" | head -3
[ "$verdict" = GREEN ]
