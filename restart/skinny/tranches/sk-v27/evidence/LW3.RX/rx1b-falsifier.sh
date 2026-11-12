#!/usr/bin/env bash
# sk-v27 LW3.RX-1b falsifier: Look as an automaton condition.
#
# Usage: rx1b-falsifier.sh <parse-that checkout> <log dir> <label>
#   wb:     word_boundary::engine_reproduces_the_oracle (15 of 15 A-9 cases);
#   look:   look_conditions (27 anchor and boundary cases at offsets);
#   leaf:   parse_that regex_look_context_test (the leaf reads look-behind);
#   end:    oracle_leaf_corpus (the `$` leaves #1 and #51 among the DFA tier);
#   oracle: oracle.mjs on smoke.json and wordboundary.json: V8 and JSC
#           self-consistent and agreeing.
set -u
repo=$1; logs=$2; label=$3
mkdir -p "$logs"
guard=~/.claude/guard/guarded
. ~/.dev-logs/bbnf-lockstep/lw3/s4-rx/env-pt.sh
cd "$repo" || exit 2
sha=$(git rev-parse --short=9 HEAD); tree=$(git write-tree 2>/dev/null || echo -)
cd rust || exit 2
"$guard" --timeout 60 -- cargo test -p parse-that-regex --locked --no-fail-fast --test word_boundary --test look_conditions --test oracle_leaf_corpus -- --nocapture >"$logs/$label-regex.log" 2>&1
rx=$?
"$guard" --timeout 60 -- cargo test -p parse_that --locked --test regex_look_context_test >"$logs/$label-leaf.log" 2>&1
leaf=$?
"$guard" --light --timeout 10 -- node regex/oracle/oracle.mjs --corpus regex/oracle/smoke.json --out "$logs/$label-oracle" >"$logs/$label-oracle-smoke.log" 2>&1
o1=$?
"$guard" --light --timeout 10 -- node regex/oracle/oracle.mjs --corpus regex/oracle/wordboundary.json --out "$logs/$label-oracle" >"$logs/$label-oracle-wb.log" 2>&1
o2=$?
verdict=RED
[ "$rx" = 0 ] && [ "$leaf" = 0 ] && [ "$o1" = 0 ] && [ "$o2" = 0 ] && verdict=GREEN
echo "rx1b $label @ $sha (tree $tree): regex tests rc=$rx; leaf rc=$leaf; oracle smoke rc=$o1, wb rc=$o2 -> $verdict"
grep -hE '^test .*FAILED|^anchors:|^DIFFER|^oracle leaf corpus|ENGINE value.js@8d7bb5b8#(1|51) |refused|panicked at' "$logs/$label-regex.log" "$logs/$label-leaf.log" | head -40
grep -hE '^oracle ' "$logs/$label-oracle-smoke.log" "$logs/$label-oracle-wb.log"
[ "$verdict" = GREEN ]
