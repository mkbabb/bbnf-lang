#!/usr/bin/env bash
# sk-v27 LW3.RX-1c falsifier, parse-that half: the leftmost-first priority cut.
#
# Usage: rx1c-falsifier.sh <parse-that checkout> <log dir> <label>
#   alt:   born_red_lw1::alt_fusion_is_leftmost_first (`a|ab` on "ab" = 1),
#          read with --include-ignored so an #[ignore] cannot hide it, and
#          GREEN only when the attribute is gone;
#   lazy:  the five lazy-quantifier tests of parse_that tests/regex/dfa_test.rs
#          (oracle answers);
#   S4-B:  parse_that tests/regex_empty_match_test.rs (a zero-length match is
#          a match) holds;
#   api:   no `find_at_shortest`, and no public semantics switch
#          (`MatchKind|leftmost_longest|longest` in a `pub` item) in rust/regex/src.
set -u
repo=$1; logs=$2; label=$3
mkdir -p "$logs"
guard=~/.claude/guard/guarded
. ~/.dev-logs/bbnf-lockstep/lw3/s4-rx/env-pt.sh
cd "$repo" || exit 2
sha=$(git rev-parse --short=9 HEAD)
ignored=$(grep -B1 -c 'fn alt_fusion_is_leftmost_first' rust/regex/tests/born_red_lw1.rs >/dev/null; grep -B1 'fn alt_fusion_is_leftmost_first' rust/regex/tests/born_red_lw1.rs | grep -cE '^\s*#\[ignore')
shortest=$(git grep -n 'find_at_shortest' -- 'rust/**/*.rs' | wc -l | tr -d ' ')
switch=$(git grep -nE 'pub .*(MatchKind|leftmost_longest|longest)' -- rust/regex/src | wc -l | tr -d ' ')
cd rust || exit 2
"$guard" --timeout 60 -- cargo test -p parse-that-regex --locked --test born_red_lw1 alt_fusion_is_leftmost_first -- --include-ignored >"$logs/$label-alt.log" 2>&1
alt=$?
"$guard" --timeout 60 -- cargo test -p parse_that --locked --test regex -- dfa_test::lazy dfa_test::css_block_comment >"$logs/$label-lazy.log" 2>&1
lazy=$?
"$guard" --timeout 60 -- cargo test -p parse_that --locked --test regex_empty_match_test >"$logs/$label-s4b.log" 2>&1
s4b=$?
verdict=RED
[ "$alt" = 0 ] && [ "$ignored" = 0 ] && [ "$lazy" = 0 ] && [ "$s4b" = 0 ] && [ "$shortest" = 0 ] && [ "$switch" = 0 ] && verdict=GREEN
echo "rx1c $label @ $sha: alt rc=$alt (ignore attrs $ignored); lazy rc=$lazy; S4-B rc=$s4b; find_at_shortest lines=$shortest; pub switch lines=$switch -> $verdict"
grep -hE '^test .*(FAILED|ok)$|panicked at|left:|right:' "$logs/$label-alt.log" "$logs/$label-lazy.log" | head -24
grep -E '^test result' "$logs/$label-s4b.log"
[ "$verdict" = GREEN ]
