#!/usr/bin/env bash
# sk-v27 LW3.RX-1c falsifier, bbnf-lang half: the emitter prints the
# engine's leftmost-first tables and carries no match-mode choice.
#
# Usage: rx1c-bl-falsifier.sh <bbnf-lang checkout> <log dir> <label>
#   grep:  `MatchMode|emit_dfa_shortest|contains_lazy_quantifier` in crates/ = 0;
#   pin:   the parse-that rev the workspace pins;
#   tests: crates/core/tests/dfa_fidelity.rs (the lazy star stops at the
#          first close; the CSS grammar's DFA leaves agree with the regex
#          crate on tailwind.css) and rx4_rust_fusion_absent (fusion stays
#          off for the Rust leaf until RX-1h's certificate), plus the
#          self-hosted `regex` leaf read leftmost-first (dfa_fidelity), so
#          grammar/misc/json-commented.bbnf parses: its self-parity pair
#          and its ruled rows in separated_list_audit;
#   regen: `cargo xtask regen --check` (9/9 clean).
set -u
repo=$1; logs=$2; label=$3
mkdir -p "$logs"
guard=~/.claude/guard/guarded
# Under one outer `guarded` run (GUARD_INNER=1) the steps run in its tree.
if [ -n "${GUARD_INNER:-}" ]; then guard=$(cd "$(dirname "$0")" && pwd)/inner-run.sh; fi
. ~/.dev-logs/bbnf-lockstep/lw3/s4-rx/env-bl.sh
cd "$repo" || exit 2
sha=$(git rev-parse --short=9 HEAD)
grep_lines=$(git grep -nE 'MatchMode|emit_dfa_shortest|contains_lazy_quantifier' -- 'crates/**/*.rs' | tee "$logs/$label-grep.log" | wc -l | tr -d ' ')
pin=$(grep -oE 'parse_that +=.*rev = "[0-9a-f]+"' Cargo.toml | grep -oE '[0-9a-f]{40}')
"$guard" --timeout 85 -- cargo test -p bbnf --locked --test dfa_fidelity --test rx4_rust_fusion_absent --test separated_list_audit --test bbnf_self_parity >"$logs/$label-tests.log" 2>&1
tests=$?
"$guard" --timeout 85 -- cargo xtask regen --check >"$logs/$label-regen.log" 2>&1
regen=$?
verdict=RED
[ "$grep_lines" = 0 ] && [ "$tests" = 0 ] && [ "$regen" = 0 ] && verdict=GREEN
echo "rx1c-bl $label @ $sha (pt pin ${pin:0:9}): grep lines=$grep_lines; tests rc=$tests; regen --check rc=$regen -> $verdict"
grep -hE '^test .*(FAILED|ok)$|panicked at|left:|right:|^test result' "$logs/$label-tests.log" | head -20
grep -vE '^\s+(Compiling|Running|Finished|Fresh|Blocking|Downloaded|Updating)' "$logs/$label-regen.log" | tail -12
[ "$verdict" = GREEN ]
