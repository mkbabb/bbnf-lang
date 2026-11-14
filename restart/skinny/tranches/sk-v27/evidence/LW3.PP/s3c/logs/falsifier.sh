#!/bin/zsh
# S3c-PP falsifier read: css + ebnf falsifiers and the two multi-rule tests. $1 = "clean" to cargo clean -p first.
set -u
cd ~/Programming/bbnf-lang-wt/lw3-s10
[[ "${1:-}" == clean ]] && cargo clean -p bbnf -p gorgeous
cargo test -p gorgeous --features css-grammar --test css css_ws_leaves_are_trivia_so_formatting_is_idempotent 2>&1 | grep -E "^test |test result|error|panicked" ; echo "css rc=${pipestatus[1]}"
cargo test -p gorgeous --features ebnf-grammar --test ebnf -- ebnf_prettify_keeps_every_rule_and_terminal test_prettify_multi_rule 2>&1 | grep -E "^test |test result|error|panicked"; echo "ebnf rc=${pipestatus[1]}"
cargo test -p bbnf --test ebnf_prettify prettify_multi_rule 2>&1 | grep -E "^test |test result|error|panicked"; echo "core rc=${pipestatus[1]}"
