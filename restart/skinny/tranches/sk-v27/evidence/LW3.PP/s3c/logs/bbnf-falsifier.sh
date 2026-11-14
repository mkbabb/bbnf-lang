#!/bin/zsh
# S3CPP-B falsifier read (gorgeous bbnf tests + G3). $1 = "clean" to cargo clean -p first.
set -u
cd ~/Programming/bbnf-lang-wt/lw3-s10
[[ "${1:-}" == clean ]] && cargo clean -p bbnf -p gorgeous
cargo test -p gorgeous --features bbnf-grammar --test bbnf -- bbnf_line_comment_inside_a_rule_is_read_and_kept bbnf_prettify_refuses_unconsumed_input 2>&1 | grep -E "^test |test result|^error|panicked" ; echo "bbnf rc=${pipestatus[1]}"
cargo test -p gorgeous --features bin-full --test g3_trivia 2>&1 | grep -E "^test |test result|^error|panicked|FAIL|fail" | head -20; echo "g3 rc=${pipestatus[1]}"
