#!/bin/zsh
# S3CPP-L3 read: every gorgeous bbnf test (incl. the edge tests) + G3. $1 = "clean" to cargo clean -p first.
set -u
cd ~/Programming/bbnf-lang-wt/lw3-s10
[[ "${1:-}" == clean ]] && cargo clean -p bbnf -p gorgeous
cargo test -p gorgeous --features bbnf-grammar --test bbnf 2>&1 | grep -E "^test |test result|^error|panicked" ; echo "bbnf rc=${pipestatus[1]}"
cargo test -p gorgeous --features bin-full --test g3_trivia 2>&1 | grep -E "^test |test result|^error|panicked|FAIL|fail" | head -20; echo "g3 rc=${pipestatus[1]}"
