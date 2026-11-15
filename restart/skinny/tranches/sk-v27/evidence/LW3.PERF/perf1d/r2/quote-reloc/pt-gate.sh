#!/bin/sh
set -u
L=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/quote-reloc
export CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-parse-that PARSE_THAT_CORPUS_DIR=$HOME/Programming/parse-that/data
cd $HOME/Programming/parse-that-wt/lw3-s9b-q/rust || exit 2
cargo test --workspace --no-fail-fast > $L/pt-test.log 2>&1; echo "rc=$?" >> $L/pt-test.log
PARSE_THAT_SCAN_TIER=scalar cargo test -p parse_that --test quote_or_backslash_test > $L/pt-test-scalar-tier.log 2>&1; echo "rc=$?" >> $L/pt-test-scalar-tier.log
cargo clippy --workspace --all-targets -- -D warnings > $L/pt-clippy.log 2>&1; echo "rc=$?" >> $L/pt-clippy.log
