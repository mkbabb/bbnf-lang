#!/bin/sh
# PERF1D-B2 (a): parse-that gate on lw3/s9b-perf-ct (compile-time body selection).
set -u
L=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/r3/ct
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-parse-that PARSE_THAT_CORPUS_DIR=$HOME/Programming/parse-that/data
cd $HOME/Programming/parse-that-wt/lw3-s9b-ct/rust || exit 2
echo "head=$(git rev-parse HEAD) dirty=$(git status --short | wc -l)" > $L/pt-head.txt
cargo test --workspace --no-fail-fast > $L/pt-test.log 2>&1; echo "rc=$?" >> $L/pt-test.log
PARSE_THAT_SCAN_TIER=scalar cargo test -p parse_that --test quote_or_backslash_test > $L/pt-test-scalar-tier.log 2>&1; echo "rc=$?" >> $L/pt-test-scalar-tier.log
cargo test -p parse_that --features parse-that-simd/force-scalar --test quote_or_backslash_test > $L/pt-test-force-scalar.log 2>&1; echo "rc=$?" >> $L/pt-test-force-scalar.log
cargo clippy --workspace --all-targets -- -D warnings > $L/pt-clippy.log 2>&1; echo "rc=$?" >> $L/pt-clippy.log
