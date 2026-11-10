#!/bin/sh
# LW2.PERF-3 (RULINGS-2) read: css_parse_test::test_parse_bootstrap_css and
# test_nested_media in debug at the default test-thread stack, plus the
# smallest RUST_MIN_STACK (64 KiB steps, binary search) at which each passes.
# Usage: perf3-bootstrap-read.sh <parse-that tree>
cd "${1:?tree}/rust" || exit 2
export CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/target-parse-that
export PARSE_THAT_CORPUS_DIR=$HOME/Programming/parse-that-wt/lw2-s6-perf/data
echo "head $(git rev-parse --short HEAD)"
T=$(mktemp -d "$HOME/.dev-logs/bbnf-lockstep/lw2/s6-perf/r3/.tmp.XXXX")
unset RUST_MIN_STACK
cargo +1.99.1 test -p parse_that --test css_parse_test --no-run >$T/b 2>&1 || { tail -5 $T/b; exit 3; }
BIN=$(ls -t $CARGO_TARGET_DIR/debug/deps/css_parse_test-* | grep -v '\.d$' | head -1)
for t in test_parse_bootstrap_css test_nested_media; do
  cargo +1.99.1 test -p parse_that --test css_parse_test -- $t --exact >$T/o 2>&1; rc=$?
  grep -E "overflow|SIGABRT|test result" $T/o | sed 's/^/  /'; echo "$t rc=$rc"
  lo=1; hi=512   # units of 64 KiB: 64 KiB .. 32 MiB
  while [ $lo -lt $hi ]; do mid=$(( (lo+hi)/2 ))
    if RUST_MIN_STACK=$((mid*65536)) "$BIN" $t --exact >/dev/null 2>&1 </dev/null; then hi=$mid; else lo=$((mid+1)); fi
  done
  echo "$t min_passing_stack_bytes=$((lo*65536))"
done
rm -rf $T
