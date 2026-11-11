#!/bin/sh
# LW2.PERF-3 falsifier read (v2: records cargo rc per test). Usage: perf3-read2.sh
cd ~/Programming/parse-that-wt/lw2-s6-perf/rust || exit 2
echo "head $(git rev-parse --short HEAD)"
unset RUST_MIN_STACK
cargo test -p parse_that --test css_parse_test -- test_nested_media --exact >/tmp/.p3a 2>&1; rc=$?
grep -E "overflow|SIGABRT|test result" /tmp/.p3a; echo "test_nested_media rc=$rc"
cargo test -p parse_that --test css_stack_depth -- nested_at_rules_depth_16_default_stack --exact >/tmp/.p3a 2>&1; rc=$?
grep -E "overflow|SIGABRT|test result" /tmp/.p3a; echo "depth_16_default_stack rc=$rc"
cargo test -p parse_that --test css_stack_depth -- nested_at_rule_stack_slope_is_constant --exact --nocapture >/tmp/.p3a 2>&1; rc=$?
grep -E "^[0-9]+	|record|panicked|test result" /tmp/.p3a; echo "slope rc=$rc"
rm -f /tmp/.p3a
