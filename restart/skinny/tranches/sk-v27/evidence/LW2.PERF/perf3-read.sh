#!/bin/sh
# LW2.PERF-3 falsifier read: debug test_nested_media, depth-16 on the default
# test-thread stack, and the depth sweep. Usage: perf3-read.sh <tag>
cd ~/Programming/parse-that-wt/lw2-s6-perf/rust || exit 2
echo "head $(git rev-parse --short HEAD)"
unset RUST_MIN_STACK
cargo test -p parse_that --test css_parse_test -- test_nested_media --exact 2>&1 | grep -E "overflow|SIGABRT|test result" ; echo "nested_media rc=$?"
cargo test -p parse_that --test css_stack_depth -- nested_at_rules_depth_16_default_stack --exact 2>&1 | grep -E "overflow|SIGABRT|test result"
cargo test -p parse_that --test css_stack_depth -- nested_at_rule_stack_slope_is_constant --exact --nocapture 2>&1 | grep -E "^[0-9]+	|record|panicked|test result"
