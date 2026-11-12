#!/bin/sh
# usage asan.sh <log>: the kernel laws under AddressSanitizer (nightly, build-std)
export CARGO_TARGET_DIR=/Users/mkbabb/.dev-logs/bbnf-lockstep/lw3/target-parse-that
export RUSTFLAGS=-Zsanitizer=address RUSTDOCFLAGS=-Zsanitizer=address
cd /Users/mkbabb/Programming/parse-that-wt/lw3-s6-kernel/rust || exit 2
{ echo "# head $(git rev-parse HEAD)"; echo "# date $(date -Iseconds)"; } > $1
cargo +nightly-2026-04-11 test -Zbuild-std --target aarch64-apple-darwin -p parse_that --locked --test kernel --test kernel_allocation --test kernel_regex --test kernel_abi >> $1 2>&1
echo "# rc $?" >> $1
