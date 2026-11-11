#!/bin/sh
# LW2.PERF-1b parse-that reads. Usage: perf1b-pt-reads.sh <parse-that tree> <fuzz inputs>
cd "${1:?tree}/rust" || exit 2; N=${2:-100000}
export CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/target-parse-that PARSE_THAT_CORPUS_DIR=$PWD/../data
echo "tree $(git rev-parse --short HEAD) (+ $(git status --short | wc -l | tr -d ' ') working paths)"
T=$(mktemp -d "$HOME/.dev-logs/bbnf-lockstep/lw2/s6-perf/r3/.tmp.XXXX")
sum() { grep -E 'test result' $T/o | awk '{p+=$4; f+=$6} END {print "  passed "p" failed "f}'; }
cargo +1.99.1 test -p parse-that-simd --locked --no-fail-fast >$T/o 2>&1; echo "native suites rc=$?"; grep -E 'Running|test result' $T/o | paste - - | sed -E 's/.*Running ([^ ]+).*test result: ([^.]*)\. ([0-9]+ passed; [0-9]+ failed).*/  \1 \2 \3/'; grep -E 'Doc-tests' -A99 $T/o | grep 'test result' | sed 's/^/  doctests /'
PARSE_THAT_SIMD_FUZZ_INPUTS=$N cargo +1.99.1 test -p parse-that-simd --locked --release --test prim_differential -- --nocapture --test-threads 1 >$T/o 2>&1; echo "prim_differential (native, release) rc=$?"; grep -o 'prim_differential kernel=.*' $T/o | sed 's/^/  /'
PARSE_THAT_SCAN_TIER=scalar cargo +1.99.1 test -p parse-that-simd --locked --release --test prim_differential -- --nocapture --test-threads 1 >$T/o 2>&1; echo "prim_differential (PARSE_THAT_SCAN_TIER=scalar) rc=$?"; grep -o 'prim_differential kernel=[a-z0-9_]* tier=[a-z0-9]*' $T/o | sort -u | sed 's/^/  /'
cargo +1.99.1 test -p parse-that-simd --locked --no-fail-fast --target x86_64-apple-darwin >$T/o 2>&1; echo "x86_64 (Rosetta) suites rc=$?"; sum
PARSE_THAT_SIMD_FUZZ_INPUTS=$N cargo +1.99.1 test -p parse-that-simd --locked --release --target x86_64-apple-darwin --test prim_differential -- --nocapture >$T/o 2>&1; echo "prim_differential (x86_64 Rosetta, release) rc=$?"; grep -o 'prim_differential kernel=[a-z0-9_]* tier=[a-z0-9]*.*divergences=[0-9]*' $T/o | sed 's/^/  /'
export CARGO_TARGET_WASM32_WASIP1_RUNNER="node --no-warnings $PWD/scripts/wasi-run.mjs"
RUSTFLAGS="-C target-feature=+simd128" cargo +1.99.1 test -p parse-that-simd --locked --no-fail-fast --target wasm32-wasip1 >$T/o 2>&1; echo "wasm32+simd128 suites rc=$?"; sum
cargo +1.99.1 test -p xtask --test public_api_callers --locked >$T/o 2>&1; echo "public_api_callers rc=$?"; grep 'test result' $T/o | sed 's/^/  /'
cargo +1.99.1 clippy -p parse-that-simd -p parse_that --all-targets --locked -- -D warnings >$T/o 2>&1; echo "clippy native rc=$?"
cargo +1.99.1 clippy -p parse-that-simd --all-targets --locked --target x86_64-apple-darwin -- -D warnings >$T/o 2>&1; echo "clippy x86_64 rc=$?"; grep -E '^(warning|error)' $T/o | head -3
RUSTFLAGS="-C target-feature=+simd128" cargo +1.99.1 clippy -p parse-that-simd --all-targets --locked --target wasm32-wasip1 -- -D warnings >$T/o 2>&1; echo "clippy wasm32+simd128 rc=$?"; grep -E '^(warning|error)' $T/o | head -3
cargo +1.99.1 check -p parse-that-simd --locked --target wasm32-wasip1 >$T/o 2>&1; echo "check wasm32 (no simd128) rc=$?"
rm -rf $T
