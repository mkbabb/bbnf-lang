#!/bin/sh
# LW2.PERF-1 parse-that reads. Usage: perf1-pt-reads.sh <parse-that tree> <fuzz inputs>
cd "${1:?tree}/rust" || exit 2; N=${2:-100000}
echo "tree $(git rev-parse --short HEAD) (+ $(git status --short | wc -l | tr -d ' ') working paths)"
T=$(mktemp -d)
cargo +1.99.1 test -p parse-that-simd --locked --no-fail-fast >$T/o 2>&1; echo "native suites rc=$?"; grep -E 'Running|test result' $T/o | paste - - | sed -E 's/.*Running ([^ ]+).*test result: ([^.]*)\. ([0-9]+ passed; [0-9]+ failed).*/  \1 \2 \3/'; grep -E 'Doc-tests' -A99 $T/o | grep 'test result' | sed 's/^/  doctests /'
PARSE_THAT_SIMD_FUZZ_INPUTS=$N cargo +1.99.1 test -p parse-that-simd --locked --release --test differential -- --nocapture >$T/o 2>&1; echo "differential (native, release) rc=$?"; grep -o 'differential tier=.*' $T/o | sed 's/^/  /'
cargo +1.99.1 test -p parse-that-simd --locked --no-fail-fast --target x86_64-apple-darwin --target-dir target/x86 >$T/o 2>&1; echo "x86_64 (Rosetta) suites rc=$?"; grep -E 'test result' $T/o | awk '{p+=$4; f+=$6} END {print "  passed "p" failed "f}'
PARSE_THAT_SIMD_FUZZ_INPUTS=$N cargo +1.99.1 test -p parse-that-simd --locked --release --target x86_64-apple-darwin --target-dir target/x86 --test differential -- --nocapture >$T/o 2>&1; echo "differential (x86_64 Rosetta, release) rc=$?"; grep -o 'differential tier=.*' $T/o | sed 's/^/  /'
export CARGO_TARGET_WASM32_WASIP1_RUNNER="node --no-warnings $PWD/scripts/wasi-run.mjs" PARSE_THAT_CORPUS_DIR=$PWD/../data
RUSTFLAGS="-C target-feature=+simd128" cargo +1.99.1 test -p parse-that-simd --locked --no-fail-fast --target wasm32-wasip1 --target-dir target/wasm >$T/o 2>&1; echo "wasm32+simd128 suites rc=$?"; grep -E 'test result' $T/o | awk '{p+=$4; f+=$6} END {print "  passed "p" failed "f}'
RUSTFLAGS="-C target-feature=+simd128" PARSE_THAT_SIMD_FUZZ_INPUTS=$N cargo +1.99.1 test -p parse-that-simd --locked --release --target wasm32-wasip1 --target-dir target/wasm --test differential -- --nocapture >$T/o 2>&1; echo "differential (wasm32+simd128, release) rc=$?"; grep -o 'differential tier=.*' $T/o | sed 's/^/  /'
cargo +1.99.1 test -p xtask --test public_api_callers --locked >$T/o 2>&1; echo "public_api_callers rc=$?"; grep 'test result' $T/o | sed 's/^/  /'
cargo +1.99.1 clippy -p parse-that-simd -p parse_that --all-targets --locked -- -D warnings >$T/o 2>&1; echo "clippy native rc=$?"
cargo +1.99.1 clippy -p parse-that-simd --all-targets --locked --target x86_64-apple-darwin --target-dir target/x86 -- -D warnings >$T/o 2>&1; echo "clippy x86_64 rc=$?"
RUSTFLAGS="-C target-feature=+simd128" cargo +1.99.1 clippy -p parse-that-simd --all-targets --locked --target wasm32-wasip1 --target-dir target/wasm -- -D warnings >$T/o 2>&1; echo "clippy wasm32+simd128 rc=$?"
cargo +1.99.1 check -p parse-that-simd --locked --target wasm32-wasip1 --target-dir target/wasm-nosimd >$T/o 2>&1; echo "check wasm32 (no simd128) rc=$?"
rm -rf $T
