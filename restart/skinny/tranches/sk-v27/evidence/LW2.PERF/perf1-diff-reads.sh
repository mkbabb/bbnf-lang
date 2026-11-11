#!/bin/sh
# Differential fuzz reads only (x86_64 Rosetta, wasm32+simd128), 10^5 inputs per tier and alphabet.
cd ~/Programming/parse-that-wt/lw2-s6-perf/rust || exit 2; N=100000; T=$(mktemp -d)
echo "tree $(git rev-parse --short HEAD)"
PARSE_THAT_SIMD_FUZZ_INPUTS=$N cargo +1.99.1 test -p parse-that-simd --locked --release --target x86_64-apple-darwin --target-dir target/x86 --test differential -- --nocapture >$T/o 2>&1; echo "differential (x86_64 Rosetta, release) rc=$?"; grep -o 'differential tier=.*' $T/o | sed 's/^/  /'
export CARGO_TARGET_WASM32_WASIP1_RUNNER="node --no-warnings $PWD/scripts/wasi-run.mjs"
RUSTFLAGS="-C target-feature=+simd128" PARSE_THAT_SIMD_FUZZ_INPUTS=$N cargo +1.99.1 test -p parse-that-simd --locked --release --target wasm32-wasip1 --target-dir target/wasm --test differential -- --nocapture >$T/o 2>&1; echo "differential (wasm32+simd128, release) rc=$?"; grep -o 'differential tier=.*' $T/o | sed 's/^/  /'
rm -rf $T
