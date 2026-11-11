#!/bin/sh
# PERF-1 RED readings on mutants of the parse-that lane tree; every mutant is restored on exit.
S=~/.dev-logs/bbnf-lockstep/lw2/s6-perf; cd ~/Programming/parse-that-wt/lw2-s6-perf/rust || exit 2
BASE_WASM=$S/base-wasm.rs; B=$(mktemp -d); cp simd/src/wasm.rs simd/src/stripe.rs simd/src/lib.rs $B/
restore() { cp $B/wasm.rs $B/stripe.rs $B/lib.rs simd/src/; rm -rf $B; }
trap restore EXIT INT TERM
echo "tree $(git rev-parse --short HEAD)"
export CARGO_TARGET_WASM32_WASIP1_RUNNER="node --no-warnings $PWD/scripts/wasi-run.mjs"
# M1: the base (bbnf-lang crates/simd-scan) wasm kernel, in place of the port.
cp $BASE_WASM simd/src/wasm.rs
RUSTFLAGS="-C target-feature=+simd128" cargo +1.99.1 test -p parse-that-simd --target wasm32-wasip1 --target-dir target/wasm --test wasm_bounds >$B/o 2>&1; echo "M1 base wasm kernel, wasm_bounds rc=$?"; grep -E 'RuntimeError|test result' $B/o | sed 's/^/  /'
cp $B/wasm.rs simd/src/wasm.rs
# M2: the shared stripe ignores escapes (a one-token fault).
sed -i '' 's/\*m &= !escape & valid;/*m \&= valid;/' simd/src/stripe.rs
cargo +1.99.1 test -p parse-that-simd --test differential -- --nocapture >$B/o 2>&1; echo "M2 escape-blind stripe, differential rc=$?"; grep -o 'differential tier=.*' $B/o | sed 's/^/  /'
cp $B/stripe.rs simd/src/stripe.rs
# M3: one uncalled pub fn in parse-that-simd (ruling A-2's falsifier).
printf '\n/// Mutant.\npub fn s6_uncalled_mutant() {}\n' >> simd/src/lib.rs
cargo +1.99.1 test -p xtask --test public_api_callers --locked >$B/o 2>&1; echo "M3 uncalled pub fn, public_api_callers rc=$?"; grep -E 's6_uncalled_mutant|test result' $B/o | sed 's/^/  /'
