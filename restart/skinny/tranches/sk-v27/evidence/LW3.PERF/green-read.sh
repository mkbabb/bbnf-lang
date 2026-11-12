#!/bin/sh
# S9-PERF GREEN read of LW3.PERF-1a and LW3.PERF-2 at the lane head.
# Usage: green-read.sh <read-number> <worktree>
set -u
R=$1; WT=$2
G=~/.claude/guard/guarded
L=~/.dev-logs/bbnf-lockstep/lw3/s9-perf/green/r$R; mkdir -p $L
export CARGO_TARGET_DIR=~/.dev-logs/bbnf-lockstep/lw3/target-parse-that
export PARSE_THAT_CORPUS_DIR=$HOME/Programming/parse-that/data
cd $WT/rust || exit 2
HEAD=$(git rev-parse HEAD); echo "head $HEAD" > $L/head.txt
git status --porcelain > $L/status.txt; echo "dirty lines: $(wc -l < $L/status.txt)" >> $L/head.txt
step() { name=$1; shift; "$@" > $L/$name.log 2>&1; rc=$?; echo "$name rc=$rc" | tee -a $L/rc.txt; }
step clean      $G --light --timeout 10 -- sh -c 'cargo clean -p parse-that-simd -p xtask && cargo clean -p parse-that-simd --target x86_64-apple-darwin && cargo clean -p parse-that-simd --target wasm32-wasip1 && cargo +nightly-2026-04-11 clean -p parse-that-simd --target aarch64-apple-darwin && CARGO_TARGET_DIR=$CARGO_TARGET_DIR/miri cargo +nightly-2026-04-11 clean -p parse-that-simd --target aarch64-apple-darwin'
step test       $G --timeout 40 -- cargo test --locked -p parse-that-simd -- --nocapture
step test-x86   $G --timeout 40 -- cargo test --locked -p parse-that-simd --target x86_64-apple-darwin
step test-wasm  $G --timeout 40 -- sh -c 'CARGO_TARGET_WASM32_WASIP1_RUNNER="node --no-warnings $PWD/scripts/wasi-run.mjs" RUSTFLAGS="-C target-feature=+simd128" PARSE_THAT_REQUIRE_TIERS=simd128 cargo test --locked -p parse-that-simd --target wasm32-wasip1 --lib --tests -- --nocapture'
step check-wasm-scalar $G --timeout 20 -- cargo check --locked -p parse-that-simd --target wasm32-wasip1
step clippy     $G --timeout 40 -- sh -c 'cargo clippy --locked -p parse-that-simd --all-targets -- -D warnings && cargo clippy --locked -p parse-that-simd --all-targets --target x86_64-apple-darwin -- -D warnings && RUSTFLAGS="-C target-feature=+simd128" cargo clippy --locked -p parse-that-simd --all-targets --target wasm32-wasip1 -- -D warnings'
step fuzz       $G --timeout 85 -- env PARSE_THAT_FUZZ_CASES=100000 PARSE_THAT_REQUIRE_TIERS=neon cargo test --locked --release -p parse-that-simd --test stage1 --test ascii_prefix --test dispatch_tiers -- fuzz_ required_tiers --nocapture
step miri       $G --timeout 85 -- cargo +nightly-2026-04-11 miri test --locked -p parse-that-simd --lib --test stage1 --test ascii_prefix --test dispatch_tiers
step asan-neon  $G --timeout 85 -- env RUSTFLAGS=-Zsanitizer=address RUSTDOCFLAGS=-Zsanitizer=address PARSE_THAT_REQUIRE_TIERS=neon cargo +nightly-2026-04-11 test -Zbuild-std --target aarch64-apple-darwin -p parse-that-simd --locked --no-fail-fast --lib --tests
step workspace  $G --timeout 60 -- cargo test --locked --workspace --no-fail-fast
step audit      $G --timeout 20 -- cargo test --locked -p xtask --test public_api_callers
step nightly-clippy $G --timeout 30 -- cargo +nightly-2026-04-11 clippy -p parse_that --features nightly --lib --tests -- -D warnings
cd $WT
step grep-bbnf  sh -c 'n=$(git grep -niE bbnf -- rust/simd | wc -l | tr -d " "); echo "git grep -niE bbnf -- rust/simd: $n"; test "$n" = 0'
step tree       sh -c 'cd rust && cargo tree --locked -p parse-that-simd -e normal,build,dev > /dev/stderr; ! cargo tree --locked -p parse-that-simd -e normal,build,dev 2>/dev/null | grep -iE "simdjson|simd-json"'
step arch-grep  sh -c 'git grep -l "core::arch" -- "rust/**/*.rs" | grep -v "^rust/simd/" ; true'
echo "done r$R"; cat $L/rc.txt
