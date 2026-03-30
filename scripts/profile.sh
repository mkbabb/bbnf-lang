#!/usr/bin/env bash
set -euo pipefail

BENCH_TARGET="${1:?Usage: profile.sh <bench_target> [filter] [--open]}"
FILTER="${2:-}"
PROFILE_DIR="profiles"

mkdir -p "$PROFILE_DIR"

# Build the benchmark binary with debug info for profiling
echo "Building $BENCH_TARGET with debug info..."
cd rust
CARGO_PROFILE_RELEASE_DEBUG=2 cargo build --bench "$BENCH_TARGET" --release 2>&1

# Find the bench binary
BENCH_BIN=$(find target/release/deps -name "${BENCH_TARGET}-*" -type f -perm +111 ! -name "*.d" | head -1)
if [ -z "$BENCH_BIN" ]; then
    echo "Error: Could not find bench binary for $BENCH_TARGET"
    exit 1
fi

TIMESTAMP=$(date +%Y%m%d_%H%M%S)
PROFILE_NAME="${BENCH_TARGET}"
[ -n "$FILTER" ] && PROFILE_NAME="${BENCH_TARGET}_${FILTER}"
PROFILE_FILE="../${PROFILE_DIR}/${PROFILE_NAME}_${TIMESTAMP}.json"

echo "Profiling with samply..."
if [ -n "$FILTER" ]; then
    samply record -o "$PROFILE_FILE" -- "$BENCH_BIN" --bench "$FILTER"
else
    samply record -o "$PROFILE_FILE" -- "$BENCH_BIN" --bench
fi

echo "Profile saved to $PROFILE_FILE"

# Open if requested
if [ "${3:-}" = "--open" ]; then
    echo "Opening in Firefox Profiler..."
    open "https://profiler.firefox.com/from-file/$PROFILE_FILE"
fi
