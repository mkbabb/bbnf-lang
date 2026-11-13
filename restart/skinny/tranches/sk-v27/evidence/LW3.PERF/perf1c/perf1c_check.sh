#!/bin/sh
# sk-v27 LW3.PERF-1c falsifier (LW3-ORDER row PERF-1c, amendment 5): one `KernelShape`.
# Usage: perf1c_check.sh <bbnf-lang repo> <bl rev> <parse-that repo> <pt rev>
# Lists every Rust file in either tree that declares `enum KernelShape` (the order's and
# ledger's own text names the pattern, so the read is over `*.rs`). Exit 0 iff exactly one
# file is listed and it is parse-that's `rust/simd/src/alphabet.rs`.
set -u
BLR=$1; BL=$2; PTR=$3; PT=$4
out=$( { git -C "$BLR" grep -l 'enum KernelShape' "$BL" -- '*.rs' | sed 's/^/bbnf-lang:/';
         git -C "$PTR" grep -l 'enum KernelShape' "$PT" -- '*.rs' | sed 's/^/parse-that:/'; } )
n=$(printf '%s\n' "$out" | grep -c .)
echo "bl=$(git -C "$BLR" rev-parse "$BL") pt=$(git -C "$PTR" rev-parse "$PT") files=$n"
printf '%s\n' "$out"
[ "$n" -eq 1 ] && printf '%s\n' "$out" | grep -q ":rust/simd/src/alphabet.rs$"
