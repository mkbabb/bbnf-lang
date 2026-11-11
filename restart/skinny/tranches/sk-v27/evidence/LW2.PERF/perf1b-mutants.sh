#!/bin/sh
# PERF-1b RED readings: one-token mutants of prim/neon.rs; restored on exit.
cd ~/Programming/parse-that-wt/lw2-s6-perf/rust || exit 2
export CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/target-parse-that
F=simd/src/prim/neon.rs; B=$(mktemp -d "$HOME/.dev-logs/bbnf-lockstep/lw2/s6-perf/r3/.tmp.XXXX"); cp $F $B/neon.rs
restore() { cp $B/neon.rs $F; rm -rf $B; }
trap restore EXIT INT TERM
echo "tree $(git rev-parse --short HEAD)"
# M4: the eq-set fan skips the last 16-byte stripe.
python3 - "$F" <<'P'
import sys; p=sys.argv[1]; s=open(p).read(); s=s.replace("            for k in 0..4 {\n                m[k] = vorrq_u8","            for k in 0..3 {\n                m[k] = vorrq_u8",1); open(p,'w').write(s)
P
cargo +1.99.1 test -p parse-that-simd --release --test prim_differential -- --nocapture --test-threads 1 >$B/o 2>&1; echo "M4 eq-set fan skips stripe 3, prim_differential rc=$?"; grep -o 'prim_differential kernel=.*divergences=[0-9]*' $B/o | sed 's/^/  /'
cp $B/neon.rs $F
# M5: the hex decoder's alpha adjust is 8, not 9.
python3 - "$F" <<'P'
import sys; p=sys.argv[1]; s=open(p).read(); s=s.replace("vandq_u8(is_alpha, vdupq_n_u8(9))","vandq_u8(is_alpha, vdupq_n_u8(8))",1); open(p,'w').write(s)
P
cargo +1.99.1 test -p parse-that-simd --release --test prim_differential -- --nocapture --test-threads 1 >$B/o 2>&1; echo "M5 hex alpha adjust 8, prim_differential rc=$?"; grep -o 'prim_differential kernel=.*divergences=[0-9]*' $B/o | sed 's/^/  /'
