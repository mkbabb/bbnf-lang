#!/bin/sh
# GEN-RECURSE site probe: depth guard applied (probe.py), the repro and every MANIFEST entry through root; restored after.
set -u
W=$1; L=$HOME/.dev-logs/bbnf-lockstep/lw3/gen-recurse; mkdir -p $L
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
E=$W/restart/skinny/tranches/sk-v27/evidence/LW3.GEN-RECURSE; cd $W || exit 2
python3 -I $E/probe.py crates/core/src/backend/driver/alt.rs || exit 3
git diff > $L/probe.diff
cp $E/gen_recurse_repro.rs crates/core/tests/gen_recurse_repro.rs; cp $E/site_probe.rs crates/core/tests/site_probe.rs
cargo nextest run --cargo-profile ax-iter -p bbnf --test gen_recurse_repro --no-fail-fast --retries 0 > $L/probe-repro.log 2>&1; echo "rc=$?" >> $L/probe-repro.log
cargo nextest run --cargo-profile ax-iter -p bbnf --test site_probe --no-capture --retries 0 > $L/probe-manifest.log 2>&1; echo "rc=$?" >> $L/probe-manifest.log
rm -f crates/core/tests/gen_recurse_repro.rs crates/core/tests/site_probe.rs; git checkout -q -- crates/core/src/backend/driver/alt.rs
echo "restored dirty=$(git status --short | grep -vc '^??')" >> $L/probe-manifest.log
