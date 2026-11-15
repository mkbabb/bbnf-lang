#!/bin/sh
# LW3.COMP-1c RED read n (named born-RED gate, evidence form of LW3.COMP-6-BR3): the falsifier
# evidence/LW3.COMP/comp1c/spine_manifest.rs is copied into crates/core/tests, run after
# `cargo clean -p bbnf` (TGT-1), and removed; it never enters the merged suite until COMP-1c is GREEN.
set -u
n=$1; L=$HOME/.dev-logs/bbnf-lockstep/lw3/comp-1c; W=$HOME/Programming/bbnf-lang-wt/lw3-comp1c
E=$W/restart/skinny/tranches/sk-v27/evidence/LW3.COMP/comp1c
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
cd $W || exit 2
echo "head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??') falsifier-sha256=$(shasum -a 256 $E/spine_manifest.rs | cut -c1-16)" > $L/red-r$n.log
cp $E/spine_manifest.rs crates/core/tests/spine_manifest.rs
cargo clean -p bbnf >> $L/red-r$n.log 2>&1
cargo nextest run --cargo-profile ax-iter -p bbnf --test spine_manifest --no-capture --no-fail-fast >> $L/red-r$n.log 2>&1; echo "rc=$?" >> $L/red-r$n.log
rm crates/core/tests/spine_manifest.rs
