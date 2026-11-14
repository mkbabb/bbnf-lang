#!/bin/sh
# Item 2 (S3c-PP tidy) read at lw3-s10: iter-clippy, then the MANIFEST digest refreshed in this path.
set -u
L=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/s10; mkdir -p $L; W=$HOME/Programming/bbnf-lang-wt/lw3-s10
TOOL=$W/restart/skinny/tranches/sk-v27/evidence/LW3.COMP/comp6
export CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
export BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus
cd $W || exit 2
echo "head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')" > $L/head.log
cargo iter-clippy > $L/clippy.log 2>&1; echo "rc=$?" >> $L/clippy.log
cp $TOOL/manifest_digest.rs crates/core/tests/manifest_digest.rs
sh $TOOL/manifest_digest.sh $W $L/digest-cssebnf-refresh.tsv
rm crates/core/tests/manifest_digest.rs
echo "after head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')" >> $L/head.log
