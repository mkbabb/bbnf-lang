#!/bin/sh
# LW3.COMP-1c lane read n (RULINGS 2026-11-15a ORDER-1115a: every pushed step green on the full nextest): the shared clean (every workspace member,
# dev and ax-iter, proved), the falsifier, regen --check, iter-clippy (-D warnings), the full nextest; hashes after.
set -u
n=$1; W=$HOME/Programming/bbnf-lang-wt/lw3-comp1c; L=$HOME/.dev-logs/bbnf-lockstep/lw3/comp-1c/lane-gate-$n; mkdir -p $L
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus BBNF_ORACLE_CACHE=$HOME/.dev-logs/bbnf-lockstep/lw3/s5b-oracle/oracle-cache
S=$W/restart/skinny/tranches/sk-v27/evidence/LW3.AUDIT/tgt-clean.sh
cd $W || exit 2
echo "head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')" > $L/head.log
sh $S all $W dev,ax-iter > $L/clean.log 2>&1; echo "rc=$?" >> $L/clean.log
cargo xtask regen --check > $L/regen-check.log 2>&1; echo "rc=$?" >> $L/regen-check.log
cargo iter-clippy > $L/clippy.log 2>&1; echo "rc=$?" >> $L/clippy.log
cargo iter-test --no-fail-fast > $L/nextest.log 2>&1; echo "rc=$?" >> $L/nextest.log
sh $S hash ax-iter bbnf xtask > $L/hash.log
for f in regen-check clippy nextest; do echo "$f $(tail -1 $L/$f.log) $(grep -E '^ +Summary' $L/$f.log | tail -1 | sed 's/^ *//')"; done | tee $L/summary.txt
