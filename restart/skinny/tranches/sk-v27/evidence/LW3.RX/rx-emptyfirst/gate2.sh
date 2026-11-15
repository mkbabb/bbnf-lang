#!/bin/sh
# LW3.RX-EMPTYFIRST + -B merge-gate GREEN read n (RULINGS 2026-11-14j RXEF-A, 2026-11-14k RXEFB-C, ORDER-1114k):
# the shared clean (LW3.AUDIT/tgt-clean.sh, every workspace member, dev and ax-iter, proved), the falsifier
# (three tests incl. the google_sheets pin of `1+2`, `A1`, `42`, `=1+2`), the two corrected CSS tests,
# regen --check, iter-clippy (-D warnings), the full nextest; the library hashes after the build.
set -u
n=$1; W=${2:-$HOME/Programming/bbnf-lang-wt/lw3-rx-emptyfirst}; L=$HOME/.dev-logs/bbnf-lockstep/lw3/rx-emptyfirst/gate2-r$n; mkdir -p $L
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus BBNF_ORACLE_CACHE=$HOME/.dev-logs/bbnf-lockstep/lw3/s5b-oracle/oracle-cache
S=$W/restart/skinny/tranches/sk-v27/evidence/LW3.AUDIT/tgt-clean.sh
cd $W || exit 2
echo "head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')" > $L/head.log
sh $S all $W dev,ax-iter > $L/clean.log 2>&1; echo "rc=$?" >> $L/clean.log
cargo nextest run --cargo-profile ax-iter -p bbnf --test rx_emptyfirst --no-fail-fast > $L/falsifier.log 2>&1; echo "rc=$?" >> $L/falsifier.log
cargo nextest run --cargo-profile ax-iter -p bbnf --test projection_totality --test named_type_preservation --no-fail-fast > $L/css.log 2>&1; echo "rc=$?" >> $L/css.log
cargo xtask regen --check > $L/regen-check.log 2>&1; echo "rc=$?" >> $L/regen-check.log
cargo iter-clippy > $L/clippy.log 2>&1; echo "rc=$?" >> $L/clippy.log
cargo iter-test --no-fail-fast > $L/nextest.log 2>&1; echo "rc=$?" >> $L/nextest.log
sh $S hash ax-iter bbnf xtask > $L/hash.log
for f in falsifier css regen-check clippy nextest; do echo "$f $(tail -1 $L/$f.log) $(grep -E '^ +Summary' $L/$f.log | tail -1 | sed 's/^ *//')"; done | tee $L/summary.txt
