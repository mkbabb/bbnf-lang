#!/bin/sh
# LW3.GEN-RECURSE gate GREEN read n (RULINGS 2026-11-15a GENREC-Q (a)): the shared clean (every workspace member,
# dev and ax-iter, proved), the falsifier, regen --check, iter-clippy (-D warnings), the full nextest; hashes after.
set -u
n=$1; W=$HOME/Programming/bbnf-lang-wt/lw3-gen-recurse; L=$HOME/.dev-logs/bbnf-lockstep/lw3/gen-recurse/gate-r$n; mkdir -p $L
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus BBNF_ORACLE_CACHE=$HOME/.dev-logs/bbnf-lockstep/lw3/s5b-oracle/oracle-cache
S=$W/restart/skinny/tranches/sk-v27/evidence/LW3.AUDIT/tgt-clean.sh
cd $W || exit 2
echo "head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')" > $L/head.log
sh $S all $W dev,ax-iter > $L/clean.log 2>&1; echo "rc=$?" >> $L/clean.log
cargo nextest run --cargo-profile ax-iter -p bbnf --test gen_recurse --no-fail-fast > $L/falsifier.log 2>&1; echo "rc=$?" >> $L/falsifier.log
cargo xtask regen --check > $L/regen-check.log 2>&1; echo "rc=$?" >> $L/regen-check.log
cargo iter-clippy > $L/clippy.log 2>&1; echo "rc=$?" >> $L/clippy.log
cargo iter-test --no-fail-fast > $L/nextest.log 2>&1; echo "rc=$?" >> $L/nextest.log
sh $S hash ax-iter bbnf xtask > $L/hash.log
for f in falsifier regen-check clippy nextest; do echo "$f $(tail -1 $L/$f.log) $(grep -E '^ +Summary' $L/$f.log | tail -1 | sed 's/^ *//')"; done | tee $L/summary.txt
