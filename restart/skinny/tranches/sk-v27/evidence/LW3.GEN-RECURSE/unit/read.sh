#!/bin/sh
# LW3.GEN-RECURSE falsifier read (RULINGS 2026-11-15a GENREC-Q (a)). Usage: read.sh <tag> [worktree]
# The shared clean of bbnf (ax-iter) first, proved; then the gen_recurse target; the library hash after.
set -u
tag=$1; W=${2:-$HOME/Programming/bbnf-lang-wt/lw3-gen-recurse}; L=$HOME/.dev-logs/bbnf-lockstep/lw3/gen-recurse; mkdir -p $L
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
S=$W/restart/skinny/tranches/sk-v27/evidence/LW3.AUDIT/tgt-clean.sh; O=$L/$tag.log
cd $W || exit 2
echo "head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')" > $O
git diff >> $O
sh $S clean $W ax-iter bbnf >> $O 2>&1
cargo nextest run --cargo-profile ax-iter -p bbnf --test gen_recurse --no-fail-fast --retries 0 >> $O 2>&1; echo "rc=$?" >> $O
sh $S hash ax-iter bbnf >> $O
