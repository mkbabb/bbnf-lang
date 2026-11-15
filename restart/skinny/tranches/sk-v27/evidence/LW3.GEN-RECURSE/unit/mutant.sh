#!/bin/sh
# LW3.GEN-RECURSE mutant read n: the visited set ignored (a worktree edit of driver/alt.rs, restored after);
# expected: both falsifier tests abort on the stack overflow again.
set -u
n=$1; W=$HOME/Programming/bbnf-lang-wt/lw3-gen-recurse; L=$HOME/.dev-logs/bbnf-lockstep/lw3/gen-recurse; O=$L/mutant-r$n.log
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
S=$W/restart/skinny/tranches/sk-v27/evidence/LW3.AUDIT/tgt-clean.sh
cd $W || exit 2
echo "head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')" > $O
python3 -I - <<'PY'
p='crates/core/src/backend/driver/alt.rs'
s=open(p).read(); old='            if visited.contains(target) {'
assert s.count(old)==1; open(p,'w').write(s.replace(old,'            if false && visited.contains(target) {'))
PY
git diff >> $O
sh $S clean $W ax-iter bbnf >> $O 2>&1
cargo nextest run --cargo-profile ax-iter -p bbnf --test gen_recurse --no-fail-fast --retries 0 >> $O 2>&1; echo "rc=$?" >> $O
sh $S hash ax-iter bbnf >> $O
git checkout -- crates/core/src/backend/driver/alt.rs
echo "after dirty=$(git status --short | grep -vc '^??')" >> $O
