#!/bin/sh
# LW3.RX-EMPTYFIRST-B head mutant read n (RULINGS 2026-11-14k RXEFB-C (a)): in a throwaway detached worktree at
# the cure, the ArgList head deposit is dropped (`is_name_head` answers false), `cargo xtask regen`, then the
# two corrected CSS tests. Expected: both RED (the call keeps no name). Nothing is committed.
set -u
n=$1; L=$HOME/.dev-logs/bbnf-lockstep/lw3/rx-emptyfirst; M=$HOME/Programming/bbnf-lang-wt/lw3-rx-mutant-c
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
cd $HOME/Programming/bbnf-lang-wt/lw3-rx-emptyfirst || exit 2
CURE=$(git rev-parse HEAD); S=$PWD/restart/skinny/tranches/sk-v27/evidence/LW3.AUDIT/tgt-clean.sh
git worktree add -q --detach $M $CURE || exit 2
cd $M
python3 -I - <<'PY'
p='crates/core/src/backend/rust/emitter/shapes/arglist.rs'
s=open(p).read(); old='    opens_after && matches!(strip(node), IrNode::Regex(_) | IrNode::Literal(_))'
assert s.count(old)==1; open(p,'w').write(s.replace(old,'    let _ = opens_after; false && matches!(strip(node), IrNode::Regex(_) | IrNode::Literal(_))'))
PY
echo "cure=$CURE mutant-diff:" > $L/mutant-c-r$n.log; git diff >> $L/mutant-c-r$n.log
sh $S clean $M ax-iter bbnf xtask >> $L/mutant-c-r$n.log 2>&1
cargo xtask regen >> $L/mutant-c-r$n.log 2>&1; echo "regen rc=$?" >> $L/mutant-c-r$n.log
git diff --stat >> $L/mutant-c-r$n.log
cargo nextest run --cargo-profile ax-iter -p bbnf --test projection_totality --test named_type_preservation --no-fail-fast >> $L/mutant-c-r$n.log 2>&1; echo "rc=$?" >> $L/mutant-c-r$n.log
sh $S hash ax-iter bbnf >> $L/mutant-c-r$n.log
cd $HOME/Programming/bbnf-lang-wt/lw3-rx-emptyfirst && git worktree remove --force $M
