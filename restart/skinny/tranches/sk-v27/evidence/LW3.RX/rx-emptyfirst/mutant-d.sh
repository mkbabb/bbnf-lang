#!/bin/sh
# RXEFB-D law mutants (RULINGS 2026-11-15a): a worktree edit of shapes/arglist.rs, restored after.
# d1: `is_name_head` answers false (no head recorded)        -> the law's first assertion RED.
# d2: `is_name_head` ignores the next position (every head)  -> the law's second assertion RED.
set -u
m=$1; n=$2; W=$HOME/Programming/bbnf-lang-wt/lw3-rx-emptyfirst; L=$HOME/.dev-logs/bbnf-lockstep/lw3/rx-emptyfirst
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
S=$W/restart/skinny/tranches/sk-v27/evidence/LW3.AUDIT/tgt-clean.sh; O=$L/mutant-$m-r$n.log
cd $W || exit 2
echo "head=$(git rev-parse HEAD) dirty=$(git status --short | grep -vc '^??')" > $O
python3 -I - "$m" <<'PY'
import sys
p='crates/core/src/backend/rust/emitter/shapes/arglist.rs'
s=open(p).read(); old='    opens_after && matches!(strip(node), IrNode::Regex(_) | IrNode::Literal(_))'
new={'d1':'    let _ = opens_after; false && matches!(strip(node), IrNode::Regex(_) | IrNode::Literal(_))',
     'd2':'    let _ = opens_after; matches!(strip(node), IrNode::Regex(_) | IrNode::Literal(_))'}[sys.argv[1]]
assert s.count(old)==1; open(p,'w').write(s.replace(old,new))
PY
git diff >> $O
sh $S clean $W ax-iter bbnf >> $O 2>&1
cargo nextest run --cargo-profile ax-iter -p bbnf --test rx_emptyfirst --no-fail-fast >> $O 2>&1; echo "rc=$?" >> $O
git checkout -- crates/core/src/backend/rust/emitter/shapes/arglist.rs
echo "after dirty=$(git status --short | grep -vc '^??')" >> $O
