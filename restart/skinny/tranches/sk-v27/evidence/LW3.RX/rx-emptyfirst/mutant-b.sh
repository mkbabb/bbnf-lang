#!/bin/sh
# LW3.RX-EMPTYFIRST-B mutant read n: in a throwaway detached worktree at the cure, the ArgList
# emitter's inline arm is reverted to the root dispatcher call (the cure's one decision), the trait
# import kept; `cargo xtask regen`, then the falsifier. Expected: the ArgList test RED, the other two
# GREEN. The worktree is removed after; nothing is committed.
set -u
n=$1; L=$HOME/.dev-logs/bbnf-lockstep/lw3/rx-emptyfirst; M=$HOME/Programming/bbnf-lang-wt/lw3-rx-mutant-b
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
cd $HOME/Programming/bbnf-lang-wt/lw3-rx-emptyfirst || exit 2
CURE=$(git rev-parse HEAD)
git worktree add -q --detach $M $CURE || exit 2
cd $M
python3 - <<'PY'
p='crates/core/src/backend/rust/emitter/shapes/arglist.rs'
s=open(p).read()
old='''        | IrNode::TokenDispatch { .. } => super::flat::emit_position_core_struct_direct(
            node,
            support_mod,
            dispatcher_ident,
            grammar_suffix,
            ir,
        ),'''
new='''        | IrNode::TokenDispatch { .. } => quote! {
            let _ = #dispatcher_ident(input, p, state, builder, cursor)?;
        },'''
assert old in s
open(p,'w').write(s.replace(old,new))
PY
echo "cure=$CURE mutant-diff:" > $L/mutant-b-r$n.log; git diff >> $L/mutant-b-r$n.log
cargo clean -p bbnf --profile ax-iter >> $L/mutant-b-r$n.log 2>&1
cargo xtask regen >> $L/mutant-b-r$n.log 2>&1; echo "regen rc=$?" >> $L/mutant-b-r$n.log
git diff --stat >> $L/mutant-b-r$n.log
cargo nextest run --cargo-profile ax-iter -p bbnf --test rx_emptyfirst --no-fail-fast >> $L/mutant-b-r$n.log 2>&1; echo "rc=$?" >> $L/mutant-b-r$n.log
cd $HOME/Programming/bbnf-lang-wt/lw3-rx-emptyfirst && git worktree remove --force $M
