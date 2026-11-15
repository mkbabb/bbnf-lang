#!/bin/sh
# LW3.RX-EMPTYFIRST mutant read n: in a throwaway detached worktree at the cure, PatternFirstBytes::from_pattern
# reads every pattern as non-nullable (the cure's one decision reverted), `cargo xtask regen`, then the falsifier.
# Expected: both tests RED. The worktree is removed after; nothing is committed.
set -u
n=$1; L=$HOME/.dev-logs/bbnf-lockstep/lw3/rx-emptyfirst; M=$HOME/Programming/bbnf-lang-wt/lw3-rx-mutant
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
cd $HOME/Programming/bbnf-lang-wt/lw3-rx-emptyfirst || exit 2
CURE=$(git rev-parse HEAD)
git worktree add -q --detach $M $CURE || exit 2
cd $M
sed -i '' 's/nullable: regex_is_nullable(pattern),/nullable: false \&\& regex_is_nullable(pattern),/' crates/core/src/generate/regex/byte_class.rs
echo "cure=$CURE mutant-diff:" > $L/mutant-r$n.log; git diff >> $L/mutant-r$n.log
cargo clean -p bbnf --profile ax-iter >> $L/mutant-r$n.log 2>&1
cargo xtask regen >> $L/mutant-r$n.log 2>&1; echo "regen rc=$?" >> $L/mutant-r$n.log
git diff --stat >> $L/mutant-r$n.log
cargo nextest run --cargo-profile ax-iter -p bbnf --test rx_emptyfirst --no-fail-fast >> $L/mutant-r$n.log 2>&1; echo "rc=$?" >> $L/mutant-r$n.log
cd $HOME/Programming/bbnf-lang-wt/lw3-rx-emptyfirst && git worktree remove --force $M
