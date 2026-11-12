#!/bin/sh
export CARGO_TARGET_DIR=/Users/mkbabb/.dev-logs/bbnf-lockstep/lw3/target-parse-that
cd /Users/mkbabb/Programming/parse-that-wt/lw3-s6-kernel/rust || exit 2
f=parse_that/src/kernel/mod.rs
for n in 1 2; do
  sed -i '' 's/^pub const KERNEL_ABI: u32 = 3;/pub const KERNEL_ABI: u32 = 2;/' $f
  o=/Users/mkbabb/.dev-logs/bbnf-lockstep/lw3/s6-kernel/logs/c1a-mutant-$n.log
  { echo "# head $(git rev-parse HEAD) + mutant KERNEL_ABI=2"; git diff -- $f; } > $o
  cargo test --locked -p parse_that --test kernel >> $o 2>&1; echo "# rc $?" >> $o
  git checkout -- $f
  git diff --quiet -- $f && echo "# restored byte-equal" >> $o
done
