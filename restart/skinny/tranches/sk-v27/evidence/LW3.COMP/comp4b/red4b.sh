#!/bin/sh
# COMP-4b read N on three runtimes: usage red4b.sh <tag> N
t=$1; n=$2
export CARGO_TARGET_DIR=/Users/mkbabb/.dev-logs/bbnf-lockstep/lw3/target-parse-that
L=/Users/mkbabb/.dev-logs/bbnf-lockstep/lw3/s6-kernel/logs; W=/Users/mkbabb/Programming/parse-that-wt/lw3-s6-kernel
o=$L/c4b-$t-$n
cd $W/rust || exit 2
{ echo "# head $(git rev-parse HEAD) dirty=$(git status --porcelain --untracked-files=no | wc -l | tr -d ' ')"; echo "# date $(date -Iseconds)"; } > $o.head.log
cargo test --locked -p parse_that --test kernel failure_contract > $o.kernel.log 2>&1; echo "# rc $?" >> $o.kernel.log
cargo test --locked -p parse_that --features diagnostics --test expected_sorted_test > $o.legacy.log 2>&1; echo "# rc $?" >> $o.legacy.log
cd $W/typescript && npx vitest run --maxWorkers=2 test/expected-set.test.ts > $o.ts.log 2>&1; echo "# rc $?" >> $o.ts.log
