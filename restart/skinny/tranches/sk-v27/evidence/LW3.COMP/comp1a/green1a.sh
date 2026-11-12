#!/bin/sh
# COMP-1a GREEN read N: usage green1a.sh N
n=$1
export CARGO_TARGET_DIR=/Users/mkbabb/.dev-logs/bbnf-lockstep/lw3/target-parse-that
L=/Users/mkbabb/.dev-logs/bbnf-lockstep/lw3/s6-kernel/logs
W=/Users/mkbabb/Programming/parse-that-wt/lw3-s6-kernel
o=$L/c1a-green-$n
cd $W/rust || exit 2
{ echo "# head $(git rev-parse HEAD) dirty=$(git status --porcelain --untracked-files=no -- . ../.github | wc -l | tr -d ' ')"; echo "# date $(date -Iseconds)"; cargo --version; } > $o.head.log
cargo clean -p parse_that -p regex-bootstrap -p parse-that-corpus > $o.clean.log 2>&1; echo "# rc $?" >> $o.clean.log
cargo test --locked -p parse_that --test kernel --test kernel_allocation --test kernel_regex > $o.laws.log 2>&1; echo "# rc $?" >> $o.laws.log
cargo test --locked -p parse_that --test kernel --test kernel_allocation -- --list --format terse > $o.kernel-list.log 2>&1; echo "# rc $?" >> $o.kernel-list.log
cargo test --locked --workspace --no-fail-fast > $o.workspace.log 2>&1; echo "# rc $?" >> $o.workspace.log
cargo test --locked --workspace -- --list --format terse > $o.list.log 2>&1; echo "# rc $?" >> $o.list.log
cargo clippy --locked -p parse_that -p regex-bootstrap --all-targets -- -D warnings > $o.clippy.log 2>&1; echo "# rc $?" >> $o.clippy.log
{ git grep -niE 'bbnf' -- parse_that/src/kernel 'parse_that/tests/kernel*' bootstrap; echo "# rc $? (1 = no match)"; } > $o.grep.log 2>&1
(cd $W && sh scripts/check-repo-dag.sh) > $o.dag.log 2>&1; echo "# rc $?" >> $o.dag.log
