#!/bin/sh
# COMP-4b GREEN read N: usage green4b.sh N
n=$1
export CARGO_TARGET_DIR=/Users/mkbabb/.dev-logs/bbnf-lockstep/lw3/target-parse-that
L=/Users/mkbabb/.dev-logs/bbnf-lockstep/lw3/s6-kernel/logs; W=/Users/mkbabb/Programming/parse-that-wt/lw3-s6-kernel
o=$L/c4b-green-$n
cd $W/rust || exit 2
{ echo "# head $(git rev-parse HEAD) dirty=$(git status --porcelain --untracked-files=no -- .. | wc -l | tr -d ' ')"; echo "# date $(date -Iseconds)"; cargo --version; } > $o.head.log
cargo clean -p parse_that -p regex-bootstrap -p parse-that-corpus > $o.clean.log 2>&1; echo "# rc $?" >> $o.clean.log
cargo test --locked -p parse_that --test kernel --test kernel_allocation --test kernel_regex --test kernel_abi > $o.kernel.log 2>&1; echo "# rc $?" >> $o.kernel.log
cargo test --locked -p parse_that --features diagnostics > $o.legacy-diagnostics.log 2>&1; echo "# rc $?" >> $o.legacy-diagnostics.log
cargo test --locked --workspace --no-fail-fast > $o.workspace.log 2>&1; echo "# rc $?" >> $o.workspace.log
cargo clippy --locked -p parse_that --all-targets -- -D warnings > $o.clippy.log 2>&1; echo "# rc $?" >> $o.clippy.log
cargo bench --locked -p parse_that --bench kernel_success_path --no-run > $o.bench-build.log 2>&1; echo "# rc $?" >> $o.bench-build.log
{ git grep -niE 'bbnf' -- parse_that/src/kernel 'parse_that/tests/kernel*' parse_that/KERNEL-ABI.md parse_that/benches/kernel bootstrap; echo "# rc $? (1 = no match)"; } > $o.grep.log 2>&1
(cd $W && sh scripts/check-repo-dag.sh) > $o.dag.log 2>&1; echo "# rc $?" >> $o.dag.log
cd $W/typescript && { npx tsc --noEmit > $o.ts-tsc.log 2>&1; echo "# rc $?" >> $o.ts-tsc.log; }
npx vitest run --maxWorkers=2 > $o.ts.log 2>&1; echo "# rc $?" >> $o.ts.log

