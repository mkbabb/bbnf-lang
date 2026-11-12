#!/bin/sh
# PT consumer half of LW3.PP-2/PP-3a, one reading. $1 = reading number, $2 = PT dir.
set -u
L=$HOME/.dev-logs/bbnf-lockstep/lw3/s3b-pp
G=$HOME/.claude/guard/guarded
n=$1; PT=$2
out=$L/pt-green-r$n.log
: > $out
echo "PT $(git -C $PT rev-parse HEAD) dirty=$(git -C $PT status --short --untracked-files=no | wc -l | tr -d ' ')" >> $out
cd $PT/rust
export CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-parse-that
$G --light --timeout 5 -- cargo clean -p pprint -p parse_that_cli -p parse-that-corpus -p parse_that -p parse-that-regex -p parse-that-egraph -p parse-that-egraph-derive -p parse-that-simd -p regex-bootstrap -p xtask --profile ax-iter >> $out 2>&1
$G --timeout 80 -- sh -c 'cargo clippy --profile ax-iter --workspace --all-targets --locked -- -D warnings && cargo nextest run --cargo-profile ax-iter --workspace --locked --no-fail-fast && cargo nextest run --cargo-profile ax-iter --workspace --locked --features diagnostics --no-fail-fast' > $L/pt-green-r$n-full.log 2>&1
echo "PT clippy+nextest+diagnostics rc=$?" >> $out
grep -E "Summary" $L/pt-green-r$n-full.log >> $out
echo "PT pprint: $($G --light --timeout 5 -- cargo tree -i pprint --workspace --locked --prefix none 2>/dev/null | head -1)" >> $out
(cd $PT && $G --light --timeout 5 -- sh scripts/check-repo-dag.sh > /dev/null 2>&1; echo "PT dag rc=$?") >> $out
cat $out
