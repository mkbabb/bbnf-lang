#!/bin/sh
# BL half of LW3.PP-2/PP-3a (consumer port) plus PP1-C and the G3 read, one reading. $1 = n.
set -u
L=$HOME/.dev-logs/bbnf-lockstep/lw3/s3b-pp
G=$HOME/.claude/guard/guarded
n=$1; BL=/Users/mkbabb/Programming/bbnf-lang-wt/lw3-s3b-pp
out=$L/bl-green-r$n.log
: > $out
echo "BL $(git -C $BL rev-parse HEAD) dirty=$(git -C $BL status --short --untracked-files=no | wc -l | tr -d ' ')" >> $out
cd $BL
export CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
for prof in dev ax-iter; do $G --light --timeout 10 -- cargo clean -p bbnf -p bbnf-ir -p bbnf-ser -p bbnf-corpus -p bbnf-path -p bbnf-analysis -p bbnf-lsp -p gorgeous -p bbnf-bootstrap -p bbnf-path-ts -p bbnf-oracle -p xtask -p pprint -p pprint_derive -p parse_that -p parse-that-regex -p parse-that-simd --profile $prof >> $out 2>&1; done
$G --timeout 85 -- sh -c 'cargo xtask regen --check && cargo iter-clippy && cargo nextest run --workspace --cargo-profile ax-iter --locked --no-fail-fast' > $L/bl-green-r$n-full.log 2>&1
echo "BL regen+clippy+nextest rc=$?" >> $out
grep -E "regen --check:|Summary" $L/bl-green-r$n-full.log >> $out
echo "BL pprint sources: $($G --light --timeout 5 -- cargo tree --workspace --locked -e normal,build,dev --prefix none 2>/dev/null | grep -E '^pprint ' | sed 's/ (\*)//' | sort -u | tr '\n' ' ')" >> $out
echo "BL dup pprint: $($G --light --timeout 5 -- cargo tree -d --workspace --locked -e normal,build,dev 2>/dev/null | grep -cE '^pprint ')" >> $out
$G --light --timeout 5 -- sh scripts/check-repo-dag.sh > /dev/null 2>&1; echo "BL dag rc=$?" >> $out
$G --light --timeout 5 -- $L/pin_check.sh $BL HEAD >> $out 2>&1
(cd wasm && $G --timeout 45 -- cargo check --locked --all-targets > $L/bl-green-r$n-wasm.log 2>&1; echo "BL wasm check rc=$?") >> $out
$G --timeout 30 -- cargo test -p gorgeous --features vm --test vm > $L/bl-green-r$n-vm.log 2>&1; echo "BL gorgeous vm rc=$?" >> $out
grep -E "test result" $L/bl-green-r$n-vm.log >> $out
$G --timeout 30 -- cargo test -p gorgeous --features bin-full --test g3_trivia > $L/bl-green-r$n-g3.log 2>&1; echo "BL G3 rc=$?" >> $out
grep -E "^G3: [0-9]|^[a-z-]+/[a-z.-]+: " $L/bl-green-r$n-g3.log >> $out
git show HEAD:docs/tranches/AZ-IV/audit/W5-node-execute.txt > docs/tranches/AZ-IV/audit/W5-node-execute.txt 2>/dev/null
echo "BL dirty after=$(git status --short --untracked-files=no | wc -l | tr -d ' ')" >> $out
cat $out
