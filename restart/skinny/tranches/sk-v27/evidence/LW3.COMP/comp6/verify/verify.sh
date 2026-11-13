#!/bin/sh
# sk-v27 LW3.COMP-6 stage-verifier falsifier (LW3-ORDER row COMP-6: "an injected dangling RuleId is
# not caught" -> "the verifier rejects 50 of 50 mutants"). Runs the in-crate suite
# `compiler::verify::mutants` (bbnf lib): each mutant test breaks one law in one spine carrier
# (Program, DependencySolution, TypeSolution, AnalysisFacts, LoweringPlan, ScalarEventPlan) and
# passes only if the verifier reports exactly that stage and law; the two fixture tests read the
# lawful fixture clean and reaching every node kind.
# Usage: verify.sh <bl worktree> <log dir>. Prints `mutants= rejected= fixture=`; exit 0 iff
# every mutant is rejected, there are at least 50, and both fixture tests pass.
set -u
BL=$1; L=$2; mkdir -p "$L"
export CARGO_TARGET_DIR=${CARGO_TARGET_DIR:-$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang}
cd "$BL" || exit 2
echo "bl=$(git rev-parse HEAD) tree=$(git rev-parse 'HEAD^{tree}')" > "$L/heads.txt"
git diff --quiet HEAD -- crates/core/src || echo "worktree edits under crates/core/src (a mutant read)" >> "$L/heads.txt"
cargo nextest run -p bbnf --lib --locked --no-fail-fast --retries 0 \
  -E 'test(/^compiler::verify::mutants::/)' > "$L/nextest.log" 2>&1
rc=$?
grep -E '^ +(PASS|FAIL) ' "$L/nextest.log" | sed -E 's/^ +//; s/\[[^]]*\] \([^)]*\) //; s/^(PASS|FAIL) +[^ ]+ /\1 /' | sort -u > "$L/tests.txt"
fixture=$(grep -cE '^PASS .*mutants::the_fixture_(is_lawful|reaches_every_kind)$' "$L/tests.txt")
mutants=$(grep -vcE 'mutants::the_fixture_' "$L/tests.txt")
rejected=$(grep -E '^PASS ' "$L/tests.txt" | grep -vcE 'mutants::the_fixture_')
echo "rc=$rc mutants=$mutants rejected=$rejected fixture=$fixture/2" | tee "$L/result.txt"
[ "$rc" -eq 0 ] && [ "$mutants" -ge 50 ] && [ "$rejected" -eq "$mutants" ] && [ "$fixture" -eq 2 ]
