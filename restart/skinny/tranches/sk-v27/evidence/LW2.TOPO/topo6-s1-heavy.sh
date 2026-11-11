#!/bin/zsh
# One heavy run for S1: bbnf-lang regen erratum reads + regen, then TOPO-6 falsifiers both repos, then parse-that lock/clippy/tests.
set -u
L=~/.dev-logs/bbnf-lockstep/lw2/s1-custody/topo6
B=~/Programming/bbnf-lang-wt/lw2-s1-custody
P=~/Programming/parse-that-wt/lw2-s1-custody
echo "#### A. bbnf-lang regen --check at $(git -C $B rev-parse --short HEAD) (RED x2 expected: TOPO-0 merge left generated files stale)"
cd $B
for i in 1 2; do cargo xtask regen --check > $L/regen-red-$i.log 2>&1; echo "regen-check RED read $i rc=$?: $(grep -E 'drift:|drifted' $L/regen-red-$i.log | sed 's#.*/generated/##' | tr '\n' ' ')"; done
cargo xtask regen > $L/regen-write.log 2>&1; echo "regen write rc=$?"
git status --short crates/core/src/grammar/generated
for i in 1 2; do cargo xtask regen --check > $L/regen-green-$i.log 2>&1; echo "regen-check GREEN read $i rc=$?"; done
echo "#### B. bbnf-lang local corpus tests"
cargo nextest run --cargo-profile ax-iter -p simd-scan --test correctness -p bbnf --test pipeline --test css_l4 --test structural --test json_slab --test ax_w0a2s_real_css_probe --test sheets_self_parity --no-fail-fast > $L/bbnf-local-tests.log 2>&1; echo "local tests rc=$?: $(grep -E 'Summary' $L/bbnf-local-tests.log)"
echo "#### C. parse-that lock + clippy (new crates) + corpus tests"
cd $P/rust
cargo metadata --format-version 1 > /dev/null 2>$L/pt-meta.log; echo "metadata rc=$?"; git -C $P diff --stat rust/Cargo.lock
cargo clippy --profile ax-iter -p parse-that-corpus -p xtask --all-targets -- -D warnings > $L/pt-clippy.log 2>&1; echo "pt clippy(new crates) rc=$?"
cargo xtask corpus verify; echo "pt local verify rc=$?"
cargo nextest run --cargo-profile ax-iter -p parse_that --test css_parse_test --test json_test --test csv_test --no-fail-fast > $L/pt-local-tests.log 2>&1; echo "pt local tests rc=$?: $(grep -E 'Summary' $L/pt-local-tests.log)"; grep -E "^\s+FAIL" $L/pt-local-tests.log | sort -u | head
echo "#### D. TOPO-6 falsifiers on clean checkouts"
$L/topo6-falsifier.sh bbnf-lang $(git -C $B rev-parse d6b434167) . $B/target > $L/falsifier-bbnf-lang.log 2>&1; echo "bbnf falsifier done"
$L/topo6-falsifier.sh parse-that $(git -C $P rev-parse 07d1039) rust $P/rust/target > $L/falsifier-parse-that.log 2>&1; echo "pt falsifier done"
