#!/bin/sh
# sk-v27 LW3.AUDIT-CLEAN GREEN reads (RULINGS 2026-11-14k CLEAN-PROFILE): every Phase 3b1 unit whose gate
# cleaned with the plain `cargo clean -p`, re-read once on the integration tip, after a proven clean of
# every workspace member in both profiles the reads use (dev for plain nextest/cargo test; ax-iter for
# `cargo xtask`, `--cargo-profile ax-iter`, `iter-*`). One row per unit in $L/green.tsv: unit, rc, read.
# Usage: green.sh <worktree> <log dir>
set -u
W=$1; L=$2; mkdir -p "$L"
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
export BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus
export BBNF_ORACLE_CACHE=$HOME/.dev-logs/bbnf-lockstep/lw3/s5b-oracle/oracle-cache
E=$W/restart/skinny/tranches/sk-v27/evidence; S=$E/LW3.AUDIT/tgt-clean.sh
cd "$W" || exit 2
echo "head=$(git rev-parse HEAD) tree=$(git rev-parse 'HEAD^{tree}') dirty=$(git status --short | grep -vc '^??')" > "$L/head.txt"
H="bbnf bbnf_ir gorgeous xtask bbnf_oracle bbnf_bootstrap"
for p in dev ax-iter; do $S hash $p $H; done > "$L/hash-before.txt"
$S all "$W" dev,ax-iter > "$L/clean.txt" 2>&1; echo "clean rc=$?" >> "$L/clean.txt"
for p in dev ax-iter; do $S hash $p $H; done > "$L/hash-after-clean.txt"
: > "$L/green.tsv"
row() { printf '%s\t%s\t%s\n' "$1" "$2" "$3" >> "$L/green.tsv"; }
# TREE-1 (dev): a test run writes no tracked file.
sh $E/LW3.P3A/tree_write_check.sh "$W" "$L/tree1" > "$L/tree1.out" 2>&1; row TREE-1 $? "$(tail -1 $L/tree1/result.txt)"
# PERF-1c (a tree grep, no build): one KernelShape.
PT=$(grep -oE 'parse_that +=.*rev = "[0-9a-f]+"' Cargo.toml | grep -oE '[0-9a-f]{40}')
sh $E/LW3.PERF/perf1c/perf1c_check.sh "$W" HEAD $HOME/Programming/parse-that $PT > "$L/perf1c.out" 2>&1; row PERF-1c $? "$(head -1 $L/perf1c.out)"
# COMP-6 lattice (dev, bbnf-ir) and the stage verifier (dev, bbnf lib).
sh $E/LW3.COMP/comp6/laws.sh "$W" "$L/comp6-laws" > "$L/comp6-laws.out" 2>&1; row COMP-6-lattice $? "$(cat $L/comp6-laws/result.txt)"
sh $E/LW3.COMP/comp6/verify/verify.sh "$W" "$L/comp6-verify" > "$L/comp6-verify.out" 2>&1; row COMP-6-verifier $? "$(cat $L/comp6-verify/result.txt)"
# STALE-0 (dev): the metadata identity law.
cargo nextest run -p bbnf --locked --no-fail-fast --retries 0 --test w3_replay_public_contract \
  -E 'test(cargo_graph_build_feature_surfaces_and_generated_products_are_closed)' > "$L/stale0.log" 2>&1
row STALE-0 $? "$(grep -E '^ +Summary' $L/stale0.log | sed 's/^ *//')"
# COMP-5a (dev): the I7 lint and the root rename test.
cargo nextest run -p bbnf --locked --no-fail-fast --retries 0 --test no_grammar_name_branch --test plan_contract > "$L/comp5a.log" 2>&1
row COMP-5a $? "$(grep -E '^ +Summary' $L/comp5a.log | sed 's/^ *//')"
# S3c-PP css + ebnf (dev, cargo test).
{ cargo test -p gorgeous --features css-grammar --test css css_ws_leaves_are_trivia_so_formatting_is_idempotent; a=$?
  cargo test -p gorgeous --features ebnf-grammar --test ebnf -- ebnf_prettify_keeps_every_rule_and_terminal test_prettify_multi_rule; b=$?
  cargo test -p bbnf --test ebnf_prettify prettify_multi_rule; c=$?; echo "css=$a ebnf=$b core=$c"; } > "$L/s3cpp-cssebnf.log" 2>&1
r=$(tail -1 $L/s3cpp-cssebnf.log); case "$r" in "css=0 ebnf=0 core=0") rc=0;; *) rc=1;; esac; row S3c-PP-css-ebnf $rc "$r $(grep -c '^test .* ok$' $L/s3cpp-cssebnf.log) ok"
# S3c-PP bbnf (dev, cargo test).
{ cargo test -p gorgeous --features bbnf-grammar --test bbnf; a=$?
  cargo test -p gorgeous --features bin-full --test g3_trivia; b=$?; echo "bbnf=$a g3=$b"; } > "$L/s3cpp-bbnf.log" 2>&1
r=$(tail -1 $L/s3cpp-bbnf.log); case "$r" in "bbnf=0 g3=0") rc=0;; *) rc=1;; esac; row S3c-PP-bbnf $rc "$r $(grep -E '^test result' $L/s3cpp-bbnf.log | tr '\n' ' ')"
# PERF-1d (ax-iter xtask): no core::arch / std::simd in the emitter or the nine generated grammars; regen --check.
n=$(git grep -c 'core::arch\|std::simd' -- crates/core/src/backend crates/core/src/grammar/generated | wc -l | tr -d ' ')
cargo xtask regen --check > "$L/perf1d-regen.log" 2>&1; rc=$?
[ "$n" -eq 0 ] && [ $rc -eq 0 ]; row PERF-1d $? "grep files=$n regen-check rc=$rc"
# ORC2-WS-C (ax-iter): the falsifier, every test incl. the born-RED one.
cargo nextest run --cargo-profile ax-iter -p bbnf --locked --test rb_root_whitespace --run-ignored all --no-fail-fast > "$L/orc2wsc.log" 2>&1
row ORC2-WS-C $? "$(grep -E '^ +Summary' $L/orc2wsc.log | sed 's/^ *//')"
for p in dev ax-iter; do $S hash $p $H; done > "$L/hash-after.txt"
cat "$L/green.tsv"
