#!/bin/zsh
S=$HOME/.dev-logs/bbnf-lockstep/lw3/s5b-oracle; R=$S/bin/read.sh; W=$S/red-wt
export CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
cd $W; cargo clean -p bbnf -p bbnf-oracle -p xtask 2>&1 | tail -1 > $S/logs/green-bc-clean.receipt
for i in 1 2; do
  $R $S/logs/bom-green-r$i.log $W "LW3.ORC1-BOM GREEN r$i" -- cargo test -p bbnf-oracle --test orc1_bom
  $R $S/logs/csv-green-r$i.log $W "LW3.ORC1-CSV GREEN r$i" -- zsh -c 'cargo test -p bbnf --test orc1_entry_eof 2>&1 | grep -E "test result|panicked|accepted|refused"; cargo xtask regen --check 2>&1 | tail -1'
done
sed -i '' 's/    let trailing_ws = if ws_alias_entry {/    let trailing_ws = if true || ws_alias_entry {/' crates/core/src/backend/rust/emitter/grammar.rs
cargo xtask regen >/dev/null 2>&1
for i in 1 2; do
  $R $S/logs/csv-mutant-r$i.log $W "LW3.ORC1-CSV MUTANT r$i (the trailing skip emitted for every entry; regenerated)" -- cargo test -p bbnf --test orc1_entry_eof
done
git checkout -q -- crates/core
sed -i '' 's/{ fatal: true, ignoreBOM: true }/{ fatal: true }/' crates/oracle/ts/driver.mjs
for i in 1 2; do
  $R $S/logs/bom-mutant-r$i.log $W "LW3.ORC1-BOM MUTANT r$i (the TS driver decodes with the default, BOM removed)" -- cargo test -p bbnf-oracle --test orc1_bom
done
git checkout -q -- crates/oracle
git status --short
