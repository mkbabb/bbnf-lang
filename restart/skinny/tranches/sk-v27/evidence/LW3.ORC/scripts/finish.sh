#!/bin/zsh
# Finish: ORC2 with the committed falsifier (RED x2 at 118e9d097, GREEN x2 + stale-transcription mutant x2 at 355276cdc),
# the STR mutant x2 at its cure c7d99e431 (the earlier r1 log was cut), regen --check x2 at 355276cdc.
S=$HOME/.dev-logs/bbnf-lockstep/lw3/s5b-oracle; R=$S/bin/read.sh; W=$S/red-wt; L=$S/logs; C=$S/bin/orc2_check.sh
export CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang BBNF_CORPUS_DIR=$S/corpus-v3-r1
G=$CARGO_TARGET_DIR/debug/g1
cd $W
git checkout -q --detach 118e9d097
cargo clean -p bbnf-oracle -p bbnf 2>&1 | tail -1 > $L/orc2c-red-clean.receipt
cargo build -q -p bbnf-oracle --bin g1 || exit 3
for i in 1 2; do $R $L/orc2c-red-r$i.log $W "LW3.ORC2 falsifier orc2_check.sh RED r$i" -- $C $G; done
git checkout -q --detach 355276cdc
cargo clean -p bbnf-oracle -p bbnf -p xtask 2>&1 | tail -1 > $L/orc2c-green-clean.receipt
cargo build -q -p bbnf-oracle --bin g1 || exit 3
for i in 1 2; do $R $L/orc2c-green-r$i.log $W "LW3.ORC2 falsifier orc2_check.sh GREEN r$i" -- restart/skinny/tranches/sk-v27/evidence/LW3.ORC/orc2_check.sh $G; done
for i in 1 2; do $R $L/regen-check-r$i.log $W "regen --check at the lane r$i" -- cargo xtask regen --check; done
printf '\n' >> grammar/ebnf/ebnf.bbnf
for i in 1 2; do $R $L/orc2c-mutant-r$i.log $W "LW3.ORC2 MUTANT r$i (ebnf.bbnf gains a byte; its transcription is stale)" -- restart/skinny/tranches/sk-v27/evidence/LW3.ORC/orc2_check.sh $G; done
git checkout -q -- grammar/ebnf/ebnf.bbnf
git checkout -q --detach c7d99e431
cargo clean -p bbnf 2>&1 | tail -1 > $L/str-mutant-clean.receipt
F=crates/core/src/backend/rust/emitter/shapes/string.rs
sed -i '' 's/^fn refused_plain_bytes(rule: &IrRule, ir: &GrammarIR, quote: u8) -> Vec<u8> {$/&\
    if quote != 0 { return Vec::new(); }/' $F
git diff --stat
cargo xtask regen --grammar json >/dev/null 2>&1
for i in 1 2; do $R $L/str-mutant-r$i.log $W "LW3.ORC1-STR MUTANT r$i (refused_plain_bytes returns no byte: the scanner ignores the class's refused bytes; json regenerated)" -- cargo test -p bbnf --test orc1_json_string; done
git checkout -q -- crates
git status --short
git checkout -q --detach 355276cdc
