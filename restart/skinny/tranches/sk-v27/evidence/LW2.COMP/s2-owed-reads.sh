#!/bin/zsh
# S2 owed merge reads (S14 S12-7, S13-F5), x2 each, at lw2/s2-census HEAD
cd ~/Programming/bbnf-lang-wt/lw2-s2-census || exit 9
d=~/.dev-logs/bbnf-lockstep/lw2/s2-census
echo "HEAD $(git rev-parse HEAD)"
for i in 1 2; do
  cargo nextest run -p bbnf --test pratt_const_fold > $d/owed-pratt-$i.log 2>&1; echo "pratt_const_fold #$i rc=$?"
  cargo xtask regen --check > $d/owed-regen-$i.log 2>&1; echo "regen --check #$i rc=$?"
  cargo test -p bbnf-ir --test substrate_audit -- --nocapture > $d/owed-audit-$i.log 2>&1; echo "substrate_audit #$i rc=$?"
  cargo clippy -p bbnf --benches --features wasm-bench -- -D warnings > $d/owed-clippy-wasmbench-$i.log 2>&1; echo "clippy wasm-bench #$i rc=$?"
done
