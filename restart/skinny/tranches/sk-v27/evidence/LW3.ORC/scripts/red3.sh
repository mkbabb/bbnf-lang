#!/bin/zsh
S=$HOME/.dev-logs/bbnf-lockstep/lw3/s5b-oracle; R=$S/bin/read.sh; W=$S/red-wt
export CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
cd $W; cargo clean -p bbnf -p bbnf-oracle 2>&1 | tail -1 > $S/logs/red3-clean.receipt
for i in 1 2; do
  $R $S/logs/bom-red-r$i.log $W "LW3.ORC1-BOM RED r$i" -- cargo test -p bbnf-oracle --test orc1_bom
  $R $S/logs/ws-red-r$i.log $W "LW3.ORC1-WS RED r$i" -- cargo test -p bbnf-oracle --test orc1_json_ws
  $R $S/logs/csv-red-r$i.log $W "LW3.ORC1-CSV RED r$i" -- cargo test -p bbnf --test orc1_entry_eof
done
