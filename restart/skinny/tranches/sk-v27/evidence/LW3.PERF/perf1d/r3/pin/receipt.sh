#!/bin/sh
set -u
P=$HOME/.dev-logs/bbnf-lockstep/lw3/perf-1d/r3/pin
export CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang BBNF_CORPUS_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/s7-spine/corpus
cd $HOME/Programming/bbnf-lang-wt/lw3-s9b || exit 2
BBNF_W3_ABI4_RECEIPT_OUT=$P/receipt-new.json cargo iter-test-core --test w3_replay_public_contract -E 'test(exact_packages_cross_a_fresh_local_registry_and_public_runtime)' > $P/receipt-record.log 2>&1; echo "rc=$?" >> $P/receipt-record.log
