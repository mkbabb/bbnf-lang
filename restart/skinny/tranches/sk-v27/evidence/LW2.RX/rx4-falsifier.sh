#!/usr/bin/env bash
# sk-v27 LW2.RX-4 falsifier: FuseAltRegexBranches is absent from the pass list
# of the DFA-leaf backends and present in the TS backend's.
#
# Usage: rx4-falsifier.sh <bbnf-lang checkout> <log dir> <label>
# Reads, in the checkout (each cargo run through the machine guard):
#   1. crates/core/tests/rx4_rust_fusion_absent.rs (the VM run of
#      Alt(/a/, /ab/) on "ab" consumes 1 byte; the Rust product carries no
#      fused `a|ab`; the TS product does);
#   2. the printed e-graph pass list (BBNF_EGRAPH_REPORT=1) for a Rust-target
#      compile and a TS-target compile: `rule=fuse-alt-regex-branches` lines;
#   3. the TS emission of the 9 workspace entry grammars, hashed into
#      <log dir>/<label>-ts-emit.sha256 (compared across base and cure).
# GREEN (exit 0): (1) passes, the Rust list has 0 fusion lines and the TS
# list has 1. Anything else is RED (exit 1).
set -u
repo=$1; logs=$2; label=$3
mkdir -p "$logs"
guard=~/.claude/guard/guarded
export CARGO_TARGET_DIR=${CARGO_TARGET_DIR:-$HOME/.dev-logs/bbnf-lockstep/lw2/s7-rx/target-bbnf}
cd "$repo" || exit 2
sha=$(git rev-parse --short=9 HEAD)
t=rx4_rust_fusion_absent
"$guard" --timeout 60 -- cargo test -p bbnf --locked --test $t -- --skip rx4_ >"$logs/$label-tests.log" 2>&1
tests_rc=$?
BBNF_EGRAPH_REPORT=1 "$guard" --timeout 30 -- cargo test -p bbnf --locked --test $t rx4_print_pass_list_rust -- --nocapture >"$logs/$label-passlist-rust.log" 2>&1
BBNF_EGRAPH_REPORT=1 "$guard" --timeout 30 -- cargo test -p bbnf --locked --test $t rx4_print_pass_list_ts -- --nocapture >"$logs/$label-passlist-ts.log" 2>&1
rust_fuse=$(grep -c 'rule=fuse-alt-regex-branches' "$logs/$label-passlist-rust.log")
ts_fuse=$(grep -c 'rule=fuse-alt-regex-branches' "$logs/$label-passlist-ts.log")
dump="$logs/$label-ts-emit"
rm -rf "$dump"
RX4_TS_DUMP="$dump" "$guard" --timeout 60 -- cargo test -p bbnf --locked --test $t rx4_ts_emit_dump >"$logs/$label-ts-dump.log" 2>&1
(cd "$dump" && shasum -a 256 * | sort -k2) >"$logs/$label-ts-emit.sha256"
files=$(wc -l <"$logs/$label-ts-emit.sha256" | tr -d ' ')
verdict=RED
[ "$tests_rc" = 0 ] && [ "$rust_fuse" = 0 ] && [ "$ts_fuse" = 1 ] && verdict=GREEN
echo "rx4 $label @ $sha: tests rc=$tests_rc; rust pass list fusion lines=$rust_fuse; ts pass list fusion lines=$ts_fuse; ts emissions hashed=$files -> $verdict"
grep -E '^test .*\.\.\. (ok|FAILED)|panicked|assertion' "$logs/$label-tests.log" | head -20
[ "$verdict" = GREEN ]
