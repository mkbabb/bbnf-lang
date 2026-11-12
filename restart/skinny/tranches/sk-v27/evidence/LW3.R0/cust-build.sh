#!/bin/sh
# DIG-CUST-1 compile proof: the committed bounded_sum builds and its tests pass
# in the workspace it was written in, on that workspace's pinned toolchain.
# Usage: cust-build.sh <worktree> <example-or-none>
set -u
W=$1; EX=$2
export CARGO_TARGET_DIR=$HOME/.dev-logs/bbnf-lockstep/lw3/target-bbnf-lang
cd "$W" || exit 2
echo "head $(git rev-parse HEAD) tree $(git rev-parse HEAD^{tree}) dirty=$(git status --porcelain | wc -l | tr -d ' ')"
TC=$(sed -n 's/^channel = "\(.*\)"/\1/p' rust-toolchain.toml)
echo "toolchain $TC: $(rustup run "$TC" rustc --version)"
echo "== cargo clean -p csp-solver (TGT-1 receipt)"
cargo +"$TC" clean -p csp-solver 2>&1 | tail -2
echo "== cargo test -p csp-solver --test bounded_sum"
cargo +"$TC" test -p csp-solver --test bounded_sum 2>&1 | grep -E '^(test |test result|error|warning: unused)|Running|Finished' ; rc1=$?
r=$(cargo +"$TC" test -p csp-solver --test bounded_sum 2>&1 | grep -c '^test result: ok')
if [ "$EX" != none ]; then
  echo "== cargo build -p csp-solver --example $EX"
  cargo +"$TC" build -p csp-solver --example "$EX" 2>&1 | tail -2; rc2=$?
  ex=$(cargo +"$TC" build -p csp-solver --example "$EX" 2>&1 | grep -c 'Finished')
else ex=1; fi
echo "dirty-after=$(git status --porcelain | wc -l | tr -d ' ')"
[ "$r" -eq 1 ] && [ "$ex" -eq 1 ] && { echo GREEN; exit 0; }
echo RED; exit 1
