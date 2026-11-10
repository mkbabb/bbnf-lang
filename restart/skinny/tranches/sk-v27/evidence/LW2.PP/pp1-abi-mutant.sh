#!/bin/sh
# sk-v27 LW2.PP-1 falsifier: a 32-byte FmtOp mutant must fail to compile.
# Usage (under guarded): pp1-abi-mutant.sh <pprint-worktree>
# The mutant widens Sep.flat from [u8; 8] to [u8; 24] at its two sites (the
# variant and the builder's buffer), so the stream still type-checks and the
# only possible refusal is op.rs's const size assert. The files are restored
# from copies afterwards. Exit 0 iff the mutant is refused by that assert.
set -u
WT=$1; R=$WT/rust; S=$(mktemp -d "$HOME/.dev-logs/bbnf-lockstep/lw2/s8-pp/pp1mut.XXXXXX")
cp "$R/src/op.rs" "$S/op.rs"; cp "$R/src/builder.rs" "$S/builder.rs"
restore() { cp "$S/op.rs" "$R/src/op.rs"; cp "$S/builder.rs" "$R/src/builder.rs"; rm -rf "$S"; }
trap restore EXIT
sed -i '' 's/flat: \[u8; 8\],/flat: [u8; 24],/' "$R/src/op.rs"
sed -i '' 's/let mut flat_buf = \[0u8; 8\];/let mut flat_buf = [0u8; 24];/' "$R/src/builder.rs"
grep -q 'flat: \[u8; 24\]' "$R/src/op.rs" && grep -q '\[0u8; 24\]' "$R/src/builder.rs" || { echo "mutant not applied"; exit 2; }
( cd "$R" && cargo check --lib --message-format short 2>&1 ) > "$S/out.log"; rc=$?
grep -E 'error|assert' "$S/out.log" | head -8
echo "cargo check rc=$rc"
if [ $rc -ne 0 ] && grep -q 'size_of::<FmtOp' "$S/out.log"; then echo "MUTANT REFUSED by the size assert"; exit 0; fi
echo "MUTANT NOT REFUSED"; exit 1
