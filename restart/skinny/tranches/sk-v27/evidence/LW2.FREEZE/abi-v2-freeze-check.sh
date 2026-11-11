#!/bin/sh
# sk-v27 LW2 LG-2.3 falsifier (S15 FREEZE): op.rs ABI v2 is frozen in pprint,
# PROVISIONAL for consumers (delegate ruling A-8).
# Usage: abi-v2-freeze-check.sh <pprint-worktree>
# GREEN (exit 0) iff all hold in the tree:
#  1. src/op.rs carries the three layout const asserts (FmtOp <= 24, align 8, GroupHdr == 16);
#  2. the crate is on the 0.4 version line;
#  3. ABI-v2.md has a "Freeze status" section that says FROZEN in pprint and
#     PROVISIONAL for consumers, citing A-8 and PP-U9;
#  4. rust/ABI-v2.sha256 exists and `shasum -a 256 -c` over it is rc 0, and it
#     names exactly src/op.rs and ABI-v2.md (so any later edit to either needs a
#     re-taken record, which is an ABI-v2.md addendum).
set -u
R=$1/rust; fail=0
no() { echo "RED: $*"; fail=1; }
for a in 'assert!(std::mem::size_of::<FmtOp<'"'"'static>>() <= 24);' \
         'assert!(std::mem::align_of::<FmtOp<'"'"'static>>() == 8);' \
         'assert!(std::mem::size_of::<GroupHdr>() == 16);'; do
  grep -qF "const _: () = $a" "$R/src/op.rs" || no "op.rs lacks const $a"
done
grep -qE '^version = "0\.4\.' "$R/Cargo.toml" || no "Cargo.toml not on the 0.4 line"
if grep -q '^## Freeze status' "$R/ABI-v2.md"; then
  sec=$(sed -n '/^## Freeze status/,/^## /p' "$R/ABI-v2.md")
  for w in FROZEN PROVISIONAL A-8 PP-U9; do
    printf '%s' "$sec" | grep -q "$w" || no "Freeze status section lacks $w"
  done
else no "ABI-v2.md has no Freeze status section"; fi
if [ -f "$R/ABI-v2.sha256" ]; then
  names=$(awk '{print $2}' "$R/ABI-v2.sha256" | sort | tr '\n' ' ')
  [ "$names" = "ABI-v2.md src/op.rs " ] || no "ABI-v2.sha256 names [$names], not exactly ABI-v2.md src/op.rs"
  ( cd "$R" && shasum -a 256 -c ABI-v2.sha256 ) || no "shasum -c over ABI-v2.sha256 failed"
else no "rust/ABI-v2.sha256 absent"; fi
[ $fail -eq 0 ] && { echo "GREEN: op.rs ABI v2 frozen in pprint, PROVISIONAL for consumers (A-8)"; exit 0; }
exit 1
