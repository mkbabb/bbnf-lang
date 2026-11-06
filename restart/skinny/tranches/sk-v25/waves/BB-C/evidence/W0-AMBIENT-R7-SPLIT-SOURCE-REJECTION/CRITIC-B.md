MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: hostile source critic B.

Model and effort are self-reported and independently unverified.

# Verdict: REJECT

Date: 2026-07-22. Isolated regeneration is not authorized.

Custody passed: the source receipt hashes to
`f6b2493c5ad479d1bd08faec62be7a2070fc95e09c80729037cd2bffda7851d5`;
all 36 members, byte count, and ledger hashes reproduce. The predecessor
rejection identity also matches.

## Blocking defects

1. CSS checkpointing stores only `span_starts_len`. A speculative end can pop a
   pre-checkpoint value which `truncate` cannot restore. Snapshot/restore the
   complete vector or use a reversible journal; test start, checkpoint,
   speculative end, rollback, real end.
2. BBNF Pratt emits flat repeatable U8 Tag leaves while its lowerer recognizes
   only Span operators or a branch-tagged three-child reducer. Tags are treated
   as operands and lowering can panic. Consume authoritative tag metadata and
   prove repeated/mixed chains end to end.
3. Tape inline-packs `u64::from(u32::MAX)` as `TAPE_NONE`; cursor decoding then
   returns zero. Arena-store that boundary or add an independent residency bit;
   test `u32::MAX-1`, `u32::MAX`, `u32::MAX+1`, and `u64::MAX`.
4. DTA grammar-fact payload propagation loses mapped payload ownership through
   composites, including Seq/Repeat/Next/Skip, while signed types are projected
   into unsigned payload variants. Define composite ownership (kept side for
   Next/Skip), retain signedness, and add negative/mixed/non-U8 tests.
5. Alt, Wrap, Flat, and Keyword cast every non-U8 integer constant to U64;
   negative I64 values corrupt. HRegex has missing integer arms and silently
   maps decoding or overflow failure to zero. Carry exact signed and unsigned
   variants and fail structurally on invalid conversion.
6. The widened Alt domain is narrowed in `token_led_branches` with
   `branches.len() as u16`; 65,536 becomes zero. Keyword mining also forms a
   partial first-256 sidecar. Every narrower optimization must use a checked
   admission and disable the whole optimization outside its domain. Exercise
   255/256 and 65,535/65,536/65,537.
7. Non-generated tests retain `push_branch_tag`, so regeneration alone cannot
   compile the fleet. Migrate each call according to structural or semantic
   ownership with no compatibility path.
8. JSON maps every U8 to Null. Add an exact-zero guard and a nonzero rejection
   test.

Positive static facts do not cure the blockers: Sheets appends repeated U8
tags, CSS retains 255 plus a generic U8 fallback, simple runtimes preserve U8,
and current JSON zero becomes Null. No files were modified, no regeneration or
profiling ran, `cargo check -p xtask` passed, and the struct-registry test passed
14/14.
