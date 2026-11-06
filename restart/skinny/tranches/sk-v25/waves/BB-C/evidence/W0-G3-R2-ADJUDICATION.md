# BB-C.W0 G3 deletion R2 — terminal adjudication

MODEL: `gpt-5.6-sol`; REASONING: `ultra`; WORKFLOW: `v2`

Fresh adjudicator identity:
`/root/g3_r2_critic_a/g3_r2_adjudicator`.

Status: **RED EVIDENCE GATE; REJECT R2 EVIDENCE; ACCEPT THE NARROW
STATIC PRODUCTION DELETION; HOSTILE CLEAN COUNT 0/2.**

## Frozen replay

All five R2 subject hashes and both critic-report hashes matched. Current Apple
ARM replay passed 9/9 IR and 1/1 semantic tests; working `Cargo.lock` remained
`480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9`.
An independently reconstructed historical overlay reproduced all three recorded
exit-101 failures and retained crater lock
`a6a75b64633dac671206b06bea3c8b8b18ccd180b04712dc04dcc4c423160908`.

## Rulings

1. **The diagnostic counterexample is not G3-reachable — correct.** Prefix
   factoring can synthesize `Alt([Ref(atom), Epsilon])`, but historical G3 accepts
   a `Ref` only when its rule has a scalar entry in `ir.types`. Lowering initializes
   that table empty, type projection runs after the e-graph, and recursive `atom`
   is not a literal leaf. The historical command also aborts at the preceding
   empty-input assertion before constructing the diagnostic program. No
   diagnostic-plane RED was executed.
2. **The crater dependency closure is unsealed — correct.** Archived Cargo config
   resolves `parse_that`, `bbnf-regex`, `pprint`, and `pprint_derive` from live
   sibling paths. Path dependencies have no content checksum in the lock, and no
   sibling file ledger or immutable archive was retained.
3. **Twin equality is uncheckable — correct.** Digest `9784c4ef…` exists only in
   prose. There are no raw runs, exit receipts, normalized payloads, executable
   normalization recipe, or toolchain identity from which to recompute it.

## Credit retained and denied

Retained: byte-identical production deletion; absence of the G3 implementation,
match/type machinery, export, registration, mechanics tests, and named route;
zero forbidden-symbol census; exact default allowlist; real saturation/write-back
negative witness; and exact empty/`"x"` values, trees, tags, children, spans,
offsets, and diagnostics.

Denied: R2 evidence or hostile-pass credit, BB-C.W0 completion, optimizer, graph,
package, performance, wave, movement, and BB-A credit.

## Required R3

1. Add an independent required-context nullable-alternative diagnostic test and
   assert its complete failure result.
2. Execute that test independently against pre-deletion bytes and retain its own
   intended RED.
3. Archive and content-ledger the complete parse-that/pprint path dependency
   closure, then retain locked/offline Cargo metadata proving those resolved
   package paths.
4. Retain raw A/B streams, exits, exact commands/environment, Apple ARM toolchain
   identity, executable normalization recipe, normalized payloads, per-run
   hashes/equality, and before/after working/crater lock hashes.
5. Preserve accepted production bytes, freeze the R3 subject/artifacts, and reset
   both hostile passes to zero.

No production movement follows.
