# SK-V7 Wave 10c R2 - Closure Protocol

Date: 2026-05-16.
Scope: research-only closure protocol for W10c if it admits only B6 Stage 1
while both W10 bitmap asm body fills remain rejected with measurement.
Output: this file.

## Conclusion

W10c may close as an **admit** only for the narrowed intervention: B6 Stage 1
stack-canary XOR-fold hardening. It must not claim the original W10 §12 bitmap
body-fill exit gate was met. The PMULL prefix-XOR body and CSSC/`ctz`
next-bit/bulk-consumer body remain **rejected for SK-V7** because REDRESS items
88 and 89 measured parse-row regressions after correctness passed.

The W10c redress commit should therefore be named as an admit, not a reject,
but with a concrete B6-only subject:

```text
feat(sk-v7-wave10c): admit B6 stack-canary Stage 1
```

Do not use a subject that implies the asm bodies landed, such as
`admit bitmap body fills`, `admit W10`, or `admit CTZ/PMULL`.

Pass Alpha may proceed after W10c only under an honest convergence reading:
W10/W10b executed the bitmap candidates and rejected them with measurement;
W10c admits the independent B6 hardening slice with zero `RESULTS.md` diff; the
remaining bitmap body fills are named as routed SK-V8 work, not silently closed.

## Basis

`restart/skinny/tranches/sk-v7/SPEC.md` §12 originally required both bitmap
primitives admitted, checkasm green, same-wave consumer wired, B6 Stage 1
landed, and no row regressions. That composite exit gate is no longer available
after the measured W10 and W10b rejections.

`skinny/REDRESS.md` item 88 rejects the first W10 consumed bitmap candidate:
PMULL prefix-XOR was correct and visible in asm, but JSON parse measurement
regressed hard rows, including `numbers/track1_generated` and
`unicode_escapes` Track 1/2. Item 88's next shape kept B6 and CTZ while leaving
PMULL scalar on the hot path.

`skinny/REDRESS.md` item 89 then rejects that narrowed W10b shape:
prefix-XOR stayed scalar and PMULL was not reopened, but the production
`bitmap_next_set_bit`/bulk-consumer change still regressed multiple Track 1/2
rows by more than the W10b maintain invariant allowed. Item 89's next candidate
is W10c: admit only B6 canary hardening as Stage 1, leave both bitmap asm body
fills rejected, and require zero `RESULTS.md` diff.

`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` permits a wave to
converge when planned candidates have been executed as admit or reject with
measurement, or when no remaining candidate from the shortlist would lift a
named row. It also requires redress commits to be named by the measured outcome
of the planned intervention. For W10c, the planned intervention is B6-only.

## REDRESS Entry Requirements

The W10c REDRESS entry must state all of the following.

1. **B6 Stage 1 is ADMITTED.** Name the exact admitted slice as checkasm
   stack-canary XOR-fold hardening, not a production SIMD primitive. Cite the
   touched test-harness paths and the live wrapper reach:
   `checkasm_common::with_stack_canary_xor_fold`, compatibility forwarders,
   and the migrated private canary wrappers.

2. **Both bitmap asm body fills remain REJECTED for SK-V7.** Cite REDRESS item
   88 for PMULL prefix-XOR and item 89 for CTZ next-bit/bulk consumer. The entry
   should say W10c did not edit or admit the production body files for those
   candidates and did not reopen their failure modes.

3. **Measurement evidence is explicit.** Include the W10c gates:
   static wrapper audit, negative canary-reach failures under temporary
   injection, `primitive-checkasm` after injection removal, and the required
   zero `RESULTS.md` diff. The entry should also summarize the item 88/89
   regression evidence as the reason the bitmap bodies remain rejected.

4. **Original §12 status is reconciled honestly.** State that the original W10
   composite exit gate is not met as written because "both primitives admitted"
   is false. The tranche closure disposition is partial: B6 Stage 1 admitted;
   bitmap body fills rejected and routed.

5. **Followup is named.** Route remaining bbnf.asm body-fill work to SK-V8 or
   the next Pass Alpha packet. Do not use a generic future placeholder.

## Pass Alpha Disposition

Pass Alpha may proceed after W10c if these conditions are true:

- W10c's B6-only gate passes, including zero `RESULTS.md` diff.
- REDRESS contains the B6 admit plus the explicit retained rejection status for
  PMULL and CTZ/body-consumer routes.
- `PROGRESS.md`, `FINAL.md`, or the SK-V7 close material marks W10 §12 as
  partially admitted/routed rather than fully met.
- The next-letter packet names the remaining bitmap body fills as SK-V8 input.

Pass Alpha must not proceed on the claim that W10 §12 closed green. It may
proceed on convergence: every W10 candidate has either been admitted in the
narrowed B6-only form or rejected with measurement, and the remaining primitive
body work has a named destination.
