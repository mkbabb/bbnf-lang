# SK-V10 W1 CHALLENGE - Direct Output Contract

Pass: CHALLENGE.
Cycle: W1.
Date: 2026-05-19.
Plan under review: `restart/skinny/tranches/sk-v10/research/w1/w1-plan.md`.
Disposition: ACCEPT.

## CH1 Correctness - ACCEPT

The plan targets the missing correctness boundary: a future direct row cannot
move by changing only `Outcome` and `Verdict`. Requiring `A / GO`, digest
output plane, strict direct semantics, measured-row validation, REDRESS
provenance, non-gate-only consumer, Track 2 independence, and same-run native
direct comparator sources is sufficient for W1's contract-only role.

Redress requirement: add negative tests for output-plane mismatch,
view-boundary validation, gate-only consumer, and missing REDRESS.

## CH2 Generality / Lock 14 - ACCEPT

The plan edits only JSON bench report validation. It does not add generic JSON
policy to generic crates, codegen, `bbnf-simd`, `parse-that-regex`, or runtime
outside JSON. Section 2.1 generic-proof obligations are not triggered.

## CH3 Regression / REDRESS - ACCEPT

The plan preserves REDRESS 73, 93, 50-55, and 66-69 blocks. It does not
reintroduce helper-shape transfer, scalar-parent folding, source-hook receivers,
scratch buffers, direct byte-output unescape, or semantic string facts. It also
does not move `RESULTS.md`, so existing W0/W1 row state is preserved.

## CH4 Cost - ACCEPT

The plan is within the 180-320 docs/gate LOC budget. The expected source diff
is a report predicate plus tests. No Criterion rerun is needed because W1 is
contract-only; gate-json over the frozen W1 capture is the correct evidence.

## CH5 Hidden Coupling / Lock 1 - ACCEPT

The contract is report-side and does not create a substrate, sidecar producer,
parser-owned state, or direct materialization path. It keeps direct digest,
typed product, and parse-only planes distinct.

## CH6 Anti-Paper-Close - ACCEPT

The plan closes only if `gate-json` consumes the predicate in the same wave and
tests prove rejects for incomplete movement. A contract function that is not
called by report validation would be a paper-close and is not accepted.

## Accepted Redress Conditions

- `Report::validate_sk_v8_w0` must call the new direct movement contract.
- Current frozen W0 rows must still pass unchanged.
- A baseline `N-direct` row changed to `A / GO` must be accepted only after the
  direct contract fields are updated.
- Missing strictness, measured validation path, non-gate consumer, REDRESS
  entry, or direct comparator provenance must fail closed.
- No `RESULTS.md` row movement is permitted in W1.
