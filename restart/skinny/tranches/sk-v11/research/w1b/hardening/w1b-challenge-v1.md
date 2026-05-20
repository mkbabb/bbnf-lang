# SK-V11 W1b CHALLENGE V1

Date: 2026-05-20.
Wave: W1b - Generated Non-JSON Baseline And Oracle Lane.
Plan under review:

- `restart/skinny/tranches/sk-v11/research/w1b/w1b-plan-implementation.md`
- `restart/skinny/tranches/sk-v11/research/w1b/w1b-plan-gate-matrix.md`

Disposition: REJECT. The plan returns to Phase 2 for a narrow revision.

## Lens Dispositions

| Lens | Disposition | Finding |
|---|---|---|
| CH1 correctness | ACCEPT | The selected row is exactly `css_l4/declaration_values/direct/main`; strict direct fact-byte equality, generated Track 1, and independent oracle evidence are required. Missing generated Track 1 is an honest REDRESS result, not a W1b close. |
| CH2 generality / Lock 14 | ACCEPT | The plan rejects prose-only generality, JSON-provider relabeling, generated JSON reuse, and old hand CSS runtime substitution. Current source still lacks generated CSS Track 1 authority. |
| CH3 regression / REDRESS | REJECT | The plan authorizes `skinny/crates/bbnf-bench/src/track2/css_l4.rs` and `track2/mod.rs`, but SPEC Section 5 owns `skinny/crates/bbnf-bench/src/bin/gate.rs`, `report.rs`, `benches/`, and not `src/track2/`. |
| CH4 cost | ACCEPT | The measured-rejection route fits the <=360 handwritten LOC and <=90 min W1b budget. A positive generated CSS route would overrun unless generated Track 1 already exists. |
| CH5 hidden coupling | REJECT | The field table does not yet require gate-consumed `track1_source_artifact`, generated input/output provenance, source kind, or strict equality/mismatch artifact fields. The oracle independence failure classes must become required implementation tests. |
| CH6 anti-paper-close | ACCEPT | The plan blocks schema-only and prose-only close, requires `S / NO-GO` for any positive baseline report, and preserves W2's dependency on an actual W1b baseline. |

## Required Redress To Plan

1. Remove `skinny/crates/bbnf-bench/src/track2/` from all W1b owner, budget,
   and revert language. Any positive-route oracle source must live in a
   reviewable module under an existing W1b owner path.
2. Add gate-consumed W1b report fields for Track 1 source artifact, generated
   input/output provenance, Track 1 source kind, and strict fact-byte equality
   status/artifact.
3. Mark the R4 digest-route and R5 typed-route research recommendations as
   superseded by the Phase 2 selection:
   `css_l4/declaration_values/direct/main` on
   `css_l4_declaration_value_fact_bytes`.
4. Promote every oracle-coupling class named by CH5 into an explicit required
   implementation test or fixture class.

The plan may return to CHALLENGE after those narrow revisions.
