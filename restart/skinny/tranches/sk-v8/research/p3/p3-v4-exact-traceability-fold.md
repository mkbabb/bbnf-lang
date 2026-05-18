# SK-V8 S-P3 V4 Exact Traceability Fold

Date: 2026-05-18.
Scope: Fold CH1 V3 correctness/doc-link critique without changing SK-V8 wave semantics.

## Inputs

- `restart/skinny/tranches/sk-v8/research/p3/hardening/V3/CH1.md`
- `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md`

## CH1 V3 Disposition

CH1 rejected V3 at 88 confidence because several inline references were still broad enough to be paper-close artifacts rather than audit-ready links. V4 folds that critique by replacing the rejected citation bundles with exact labels or current file:line anchors:

| Rejected V3 citation shape | V4 disposition |
|---|---|
| Broad SPEC multi-section bundles | Replaced with exact labels such as `SPEC Section 0.4 - Required Telemetry`, `SPEC Section 6 - W3 Tier A Tape Plus Structural-Projection Union`, and `SPEC Section 11 - G-Alpha And Dispatch Scope` at the claim sites. |
| Broad HANDOFF multi-section bundles | Replaced with exact labels such as `HANDOFF Section 2 - Current Measured State`, `HANDOFF Section 3a - Substrate-Ceiling Finding`, `HANDOFF Section 5 - Entry Gates`, `HANDOFF Section 6 - Exit Condition`, and `HANDOFF Section 10 - G-Alpha Decision`. |
| Generic RESULTS current-row/Track-2 placeholders | Replaced with current anchors `skinny/RESULTS.md:3-42` and `skinny/RESULTS.md:217-218`. |
| Generic REDRESS named-row placeholders | Replaced with `skinny/REDRESS.md` plus live anchor spans `skinny/REDRESS.md:1214-1219`, `skinny/REDRESS.md:1301-1312`, and `skinny/REDRESS.md:1331-2605` where the broader digest spans are being referenced. |

## Preserved Semantics

The fold is intentionally traceability-only. It preserves:

- G-Alpha/W0-only dispatch lock;
- strict-vs-strict comparator discipline;
- Lock 14 grammar-neutrality and non-JSON proof obligations;
- no new directive, BIR variant, substrate, `BackendShape`, `UnionTape`, public substrate API, parser-owned cursor/facts, sidecar substrate, or consumer-later primitive;
- W2 typed seed table and W2 plan-update requirement;
- W3 Tier A/Tier B split, scalar/checkasm requirement, same-wave production consumer requirement, and `tape_vs_tape` demotion;
- per-wave 90-minute implementation/redress cap and source/edit LOC budgets;
- pre-blocked REDRESS route coverage.

## Verification

Before V4 challenge, the fold must satisfy:

- no remaining instance of the four rejected broad V3 citation bundles in P3 artifacts;
- no unresolved local path introduced by this fold;
- `git diff --check` passes.

## Next Challenge

Dispatch CH1-CH6 against V4. A qualifying cycle requires every challenged role to return ACCEPT with confidence >=95 and no open critical defect. If V4 qualifies, run one more independent unchanged challenge cycle before declaring S-P3 converged.
