# CH3 S-P3 V3 Regression / Pre-Block Challenge

Date: 2026-05-18
Pass: SK-V8 S-P3 hardening V3
Lens: CH3 regression / pre-block challenge

## Scope

Reviewed the V3 citation fold against the live S-P3 packet and historical route ledger:

- `restart/prompts/ORCHESTRATOR.md` sections 3W and 3Z
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3-v3-citation-fold.md`
- `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

This review checks only regression and pre-block safety: no reopened REDRESS or historical blocked route, no loosened strict-vs-strict comparator, no lost direct/typed guard rows, and no behavior/status regression without row gates.

## Verdict

ACCEPT, confidence 96%.

## Blockers

None.

## Evidence

### V3 fold is citation-only and preserves V2 gates

`p3-v3-citation-fold.md` scopes V3 to disposition of V2 CH1 into P3-A through P3-F, replacing stale line-number citations with stable section labels and preserving V2 hardening content. Its preserved-gates list keeps the W2 typed seed table and recompute gate, W0 future-artifact pattern, LOC/time governance, W3 pre-redress fit/split rule, strict-vs-strict comparator discipline, Lock 14 grammar-neutrality, no new directive/BIR/substrate/API/BackendShape/UnionTape, and the G-Alpha rule that a closed G-Alpha authorizes W0 only.

`HARDENING-S-P3-V2-CONSOLIDATED.md` left only citation traceability as the V3 fold requirement. CH3 in V2 already ACCEPTed the regression/pre-block lens at 96%, including REDRESS/pre-block preservation, strict-vs-strict preservation, direct/typed guard rows, and behavior/status row gates. V3 addresses that citation-only blocker without changing the wave plan substance.

### Pre-blocked routes remain closed

`ORCHESTRATOR.md` section 3W requires CH3 to verify that no proposal reopens a route in `skinny/REDRESS.md`, that the pre-block list is correctly identified, and that no admitted row silently regressed. Section 3Z does not allow convergence without zero unresolved REVISE findings.

`PASS-3-SYNTHESIS-PLAN.md` requires P3-E to enumerate REDRESS/pre-blocked routes and specifically names REDRESS 28+33, 50-55, 60-72, 80, 82-84, 88, and 89 plus historical blocked routes. It also requires same-row falsifiability, same-wave consumers, no hypothesis transfer, no wave without gates, and no reopening pre-blocked REDRESS.

The live packet carries that ledger in three places:

- `SPEC.md` section 10 requires fresh W0 evidence, same-wave consumer, scalar/checkasm where relevant, no-regression gate, REDRESS citation, and challenge acceptance before any reopened route. Its global and specific blocks include directive/BIR/substrate/BackendShape/UnionTape/API/parser-owned/sidecar/parallel routes, stale/permissive/lossy strict anchors, `tape_vs_tape`, parse-only telemetry as W3 consumer, Track1/Track2 coupling, automatic dispatch, REDRESS 28+33, 49-55, 59-65, 66-72, 80, 82-84, 88-90, B6 constraints, and Tier B in W3.
- `DISPATCH-PROMPT.md` repeats the conditional gates and pre-block routes, including REDRESS 28+33, 50-55, 60-72, 80, 82-84, 88, 89, B6 canary limits, historical function-pointer/pair-token/12-byte/separator/SWAR/capacity/sidecar/raw-f64/orphan routes, and Tier B blocked from W3.
- `p3e-preblocked-ledger.md` carries the required P3-E ledger and keeps `tape_vs_tape`, parse-only, and telemetry out of W3 consumer status.

Historical blocked routes remain materially covered by `REDRESS.md`: REDRESS 28 and 33 reject tiny-string and `match_tiny_plain_string`; 50-55 reject aux side tables, EventCursor, parser-local structural cursor, decoded stats, and quote-source materialization; 60-72 reject retained string-boundary/direct-fusion routes; 80 rejects stale mantissa widen; 82-84 reject single-quartet Unicode, StringBlock16, and object-pair value-byte routes; 88 and 89 reject PMULL default and CTZ/bulk bodies. V3 does not authorize any of these as implementation routes.

### Strict-vs-strict comparator discipline remains intact

`SPEC.md` section 0.2 separates same-run strict anchors from flaw probes and sidecar planning signals, and rejects plane mismatch, strictness mismatch, stale sidecars, and view-boundary or historical anchors. Section 2 keeps strict-vs-strict as a non-negotiable and rejects stale sidecar, lossy, historical, or view-boundary strict comparisons.

`p3c-falsifiability-gates.md` keeps strict admission executable by gate, not narrative: sidecar telemetry is planning-only, parse-only cannot be promoted to structural behavior proof, and strict anchors must be same-run and same-plane. `RESULTS.md` still records the current outcome as `N-direct / NoGo`, with Track1 generated and Track2 independent, and explicitly marks C++ sidecar columns as planning signals rather than same-run strict anchors.

No V3 text converts K/S flaw-probe rows, sidecar measurements, parse-only deltas, `tape_vs_tape`, or Track2 audit data into strict admission evidence.

### Direct and typed guard rows remain mandatory

`SPEC.md` section 0.5 preserves the opening row goalset: 17 `parse_only` rows are substrate-guard NO-GO, six direct rows are A/GO with eleven direct rows N-direct/NO-GO, and four `real_typed_struct` rows are A/GO. W2 is bound to the seed table unless a later accepted S-P3 revision expands it, and it must maintain current real-typed GO floors and existing direct GO guard floors. W4 must maintain existing direct GO and real-typed GO rows while proving any selected direct rows with generated Track1 and independent Track2.

`p3c-falsifiability-gates.md` and `p3f-spec-draft.md` preserve the V2 folds that direct digest rows are guard rows only, not typed-product proof; W2 must keep the existing typed GO maintain rows and direct GO guard rows; W3 must not substitute telemetry-only or direct/path rows for structural product proof.

`RESULTS.md` still provides the live row/status artifacts for these gates, including the four real typed GO rows (`twitter`, `update_center`, `mesh`, `marine_ik`) and the existing direct GO guard rows (`citm_catalog`, `apache_builds`, `mesh`, `marine_ik`, `numbers`, `unicode_basic`). V3 does not drop or weaken those row gates.

### W3 does not smuggle Tier B, `tape_vs_tape`, PMULL/CTZ, sidecars, or parser-owned cursor

`SPEC.md` W3 entry and exit gates constrain the wave to one Tier A Tape representation replacement with generated parser production consumer, exactly one tape, scalar/checkasm evidence, Lock 14 proof, full-table no-regression, and challenge acceptance. It explicitly blocks Tier B, `tape_vs_tape` as consumer, PMULL/CTZ, sidecar event vectors, aux tables, parser-owned structural cursors, old append survival, second scans, BackendShape, UnionTape, directive/BIR/API, and local materialization routes.

`DISPATCH-PROMPT.md` repeats that W3 is conditional on W0/W1, fresh plan, challenge, Lock 1, pre-redress fit, and generated retained parser consumer, with Tier B, `tape_vs_tape`, sidecar/aux/parser-owned cursor, old append, new substrate, UnionTape, BackendShape, directive/BIR/API blocked.

`p3a-candidate-shortlist.md`, `p3b-wave-sequencing.md`, `p3c-falsifiability-gates.md`, and `p3f-spec-draft.md` all keep W3 as Tier A only and keep Tier B, `tape_vs_tape`, PMULL/CTZ, sidecars, and parser-owned cursor out of W3 implementation scope. V3 does not reopen those routes.

### Behavior/status regression remains gated

`SPEC.md` section 0.1 requires W0 baseline telemetry across all current rows before any source change, requires behavior waves to meet named row thresholds and full-table maintain gates or write REDRESS, and requires RESULTS/REDRESS/HANDOFF agreement before close. Section 0.4 rejects missing telemetry, stale sidecars, producer-only telemetry, W0 behavior drift, W1 CostFacts absence, W3 side substrate, W3 telemetry substitution, Lock 14 leaks, and cap overflow. W6 requires rows/status artifacts to match latest evidence before close.

`DISPATCH-PROMPT.md` keeps W0 telemetry-only, forbids parser/scanner/SIMD/asm/codegen/product/generated-output changes in W0, and makes W1-W6 conditional on admitted W0, exact owner paths, row gates, pre-block citations, rollback, same-wave consumers, challenge where required, Lock 14, and 90 minute split rules.

`HANDOFF.md` preserves the current measured state and explicitly says G-Alpha closed dispatches W0 only; no implementation is authorized before G-Alpha, and W1-W6 remain conditional.

No V3 fold allows a behavior/status change without same-row thresholds, full-table maintain gates, REDRESS routing, and artifact consistency.

## Residual Non-Blocking Risks

- Some P3-A through P3-E documents retain earlier cycle labels in headings or prose. V2 consolidated hardening already classified this as non-blocking for CH3 because the substantive gates and route ledger are preserved.
- `HANDOFF.md` compresses part of the REDRESS 50-55 family into shorthand wording, but `SPEC.md`, `DISPATCH-PROMPT.md`, `p3e-preblocked-ledger.md`, and `REDRESS.md` carry the precise route ledger. This is not a V3 regression.
- V3 is still a plan-hardening acceptance only. It does not close G-Alpha, authorize W1-W6, or dispatch implementation beyond the eventual W0-only authorization if G-Alpha closes.

## Required Fold If REVISE

None. No CH3 regression/pre-block blocker found.

## Self-Verdict

ACCEPT, confidence 96%.
