---
agent: 1F
pass: T-P1-excavation
cycle: V4
generated_at: 2026-05-21T00:00:00-04:00
spec_surfaces_audited: [restart/ARCHITECTURE.md, restart/MASTER-PLAN.md, restart/locks/LOCKS.md, restart/HANDOFF.md, restart/skinny/INDEX.md, restart/skinny/SUBSTRATE.md, restart/skinny/COMPILER.md, restart/skinny/BENCH.md, restart/skinny/WORKSPACE.md, restart/skinny/HARDENING.md, skinny/REDRESS.md, skinny/RESULTS.md]
files_audited_count: 2211
live_truth_method: "nl -ba line-citation reads; uncaptured rg/find/wc/child-count scans are treated as V2 verify actions unless exact output is cited in-row; source tree child-count scan excluded target"
v4_metadata_fold: "V4 is a metadata-only active-cycle fold after V3 CH1; no substantive 1F coherence evidence claims changed."
prior_cycle_dispositions_folded:
  accepted: [CH1-1D-row-mapping, CH1-1F-coherence-drift, CH2-1C-runtime-audit, CH2-bbnf-simd-genericity-disposition, CH3-1F-rejected-route-history, CH4-1F-cost-shape]
  rejected: []
  revised: [CH1-command-output-hygiene, CH1-lock13-child-count-narrowing, CH2-grammar-name-vs-shape-leaks, CH3-sk-v13-preblock-split, CH4-hard-cap-metadata, CH5-css-sidecar-plane, CH5-proof-witness-root-coupling, CH6-closure-wording]
  first_cycle_additions: [COH-001, COH-002, COH-003, COH-004, COH-005, COH-006, AP-001, AP-002, AP-003, AP-004, AP-005, PC-001, PC-002, PC-003, PC-004, PC-005, PC-006]
divergence_count:
  spec_claims_implemented: 8
  spec_claims_unimplemented: 9
  impl_exceeds_spec: 2
  unknown: 3
locks_amendment_candidates: 0
---

## Executive Summary

1F found six cross-surface coherence divergences, five live anti-pattern clusters, and six prior-corpus findings that must be treated as pre-blocks rather than re-researched. The highest-risk drift is temporal: `restart/HANDOFF.md` still declares SK-V6 current state while live results and tranche directories carry SK-V9 through SK-V13 evidence, including an SK-V12 campaign close row. The second major drift is Lock 14: the spec bars grammar names and grammar-shaped policy in generic crates, but live `skinny/crates/codegen`, `runtime`, `grammar`, and `passes` still route through explicit JSON/CSS/SHEETS names or JSON-shaped structural roles. The third is Lock 13: several non-generated source files exceed 500 LOC, while directory child-count claims remain V2 verify actions unless the row also proves mixed concerns rather than merely cohesive fanout.

Companion outputs are warranted: `1F-anti-pattern.md` carries the live code evidence; `1F-past-corpora.md` carries the prior-corpora pre-block ledger.

## Spec-Claim <-> Implementation Table

| ID | Claim path:line | Impl / counter-surface path:line | Verdict | LOC / risk estimate | Note |
|---|---|---|---|---|---|
| COH-001 | `restart/HANDOFF.md:3-4` says SK-V6 SOTA recovery is current and `N-direct / NoGo` is current. | `skinny/RESULTS.md:145-148` reports overall `A / Go`, SK-V12 campaign close, and a CSS L4 admission row. | spec-surface drift | 60-120 LOC doc reconciliation; high orchestration risk | Current-cycle authority is stale across HANDOFF vs live results. |
| COH-002 | `restart/HANDOFF.md:17` says adding a grammar requires two declarative surfaces. | `restart/locks/LOCKS.md:78` says three declarative surfaces, including optional per-grammar declaration crate; `restart/ARCHITECTURE.md:37` calls the declaration crate a rare escape valve. | spec-surface drift | 20-40 LOC doc edit; medium Lock 14 onboarding risk | Two-surface and three-surface wording must be normalized. |
| COH-003 | `restart/MASTER-PLAN.md:151-154` says Rust-line SOTA close gates measure H.W1/H.W2/H.W4/H.W5/H.W6 and WASM defers. | `restart/locks/LOCKS.md:66` says V1 SOTA close gates measure Rust line only at H.W3 and H.W4. | spec-surface drift | 20-50 LOC doc edit; medium gate-routing risk | H-wave numbering diverges across governing surfaces. |
| COH-004 | `restart/ARCHITECTURE.md:1090-1098` defines the 8-step `BackendShape` derivation. | `skinny/crates/passes/src/lib.rs:44-45` calls shape derivation, but tests assert every JSON rule is `OffsetTape` at `skinny/crates/passes/src/lib.rs:1497-1503`. | partially implemented | 150-300 LOC plus tests; high cost-model risk | The field exists, but measured per-rule selection is not proven by the current test posture. |
| COH-005 | `restart/ARCHITECTURE.md:1129-1131` itself states remaining generic-pass grammar-specific mining must be removed. | `skinny/crates/passes/src/lib.rs:324-349` still derives recognizers from JSON punctuation bytes; materialization roles infer JSON-like object/array/pair/string/number/bool/null at `skinny/crates/passes/src/lib.rs:978-1119`. | unimplemented cleanup | 300-600 LOC; high Lock 14 risk | This is acknowledged spec debt and live code debt. |
| COH-006 | `restart/skinny/INDEX.md:42-56` says current measured split is 13 retained G rows plus `N-direct / NoGo`. | `skinny/RESULTS.md:94-148` includes a CSS L4 SK-V12 admission row and overall `A / Go`. | spec-surface drift | 80-160 LOC doc/result authority update; high gate risk | Skinny surfaces lag after later tranche results. |
| AP-001 | `restart/locks/LOCKS.md:76` forbids files >500 LOC outside generated. | Prior `wc -l` scan reports multiple non-generated source files above 500 LOC; exact output is not captured in this artifact, so `1F-anti-pattern.md` carries the verify-count downgrade. | unimplemented / verify count | 400-900 LOC split; medium-high maintenance risk | Lock 13 appears unenforced in live skinny source, but implementation ordering needs a captured LOC transcript. |
| AP-002 | `restart/locks/LOCKS.md:78` bars grammar-named modules in generic crates. | `skinny/crates/codegen/src/grammar_profile.rs:11-15` hardcodes `Json` and `CssL4DeclarationValues`; runtime exposes generated grammar modules at `skinny/crates/runtime/src/lib.rs:3-19`. | unimplemented | 300-700 LOC; high Lock 14 risk | More evidence in `1F-anti-pattern.md`; this is a grammar-name leak, distinct from grammar-shape leaks in passes. |
| AP-003 | `restart/ARCHITECTURE.md:1571-1580` says mask streams are transient and EventCursor sidecars must not become retained prepasses. | Current tree has no cited retained EventCursor implementation in this artifact; runtime proof witnesses still use grammar-specific event witness modules at `skinny/crates/runtime/src/lib.rs:9-15`. | partial / residue | 80-160 LOC cleanup; medium hidden-coupling risk | Refuted prepass is a historical pre-block unless a current targeted-scan transcript is added; proof fixtures still carry grammar names. |
| PC-001 | `skinny/REDRESS.md:216-224` rejects dispatch-table/function-pointer alternates. | Historical REDRESS rejection remains binding absent fresh row evidence; this V2 row does not claim a live no-match scan. | accepted historical pre-block | 0 LOC; low if ledger respected | Do not re-open without a fresh before/after row. |
| PC-002 | `skinny/REDRESS.md:226-234` rejects 12-byte token shape as canonical. | Lazy-offset tape migration supersedes old token churn at `skinny/REDRESS.md:246-256`. | accepted historical pre-block | 0 LOC; medium if relitigated | Treat token-width churn as blocked. |
| PC-003 | `skinny/REDRESS.md:394-413` invalidates Class A tiny-string NEON as parse-G fix. | `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs` remains as a primitive, not canonical parser wiring. | accepted historical pre-block | 0 LOC; medium if relitigated | Primitive may be reused only with new row-local evidence. |

## Divergences Catalogued

The ID-keyed `V2 Planning Metadata` table is the authoritative CH4 carrier for LOC, risk, wave, hard-cap, same-wave-consumer, and evidence-basis fields; this divergences table is an index only.

| ID | Divergence | Evidence | LOC / risk |
|---|---|---|---|
| COH-001 | HANDOFF stale against live results. | `restart/HANDOFF.md:3-4`; `skinny/RESULTS.md:145-148` | 60-120 LOC; high |
| COH-002 | Grammar onboarding is two-surface in HANDOFF and three-surface/escape-valve in LOCKS/ARCH. | `restart/HANDOFF.md:17`; `restart/locks/LOCKS.md:78`; `restart/ARCHITECTURE.md:37` | 20-40 LOC; medium |
| COH-003 | H-wave SOTA gate numbering differs between MASTER and LOCKS. | `restart/MASTER-PLAN.md:151-154`; `restart/locks/LOCKS.md:66` | 20-50 LOC; medium |
| COH-004 | BackendShape derivation surface exists, but tests pin all JSON rules to OffsetTape rather than proving 8-step selection. | `restart/ARCHITECTURE.md:1090-1098`; `skinny/crates/passes/src/lib.rs:1497-1503` | 150-300 LOC; high |
| COH-005 | Spec acknowledges generic-pass grammar leaks, and live passes still carry JSON-shaped inference. | `restart/ARCHITECTURE.md:1129-1131`; `skinny/crates/passes/src/lib.rs:324-349`; `skinny/crates/passes/src/lib.rs:978-1119` | 300-600 LOC; high |
| COH-006 | Skinny INDEX current-state prose lags live SK-V12 result authority. | `restart/skinny/INDEX.md:42-56`; `skinny/RESULTS.md:94-148` | 80-160 LOC; high |

## V2 Planning Metadata

Rows that imply work use the V2 hardening metadata below. Historical pre-block rows remain 0-LOC ledger constraints, not implementation closure claims.

| ID | loc_budget | risk | wave | hard_cap | same_wave_consumer | evidence_basis |
|---|---:|---|---|---:|---|---|
| COH-001 | 60-120 LOC docs | high | T-P3 governance | 160 LOC | totality handoff consumer | `restart/HANDOFF.md:3-4`; `skinny/RESULTS.md:145-148` |
| COH-002 | 20-40 LOC docs | medium | T-P3 governance | 60 LOC | grammar onboarding docs | `restart/HANDOFF.md:17`; `restart/locks/LOCKS.md:78`; `restart/ARCHITECTURE.md:37` |
| COH-003 | 20-50 LOC docs | medium | T-P3 governance | 80 LOC | H-wave gate plan | `restart/MASTER-PLAN.md:151-154`; `restart/locks/LOCKS.md:66` |
| COH-004 | 150-300 LOC code/tests | high | pass-hardening wave | 400 LOC | codegen backend-shape selection tests | `restart/ARCHITECTURE.md:1090-1098`; `skinny/crates/passes/src/lib.rs:1497-1503` |
| COH-005 | 300-600 LOC code/tests | high | Lock 14 wave | 800 LOC | CSS/Sheets/BBNF-self recognizer fixtures | `restart/ARCHITECTURE.md:1129-1131`; `skinny/crates/passes/src/lib.rs:324-349`; `skinny/crates/passes/src/lib.rs:978-1119` |
| COH-006 | 80-160 LOC docs/results | high | T-P3 governance | 220 LOC | current-state authority page | `restart/skinny/INDEX.md:42-56`; `skinny/RESULTS.md:94-148` |
| AP-001 | 400-900 LOC movement | medium-high | Lock 13 source-split wave | 1200 LOC | Lock 13 lint/report consumer | `restart/locks/LOCKS.md:76`; exact LOC scan must be captured before work order |
| AP-002 | 300-700 LOC codegen/runtime | high | Lock 14 registry wave | 900 LOC | generated registry plus runtime root consumer | `skinny/crates/codegen/src/grammar_profile.rs:11-15`; `skinny/crates/runtime/src/lib.rs:3-19` |
| AP-003 | 80-160 LOC cleanup/proof relocation | medium | proof-surface wave | 220 LOC | proof-gated witness consumer only | `restart/ARCHITECTURE.md:1571-1580`; `skinny/crates/runtime/src/lib.rs:9-15` |

## Gaps / Missing Primitives

| Gap | Evidence | LOC / risk |
|---|---|---|
| No single current-state authority surface spans SK-V6 through SK-V13. | HANDOFF points to SK-V6 at `restart/HANDOFF.md:3-9`; tranche research directories span SK-V3.5 and SK-V5-SK-V13 by `find restart/skinny/tranches -maxdepth 2`. | 100-200 LOC synthesis; high |
| Lock 14 lint named by ARCH is not visibly enforced in the live skinny code path. | Diagnostic exists at `restart/ARCHITECTURE.md:1182-1184`; grammar names remain in generic crates at `skinny/crates/codegen/src/grammar_profile.rs:11-15` and `skinny/crates/runtime/src/lib.rs:3-19`. | 200-500 LOC lint + remediations; high |
| Lock 13 LOC ceiling lacks a live source split plan for current skinny files. | Ceiling at `restart/locks/LOCKS.md:76`; largest live source files listed in `1F-anti-pattern.md`. | 400-900 LOC movement; medium-high |

## Open Questions

| UNKNOWN | Blocking question | verify_action |
|---|---|---|
| U-COH-001 | Which surface is authoritative for the current totality cycle after SK-V12/SK-V13 evidence: HANDOFF, RESULTS, or latest tranche HANDOFF? | Read latest `restart/skinny/tranches/sk-v13/HANDOFF.md`, SK-V12 close artifacts, then update `restart/HANDOFF.md` or explicitly mark it superseded. |
| U-COH-002 | Is `restart/locks/LOCKS.md:1-17` intended to be a permanent scoped allowance or a transient SK-V9 note that should move elsewhere? | Ask 1E to classify the leading SK-V9 allowance against the 16-lock baseline and decide whether T-P3 should relocate or preserve it. |
| U-COH-003 | Does the `files_audited_count` for future cycles include generated target Criterion artifacts under `skinny/crates/bbnf-bench/target/`? | T-P1 orchestrator should define whether generated bench output is evidence input or excluded from source-audit counts; this scan excluded target dirs from anti-pattern child-counts but counted research files for past-corpora scope. |
