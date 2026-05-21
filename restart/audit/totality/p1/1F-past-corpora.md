---
agent: 1F
pass: T-P1-excavation
cycle: V2
generated_at: 2026-05-21T00:00:00-04:00
spec_surfaces_audited: [skinny/REDRESS.md, skinny/RESULTS.md, restart/HANDOFF.md, restart/skinny/INDEX.md, restart/skinny/tranches/sk-v5/research, restart/skinny/tranches/sk-v6/research, restart/skinny/tranches/sk-v7/research, restart/skinny/tranches/sk-v8/research, restart/skinny/tranches/sk-v9/research, restart/skinny/tranches/sk-v10/research, restart/skinny/tranches/sk-v11/research, restart/skinny/tranches/sk-v12/research, restart/skinny/tranches/sk-v13/research, audit, restart/audit]
files_audited_count: 1664
live_truth_method: "nl -ba cited REDRESS/RESULTS/HANDOFF/INDEX/SK-V13 rows; prior find/rg corpus counts are scan summaries and must be recaptured before exact-count closure"
prior_cycle_dispositions_folded:
  accepted: [CH1-1D-results-redress-mapping, CH2-bbnf-simd-current-genericity, CH3-rejected-route-ledger, CH3-direct-codegen-honesty, CH4-zero-loc-preblock-treatment]
  rejected: []
  revised: [CH3-current-sk-v13-preblocks, CH3-unblocked-vs-accepted-route, CH4-hard-cap-metadata, CH5-track2-shared-substrate-caveat, CH6-accepted-preblock-wording, CH6-command-output-hygiene]
  first_cycle_additions: [PC-001, PC-002, PC-003, PC-004, PC-005, PC-006]
divergence_count:
  spec_claims_implemented: 6
  spec_claims_unimplemented: 2
  impl_exceeds_spec: 1
  unknown: 2
locks_amendment_candidates: 0
---

## Executive Summary

The prior corpus is large enough to be a first-class input, but V2 treats exact corpus counts as scan-derived until command output is captured. The totality cycle must not re-derive at least six settled findings: dispatch-table alternates are rejected; 12-byte token churn is rejected; EventCursor sidecar prepasses are rejected; Class A tiny-string NEON is not the parse-G fix; bench-private SinkOnly dishonesty is closed; and generated SinkOnly-from-BIR is landed but still has throughput residuals. V2 distinguishes accepted historical pre-blocks from live implementation closure: most REDRESS rejections below are ledger constraints, not claims that current no-match scans prove absence. Current docs also lag later results: `restart/HANDOFF.md` and `restart/skinny/INDEX.md` describe SK-V6 / N-direct NoGo, while `skinny/RESULTS.md` includes SK-V12 `A / Go` close evidence, and SK-V13 reopens rows only with fresh evidence.

## Spec-Claim <-> Implementation Table

| ID | Prior finding path:line | Current evidence path:line | Verdict | LOC / risk estimate | Note |
|---|---|---|---|---|---|
| PC-001 | Dispatch-table/function-pointer alternate rejected at `skinny/REDRESS.md:216-224`. | Current totality should preserve Rust `match` canonical dispatch unless a fresh row overturns it. | accepted historical pre-block | 0 LOC; medium if reopened | Do not re-run as a default optimization idea. |
| PC-002 | 12-byte skipless token shape rejected as canonical at `skinny/REDRESS.md:226-234`. | Lazy-offset tape migration supersedes old token churn at `skinny/REDRESS.md:246-256`. | accepted historical pre-block | 0 LOC; medium if reopened | Token-width churn is not the next substrate route. |
| PC-003 | EventCursor sidecar/prepass shape rejected in SK-V5 research at `restart/skinny/tranches/sk-v5/research/skv5-A4-tape-union-audit.md:57-80`. | ARCH requires cursor as lowering boundary, not retained sidecar, at `restart/ARCHITECTURE.md:1571-1580`; current no-match scan is not captured here. | accepted historical pre-block; current absence UNKNOWN | 0-160 LOC; high if reopened | Only inline generated consumption over the existing tape projection is admissible; capture exact `rg` output before claiming live absence. |
| PC-004 | Class A tiny-string NEON wiring invalidated as parse-G fix at `skinny/REDRESS.md:394-413`. | `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs` remains a primitive; no current result row claims it closes parse-G. | accepted historical pre-block | 0 LOC; medium | Primitive admission is not route admission. |
| PC-005 | Bench-private SinkParser dishonesty identified and closed at `skinny/REDRESS.md:420-438`. | Generated `parse_direct` now exists at `skinny/crates/runtime/src/grammars/json/generated.rs:393-407`; REDRESS says Track 1 calls generated runtime at `skinny/REDRESS.md:535-557`. | implemented | 0 LOC; low if respected | Do not treat old sink-only throughput rows as generated-code evidence. |
| PC-006 | SinkOnly lowerer now consumes BIR but has no throughput claim at `skinny/REDRESS.md:662-683`. | `skinny/crates/codegen/src/lib.rs:145-150` requires lowered `SinkOnlyProgram`; direct source test checks BIR marker at `skinny/crates/codegen/src/lib.rs:413-416`. | implemented with residual | 0 LOC for authority; throughput work separate | Codegen honesty closed; performance remains separate. |
| PC-007 | Current SK-V6 authority says current full gate is `N-direct / NoGo` at `restart/skinny/INDEX.md:42-56`. | Later `skinny/RESULTS.md:145-148` reports `A / Go` and SK-V12 campaign close. | corpus drift | 80-160 LOC; high | Totality must fold latest results before reusing SK-V6 conclusions. |
| PC-008 | Lock 14 `bbnf-simd` JSON hardcoded scalar references were pending in REDRESS at `skinny/REDRESS.md:460-478`. | Current `bbnf-simd` exposes generic alphabet/table APIs at `skinny/crates/bbnf-simd/src/lib.rs:20-49` and aarch64 table construction at `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:7-14`. | revised/partially closed | 80-200 LOC verify; medium | Need 1E/CH2 to verify all old JSON alphabet sites are gone. |

## Current SK-V13 Pre-Block Table

These are current hard pre-blocks from `restart/skinny/tranches/sk-v13/SYNTHESIS.md:237-253`. The adjacent unblocked set at `restart/skinny/tranches/sk-v13/SYNTHESIS.md:255-262` means "fresh evidence may reopen," not "route accepted."

| ID | Current SK-V13 disposition | Evidence | V2 implication |
|---|---|---|---|
| SKV13-PB-001 | Do not claim SK-V13 close from the single SK-V12 CSS declaration-values row. | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:241` | CSS admission remains evidence, not totality closure. |
| SKV13-PB-002 | Do not use lossy/permissive/different-plane comparators as SOTA anchors. | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:242-243` | Comparator plane must be explicit for every row. |
| SKV13-PB-003 | Do not treat `parse_only` as diagnostic-only. | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:244`; reopened JSON rows at `restart/skinny/tranches/sk-v13/SYNTHESIS.md:95-106` | Parse-only rows need fresh strict evidence. |
| SKV13-PB-004 | Do not close JSON through REDRESS-119 history. | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:245`; history-only note at `restart/skinny/tranches/sk-v13/SYNTHESIS.md:105-106` | Prior direct fixpoint is not current closure authority. |
| SKV13-PB-005 | Do not accept producer-only SIMD, union, resolver, or codegen artifacts without same-wave consumer measurement. | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:246-247` | Every primitive-producing row needs same-wave consumer metadata. |
| SKV13-PB-006 | Do not reuse non-JSON/shared `bbnf-simd` alphabet-only dispatch without `G-SIMD-GRAMMAR-POLICY`. | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:248-250` | Old `bbnf-simd` partial closure remains verify-before-rederive. |
| SKV13-PB-007 | Do not admit grammar-name branches, parser-owned sidecars, hidden Track 1/Track 2 coupling, or stale comparator sidecars. | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:251-252` | Folded into 1F anti-pattern rows for runtime root witnesses, structural scanner, and CSS source-sidecar comparator. |
| SKV13-PB-008 | Do not dispatch Wave 0 before G-Omega closes. | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:253` | Work-order rows need wave metadata and a hard cap. |

## Divergences Catalogued

| ID | Divergence / pre-block | Evidence | LOC / risk |
|---|---|---|---|
| PC-001 | Rejected alternates must remain blocked absent new measurements. | `skinny/REDRESS.md:216-224`; `skinny/REDRESS.md:291-297` | 0 LOC; medium |
| PC-002 | EventCursor can be a lowering boundary only, not a parallel prepass. | `restart/ARCHITECTURE.md:1571-1580`; `restart/skinny/tranches/sk-v5/research/skv5-A4-tape-union-audit.md:191-209`; current no-match status UNKNOWN without captured scan | 0-160 LOC; high |
| PC-003 | Direct-to-struct codegen honesty is closed; do not re-diagnose as symbol absence. | `skinny/REDRESS.md:440-458`; `skinny/REDRESS.md:662-683` | 0 LOC; low |
| PC-004 | Direct throughput is a residual, not proof that SinkOnly is absent. | `skinny/REDRESS.md:535-557`; `skinny/RESULTS.md:98-144` | 0 LOC for diagnosis; high if conflated |
| PC-005 | Current authority drift after SK-V12 means older SK-V6 summaries are unsafe as final state. | `restart/HANDOFF.md:3-9`; `skinny/RESULTS.md:145-148` | 80-160 LOC; high |
| PC-006 | Lock 14 pending findings require verify-before-rederive. | `skinny/REDRESS.md:460-498`; current generic API evidence `skinny/crates/bbnf-simd/src/lib.rs:20-49` | 80-200 LOC verify; medium |

## V2 Planning Metadata

| ID | loc_budget | risk | wave | hard_cap | same_wave_consumer | evidence_basis |
|---|---:|---|---|---:|---|---|
| PC-001 | 0 LOC | medium if reopened | all waves | 0 LOC unless fresh row accepted | fresh before/after measurement required | `skinny/REDRESS.md:216-224` |
| PC-002 | 0-160 LOC only for audit/fencing | high if reopened | substrate-fencing wave | 220 LOC | retained-substrate audit consumer | `restart/ARCHITECTURE.md:1571-1580`; SK-V5 research citation |
| PC-003 | 0 LOC for diagnosis | low | closed authority | 0 LOC | none | `skinny/REDRESS.md:440-458`; `skinny/REDRESS.md:662-683` |
| PC-004 | 0 LOC diagnosis; benchmark work separate | high if conflated | SK-V13 row waves | row-specific cap required | strict row benchmark consumer | `skinny/REDRESS.md:535-557`; `skinny/RESULTS.md:98-144` |
| PC-005 | 80-160 LOC docs/results | high | T-P3 governance | 220 LOC | current-state authority page | `restart/HANDOFF.md:3-9`; `skinny/RESULTS.md:145-148` |
| PC-006 | 80-200 LOC verify | medium | SIMD policy wave | 260 LOC | same-wave SIMD production consumer if reopened | `skinny/REDRESS.md:460-498`; `skinny/crates/bbnf-simd/src/lib.rs:20-49` |
| SKV13-PB-005 | producer LOC varies; no producer-only close | high | same wave as primitive | hard cap must be row-specific | required for SIMD/union/resolver/codegen artifacts | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:246-247` |
| SKV13-PB-008 | 0 LOC until G-Omega | high | G-Omega first | 0 LOC before unblock | G-Omega closure | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:253` |

## Gaps / Missing Primitives

| Gap | Evidence | LOC / risk |
|---|---|---|
| No compact "do not rederive" ledger exists for SK-V5-SK-V13. | Prior scan found a large research corpus under `restart/skinny/tranches/sk-v*/research`; exact file count must be recaptured before use as closure evidence. | 200-400 LOC ledger; high process risk |
| No single row maps REDRESS accepted/rejected items to current code symbols and current result rows. | REDRESS direct close rows at `skinny/REDRESS.md:535-557`; RESULTS rows at `skinny/RESULTS.md:3-30` and `skinny/RESULTS.md:94-148`. | 300-600 LOC tooling/report; medium-high |
| Lock 14 pending findings from SK-V5 need explicit current disposition. | Pending rows at `skinny/REDRESS.md:460-498`; current code evidence suggests partial closure. | 80-200 LOC verify; medium |

## Open Questions

| UNKNOWN | Blocking question | verify_action |
|---|---|---|
| U-PC-001 | Which SK-V13 research findings supersede SK-V12 close rows for the current totality cycle? | Read `restart/skinny/tranches/sk-v13/HANDOFF.md`, `SPEC.md`, and research close files; append only superseding findings to the pre-block ledger. |
| U-PC-002 | Are the SK-V5 pending Lock 14 `bbnf-simd` findings fully closed, partially closed, or still open after table-driven classifier work? | Run targeted `rg -n 'JSON_STRUCTURAL|scan_json|JsonParseIndex|classify_block_scalar\\(.*json|b\"{}\\[\\],:\\\\\"\"' skinny/crates/bbnf-simd skinny/crates/runtime` and cite every remaining hit. |
| U-PC-003 | Which historical no-match claims are still true on the live tree? | Capture exact `rg` output for EventCursor, alternate dispatch, and stale comparator sidecar terms before promoting accepted historical pre-blocks to live absence claims. |
