---
agent: 1F
pass: T-P1-excavation
cycle: V1
generated_at: 2026-05-21T00:00:00-04:00
spec_surfaces_audited: [skinny/REDRESS.md, skinny/RESULTS.md, restart/HANDOFF.md, restart/skinny/INDEX.md, restart/skinny/tranches/sk-v5/research, restart/skinny/tranches/sk-v6/research, restart/skinny/tranches/sk-v7/research, restart/skinny/tranches/sk-v8/research, restart/skinny/tranches/sk-v9/research, restart/skinny/tranches/sk-v10/research, restart/skinny/tranches/sk-v11/research, restart/skinny/tranches/sk-v12/research, restart/skinny/tranches/sk-v13/research, audit, restart/audit]
files_audited_count: 1664
live_truth_method: "find counted 1555 skinny tranche research files plus 109 prior audit markdown files; rg searched rejected/invalidated/Lock14/sidecar/SinkOnly/EventCursor/NoGo terms; nl -ba cited REDRESS/RESULTS/HANDOFF/INDEX rows"
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: []
  first_cycle_additions: [PC-001, PC-002, PC-003, PC-004, PC-005, PC-006]
divergence_count:
  spec_claims_implemented: 6
  spec_claims_unimplemented: 2
  impl_exceeds_spec: 1
  unknown: 2
locks_amendment_candidates: 0
---

## Executive Summary

The prior corpus is large enough to be a first-class input: 1555 skinny tranche research files and 109 prior audit markdown files were present. The totality cycle must not re-derive at least six settled findings: dispatch-table alternates are rejected; 12-byte token churn is rejected; EventCursor sidecar prepasses are rejected; Class A tiny-string NEON is not the parse-G fix; bench-private SinkOnly dishonesty is closed; and generated SinkOnly-from-BIR is landed but still has throughput residuals. Current docs also lag later results: `restart/HANDOFF.md` and `restart/skinny/INDEX.md` describe SK-V6 / N-direct NoGo, while `skinny/RESULTS.md` includes SK-V12 `A / Go` close evidence.

## Spec-Claim <-> Implementation Table

| ID | Prior finding path:line | Current evidence path:line | Verdict | LOC / risk estimate | Note |
|---|---|---|---|---|---|
| PC-001 | Dispatch-table/function-pointer alternate rejected at `skinny/REDRESS.md:216-224`. | Current totality should preserve Rust `match` canonical dispatch unless a fresh row overturns it. | implemented pre-block | 0 LOC; medium if reopened | Do not re-run as a default optimization idea. |
| PC-002 | 12-byte skipless token shape rejected as canonical at `skinny/REDRESS.md:226-234`. | Lazy-offset tape migration supersedes old token churn at `skinny/REDRESS.md:246-256`. | implemented pre-block | 0 LOC; medium if reopened | Token-width churn is not the next substrate route. |
| PC-003 | EventCursor sidecar/prepass shape rejected in SK-V5 research at `restart/skinny/tranches/sk-v5/research/skv5-A4-tape-union-audit.md:57-80`. | ARCH requires cursor as lowering boundary, not retained sidecar, at `restart/ARCHITECTURE.md:1571-1580`; current targeted scan found no `generated_eventcursor.rs`. | implemented pre-block | 0-160 LOC; high if reopened | Only inline generated consumption over the existing tape projection is admissible. |
| PC-004 | Class A tiny-string NEON wiring invalidated as parse-G fix at `skinny/REDRESS.md:394-413`. | `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs` remains a primitive; no current result row claims it closes parse-G. | implemented pre-block | 0 LOC; medium | Primitive admission is not route admission. |
| PC-005 | Bench-private SinkParser dishonesty identified and closed at `skinny/REDRESS.md:420-438`. | Generated `parse_direct` now exists at `skinny/crates/runtime/src/grammars/json/generated.rs:393-407`; REDRESS says Track 1 calls generated runtime at `skinny/REDRESS.md:535-557`. | implemented | 0 LOC; low if respected | Do not treat old sink-only throughput rows as generated-code evidence. |
| PC-006 | SinkOnly lowerer now consumes BIR but has no throughput claim at `skinny/REDRESS.md:662-683`. | `skinny/crates/codegen/src/lib.rs:145-150` requires lowered `SinkOnlyProgram`; direct source test checks BIR marker at `skinny/crates/codegen/src/lib.rs:413-416`. | implemented with residual | 0 LOC for authority; throughput work separate | Codegen honesty closed; performance remains separate. |
| PC-007 | Current SK-V6 authority says current full gate is `N-direct / NoGo` at `restart/skinny/INDEX.md:42-56`. | Later `skinny/RESULTS.md:145-148` reports `A / Go` and SK-V12 campaign close. | corpus drift | 80-160 LOC; high | Totality must fold latest results before reusing SK-V6 conclusions. |
| PC-008 | Lock 14 `bbnf-simd` JSON hardcoded scalar references were pending in REDRESS at `skinny/REDRESS.md:460-478`. | Current `bbnf-simd` exposes generic alphabet/table APIs at `skinny/crates/bbnf-simd/src/lib.rs:20-49` and aarch64 table construction at `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:7-14`. | revised/partially closed | 80-200 LOC verify; medium | Need 1E/CH2 to verify all old JSON alphabet sites are gone. |

## Divergences Catalogued

| ID | Divergence / pre-block | Evidence | LOC / risk |
|---|---|---|---|
| PC-001 | Rejected alternates must remain blocked absent new measurements. | `skinny/REDRESS.md:216-224`; `skinny/REDRESS.md:291-297` | 0 LOC; medium |
| PC-002 | EventCursor can be a lowering boundary only, not a parallel prepass. | `restart/ARCHITECTURE.md:1571-1580`; `restart/skinny/tranches/sk-v5/research/skv5-A4-tape-union-audit.md:191-209` | 0-160 LOC; high |
| PC-003 | Direct-to-struct codegen honesty is closed; do not re-diagnose as symbol absence. | `skinny/REDRESS.md:440-458`; `skinny/REDRESS.md:662-683` | 0 LOC; low |
| PC-004 | Direct throughput is a residual, not proof that SinkOnly is absent. | `skinny/REDRESS.md:535-557`; `skinny/RESULTS.md:98-144` | 0 LOC for diagnosis; high if conflated |
| PC-005 | Current authority drift after SK-V12 means older SK-V6 summaries are unsafe as final state. | `restart/HANDOFF.md:3-9`; `skinny/RESULTS.md:145-148` | 80-160 LOC; high |
| PC-006 | Lock 14 pending findings require verify-before-rederive. | `skinny/REDRESS.md:460-498`; current generic API evidence `skinny/crates/bbnf-simd/src/lib.rs:20-49` | 80-200 LOC verify; medium |

## Gaps / Missing Primitives

| Gap | Evidence | LOC / risk |
|---|---|---|
| No compact "do not rederive" ledger exists for SK-V5-SK-V13. | Research corpus count: 1555 files under `restart/skinny/tranches/sk-v*/research`; current REDRESS is long and cumulative. | 200-400 LOC ledger; high process risk |
| No single row maps REDRESS accepted/rejected items to current code symbols and current result rows. | REDRESS direct close rows at `skinny/REDRESS.md:535-557`; RESULTS rows at `skinny/RESULTS.md:3-30` and `skinny/RESULTS.md:94-148`. | 300-600 LOC tooling/report; medium-high |
| Lock 14 pending findings from SK-V5 need explicit current disposition. | Pending rows at `skinny/REDRESS.md:460-498`; current code evidence suggests partial closure. | 80-200 LOC verify; medium |

## Open Questions

| UNKNOWN | Blocking question | verify_action |
|---|---|---|
| U-PC-001 | Which SK-V13 research findings supersede SK-V12 close rows for the current totality cycle? | Read `restart/skinny/tranches/sk-v13/HANDOFF.md`, `SPEC.md`, and research close files; append only superseding findings to the pre-block ledger. |
| U-PC-002 | Are the SK-V5 pending Lock 14 `bbnf-simd` findings fully closed, partially closed, or still open after table-driven classifier work? | Run targeted `rg -n 'JSON_STRUCTURAL|scan_json|JsonParseIndex|classify_block_scalar\\(.*json|b\"{}\\[\\],:\\\\\"\"' skinny/crates/bbnf-simd skinny/crates/runtime` and cite every remaining hit. |
