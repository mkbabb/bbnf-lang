---
agent: 1F
pass: T-P1-excavation
cycle: V1
generated_at: 2026-05-21T00:00:00-04:00
spec_surfaces_audited: [restart/ARCHITECTURE.md, restart/MASTER-PLAN.md, restart/locks/LOCKS.md, restart/skinny/INDEX.md, skinny/REDRESS.md, skinny/RESULTS.md]
files_audited_count: 633
live_truth_method: "find skinny/crates source files; wc -l for Rust LOC; rg grammar-name and substrate terms; source directory child-count scan excluding target"
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: []
  first_cycle_additions: [AP-001, AP-002, AP-003, AP-004, AP-005]
divergence_count:
  spec_claims_implemented: 3
  spec_claims_unimplemented: 5
  impl_exceeds_spec: 1
  unknown: 1
locks_amendment_candidates: 0
---

## Executive Summary

The live skinny code still contains Lock 13 and Lock 14 anti-patterns. The worst Lock 13 offenders are non-generated source files over 500 LOC: `bbnf-bench/src/report.rs` at 3732 LOC, `bbnf-bench/src/bin/gate.rs` at 2842 LOC, `passes/src/lib.rs` at 1748 LOC, `parse-that-regex/src/lib.rs` at 1214 LOC, and `ir/src/lib.rs` at 713 LOC. The worst Lock 14 offenders are generic-crate grammar profiles and generated-module exports hardcoding JSON/CSS/SHEETS names. The old SK-V5 eventcursor sidecar appears purged from the current tree, which is good; the remaining hidden-coupling risk is proof/runtime witness code that still hardcodes grammar event types.

## Spec-Claim <-> Implementation Table

| ID | Claim path:line | Impl path:line | Verdict | LOC / risk estimate | Note |
|---|---|---|---|---|---|
| AP-001 | Lock 13 forbids files >500 LOC outside generated at `restart/locks/LOCKS.md:76`; MASTER repeats no handwritten file over 500 LOC at `restart/MASTER-PLAN.md:127`. | `wc -l` found `skinny/crates/passes/src/lib.rs` 1748 LOC, `skinny/crates/parse-that-regex/src/lib.rs` 1214 LOC, `skinny/crates/ir/src/lib.rs` 713 LOC, and several bench files above 500. | unimplemented | 400-900 LOC movement; medium-high | Bench may warrant separate allowance, but `passes`, `parse-that-regex`, and `ir` are generic source. |
| AP-002 | Lock 13 says directories with >10 immediate children mixing concerns are forbidden at `restart/locks/LOCKS.md:76`. | Source child-count scan found `skinny/crates/bbnf-simd/src/aarch64` has 17 children, `skinny/crates/bbnf-simd/tests` 15, `skinny/crates/bbnf-bench/src` 15, `skinny/crates/runtime/src/grammars/json` 11, and `skinny/crates/bbnf-simd/src/x86_64` 11. | unimplemented | 200-500 LOC movement; medium | Some are cohesive ISA partitions; still above the stated hard ceiling. |
| AP-003 | Lock 14 bars grammar switches/modules/types in generic crates at `restart/locks/LOCKS.md:78`. | `skinny/crates/codegen/src/grammar_profile.rs:11-15` defines `RuntimeProvider::{Json, CssL4DeclarationValues}` and `runtime_profiles()` returns those two providers at `skinny/crates/codegen/src/grammar_profile.rs:89-93`. | unimplemented | 300-700 LOC; high | This is an explicit generic-code profile registry. |
| AP-004 | ARCH says generated grammar-specific surface belongs under runtime grammars and metadata, not generic siblings at `restart/ARCHITECTURE.md:417-419`. | `skinny/crates/runtime/src/lib.rs:3-19` exports `generated_json`, `generated_css_l4_declaration_values`, and `grammars::{json, css_l4_declaration_values}` from the runtime crate root. | unimplemented | 150-350 LOC; medium-high | Generated files are okay; root-level handwritten grammar aliases are the leak. |
| AP-005 | ARCH says EventCursor sidecar prepasses are rejected; typed event cursor must be the lowering boundary at `restart/ARCHITECTURE.md:1571-1580`. | Current tree has no `generated_eventcursor.rs` match in targeted scan; runtime proof code still imports `JsonEventGrammar` and `SheetsEventGrammar` under feature proof at `skinny/crates/runtime/src/tape/event_grammar_tests.rs:12-20`. | mostly implemented with residue | 80-160 LOC; medium | The refuted sidecar is gone; proof fixtures should be generated or moved out of generic runtime. |
| AP-006 | ARCH states lowerers never inspect Grammar IR at `restart/ARCHITECTURE.md:1017-1020`. | `skinny/crates/codegen/src/lib.rs:92-100` parses grammar source, runs passes, then emits from `BackendIr`; direct renderer requires a lowered `SinkOnlyProgram` at `skinny/crates/codegen/src/lib.rs:145-150`. | implemented | 0 LOC | Codegen authority is no longer purely decorative for SinkOnly. |
| AP-007 | REDRESS records Track 1 generated direct now calls generated runtime, not bench-private parser at `skinny/REDRESS.md:535-557`. | `skinny/crates/runtime/src/grammars/json/generated.rs:393-407` contains generated `parse_direct` and `skinny/crates/codegen/src/lib.rs:413-416` tests emitted direct source contains the BackendIr marker and `parse_direct`. | implemented | 0 LOC | Prior bench-private dishonesty should stay closed. |

## Divergences Catalogued

| ID | Divergence | Evidence | LOC / risk |
|---|---|---|---|
| AP-001 | God files above 500 LOC in generic source. | `restart/locks/LOCKS.md:76`; `skinny/crates/passes/src/lib.rs` 1748 LOC; `skinny/crates/parse-that-regex/src/lib.rs` 1214 LOC; `skinny/crates/ir/src/lib.rs` 713 LOC | 400-900 LOC; medium-high |
| AP-002 | Over-wide source directories. | `restart/locks/LOCKS.md:76`; child-count scan results listed above | 200-500 LOC; medium |
| AP-003 | Generic codegen profile registry hardcodes grammar providers. | `skinny/crates/codegen/src/grammar_profile.rs:11-15`; `skinny/crates/codegen/src/grammar_profile.rs:89-93` | 300-700 LOC; high |
| AP-004 | Runtime root exports grammar-named generated modules by hand. | `skinny/crates/runtime/src/lib.rs:3-19` | 150-350 LOC; medium-high |
| AP-005 | Generic grammar parser still exposes JSON helpers. | `skinny/crates/grammar/src/lib.rs:16-27`; JSON tests at `skinny/crates/grammar/src/lib.rs:386-394` | 80-180 LOC; medium |
| AP-006 | Pass recognizer/materialization remains JSON-shape-biased. | recognizer punctuation at `skinny/crates/passes/src/lib.rs:324-349`; object/array/pair/string/number labels at `skinny/crates/passes/src/lib.rs:978-1119` | 300-600 LOC; high |

## Gaps / Missing Primitives

| Gap | Evidence | LOC / risk |
|---|---|---|
| No generated registry abstraction for runtime profiles. | `skinny/crates/codegen/src/grammar_profile.rs:89-93` is a static hand-coded two-profile array. | 200-400 LOC; high |
| No generic root module export strategy for runtime grammars. | `skinny/crates/runtime/src/lib.rs:17-19` aliases grammar names by hand. | 150-350 LOC; medium-high |
| LOC lint budget is not aligned with current code size. | REDRESS already notes budget cliffs around bench at `skinny/REDRESS.md:384-392`; current source files still exceed Lock 13. | 150-300 LOC lint/policy; medium |

## Open Questions

| UNKNOWN | Blocking question | verify_action |
|---|---|---|
| U-AP-001 | Are bench/report files exempt from the 500 LOC Lock 13 ceiling, or must `bbnf-bench/src/report.rs` and `bin/gate.rs` be split before totality synthesis? | Ask 1E/T-P3 to decide whether Lock 13 needs a bench-harness exception or whether these files are normal violations. |
