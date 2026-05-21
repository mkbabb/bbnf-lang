---
agent: 1F
pass: T-P1-excavation
cycle: V4
generated_at: 2026-05-21T00:00:00-04:00
spec_surfaces_audited: [restart/ARCHITECTURE.md, restart/MASTER-PLAN.md, restart/locks/LOCKS.md, restart/skinny/INDEX.md, skinny/REDRESS.md, skinny/RESULTS.md]
files_audited_count: 633
live_truth_method: "line-cited source reads; uncaptured wc/rg/child-count scan summaries are V2 verify actions unless exact output is cited in-row; child-count rows must prove mixed concerns, not fanout alone"
v4_metadata_fold: "V4 is a metadata-only active-cycle fold after V3 CH1; no substantive 1F anti-pattern evidence claims changed."
prior_cycle_dispositions_folded:
  accepted: [CH2-runtime-generated-split, CH2-bbnf-simd-current-genericity, CH3-1F-preblock-history, CH4-1F-cost-shape, CH5-json-lock1-central-danger]
  rejected: []
  revised: [CH1-command-output-hygiene, CH1-lock13-child-count-narrowing, CH2-generic-crate-census, CH2-grammar-shape-leaks, CH4-hard-cap-metadata, CH5-structural-scanner-plane, CH5-css-source-sidecar-plane, CH5-proof-witness-root-coupling, CH5-track2-shared-substrate-helper, CH6-eventcursor-closure-wording]
  first_cycle_additions: [AP-001, AP-002, AP-003, AP-004, AP-005]
divergence_count:
  spec_claims_implemented: 3
  spec_claims_unimplemented: 6
  impl_exceeds_spec: 3
  unknown: 3
locks_amendment_candidates: 0
---

## Executive Summary

The live skinny code still contains Lock 13 and Lock 14 anti-patterns. The worst Lock 13 offenders are non-generated source files over 500 LOC: `bbnf-bench/src/report.rs`, `bbnf-bench/src/bin/gate.rs`, `passes/src/lib.rs`, `parse-that-regex/src/lib.rs`, and `ir/src/lib.rs`; exact LOC counts remain scan-derived and must be captured before becoming closure evidence. Lock 13 child-count claims are narrowed in V2: fanout alone is not enough because the lock also requires mixed concerns, so cohesive ISA/test partitions are follow-up inventory candidates rather than proven violations. The worst Lock 14 offenders are generic-crate grammar profiles, generated-module exports hardcoding JSON/CSS/SHEETS names, and JSON-shaped pass heuristics that leak grammar policy without `Json` symbols. The old SK-V5 EventCursor route remains a historical pre-block, but V2 weakens the current-tree closure wording and promotes proof/runtime witness exports to an explicit hidden-coupling row. V3 also classifies Track 2 JSON as independent parser authority with shared runtime substrate helpers, not parser-authority dishonesty and not retained parallel substrate.

## Spec-Claim <-> Implementation Table

| ID | Claim path:line | Impl path:line | Verdict | LOC / risk estimate | Note |
|---|---|---|---|---|---|
| AP-001 | Lock 13 forbids files >500 LOC outside generated at `restart/locks/LOCKS.md:76`; MASTER repeats no handwritten file over 500 LOC at `restart/MASTER-PLAN.md:127`. | A prior `wc -l` scan found `skinny/crates/passes/src/lib.rs`, `skinny/crates/parse-that-regex/src/lib.rs`, `skinny/crates/ir/src/lib.rs`, and several bench files above 500; exact output is not captured in this artifact. | unimplemented / verify count | 400-900 LOC movement; medium-high | Bench may warrant separate allowance, but `passes`, `parse-that-regex`, and `ir` are generic source; capture LOC transcript before work ordering. |
| AP-002 | Lock 13 says directories with >10 immediate children mixing concerns are forbidden at `restart/locks/LOCKS.md:76`. | Prior source child-count notes summarized several >10-child directories, but V2 does not treat cohesive ISA/test partitions as proven violations without a child inventory showing mixed concerns. | UNKNOWN mixed-concern status | 80-220 LOC inventory first; medium | Candidate mixed-concern review: `skinny/crates/bbnf-bench/src`; candidate cohesive partitions needing proof: SIMD ISA dirs and tests. |
| AP-003 | Lock 14 bars grammar switches/modules/types in generic crates at `restart/locks/LOCKS.md:78`. | `skinny/crates/codegen/src/grammar_profile.rs:11-15` defines `RuntimeProvider::{Json, CssL4DeclarationValues}` and `runtime_profiles()` returns those two providers at `skinny/crates/codegen/src/grammar_profile.rs:89-93`. | unimplemented | 300-700 LOC; high | This is an explicit grammar-name leak in a generic-code profile registry. |
| AP-004 | ARCH says generated grammar-specific surface belongs under runtime grammars and metadata, not generic siblings at `restart/ARCHITECTURE.md:417-419`. | `skinny/crates/runtime/src/lib.rs:3-19` exports `generated_json`, `generated_css_l4_declaration_values`, and `grammars::{json, css_l4_declaration_values}` from the runtime crate root. | unimplemented | 150-350 LOC; medium-high | Generated files are okay; root-level handwritten grammar aliases are the leak. |
| AP-005 | ARCH says EventCursor sidecar prepasses are rejected; typed event cursor must be the lowering boundary at `restart/ARCHITECTURE.md:1571-1580`. | Current no-match claim for `generated_eventcursor.rs` is scan-derived and uncaptured; runtime proof code still imports `JsonEventGrammar` and `SheetsEventGrammar` under feature proof at `skinny/crates/runtime/src/tape/event_grammar_tests.rs:12-20`. | partial / residue | 80-160 LOC; medium | Treat old EventCursor sidecar as an accepted historical pre-block; proof fixtures remain a live grammar-coupling residue. |
| AP-006 | ARCH states lowerers never inspect Grammar IR at `restart/ARCHITECTURE.md:1017-1020`. | `skinny/crates/codegen/src/lib.rs:92-100` parses grammar source, runs passes, then emits from `BackendIr`; direct renderer requires a lowered `SinkOnlyProgram` at `skinny/crates/codegen/src/lib.rs:145-150`. | implemented | 0 LOC | Codegen authority is no longer purely decorative for SinkOnly. |
| AP-007 | REDRESS records Track 1 generated direct now calls generated runtime, not bench-private parser at `skinny/REDRESS.md:535-557`. | `skinny/crates/runtime/src/grammars/json/generated.rs:393-407` contains generated `parse_direct` and `skinny/crates/codegen/src/lib.rs:413-416` tests emitted direct source contains the BackendIr marker and `parse_direct`. | implemented | 0 LOC | Prior bench-private dishonesty should stay closed. |
| AP-008 | Lock 1/ARCH require transient scanner planes not to become retained sidecars. | JSON scan source is explicitly JSON-owned at `skinny/crates/runtime/src/grammars/json/scan.rs:1`, returns `StructuralIndex` at `skinny/crates/runtime/src/grammars/json/scan.rs:22`, and uses it for capacity at `skinny/crates/runtime/src/grammars/json/scan.rs:47-52`. | transient scanner plane; UNKNOWN retained identity | 40-120 LOC audit/fencing; medium | No retained document identity is proven here; classify and fence as capacity/proof input, not substrate authority. |
| AP-009 | CSS same-plane evidence must not hide comparator sidecars as runtime substrate. | `lightningcss_facts` routes through `fixture_sidecar_facts` at `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:222-234`, writes `same-plane-source-sidecar` at `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:298-303`, and validates fixture spans at `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:504-510`. | comparator-sidecar evidence plane | 60-160 LOC classification/reporting; medium-high | Preserve CSS admission evidence, but classify the lightningcss source sidecar as comparator-only and non-runtime-authoritative. |
| AP-010 | Runtime proof witnesses are generic-root coupling surfaces, not harmless residue. | Runtime root exposes `json_event_grammar_witness` and `sheets_witness` under proof/test gates at `skinny/crates/runtime/src/lib.rs:9-15`. | proof-witness root coupling | 80-180 LOC relocation/generation; medium | Proof witnesses need generated or proof-crate routing before Lock 14 can close for runtime root. |
| AP-011 | Track 2 benchmark parsers may be independent parser authority only if shared runtime substrate helpers are explicit and non-retained. | Track 2 JSON imports runtime JSON root/errors and tape builders at `skinny/crates/bbnf-bench/src/track2/json.rs:5-8`, uses runtime structural capacity helpers and `TapeBuilder` construction at `skinny/crates/bbnf-bench/src/track2/json.rs:24-32`, and returns `JsonRoot::from_tape` at `skinny/crates/bbnf-bench/src/track2/json.rs:43`. | independent parser authority with shared runtime substrate helpers | 0 LOC classification; low | This is not parser-authority dishonesty and not retained parallel substrate; preserve the independent parser classification while keeping helper sharing visible. |

## Divergences Catalogued

The ID-keyed `V2 Planning Metadata` table is the authoritative CH4 carrier for LOC, risk, wave, hard-cap, same-wave-consumer, and evidence-basis fields; this divergences table is an index only.

| ID | Divergence | Evidence | LOC / risk |
|---|---|---|---|
| AP-001 | God files above 500 LOC in generic source. | `restart/locks/LOCKS.md:76`; exact `wc -l` output must be captured before implementation planning | 400-900 LOC; medium-high |
| AP-002 | Potential over-wide mixed-concern source directories. | `restart/locks/LOCKS.md:76`; child-count scan summary listed above but not captured | 80-220 LOC inventory before split; medium |
| AP-003 | Generic codegen profile registry hardcodes grammar providers. | `skinny/crates/codegen/src/grammar_profile.rs:11-15`; `skinny/crates/codegen/src/grammar_profile.rs:89-93` | 300-700 LOC; high |
| AP-004 | Runtime root exports grammar-named generated modules by hand. | `skinny/crates/runtime/src/lib.rs:3-19` | 150-350 LOC; medium-high |
| AP-005 | Generic grammar parser still exposes JSON helpers. | `skinny/crates/grammar/src/lib.rs:16-27`; JSON tests at `skinny/crates/grammar/src/lib.rs:386-394` | 80-180 LOC; medium |
| AP-006 | Pass recognizer/materialization remains JSON-shape-biased. | recognizer punctuation at `skinny/crates/passes/src/lib.rs:324-349`; object/array/pair/string/number labels at `skinny/crates/passes/src/lib.rs:978-1119` | 300-600 LOC; high |
| AP-008 | Structural scanner plane is live and must stay transient/non-authoritative. | `skinny/crates/runtime/src/grammars/json/scan.rs:1-52` | 40-120 LOC; medium |
| AP-009 | CSS lightningcss source sidecar is comparator evidence, not runtime substrate. | `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:222-234`; `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:298-303`; `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:504-510` | 60-160 LOC; medium-high |
| AP-010 | Runtime root proof witnesses couple generic root to grammar names. | `skinny/crates/runtime/src/lib.rs:9-15` | 80-180 LOC; medium |
| AP-011 | Track 2 JSON is an independent parser authority sharing runtime substrate helpers, not parser-authority dishonesty and not retained parallel substrate. | `skinny/crates/bbnf-bench/src/track2/json.rs:5-8`; `skinny/crates/bbnf-bench/src/track2/json.rs:24-32`; `skinny/crates/bbnf-bench/src/track2/json.rs:43` | 0 LOC classification; low |

## V2 Planning Metadata

| ID | loc_budget | risk | wave | hard_cap | same_wave_consumer | evidence_basis |
|---|---:|---|---|---:|---|---|
| AP-001 | 400-900 LOC movement | medium-high | Lock 13 source-split wave | 1200 LOC | Lock 13 lint/report consumer | `restart/locks/LOCKS.md:76`; capture exact `wc -l` output first |
| AP-002 | 80-220 LOC inventory | medium | Lock 13 source-split wave | 300 LOC before any split | mixed-concern child inventory | `restart/locks/LOCKS.md:76`; capture child list and concern labels first |
| AP-003 | 300-700 LOC | high | Lock 14 registry wave | 900 LOC | generated runtime-profile registry | `skinny/crates/codegen/src/grammar_profile.rs:11-15`; `skinny/crates/codegen/src/grammar_profile.rs:89-93` |
| AP-004 | 150-350 LOC | medium-high | Lock 14 runtime-root wave | 500 LOC | generated root export strategy | `skinny/crates/runtime/src/lib.rs:3-19` |
| AP-005 | 80-160 LOC | medium | proof-surface wave | 220 LOC | proof-only generated witness routing | `restart/ARCHITECTURE.md:1571-1580`; `skinny/crates/runtime/src/tape/event_grammar_tests.rs:12-20` |
| AP-006 | 0 LOC | low | closed authority | 0 LOC | none | `skinny/crates/codegen/src/lib.rs:92-100`; `skinny/crates/codegen/src/lib.rs:145-150` |
| AP-007 | 0 LOC | low | closed authority | 0 LOC | none | `skinny/REDRESS.md:535-557`; `skinny/crates/runtime/src/grammars/json/generated.rs:393-407` |
| AP-008 | 40-120 LOC audit/fence | medium | substrate-fencing wave | 180 LOC | retained-substrate audit consumer | `skinny/crates/runtime/src/grammars/json/scan.rs:1-52` |
| AP-009 | 60-160 LOC classification | medium-high | CSS evidence-accounting wave | 220 LOC | CSS row-plane report consumer | `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:222-234`; `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:298-303` |
| AP-010 | 80-180 LOC relocation/generation | medium | proof-surface wave | 240 LOC | proof crate or generated witness consumer | `skinny/crates/runtime/src/lib.rs:9-15` |
| AP-011 | 0 LOC classification | low | Track 2 evidence-accounting wave | 0 LOC | benchmark report classification | `skinny/crates/bbnf-bench/src/track2/json.rs:5-8`; `skinny/crates/bbnf-bench/src/track2/json.rs:24-32`; `skinny/crates/bbnf-bench/src/track2/json.rs:43` |

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
| U-AP-002 | Which >10-child directories both exceed the fanout threshold and mix concerns, as Lock 13 requires? | Capture `find` child lists for candidate directories, classify each child by concern, and only then mark AP-002 as implemented/unimplemented. |
| U-AP-003 | Are negative/no-match searches for EventCursor and old sidecar names still true on the current tree? | Capture the exact `rg` command and output for `generated_eventcursor`, `EventCursor`, and old sidecar aliases before using absence as closure evidence. |
