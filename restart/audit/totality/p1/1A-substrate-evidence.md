---
agent: 1A
pass: T-P1-excavation
cycle: V2
generated_at: 2026-05-28T04:18:00Z
spec_surfaces_audited: [ARCHITECTURE.md, LOCKS.md, PASS-1-EXCAVATION.md, ORCHESTRATOR.md]
files_audited_count: 59
live_truth_method: "Read PASS-1-EXCAVATION end-to-end; read ORCHESTRATOR §3W/§3Z; line-audited ARCHITECTURE §1/§7.1/§9 and LOCKS Lock 1 with nl -ba; enumerated live scope with rg --files/find; line-audited selected symbols in skinny/crates/runtime/src/tape, skinny/crates/ir/src, and skinny/crates/runtime/src/grammars; no source build or tests because this is docs-only evidence."
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised:
    - CH1-V1-F4 counted 1A-SUB-022 in frontmatter
    - CH3-V1-007 added EventTape / typed-event-cursor REDRESS fence
    - CH5-V1-02 added root OnceCell<StructuralIndex> coupling census
    - CH6-V1-1A-CLOSURE-WORDING downgraded substrate/scheduling/rollback closure wording
  first_cycle_additions: [1A-SUB-001, 1A-SUB-002, 1A-SUB-003, 1A-SUB-004, 1A-SUB-005, 1A-SUB-006, 1A-SUB-007, 1A-SUB-008, 1A-SUB-009, 1A-SUB-010, 1A-SUB-011, 1A-SUB-012, 1A-SUB-013, 1A-SUB-014, 1A-SUB-015, 1A-SUB-016, 1A-SUB-017, 1A-SUB-018, 1A-SUB-019, 1A-SUB-020, 1A-SUB-021, 1A-SUB-022]
divergence_count:
  spec_claims_implemented: 6
  spec_claims_unimplemented: 6
  impl_exceeds_spec: 4
  unknown: 5
locks_amendment_candidates: 0
---

## Executive Summary

The skinny substrate implements the center of Lock 1 for retained JSON:
`Tape<'input>` owns source, offsets, sparse flags, payload arena, and a tape id
at `skinny/crates/runtime/src/tape/mod.rs:94`; `ValueRef` is a tape-plus-cursor
borrow at `skinny/crates/runtime/src/tape/mod.rs:175`; and `JsonRoot` owns a
`JsonDocument` containing that tape at `skinny/crates/runtime/src/grammars/json/view.rs:12`.
The five `BackendShape` variants are live at `skinny/crates/ir/src/lib.rs:340`,
and Lock 1's substrate-target triad is live at `skinny/crates/ir/src/cost.rs:55`
and `skinny/crates/ir/src/cost.rs:111`.

The divergences are material. Grammar IR is much smaller than the §7.1 target:
live `ExprKind` starts at `skinny/crates/ir/src/lib.rs:210` and lacks
Predicate, Lookbehind, Call, LayoutDirective, and ErrorDirective variants
claimed at `restart/ARCHITECTURE.md:885`. BIR remains 13 enum variants plus a
separate `Recognizer::SimdScan` at `skinny/crates/ir/src/lib.rs:354` and
`skinny/crates/ir/src/lib.rs:392`, versus the 20-variant target at
`restart/ARCHITECTURE.md:932`. Direct `SinkOnly` JSON exists, but it walks raw
bytes and a local cursor at `skinny/crates/runtime/src/grammars/json/generated.rs:760`;
the shared typed event cursor described by §9 is not present. CSS fact streams
are generated output planes, but their config labels `admitted_fact_output` as
`W7_POLICY_BACKEND_SHAPE` at
`skinny/crates/runtime/src/grammars/css_l4_declaration_values/config.rs:5`,
which conflicts with the spec's "not a 6th BackendShape" claim at
`restart/ARCHITECTURE.md:1781`.

Dispatch context: this V1 inventory treats SK-V15 as PRUNE-then-REBUILD after
PASS-IMPL V1. CSS fact-stream contrivances and Pattern H are catalogued here as
implementation-level substrate evidence only; this agent makes no source edit,
lock edit, or commit.

## Spec-Claim ↔ Implementation Table

| id | claim path:line | impl path:line | verdict | note |
|---|---|---|---|---|
| 1A-SUB-001 | `restart/ARCHITECTURE.md:79` says `runtime` owns tape, direct-to-struct support, generated grammar modules, visitors, and document views. | `skinny/crates/runtime/src/tape/mod.rs:1`; `skinny/crates/runtime/src/grammars/json/mod.rs:2`; `skinny/crates/runtime/src/grammars/json/mod.rs:12`; `skinny/crates/runtime/src/grammars/css_l4_declaration_values/mod.rs:2` | implemented | Skinny has a tape module and grammar modules. JSON exposes parser, sink, value, view, and visitor; CSS fact-stream modules expose config/generated/parser/sink rather than the full retained-view roster. |
| 1A-SUB-002 | `restart/locks/LOCKS.md:75` says retained typed values borrow into tape as `&'i Tape<'i>` plus cursor. | `skinny/crates/runtime/src/tape/mod.rs:175`; `skinny/crates/runtime/src/tape/mod.rs:191`; `skinny/crates/runtime/src/grammars/json/view.rs:68` | implemented | The live shape is `ValueRef<'doc, 'input, K, G>` over `&'doc Tape<'input>` and a `u32` cursor; JSON `DocumentView` returns a root `ValueRef`. |
| 1A-SUB-003 | `restart/ARCHITECTURE.md:1772` and `restart/locks/LOCKS.md:75` claim tape plus direct-to-struct form one substrate family. | `skinny/crates/runtime/src/tape/mod.rs:94`; `skinny/crates/runtime/src/grammars/json/view.rs:17`; `skinny/crates/runtime/src/grammars/json/generated.rs:760`; `skinny/crates/runtime/src/grammars/json/config.rs:22` | partial / UNKNOWN routed | Retained JSON is tape-backed and JSON direct is explicitly `SinkOnly`, but the shared TapeEmit / DirectBuild event schedule is still UNKNOWN in 1A-SUB-019. Do not count this as full substrate-union closure until that schedule proof lands. |
| 1A-SUB-004 | `restart/ARCHITECTURE.md:1788` names retained tape, direct sink, admitted fact-stream output, and transient scanner/capacity as output planes. | `skinny/crates/ir/src/cost.rs:55`; `skinny/crates/ir/src/cost.rs:118`; `skinny/crates/ir/src/cost.rs:139`; `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:5` | implemented | The IR cost facts encode `SubstrateTarget::{LocalTempOnly, ExistingTape, DirectSink, AdmittedFactOutput}` and provide `Lock1PolicyTriad::fact_stream`; CSS emits a fact stream. |
| 1A-SUB-005 | `restart/ARCHITECTURE.md:961` claims five materialization access patterns: EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage. | `skinny/crates/ir/src/lib.rs:340`; `skinny/crates/ir/src/cost.rs:333` | implemented | The five-shape enum and `all_backend_shapes()` list are present exactly as the spec names them. Runtime completion differs by shape in later rows. |
| 1A-SUB-006 | `restart/ARCHITECTURE.md:1844` requires append-only tape after committed checkpoints. | `skinny/crates/runtime/src/tape/assembler.rs:61`; `skinny/crates/runtime/src/tape/assembler.rs:71`; `skinny/crates/runtime/src/tape/assembler.rs:115` | partial / UNKNOWN routed | `TapeBuilder` appends offsets and seals them into `Tape` through `finish`, but bounded checkpoint/rollback is not evidenced; see UNKNOWN 1A-SUB-020. |
| 1A-SUB-007 | `restart/ARCHITECTURE.md:1846` says tokens borrow source slices where possible and payloads live in a payload arena. | `skinny/crates/runtime/src/tape/mod.rs:94`; `skinny/crates/runtime/src/tape/mod.rs:38`; `skinny/crates/runtime/src/tape/mod.rs:65`; `skinny/crates/runtime/src/grammars/json/view.rs:384` | implemented | Tape stores the input source and payload arena; JSON scalar ranges are computed from tape offsets into source bytes. |
| 1A-SUB-008 | `restart/ARCHITECTURE.md:1789` says `SinkOnly` retains no queryable document identity after parse. | `skinny/crates/runtime/src/grammars/json/generated.rs:760`; `skinny/crates/runtime/src/grammars/json/generated.rs:765`; `skinny/crates/runtime/src/grammars/json/generated.rs:767`; `skinny/crates/runtime/src/grammars/json/sink.rs:4` | implemented | JSON direct parse accepts `&mut S: JsonSink`, walks a local byte cursor, and returns `Result<(), ParseError>` rather than a document. |
| 1A-SUB-009 | `restart/ARCHITECTURE.md:869` claims Grammar IR carries typed annotations, host references, layout/error directives, and lookbehind. | `skinny/crates/ir/src/lib.rs:33`; `skinny/crates/ir/src/lib.rs:210`; `skinny/crates/ir/src/lib.rs:234` | unimplemented | Live `GrammarIr` and `ExprKind` cover Seq, Alt, Repeat, Optional, Literal, Regex, Ref, and Annotation. They do not carry Predicate, Lookbehind, Call, LayoutDirective, or ErrorDirective as first-class variants. |
| 1A-SUB-010 | `restart/ARCHITECTURE.md:932` claims the 20-variant Backend IR alphabet. | `skinny/crates/ir/src/lib.rs:354`; `skinny/crates/ir/src/lib.rs:392` | unimplemented | Live `BackendExpr` carries 13 variants; `SimdScan` is separate as `Recognizer::SimdScan`. Missing from the enum are PrattSpine, CallHost, LayoutScope, ErrorRecover, PathEval, and DebugMark. The spec itself records this live gap at `restart/ARCHITECTURE.md:1025`. |
| 1A-SUB-011 | `restart/ARCHITECTURE.md:1829` says `runtime/src/tape` owns token, builder, span, payload, view, and trace modules. | `skinny/crates/runtime/src/tape/mod.rs:1`; `skinny/crates/runtime/src/tape/mod.rs:5`; `skinny/crates/runtime/src/tape/assembler.rs:42`; `skinny/crates/runtime/src/tape/offsets.rs:1` | unimplemented | The live tape directory is `assembler`, `event_grammar`, tests, and `offsets`. Builder and payload concepts exist, but spec-named `token`, `span`, `view`, and `trace` modules are absent from the live module declaration. |
| 1A-SUB-012 | `restart/ARCHITECTURE.md:1918` says a typed event cursor is the shared read/write abstraction for offsets, event cells, collapsed state, sealed tape, or direct fields. | `skinny/crates/runtime/src/tape/event_grammar.rs:4`; `skinny/crates/runtime/src/grammars/json/event_grammar_witness.rs:17`; `skinny/crates/runtime/src/grammars/sheets_witness/event_grammar_witness.rs:16` | unimplemented | Live `EventGrammar` is a grammar/fact admission trait, not a cursor. JSON and Sheets witnesses define admitted facts/classes, but no shared typed event cursor is present. REDRESS fence: any future typed-event cursor must not revive EventCursor sidecars, retained structural streams, retained class lanes, or parser-owned cursor lists rejected by Lock 1. |
| 1A-SUB-013 | `restart/ARCHITECTURE.md:967` names `EventTape` and `CollapsedStage` access patterns. | `skinny/crates/ir/src/lib.rs:343`; `skinny/crates/ir/src/lib.rs:345`; `skinny/crates/ir/src/cost.rs:131` | unimplemented | These exist as enum variants and policy-triad mappings only. The audited runtime/tape/grammar files do not provide EventTape cells or collapsed-stage runtime shims. REDRESS fence: EventTape implementation must remain a generated same-substrate lowering, not a retained scanner/class sidecar. |
| 1A-SUB-014 | `restart/ARCHITECTURE.md:1944` says generated per-grammar runtime lives under `runtime/src/grammars/<grammar>/` with `mod.rs`, `generated.rs`, `view.rs`, `value.rs`, `visitor.rs`, and `host.rs`. | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/mod.rs:2`; `skinny/crates/runtime/src/grammars/css_l4_declaration_values/mod.rs:5`; `skinny/crates/runtime/src/grammars/sheets_witness/mod.rs:1` | unimplemented | CSS generated fact-stream dirs contain config/generated/parser/sink/mod, not retained view/value/visitor/host. `sheets_witness` contains only an event witness module. |
| 1A-SUB-015 | `restart/ARCHITECTURE.md:1956` says grammar runtime files are template-emitted, not hand-written production crates. | `skinny/crates/runtime/src/grammars/json/scan.rs:1`; `skinny/crates/runtime/src/grammars/json/sink.rs:1`; `skinny/crates/runtime/src/grammars/sheets_witness/mod.rs:1`; `skinny/crates/runtime/src/grammars/sheets_witness/event_grammar_witness.rs:1` | unimplemented | Five scoped grammar files lack the generated header or explicitly say they are not generated: JSON scan, JSON sink, JSON event witness, Sheets mod, and Sheets event witness. |
| 1A-SUB-016 | `restart/ARCHITECTURE.md:1781` says admitted fact-stream output is not a 6th `BackendShape`; `restart/locks/LOCKS.md:106` repeats that fact-stream is not a 6th BackendShape. | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/config.rs:5`; `skinny/crates/runtime/src/grammars/css_l4_declaration_values/config.rs:6`; `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:14` | impl_exceeds_spec | CSS config writes `W7_POLICY_BACKEND_SHAPE = "admitted_fact_output"` while the same row correctly writes `W7_SUBSTRATE_TARGET = "admitted_fact_output"`. This is metadata drift: admitted fact output is a substrate target/category, not a backend shape. |
| 1A-SUB-017 | `restart/ARCHITECTURE.md:1791` allows transient scanner/capacity planes but says retained sidecars are not substrate. | `skinny/crates/runtime/src/grammars/json/scan.rs:1`; `skinny/crates/runtime/src/grammars/json/scan.rs:22`; `skinny/crates/runtime/src/grammars/json/scan.rs:47`; `skinny/crates/runtime/src/grammars/json/generated.rs:12` | impl_exceeds_spec | JSON has a grammar-owned structural scan source returning `StructuralIndex`; retained generated parse's `attach_structural_index` is a no-op. The scan plane is live and must remain fenced as `local_temp_only`. |
| 1A-SUB-018 | `restart/ARCHITECTURE.md:1956` says grammar runtime files are template-emitted. | `skinny/crates/runtime/src/grammars/json/sink.rs:1`; `skinny/crates/runtime/src/grammars/json/sink.rs:4`; `skinny/crates/runtime/src/grammars/json/generated.rs:748` | impl_exceeds_spec | JSON direct sink is explicitly "JSON-owned direct sink source" and is imported by generated direct parsing. That is useful admitted SinkOnly evidence, but it is outside the generated-runtime-only claim. |
| 1A-SUB-019 | `restart/ARCHITECTURE.md:1956` says template-emitted files are not hand-written production crates. | `skinny/crates/runtime/src/grammars/json/mod.rs:1`; `skinny/crates/runtime/src/grammars/json/mod.rs:6`; `skinny/crates/runtime/src/grammars/json/mod.rs:7` | impl_exceeds_spec | `json/mod.rs` carries a generated header while publicly wiring `scan` and `sink`, two files that are explicitly not generated at `json/scan.rs:1` and `json/sink.rs:1`. |
| 1A-SUB-020 | `restart/ARCHITECTURE.md:1877` says direct builders do not bypass the substrate event stream and both retained/direct lower from `TapeEmit`/`DirectBuild` scheduling. | `skinny/crates/runtime/src/grammars/json/parser.rs:7`; `skinny/crates/runtime/src/grammars/json/parser.rs:11`; `skinny/crates/runtime/src/grammars/json/generated.rs:760`; `skinny/crates/runtime/src/grammars/json/generated.rs:765` | unknown | Retained JSON owns `ParserState { cursor, tape }`; direct JSON owns only input bytes and a local `cursor`. The audited runtime files do not prove a shared event schedule between them. |
| 1A-SUB-021 | `restart/ARCHITECTURE.md:1845` requires bounded rollback that does not clone OpenFrame stacks. | `skinny/crates/runtime/src/tape/assembler.rs:61`; `skinny/crates/runtime/src/tape/assembler.rs:93`; `skinny/crates/runtime/src/tape/assembler.rs:115` | unknown | Append and flag patching are visible; no bounded checkpoint/rollback API is visible in the audited tape files. |
| 1A-SUB-022 | `restart/ARCHITECTURE.md:1800` says live HEAD has zero generated runtime files in the older `crates/core` Pattern H surface, while `restart/ARCHITECTURE.md:1944` states the V1 generated runtime target. | `skinny/crates/runtime/src/grammars/json/parser.rs:1`; `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:1`; `skinny/crates/runtime/src/grammars/json/scan.rs:1` | unknown | Skinny now has many `// @generated` runtime files under the new skinny path, but comments alone do not prove generator provenance. This inventory records the file-state evidence, not provenance closure. |

## V2 Hardening Fold

| fold | disposition |
|---|---|
| CH1-V1-F4 | Counted `1A-SUB-022` in frontmatter and kept it UNKNOWN rather than treating generated comments as provenance proof. |
| CH5-V1-02 | Added root structural-index sidecar census requirement: scan `OnceCell<StructuralIndex>`, `scan_structural`, `ensure_structural_index`, and `next_structural_at_or_after`; classify each hit by `substrate_target`, `retention_lifetime`, and `policy_owner` before T-P2 uses it. |
| CH6-V1-1A-CLOSURE-WORDING | Downgraded substrate-family and checkpoint rows from implemented to partial / UNKNOWN routed where their own notes depend on unresolved shared scheduling or rollback evidence. |
| CH3/CH7 EventTape fence | Added local REDRESS fences to typed-event cursor and EventTape rows so future work cannot be read as permission to revive EventCursor sidecars or retained structural streams. |

## Divergences Catalogued

| id | divergence | loc_delta_estimate | risk | evidence |
|---|---|---:|---|---|
| 1A-DIV-001 | Grammar IR is smaller than §7.1: no Predicate, Lookbehind, Call, LayoutDirective, or ErrorDirective variants in live `ExprKind`. | 400-900 LOC | high | Spec `restart/ARCHITECTURE.md:885`; live `skinny/crates/ir/src/lib.rs:210`. |
| 1A-DIV-002 | BIR is still 13 enum variants plus `Recognizer::SimdScan`, not the 20-variant target. | 600-1,200 LOC | high | Spec `restart/ARCHITECTURE.md:932`; live `skinny/crates/ir/src/lib.rs:354`; `skinny/crates/ir/src/lib.rs:392`. |
| 1A-DIV-003 | The typed event cursor described by §9 is absent; `EventGrammar` is fact/class admission, not a cursor. | 300-700 LOC | medium | Spec `restart/ARCHITECTURE.md:1918`; live `skinny/crates/runtime/src/tape/event_grammar.rs:4`. |
| 1A-DIV-004 | Runtime shape modules do not match §9.1: live tape modules are `assembler`, `event_grammar`, and `offsets`, not token/builder/span/payload/view/trace. | 250-600 LOC | medium | Spec `restart/ARCHITECTURE.md:1829`; live `skinny/crates/runtime/src/tape/mod.rs:1`. |
| 1A-DIV-005 | CSS fact-stream config mislabels `admitted_fact_output` as a backend shape. | 20-80 LOC | low | Spec `restart/ARCHITECTURE.md:1781`; live `skinny/crates/runtime/src/grammars/css_l4_declaration_values/config.rs:5`. |
| 1A-DIV-006 | Direct JSON and retained JSON use structurally separate cursor carriers in audited runtime: retained has `ParserState.cursor` plus `TapeBuilder`; direct has raw bytes and local `cursor`. | 400-900 LOC | medium/high | Spec `restart/ARCHITECTURE.md:1877`; retained `skinny/crates/runtime/src/grammars/json/parser.rs:7`; direct `skinny/crates/runtime/src/grammars/json/generated.rs:760`. |
| 1A-DIV-007 | The generated-runtime claim is not closed: JSON scan/sink and Sheets witness files are not generated template files. | 200-500 LOC | medium | Spec `restart/ARCHITECTURE.md:1956`; live `skinny/crates/runtime/src/grammars/json/scan.rs:1`; `skinny/crates/runtime/src/grammars/json/sink.rs:1`; `skinny/crates/runtime/src/grammars/sheets_witness/mod.rs:1`. |

## Gaps / Missing Primitives

| gap | missing primitive | cited basis | verify_action |
|---|---|---|---|
| G1 | Shared typed event cursor for retained tape and direct sink. | Spec `restart/ARCHITECTURE.md:1918`; live retained cursor `skinny/crates/runtime/src/grammars/json/parser.rs:10`; live direct cursor `skinny/crates/runtime/src/grammars/json/generated.rs:766`. | Search for `EventCursor`, `EventTape`, and `typed event cursor` in `skinny/crates/runtime/src` and bind every hit to a consumer row; if absent, keep 1A-DIV-003 open. |
| G2 | Full §7.1 Grammar IR carrier set. | Spec `restart/ARCHITECTURE.md:873`; live `ExprKind` `skinny/crates/ir/src/lib.rs:210`. | Add or reject each missing variant through T-P2/T-P3 disposition; do not silently collapse host/layout/error/lookbehind into `Annotation`. |
| G3 | Full 20-variant BIR enum or an amended encoding that preserves the target distinctions. | Spec `restart/ARCHITECTURE.md:932`; live `BackendExpr` `skinny/crates/ir/src/lib.rs:354`. | Run a variant census against lowerers and runtime consumers; each target-only variant needs either a live Rust variant or a G-Omega amendment. |
| G4 | Runtime provenance fence for generated grammar files. | Spec `restart/ARCHITECTURE.md:1956`; live non-generated JSON scan `skinny/crates/runtime/src/grammars/json/scan.rs:1`. | Verify generator output manifest for all 48 files under `skinny/crates/runtime/src/grammars`; quarantine hand-owned witness/scan/sink files or generate them. |
| G5 | Correct fact-stream metadata vocabulary. | Spec `restart/locks/LOCKS.md:106`; live CSS config `skinny/crates/runtime/src/grammars/css_l4_declaration_values/config.rs:5`. | Rename CSS `W7_POLICY_BACKEND_SHAPE` to a substrate category field or set it to an actual BackendShape; keep `W7_SUBSTRATE_TARGET = admitted_fact_output`. |

## Lock 1 / Substrate-Union Analysis

Lock 1's retained-tape half is live for JSON. `Tape<'input>` stores source,
offsets, sparse flag cursors/values, payloads, and `TapeId` at
`skinny/crates/runtime/src/tape/mod.rs:94`; `Tape::id` returns identity at
`skinny/crates/runtime/src/tape/mod.rs:170`; and `JsonDocument` owns the tape at
`skinny/crates/runtime/src/grammars/json/view.rs:63`. `ValueRef` carries
`&Tape` plus cursor at `skinny/crates/runtime/src/tape/mod.rs:175`, and JSON
views derive object/array/string access from that cursor at
`skinny/crates/runtime/src/grammars/json/view.rs:84`.

The direct half is admitted but not unified. JSON `parse_direct` takes `input`,
derives `bytes`, creates `let mut cursor = 0`, and writes into `JsonSink` at
`skinny/crates/runtime/src/grammars/json/generated.rs:760`,
`skinny/crates/runtime/src/grammars/json/generated.rs:765`, and
`skinny/crates/runtime/src/grammars/json/generated.rs:767`; the sink methods
begin at `skinny/crates/runtime/src/grammars/json/sink.rs:4`. That matches
`SinkOnly`'s no-document identity claim at `restart/ARCHITECTURE.md:1789`, but
the audited runtime files do not show the shared event cursor required by
`restart/ARCHITECTURE.md:1877`.

The fact-stream plane is present and must stay classified as a substrate target,
not a backend shape. Lock 1 admits `admitted_fact_output` at
`restart/locks/LOCKS.md:120`, and `Lock1PolicyTriad::fact_stream` emits that
target at `skinny/crates/ir/src/cost.rs:139`. CSS generated output writes the
policy row at `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:14`,
but the config field name at
`skinny/crates/runtime/src/grammars/css_l4_declaration_values/config.rs:5`
misstates it as a backend shape.

The transient scanner plane is live. JSON `scan_structurals` returns a
`StructuralIndex` at `skinny/crates/runtime/src/grammars/json/scan.rs:22`, and
`structural_capacity_for` consumes it for `CapacityPlan::OneShotSimd` at
`skinny/crates/runtime/src/grammars/json/scan.rs:47`. The aarch64 scanner keeps
`in_string` and `bs_carry` within one `scan` call at
`skinny/crates/runtime/src/grammars/json/scan.rs:207` through tail handoff at
`skinny/crates/runtime/src/grammars/json/scan.rs:273`; this is compatible with
Lock 1's no-cross-call classifier-state elevation at `restart/locks/LOCKS.md:137`.

Net: Lock 1 is partly honored. There is one retained tape for JSON and admitted
direct/fact/transient planes, but the substrate union is not yet proven as one
typed event cursor and one `TapeEmit`/`DirectBuild` schedule.

## Open Questions

| id | UNKNOWN | why unknown | verify_action |
|---|---|---|---|
| 1A-UNK-001 | Does any generated direct path consume a shared event cursor rather than raw bytes? | The cited JSON direct path uses `bytes` and `cursor` directly at `skinny/crates/runtime/src/grammars/json/generated.rs:765`. | `rg -n "EventCursor|DirectBuild|TapeEmit|parse_direct|cursor: &mut usize" skinny/crates/runtime/src/grammars skinny/crates/runtime/src/tape skinny/crates/ir/src` and bind every direct consumer to either shared event scheduling or raw cursor drift. |
| 1A-UNK-002 | Is bounded rollback implemented under another name? | Tape builder append and flag patching are visible at `skinny/crates/runtime/src/tape/assembler.rs:61` and `skinny/crates/runtime/src/tape/assembler.rs:93`, but no checkpoint API is cited. | `rg -n "checkpoint|rollback|mark|restore|commit" skinny/crates/runtime/src/tape skinny/crates/runtime/src/grammars` and classify hits as runtime substrate, parser-local, or absent. |
| 1A-UNK-003 | Which of the 48 grammar files are generated by the rostered generator versus marked by comment only? | Generated headers exist at `skinny/crates/runtime/src/grammars/json/parser.rs:1`, but hand-owned files exist at `skinny/crates/runtime/src/grammars/json/scan.rs:1`. | Compare `xtask`/codegen manifest output to the file list under `skinny/crates/runtime/src/grammars`; record generated, hand-owned witness, and hand-owned production buckets. |
| 1A-UNK-004 | Is CSS fact-stream `W7_POLICY_BACKEND_SHAPE` a deliberate telemetry key or a metadata bug? | Spec says fact stream is not a BackendShape at `restart/ARCHITECTURE.md:1781`; CSS writes it as `W7_POLICY_BACKEND_SHAPE` at `skinny/crates/runtime/src/grammars/css_l4_declaration_values/config.rs:5`. | Check the generator template that emits all seven CSS config files; either rename the key or cite the consumer that interprets it as non-shape telemetry. |
| 1A-UNK-005 | Are `EventTape` and `CollapsedStage` target-only by design for SK-V15, or expected live runtime shapes after PASS-IMPL V1? | They exist in enum rows at `skinny/crates/ir/src/lib.rs:343` and `skinny/crates/ir/src/lib.rs:345`, but no runtime carrier was found in audited files. | Require T-P2/T-P3 to choose: keep target-only rows with explicit wave owner, or amend the substrate/lowering spec to match current skinny scope. |
