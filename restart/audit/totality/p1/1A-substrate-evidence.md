---
agent: 1A
pass: T-P1-TOTALITY-EXCAVATION
cycle: V5-SKV18-totality
cycle_self_label: SK-V18
generated_at: 2026-06-01T00:00:00Z
spec_surfaces_audited: [restart/ARCHITECTURE.md, restart/MASTER-PLAN.md, restart/locks/LOCKS.md, restart/skinny/tranches/sk-v18/SPEC.md, restart/skinny/tranches/sk-v18/research/p2/SYNTHESIS-RESEARCH.md, restart/skinny/tranches/sk-v18/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md]
files_audited_count: 28
live_truth_method: "Line-audited ARCHITECTURE §9.1/§9.2 and LOCKS Lock 1 + SK-V17 crystallisation clauses with rg+sed; read skinny/crates/runtime/src/tape/{mod,assembler,event_grammar}.rs end-to-end; read skinny/crates/runtime/src/grammars/json/{parser,generated,sink,scan,mod,view,value,config}.rs and css_l4_declaration_values/{config,generated}.rs and sheets_witness/{mod,event_grammar_witness}.rs; censused EventGrammar/<G>/PhantomData over skinny/crates/runtime/src with rg; read skinny/crates/ir/src/{lib,cost}.rs enums; cross-checked SK-V18 SPEC §7 (G4/R-D) + SYNTHESIS-RESEARCH R-D + AUDIT-OVERFIT addendum 4 (R5). No cargo, no source edit, no commit; docs-only evidence inventory."
prior_cycle_dispositions_folded:
  accepted:
    - 1A-SUB-002 (ValueRef &'doc Tape<'input> + u32 cursor) re-verified IMPLEMENTED at HEAD
    - 1A-SUB-005 (five BackendShape variants) re-verified IMPLEMENTED at skinny/crates/ir/src/lib.rs:340-345
    - 1A-SUB-009/010 (Grammar-IR + 13-variant BIR shortfall) re-verified UNIMPLEMENTED at HEAD
    - 1A-DIV-006 (two-cursor JSON retained-vs-direct) re-verified; now a routed §9.2 Ω-A carrier item
  rejected: []
  revised:
    - 1A-SUB-016 / 1A-DIV-005 (CSS W7_POLICY_BACKEND_SHAPE mislabel) — the mislabel was REMOVED (a fact, not a resolution); CSS config.rs no longer emits any W7/BackendShape labels at HEAD; the removal OPENED an opposite-direction GAP (CSS substrate-target classification now has ZERO config evidence), graded partial / gap-routed, NOT impl_exceeds_spec (CH6-F2)
    - 1A-SUB-020 cursor-shape carrier note now folded into ARCHITECTURE §9.2 Part(b) Ω-A; a THIRD cursor carrier (codegen DirectParser.cursor) surfaced at HEAD
    - Added SK-V18 R-D lens rows (1A-SUB-023..026) absent from V4
divergence_count:
  # Schema widened per CH1-V2-F3: the four base taxonomy words do not absorb the
  # compound row labels (partial/*, DIVERGES); the buckets below enumerate every
  # SUB-001..026 row exactly once and SUM TO 26 (auditable).
  # impl_exceeds_spec dropped 2->1 per CH6-F3 (1A-SUB-018 downgraded to partial/gap-routed).
  implemented: 7          # SUB-002,004,005,007,008,019,024 (024 = "implemented (substrate-neutral confirmed)")
  unimplemented: 8        # SUB-009,010,011,012,013,014,015,023 (023 = "impl_confirms_plan / unimplemented (DELETE pending)")
  impl_exceeds_spec: 1    # SUB-017 only (SUB-018 downgraded per CH6-F3)
  unknown: 3              # SUB-020,021,022
  partial: 5              # SUB-001 (JSON-example), 003 + 006 (UNKNOWN-routed), 016 + 018 (gap-routed)
  diverges: 2             # SUB-025 (1E amendment candidate), 026 (doc carrier)
  total: 26
locks_amendment_candidates: 1
---

## Executive Summary

The Lock-1 retained half is live for JSON and grammar-neutral at the kernel:
`Tape<'input>` owns source, offsets, sparse flag cursors/values, payload arena,
and `TapeId` (`skinny/crates/runtime/src/tape/mod.rs:94`); `ValueRef<'doc,'input,
K,G>` is `&'doc Tape<'input>` + `u32` cursor (`:175`); `JsonRoot`/`JsonDocument`
own a sealed tape (`grammars/json/view.rs:12,:63`). The five `BackendShape`
variants and the four-target `SubstrateTarget` triad are live
(`ir/src/lib.rs:340`, `ir/src/cost.rs:57`). The substrate kernel IS
grammar-neutral enough to serve the un-forked generator — CSS borrows the same
sparse flag pair, no second tape (`css_l4_declaration_values/generated.rs:257`).

The SK-V18 R-D lens (DELETE phantom `<G>`/`EventGrammar` + thin `Cursor`
micro-trait) ALIGNS with Lock 1's substrate-union: the `<G>` axis is decoration
(census of non-test instantiation is EMPTY on disk — every `*EventGrammar` use
is in `event_grammar_witness.rs` defs + `event_grammar_tests.rs` under
`#[cfg(test)]`); the union claim is `&'i Tape<'i>` + cursor, which `<G>` never
touched. R-D's G4 adds a VIEW trait over the EXISTING tape (no second
substrate). The one LOCKS-amendment CANDIDATE: the **Lock 14
ValueRef/classifier-generalisation clause** (`restart/locks/LOCKS.md:620`)
asserts "`G:EventGrammar` type parameter is the generality vehicle" — which the
certified SK-V18 plan DELETES (`restart/skinny/tranches/sk-v18/SPEC.md:1202-1207`).
Direct contradiction.

Material divergences persist: BIR is 13 variants vs the 20-variant §7.1 target;
Grammar-IR `ExprKind` lacks Predicate/Lookbehind/Call/Layout/Error; the §9
shared typed event cursor is absent (`EventGrammar` is fact admission, not a
cursor); §9.1 tape modules (token/builder/span/payload/view/trace) do not match
live (`assembler`/`event_grammar`/`offsets`); no bounded checkpoint/rollback API
exists in the assembler; and two/three structurally independent cursor carriers
remain the open Ω-A item. This V18 inventory edits no source and surfaces one 1E
amendment candidate; disposition is T-P3.

## Spec-Claim <-> Implementation Table

| id | claim path:line | impl path:line | verdict | note |
|---|---|---|---|---|
| 1A-SUB-001 | `restart/ARCHITECTURE.md:79` / `restart/locks/LOCKS.md:75` say runtime owns tape, direct-to-struct, generated grammar modules, visitors, document views. | `skinny/crates/runtime/src/tape/mod.rs:1`; `grammars/json/mod.rs:2`; `grammars/css_l4_declaration_values/mod.rs:2` | partial / JSON-example implemented | JSON exposes parser/sink/value/view/visitor/host; CSS fact-stream dirs expose only config/generated/parser/sink/mod; Sheets is a 24-LOC witness stub. Not broad runtime-ownership closure. |
| 1A-SUB-002 | `restart/locks/LOCKS.md:75` retained typed values borrow into tape as `&'i Tape<'i>` + cursor. | `skinny/crates/runtime/src/tape/mod.rs:175`; `:191`; `grammars/json/view.rs:68` | implemented | Live shape `ValueRef<'doc,'input,K,G>` over `&'doc Tape<'input>` + `u32` cursor; JSON `DocumentView::root_value` returns a root `ValueRef`. |
| 1A-SUB-003 | `restart/ARCHITECTURE.md:2010` / `restart/locks/LOCKS.md:75` claim tape + direct-to-struct are one substrate family. | `skinny/crates/runtime/src/tape/mod.rs:94`; `grammars/json/view.rs:17`; `grammars/json/generated.rs:760`; `grammars/json/config.rs:22` | partial / UNKNOWN routed | Retained JSON is tape-backed; direct JSON is `SinkOnly`. Shared `TapeEmit`/`DirectBuild` event schedule remains UNKNOWN (1A-UNK-001). |
| 1A-SUB-004 | `restart/locks/LOCKS.md:120` names `local_temp_only`/`existing_tape`/`direct_sink`/`admitted_fact_output` targets. | `skinny/crates/ir/src/cost.rs:57`; `:118`; `:139` | implemented | `SubstrateTarget::{LocalTempOnly,ExistingTape,DirectSink,AdmittedFactOutput}`; `Lock1PolicyTriad::fact_stream()` emits `AdmittedFactOutput`. |
| 1A-SUB-005 | `restart/locks/LOCKS.md:107` names five `BackendShape`: EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage. | `skinny/crates/ir/src/lib.rs:340-345`; `cost.rs:334` | implemented | The five-shape enum and `all_backend_shapes(): [BackendShape;5]` are present verbatim. Runtime completion differs by shape (see EventTape/CollapsedStage rows). |
| 1A-SUB-006 | `restart/ARCHITECTURE.md:1946` requires append-only builder with bounded checkpoints. | `skinny/crates/runtime/src/tape/assembler.rs:62`; `:71`; `:115` | partial / UNKNOWN routed | `TapeBuilder` appends offsets/flags and seals via `finish`; NO bounded checkpoint/rollback API present (1A-UNK-002). |
| 1A-SUB-007 | `restart/ARCHITECTURE.md:1948` tokens borrow source slices; payloads live in a payload arena. | `skinny/crates/runtime/src/tape/mod.rs:94`; `:38`; `:65`; `grammars/json/view.rs:384` | implemented | Tape stores `source: &'input [u8]` + `PayloadArena`; JSON scalar ranges compute from tape offsets into source bytes. |
| 1A-SUB-008 | `restart/ARCHITECTURE.md:2056` `SinkOnly` retains no queryable document identity. | `skinny/crates/runtime/src/grammars/json/generated.rs:760`; `:766`; `:767`; `sink.rs:4` | implemented | `parse_direct<S: JsonSink>(input, sink)` walks a local `let mut cursor = 0` and returns `Result<(), ParseError>` — no document. `config::W7_DIRECT_BACKEND_SHAPE == "SinkOnly"` debug-asserted at `generated.rs:762`. |
| 1A-SUB-009 | `restart/ARCHITECTURE.md:869`/`:881` Grammar IR carries typed annotations, host refs, layout/error directives, predicate, lookbehind. | `skinny/crates/ir/src/lib.rs:211-237` | unimplemented | Live `ExprKind` is 8 variants (Seq, Alt, Repeat, Optional, Literal, Regex, Ref, Annotation). No Predicate, Lookbehind, Call, LayoutDirective, or ErrorDirective. |
| 1A-SUB-010 | `restart/ARCHITECTURE.md:932` claims the 20-variant Backend IR alphabet. | `skinny/crates/ir/src/lib.rs:355-392`; `:393-401` | unimplemented | Live `BackendExpr` is 13 variants (Entry, Seq, Alt, RepeatLoop, OptionalBranch, ByteLiteral, RegexProgram, CallRule, SpanMark, TapeEmit, DirectBuild, ValueProject, Return); `SimdScan` is separate as `Recognizer::SimdScan`. Missing: PrattSpine, CallHost, LayoutScope, ErrorRecover, PathEval, DebugMark. |
| 1A-SUB-011 | `restart/ARCHITECTURE.md:1941-1950` `runtime/src/tape` owns token/builder/span/payload/view/trace modules. | `skinny/crates/runtime/src/tape/mod.rs:1-5` | unimplemented | Live tape modules are `assembler`, `event_grammar`, `offsets` (+ tests). Builder/payload concepts exist inside `assembler`/`mod`; spec-named `token`/`span`/`view`/`trace` modules are absent. |
| 1A-SUB-012 | `restart/ARCHITECTURE.md:2052` typed event cursor is the shared read/write abstraction over offsets/event cells/collapsed state/sealed tape/direct fields. | `skinny/crates/runtime/src/tape/event_grammar.rs:4`; `grammars/json/event_grammar_witness.rs:17`; `grammars/sheets_witness/event_grammar_witness.rs:16` | unimplemented | Live `EventGrammar` is a fact/class admission trait (`STRUCTURAL_CLASS_COUNT`, `admits_fact`, `admits_class`), NOT a cursor. No shared typed event cursor exists. REDRESS fence: any future typed-event cursor must not revive EventCursor sidecars, retained structural streams, retained class lanes, or parser-owned cursor lists rejected by Lock 1 (`restart/locks/LOCKS.md:137-158`). |
| 1A-SUB-013 | `restart/locks/LOCKS.md:108` names `EventTape` and `CollapsedStage`. | `skinny/crates/ir/src/lib.rs:343`; `:345`; `cost.rs:131` | unimplemented | Present only as enum variants + policy-triad mappings. No EventTape cells or collapsed-stage runtime carrier in audited tape/grammar files. CollapsedStage is also aarch64-barred per Lock 10 clause (`restart/locks/LOCKS.md:614`). |
| 1A-SUB-014 | `restart/ARCHITECTURE.md:1944` generated per-grammar runtime lives under `runtime/src/grammars/<g>/` with mod/generated/view/value/visitor/host. | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/mod.rs:1-9`; `grammars/sheets_witness/mod.rs:1` | unimplemented | CSS fact-stream dirs contain config/generated/parser/sink/mod, not retained view/value/visitor/host. `sheets_witness` contains ONLY `event_grammar_witness` (1-line mod.rs). JSON is the sole full roster. |
| 1A-SUB-015 | `restart/ARCHITECTURE.md:1944` grammar runtime files are template-emitted, not hand-written. | `skinny/crates/runtime/src/grammars/json/scan.rs:1`; `json/sink.rs:1`; `sheets_witness/event_grammar_witness.rs:1` | unimplemented | JSON `scan.rs` + `sink.rs` declare "not part of the generated JSON roster"; Sheets/JSON event witnesses are hand-owned. Generator-provenance closure unproven (1A-UNK-003). |
| 1A-SUB-016 | `restart/locks/LOCKS.md:106` admitted fact output is a substrate category, NOT a 6th `BackendShape`. | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/config.rs:1-9` | partial / gap-routed | CSS config.rs at HEAD emits NO W7/BackendShape labels at all (only ROW_ID/ENTRY_RULE/hash/counts). The prior `W7_POLICY_BACKEND_SHAPE = "admitted_fact_output"` mislabel (V4 1A-SUB-016) was REMOVED — a fact, not a resolution. The removal OPENED a gap: the substrate-target classification for the CSS fact stream now has ZERO config evidence (`rg -c 'W7_|BackendShape|substrate_target' css_l4_declaration_values/config.rs` = 0). This is an opposite-direction GAP routed to 1A-DIV-005 + 1A-UNK-004, NOT the impl exceeding spec (CH6-F2). |
| 1A-SUB-017 | `restart/ARCHITECTURE.md:2051` transient scanner/mask planes allowed; retained sidecars are not substrate. | `skinny/crates/runtime/src/grammars/json/scan.rs:22`; `:47`; `skinny/crates/runtime/src/grammars/json/generated.rs:12-15` | impl_exceeds_spec | JSON `attach_structural_index` is a NO-OP stub — `debug_assert_eq!(config::STRUCTURAL_BYTES, …)` then `let _ = state;` (`skinny/crates/runtime/src/grammars/json/generated.rs:12-15`), consuming nothing and retaining no sidecar. The actual structural scan returning `StructuralIndex` for capacity is `scan.rs:22 scan_structurals`, consumed by `scan.rs:47 structural_capacity_for`. The directional conclusion HOLDS and strengthens: skinny carries NO retained structural sidecar; the scan plane is `local_temp_only`, no cross-call carry (CH5-V1-09). |
| 1A-SUB-018 | `restart/ARCHITECTURE.md:1944` grammar runtime files are template-emitted. | `skinny/crates/runtime/src/grammars/json/sink.rs:1`; `:4`; `generated.rs:748` | partial / gap-routed | JSON direct sink is "JSON-owned direct sink source; not part of the generated JSON roster," imported by generated direct parsing (`use super::sink::JsonSink` at `generated.rs:748`). The evidence shows the "template-emitted" spec claim is VIOLATED — the file is HAND-WRITTEN, NOT generated — so this is NOT the impl exceeding spec; it is a standing generated-runtime gap. Useful SinkOnly evidence as a FACT (not an exceedance). Routed to 1A-DIV-007 (generated-runtime claim not closed); R-A (G1 un-fork) targets retiring these hand-written couriers. Downgraded from `impl_exceeds_spec` per CH6-F3 (closure-word inversion: an opened gap cannot be credited as impl-exceeds-spec). |
| 1A-SUB-019 | `restart/ARCHITECTURE.md:1789` (`SinkOnly` no document identity) vs the `direct_sink` substrate-target triad. | `skinny/crates/runtime/src/grammars/json/config.rs:22-30`; `generated.rs:760-762` | implemented | JSON config emits the full triad: `W7_DIRECT_BACKEND_SHAPE="SinkOnly"`, `W7_SUBSTRATE_TARGET="direct_sink"`, `W7_RETENTION_LIFETIME="generated_function"`, `W7_POLICY_OWNER="generated_grammar"`, `W7_SAME_SUBSTRATE_UNION="pass"`. Correct vocabulary, debug-asserted in the body. |
| 1A-SUB-020 | `restart/ARCHITECTURE.md:2010` direct builders do not bypass the substrate event stream; both lower from `TapeEmit`/`DirectBuild`. | `skinny/crates/runtime/src/grammars/json/parser.rs:7-12`; `generated.rs:760`; `:766` | unknown | Retained JSON owns `ParserState{ input, bytes, cursor: usize, tape: TapeBuilder }`; direct JSON owns only `bytes` + a local `cursor`. Audited files do not prove one shared event schedule. Routed to §9.2 Ω-A. |
| 1A-SUB-021 | `restart/ARCHITECTURE.md:1957` bounded rollback that does not clone OpenFrame stacks. | `skinny/crates/runtime/src/tape/assembler.rs:62`; `:94`; `:115` | unknown | Append (`push_offset`) and ordered flag patching (`patch_flags`) are visible; no checkpoint/rollback/mark/restore API exists in the assembler (1A-UNK-002). |
| 1A-SUB-022 | `restart/ARCHITECTURE.md:1944` V1 generated-runtime target vs comment-only `@generated` headers. | `skinny/crates/runtime/src/grammars/json/parser.rs:1`; `css_l4_declaration_values/generated.rs:1`; `json/scan.rs:1` | unknown | Many `// @generated by skinny bbnf-codegen` headers exist, but comments alone do not prove rostered-generator provenance; hand-owned scan/sink/witness files coexist. Round-trip regen discipline (Lock 6 v+1, `restart/locks/LOCKS.md:185`) is the closure gate. |
| 1A-SUB-023 | SK-V18 `sk-v18/SPEC.md:1202-1207` (R-D/G4a): the phantom `<G: EventGrammar = AnyGrammar>` axis has ZERO non-test production instantiation — DELETE. | `skinny/crates/runtime/src/tape/mod.rs:175`; `:179`; `tape/event_grammar.rs:4`; `tape/event_grammar_tests.rs:18-48`; `grammars/json/event_grammar_witness.rs:4`; `grammars/sheets_witness/event_grammar_witness.rs:4` | impl_confirms_plan / unimplemented (DELETE pending) | Census on disk: every `EventGrammar`/`AnyGrammar`/`<G>`/`_grammar: PhantomData` use is the trait/enum def, the `ValueRef` field, the two witness DEFS, or `event_grammar_tests.rs` (`#[cfg(test)]` + `const _: fn() = _proof_compiles::<…>`). NO production code instantiates `G` with a real type. R-D DELETE-default is grounded; the K-axis (`_kind: PhantomData<fn() -> K>`, `:178`) is the REAL Kind axis and PRESERVED (`JsonNodeKind`/`RootKind`/`ObjectKind` dispatch in `value.rs:143`/`view.rs`). |
| 1A-SUB-024 | SK-V18 `sk-v18/SPEC.md:1254` (G4.2 conjunct 4 / R-D): the `Cursor` trait is a VIEW over the EXISTING `Tape`/`ValueRef`/`PayloadArena`; no second substrate. | `skinny/crates/runtime/src/tape/mod.rs:94`; `grammars/css_l4_declaration_values/generated.rs:257` | implemented (substrate-neutral confirmed) | The substrate kernel is already grammar-neutral: CSS retained parse "Holds exactly the existing `Tape` — no second substrate"; CSS reuses the same sparse `(flag_cursors, flag_values)` pair. No `tape/cursor.rs` exists yet (the G4b trait is unimplemented). R-D adds a trait over this kernel, fully Lock-1-compatible. |
| 1A-SUB-025 | `restart/locks/LOCKS.md:620` (Lock 14 clause): "`G:EventGrammar` type parameter is the generality vehicle." | `skinny/crates/runtime/src/tape/mod.rs:175`; vs `sk-v18/SPEC.md:1202-1207` (DELETE `<G>`) | DIVERGES (1E amendment candidate) | The certified SK-V18 plan DELETES the very `G:EventGrammar` axis the Lock 14 clause names as the generality vehicle. The clause's generality claim must transfer to the SHARED `Cursor` trait + config-breadth classifier (the clause's OWN "separate axis"), not the phantom `<G>`. Surfaced as `1A-LOCK1-AMEND-001` below; disposition T-P3, ratification Pass Omega. |
| 1A-SUB-026 | `restart/ARCHITECTURE.md:1990-2008` §9.2 "Lazy `ValueRef<G>` value-plane" prose hard-codes `G:EventGrammar` as "the generality vehicle." | `restart/ARCHITECTURE.md:1997`; vs `skinny/crates/runtime/src/tape/mod.rs:175` (phantom) | DIVERGES (doc carrier) | The §9.2 prose carries the same stale `<G>`-as-generality premise R-D deletes. Doc-prose divergence companion to 1A-SUB-025; no separate amendment row (same root). The §9.2 Part(b)/Ω-A cursor-shape carrier note is the live anchor for the cursor-unification half. |

## Divergences Catalogued

| id | divergence | loc_delta_estimate | risk | evidence |
|---|---|---:|---|---|
| 1A-DIV-001 | Grammar IR `ExprKind` is 8 variants; no Predicate, Lookbehind, Call, LayoutDirective, ErrorDirective. | 400-900 LOC | high | Spec `restart/ARCHITECTURE.md:869`,`:881`; live `skinny/crates/ir/src/lib.rs:211-237`. |
| 1A-DIV-002 | BIR is 13 `BackendExpr` variants + separate `Recognizer::SimdScan`, not the 20-variant target (missing PrattSpine, CallHost, LayoutScope, ErrorRecover, PathEval, DebugMark). | 600-1,200 LOC | high | Spec `restart/ARCHITECTURE.md:932`; live `skinny/crates/ir/src/lib.rs:355`,`:393`. |
| 1A-DIV-003 | The typed event cursor described by §9 is absent; `EventGrammar` is fact/class admission, not a cursor. R-D G4b would introduce `tape/cursor.rs` as a thin VIEW trait — NOT the §9 read/write event cursor (scoped to laziness/cursor contract only). | 300-700 LOC | medium | Spec `restart/ARCHITECTURE.md:2052`; live `skinny/crates/runtime/src/tape/event_grammar.rs:4`; plan `sk-v18/SPEC.md:1208-1212`. |
| 1A-DIV-004 | §9.1 tape modules (token/builder/span/payload/view/trace) do not match live (`assembler`/`event_grammar`/`offsets`). | 250-600 LOC | medium | Spec `restart/ARCHITECTURE.md:1941-1950`; live `skinny/crates/runtime/src/tape/mod.rs:1-5`. |
| 1A-DIV-005 | CSS fact-stream substrate-target classification has NO config evidence at HEAD. The prior `W7_POLICY_BACKEND_SHAPE` mislabel was removed, but no `substrate_target = admitted_fact_output` row replaced it in CSS config.rs. | 20-80 LOC | low/medium | Prior mislabel resolved; live `skinny/crates/runtime/src/grammars/css_l4_declaration_values/config.rs:1-9` (no W7 row); contrast JSON `json/config.rs:22-26` which carries the full triad. |
| 1A-DIV-006 | JSON retained and direct use structurally separate cursor carriers: retained `ParserState.cursor: usize` + `TapeBuilder`; direct `parse_direct` local `cursor: usize` by `&mut`; AND a THIRD codegen-template `DirectParser.cursor` with its own `checkpoint = parser.cursor` rollback. | 400-900 LOC | medium/high | Spec `restart/ARCHITECTURE.md:2010`; retained `grammars/json/parser.rs:7-12`; direct `grammars/json/generated.rs:766`; codegen `skinny/crates/codegen/src/json_typed_direct.rs:671` (the `cursor: usize` field inside `struct DirectParser<'i>` at `:668`, within `const PARSER_RUNTIME`), with the `checkpoint = parser.cursor` rollback emitted at `:361` (re-anchored per CH1-V3-F10: `:56` was the `DirectParser::new(input)` instantiation, not the field). Routed to §9.2 Part(b)/Ω-A. |
| 1A-DIV-007 | Generated-runtime claim not closed: JSON scan/sink and Sheets/JSON event witnesses are explicitly NOT generated template files. R-A (G1 un-fork) names retiring the JSON `_RS`/scan/sink couriers; PRUNE-4 adds `EventGrammar`/`*EventGrammar` to `FORBIDDEN_GENERIC_TOKENS`. | 200-500 LOC | medium | Spec `restart/ARCHITECTURE.md:1944`; live `grammars/json/scan.rs:1`,`json/sink.rs:1`,`sheets_witness/event_grammar_witness.rs:1`; plan `audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:170-176`. |
| 1A-DIV-008 | No bounded checkpoint/rollback API in the assembler; §9.1/§1 invariant "rollback is bounded and does not clone OpenFrame stacks" is unevidenced at the substrate. | 80-300 LOC | medium | Spec `restart/ARCHITECTURE.md:1957`; live `skinny/crates/runtime/src/tape/assembler.rs:42-124` (append/patch/finish only). |

## Gaps / Missing Primitives

| gap | missing primitive | cited basis | verify_action |
|---|---|---|---|
| G1 | Shared typed event cursor (or, per R-D, the thin `Cursor` micro-trait `tape/cursor.rs`) unifying retained tape + direct sink + CSS/Sheets roots. | Spec `restart/ARCHITECTURE.md:2052`; plan `sk-v18/SPEC.md:1208-1217`; live retained `grammars/json/parser.rs:10`, direct `grammars/json/generated.rs:766`. | Confirm `tape/cursor.rs` absent (it is); track G4b. Bind every direct/retained consumer to either shared scheduling or raw cursor drift before T-P2 uses it. |
| G2 | Full §7.1 Grammar-IR carrier set (Predicate/Lookbehind/Call/Layout/Error). | Spec `restart/ARCHITECTURE.md:869`; live `skinny/crates/ir/src/lib.rs:211`. | T-P2/T-P3 disposition per variant; do not collapse host/layout/error/lookbehind into `Annotation`. |
| G3 | Full 20-variant BIR or an amended encoding preserving target distinctions. | Spec `restart/ARCHITECTURE.md:932`; live `skinny/crates/ir/src/lib.rs:355`. | Variant census vs lowerers/runtime consumers; each target-only variant needs a live variant or a G-Omega amendment. |
| G4 | Runtime provenance fence for generated grammar files (round-trip regen). | Spec `restart/ARCHITECTURE.md:1944`; Lock 6 v+1 `restart/locks/LOCKS.md:185`; live non-generated `grammars/json/scan.rs:1`. | Verify generator output manifest for all 48 files under `skinny/crates/runtime/src/grammars`; quarantine/generate hand-owned witness/scan/sink files (R-A un-fork). |
| G5 | CSS fact-stream substrate-target config row (`substrate_target = admitted_fact_output`). | Spec `restart/locks/LOCKS.md:100-106`; live `grammars/css_l4_declaration_values/config.rs:1-9` (absent). | Emit the substrate-category field in the CSS config template (mirror JSON `config.rs:23`), keeping it OUT of the `BackendShape` namespace. |
| G6 | Bounded checkpoint/rollback API at the tape builder. | Spec `restart/ARCHITECTURE.md:1957`; live `tape/assembler.rs:42-124`. | Search `checkpoint|rollback|mark|restore|commit` across `tape/` + `grammars/`; classify as substrate / parser-local / absent. |

## Lock 1 / Substrate-Union Analysis (SPINE)

Lock 1's retained half is live and grammar-neutral. `Tape<'input>` stores
`source: &'input [u8]`, `offsets: Vec<u32>`, the sparse `(flag_cursors,
flag_values)` pair, `PayloadArena`, and `TapeId` at
`skinny/crates/runtime/src/tape/mod.rs:94`; `Tape::id` returns identity at
`skinny/crates/runtime/src/tape/mod.rs:170` (live-verified: `pub fn id(&self) ->
TapeId` IS at `:170`; the prior cycle's `:172` re-cite is contradicted by disk).
`ValueRef<'doc,'input,K,G>` is `&'doc Tape` + `u32` cursor at
`skinny/crates/runtime/src/tape/mod.rs:175`. JSON `JsonDocument` owns the sealed
tape at `skinny/crates/runtime/src/grammars/json/view.rs:63`, and CSS retained
parse "Holds exactly the existing `Tape` — no second substrate"
(`skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:257`)
reusing the same sparse flag pair. The kernel IS grammar-neutral enough to serve
the un-forked SK-V18 generator: no grammar-named field, no per-grammar tape type.

The direct half is admitted but not unified. JSON `parse_direct<S: JsonSink>`
creates `let mut cursor = 0` and writes into the sink (`generated.rs:760-767`),
matching `SinkOnly`'s no-document claim (`restart/ARCHITECTURE.md:2056`) with the
correct triad in `json/config.rs:22-26`. But the audited files show NO shared
typed event cursor (`restart/ARCHITECTURE.md:2010`); there are now THREE cursor
carriers (retained `ParserState.cursor`, runtime-direct local `cursor`, codegen
`DirectParser.cursor`) — the §9.2 Part(b)/Ω-A open item.

SK-V18 R-D lens. The phantom `<G: EventGrammar = AnyGrammar>` is NOT part of the
Lock-1 substrate-union claim. The union is `&'i Tape<'i>` + cursor
(`restart/locks/LOCKS.md:75`); `<G>` is a zero-cost marker the union never
touches. Census (verified on disk): every `EventGrammar`/`AnyGrammar`/`<G>`/
`_grammar: PhantomData` site is a def, the `ValueRef` field, the two witness defs
(`grammars/{json,sheets_witness}/event_grammar_witness.rs`), or
`event_grammar_tests.rs` under `#[cfg(test)]`. The non-test production-
instantiation census is EMPTY — R-D's DELETE-default is grounded. The R-D
`Cursor` micro-trait (G4b, `tape/cursor.rs`) is a VIEW over the EXISTING
`Tape`/`ValueRef`/`PayloadArena` with G4.2-conjunct-4 forbidding any second tape
or parallel cursor type (`sk-v18/SPEC.md:1254-1257`). R-D therefore ALIGNS with
Lock 1 substrate-union on every axis: it removes decoration and adds a fenced
view, no new substrate, laziness intact, K-axis preserved.

The single contradiction is the LOCKS Lock-14 clause text: `restart/locks/LOCKS.md:620`
asserts "`G:EventGrammar` type parameter is the generality vehicle," and §9.2 prose
(`restart/ARCHITECTURE.md:1990-2008`) repeats it. The certified SK-V18 plan
DELETES that axis (`sk-v18/SPEC.md:1202-1207`). The clause's OWN text already
distinguishes the "separate axis" config-breadth classifier from the value-fold;
the amendment routes the generality claim off the phantom `<G>` and onto (a) the
shared `Cursor` trait + (b) the config-breadth classifier the clause already
names. See `1A-LOCK1-AMEND-001`.

Net: Lock 1 is partly honored. One retained tape (grammar-neutral kernel) for
JSON+CSS; admitted direct/fact/transient planes; but the union is not yet ONE
typed cursor + ONE `TapeEmit`/`DirectBuild` schedule, and the LOCKS Lock-14
generality-vehicle text contradicts the certified phantom DELETE.

## LOCKS-Amendment Candidate (1E — candidate only; disposition T-P3, ratification Pass Omega)

| id | clause path:line | candidate amendment | loc_delta | wave hint | grounding |
|---|---|---|---:|---|---|
| 1A-LOCK1-AMEND-001 | Lock 14 ValueRef/classifier-generalisation clause, `restart/locks/LOCKS.md:620` (companion §9.2 prose `restart/ARCHITECTURE.md:1990`,`:1997`) | Strike "The `G:EventGrammar` type parameter is the generality vehicle." Re-anchor the generality claim on the TWO axes the clause itself already names: (a) the shared value-API `Cursor` micro-trait (R-D G4b, ≥2 non-collapsible impls), and (b) the config-breadth classifier (alphabet-as-data across 8/9 grammars). The `@generated` per-grammar emission remains the grammar-neutrality guarantor; `preserve-rich-ast` and the K-axis preservation remain verbatim. | ≈ −1..+5 LOC (a one-clause LOCKS strike + re-anchor at `LOCKS.md:620`, plus the companion §9.2 prose carrier edit at `ARCHITECTURE.md:1990`,`:1997`; the `1A-SUB-025` DIVERGES row that feeds it carries no own loc_delta — the edit surface is the clause-and-prose reconcile only) | SK-V19 LOCKS reconcile / Pass Omega (1-line LOCKS reconcile; sibling 1F COH18-008 "1 line LOCKS reconcile (SK-V19)"; cross-links the 1E sibling amendment so both surfaces share one priced disposition — CH4-V3-007) | The certified SK-V18 plan DELETES `<G>` (`sk-v18/SPEC.md:1202-1207`; addendum 4/R5 `audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:53`); the on-disk non-test instantiation census is EMPTY (1A-SUB-023); the clause's own "separate axis" sentence already isolates the config-breadth classifier from the value-fold, so the generality claim survives the DELETE intact once re-anchored. No lock count change; no shape/directive/substrate change. |

## Open Questions

| id | UNKNOWN | why unknown | verify_action |
|---|---|---|---|
| 1A-UNK-001 | Does any generated/codegen direct path consume a shared event cursor rather than raw bytes? | Runtime direct uses `bytes`+local `cursor` (`generated.rs:766`); codegen `DirectParser.cursor` is a third carrier (the `cursor: usize` field at `json_typed_direct.rs:671`, struct at `:668`; instantiated at `:56`; rollback `checkpoint = parser.cursor` at `:361` — re-anchored per CH1-V3-F10). | `rg -n "EventCursor|DirectBuild|TapeEmit|parse_direct|cursor: &mut usize|DirectParser" skinny/crates/runtime/src skinny/crates/codegen/src skinny/crates/ir/src` and bind every direct consumer to shared scheduling or record cursor drift; feed §9.2 Ω-A. |
| 1A-UNK-002 | Is bounded rollback implemented under another name? | Assembler shows append + ordered flag patch only (`assembler.rs:62`,`:94`); no checkpoint API cited. | `rg -n "checkpoint|rollback|mark|restore|commit|snapshot" skinny/crates/runtime/src/tape skinny/crates/runtime/src/grammars` and classify hits as runtime substrate / parser-local / absent. |
| 1A-UNK-003 | Which of the 48 grammar files are rostered-generator output vs `@generated`-comment-only? | Generated headers exist (`parser.rs:1`) alongside hand-owned files (`scan.rs:1`,`sink.rs:1`,`event_grammar_witness.rs:1`). | Compare `xtask`/codegen regen manifest (Lock 6 v+1 round-trip, `restart/locks/LOCKS.md:185`) to the file list under `skinny/crates/runtime/src/grammars`; bucket generated / hand-owned-witness / hand-owned-production. |
| 1A-UNK-004 | Is the CSS fact-stream substrate-target config row meant to be absent, or was it dropped in error? | CSS config.rs at HEAD has no W7 row; JSON retains the full triad (`json/config.rs:22`). | Inspect the CSS generated config template; either emit `substrate_target = admitted_fact_output` (mirror JSON) or cite the consumer that derives it elsewhere (e.g. `Lock1PolicyTriad::fact_stream`). |
| 1A-UNK-005 | Are `EventTape` and `CollapsedStage` target-only by SK-V18 design, or expected live runtime shapes? | Enum rows exist (`ir/src/lib.rs:343`,`:345`) with no runtime carrier; CollapsedStage is aarch64-barred (Lock 10 clause `restart/locks/LOCKS.md:614`). | T-P2/T-P3 choose: keep target-only with explicit wave owner, or amend substrate/lowering spec to current skinny scope; CollapsedStage stays G-Omega-gated. |
