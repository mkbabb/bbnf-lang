---
agent: 1C
pass: T-P1-excavation
cycle: V1
generated_at: 2026-05-21T05:56:41Z
spec_surfaces_audited: [PASS-1-EXCAVATION.md, ARCHITECTURE.md, LOCKS.md]
files_audited_count: 27
live_truth_method: "nl -ba + find + wc -l + rg over skinny/crates/runtime/src; focused cargo test -p runtime"
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: []
  first_cycle_additions: [1C-runtime-census, 1C-lock14-leak-audit, 1C-shape-consumption-audit]
divergence_count:
  spec_claims_implemented: 5
  spec_claims_unimplemented: 5
  impl_exceeds_spec: 2
  unknown: 3
locks_amendment_candidates: 0
---

# Executive Summary

Runtime is a partial skinny prototype, not the V1 runtime described by the architecture. The live crate has 24 Rust files and 3 grammar directories under `skinny/crates/runtime/src/grammars`: `json`, `css_l4_declaration_values`, and `sheets_witness`. JSON is the only retained document parser and it implements an offset-tape shape: `ParserState` owns `TapeBuilder`, generated parsing emits offsets, and `JsonRoot` projects views from `Tape`. JSON also has generated `SinkOnly` direct consumption in `generated.rs`, but its receiver trait is explicitly hand-written JSON source. No live runtime `EventTape` consumer or compact event-cell storage exists; only proof-only event-grammar witnesses exist. Lock 14 is materially violated at the runtime root by grammar-named module aliases and by hand-written per-grammar JSON/CSS files. The focused runtime test suite passes: `cargo test -p runtime` in `skinny/` ran 11 tests, all passing.

# Runtime Module Census

| Grammar/module | Files | LOC | Generated? | Runtime shape evidence | Risk/LOC estimate |
|---|---:|---:|---|---|---|
| `json` | 10 | 2,096 | 8 generated markers; `scan.rs` and `sink.rs` explicitly non-generated | OffsetTape retained parser in `skinny/crates/runtime/src/grammars/json/parser.rs:7-12`, `skinny/crates/runtime/src/grammars/json/generated.rs:18-24`, `skinny/crates/runtime/src/grammars/json/generated.rs:290-303`; SinkOnly direct parser in `skinny/crates/runtime/src/grammars/json/generated.rs:393-407` | High: 400-800 LOC to make sink/scan generator-owned and metadata-derived |
| `css_l4_declaration_values` | 5 | 415 | 4 generated markers; `sink.rs` lacks marker | Direct fact-stream scanner in `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:4-6`, `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:25-35`; no tape/view/value module | Medium: 150-300 LOC to classify as formal SinkOnly/fact-plane artifact or replace with generated runtime schema |
| `sheets_witness` | 2 | 25 | no generated marker | Proof-only `EventGrammar` witness in `skinny/crates/runtime/src/grammars/sheets_witness/event_grammar_witness.rs:16-23`; no parser | Low/medium: 80-200 LOC to move witness generation behind codegen/proof fixtures |
| Generic `tape` | 5 | 532 | hand-written generic substrate | Offset vector tape in `skinny/crates/runtime/src/tape/mod.rs:94-100`, `skinny/crates/runtime/src/tape/mod.rs:126-150`; generic typed cursor marker in `skinny/crates/runtime/src/tape/mod.rs:175-180` | Medium: 250-500 LOC to add true EventTape cells and generated consumers |
| Runtime root/tests | 1 | 284 | hand-written | Grammar-named root modules in `skinny/crates/runtime/src/lib.rs:3-19`; JSON/CSS tests in `skinny/crates/runtime/src/lib.rs:24-283` | Medium: 120-250 LOC to replace hardcoded root exports with metadata manifest/test fixture generation |

# Spec-Claim ↔ Implementation Table

| Spec claim | Implementation evidence | Verdict | Note |
|---|---|---|---|
| Grammar-specific surfaces are generated under `runtime/src/grammars/<name>/`, not as `bbnf` sibling dirs (`restart/ARCHITECTURE.md:410-419`). | Live grammar directories are `skinny/crates/runtime/src/grammars/css_l4_declaration_values`, `skinny/crates/runtime/src/grammars/json`, and `skinny/crates/runtime/src/grammars/sheets_witness`; root reexports them from `skinny/crates/runtime/src/lib.rs:17-19`. | Partly implemented | Directory shape exists, but the root still hardcodes grammar names. |
| Runtime public surface is tape/direct document APIs and generated-code builders (`restart/ARCHITECTURE.md:337`). | `skinny/crates/runtime/src/lib.rs:1` exports `tape`; `skinny/crates/runtime/src/tape/mod.rs:10-12` exports `CapacityPlan`, `TapeBuilder`, `EventGrammar`, `OffsetTapeStats`. | Implemented | Generic substrate APIs exist. |
| `OffsetTape` lowers as an `EventCursor` over retained structural offsets (`restart/ARCHITECTURE.md:1104-1106`). | JSON parser state owns `TapeBuilder` (`skinny/crates/runtime/src/grammars/json/parser.rs:7-12`), retained parse calls generated parser then `finish()` (`skinny/crates/runtime/src/grammars/json/parser.rs:47-51`), generated code emits offsets (`skinny/crates/runtime/src/grammars/json/generated.rs:203-207`, `skinny/crates/runtime/src/grammars/json/generated.rs:290-303`, `skinny/crates/runtime/src/grammars/json/generated.rs:331-333`), and views read `tape.offset_at` (`skinny/crates/runtime/src/grammars/json/view.rs:384-395`). | Partly implemented | Offset retention is real; no `EventCursor` type name exists in runtime. |
| `SinkOnly` lowers to direct typed-field writes with no retained queryable document (`restart/ARCHITECTURE.md:1106-1108`). | Generated JSON direct parser is labelled SinkOnly (`skinny/crates/runtime/src/grammars/json/generated.rs:393`) and calls sink methods directly (`skinny/crates/runtime/src/grammars/json/generated.rs:407-421`, `skinny/crates/runtime/src/grammars/json/generated.rs:546-574`, `skinny/crates/runtime/src/grammars/json/generated.rs:580-602`, `skinny/crates/runtime/src/grammars/json/generated.rs:688-713`). | Implemented | Direct parser emits to `JsonSink`; no `TapeBuilder` in the direct entry. |
| `EventTape` lowers through compact event cells with payload/recovery/layout facts (`restart/ARCHITECTURE.md:1106-1107`). | Runtime `Tape` stores `offsets`, `flag_cursors`, `flag_values`, and `payloads` (`skinny/crates/runtime/src/tape/mod.rs:94-100`); `rg EventCursor/EventCell/push_event` found no runtime consumer; event witnesses only implement admissibility (`skinny/crates/runtime/src/grammars/json/event_grammar_witness.rs:17-24`, `skinny/crates/runtime/src/grammars/sheets_witness/event_grammar_witness.rs:16-23`). | Unimplemented | Event grammar witnesses are proof scaffolding, not EventTape consumption. |
| All nine extant grammars have empty declaration-crate exception table (`restart/ARCHITECTURE.md:764-767`). | Runtime has only three grammar dirs, and only JSON/CSS have parser surfaces; `skinny/crates/runtime/src/grammars/sheets_witness/mod.rs:1` exposes only a witness module. | Unimplemented | V1 nine-grammar matrix is absent from live runtime. |
| Metadata may not name Rust parser types or generated modules (`restart/ARCHITECTURE.md:736-745`). | Runtime root names generated modules explicitly: `generated_json`, `generated_css_l4_declaration_values`, `json_event_grammar_witness`, `sheets_witness` (`skinny/crates/runtime/src/lib.rs:3-19`). | Diverged | Runtime root is acting as a hardcoded manifest. |
| Lock 14 forbids hand-written per-grammar runtime files (`restart/locks/LOCKS.md:78`). | `skinny/crates/runtime/src/grammars/json/sink.rs:1` says "JSON-owned direct sink source; not part of the generated JSON roster"; `skinny/crates/runtime/src/grammars/json/scan.rs:1` says the same for structural scan; `skinny/crates/runtime/src/grammars/css_l4_declaration_values/sink.rs:1-2` lacks a generated marker and defines grammar-specific sink/error code. | Diverged | Hand-written/generator split violates the lock as written. |
| Lock 14 verification expects zero grammar-named modules in generic crates (`restart/locks/LOCKS.md:78`). | `skinny/crates/runtime/src/lib.rs:3-19` declares grammar-named modules and aliases in the generic runtime root. | Diverged | Per-grammar generated subdirs are allowed by architecture, but root hardcoding is not. |
| Runtime must reject invalid UTF-8 at parse/scan boundary (`restart/ARCHITECTURE.md:1178-1204`). | JSON `parse_bytes` converts with `std::str::from_utf8` and returns `ParseErrorKind::InvalidUtf8` on failure (`skinny/crates/runtime/src/grammars/json/parser.rs:54-67`); runtime test covers invalid UTF-8 (`skinny/crates/runtime/src/lib.rs:66-80`). | Implemented | This is runtime-layer evidence only for JSON. |
| `SimdScan` transient mask stream must not become a second substrate (`restart/ARCHITECTURE.md:1025-1031`). | JSON generated `attach_structural_index` is a no-op (`skinny/crates/runtime/src/grammars/json/generated.rs:10-15`); retained substrate is the offset vector (`skinny/crates/runtime/src/tape/mod.rs:94-100`). | Implemented | No retained mask sidecar found in runtime. |
| Backend trait artifact set lists generated `{generated.rs, parser.rs, host.rs, view.rs, value.rs, visitor.rs}` for every grammar (`restart/ARCHITECTURE.md:1247-1250`). | JSON has that roster plus hand files; CSS has only `config/generated/mod/parser/sink`; Sheets witness has no parser/view/value/visitor. | Unimplemented | Artifact set is JSON-only and not wholly generated. |

# Generated SinkOnly / OffsetTape / EventTape Consumption

| Shape | Live consumer | Evidence | Divergence count impact |
|---|---|---|---|
| `OffsetTape` | JSON retained parser and view layer | `ParserState` owns `TapeBuilder` (`skinny/crates/runtime/src/grammars/json/parser.rs:7-12`); `parse` runs generated parser then returns `JsonRoot::from_tape` (`skinny/crates/runtime/src/grammars/json/parser.rs:47-51`); offsets are pushed by generated code (`skinny/crates/runtime/src/grammars/json/generated.rs:203-207`, `skinny/crates/runtime/src/grammars/json/generated.rs:290-303`); `JsonNodeKind::at_cursor` reads `tape.source()[offset]` (`skinny/crates/runtime/src/grammars/json/value.rs:28-45`). | Partly implemented |
| `SinkOnly` | JSON direct parser only | `parse_direct` takes `&mut S: JsonSink` (`skinny/crates/runtime/src/grammars/json/generated.rs:407-421`); object/array/string/number paths call sink hooks directly (`skinny/crates/runtime/src/grammars/json/generated.rs:546-574`, `skinny/crates/runtime/src/grammars/json/generated.rs:580-602`, `skinny/crates/runtime/src/grammars/json/generated.rs:608-638`, `skinny/crates/runtime/src/grammars/json/generated.rs:688-774`). | Implemented with hand-written sink dependency |
| `EventTape` | None in runtime | Generic `EventGrammar` has only admissibility functions (`skinny/crates/runtime/src/tape/event_grammar.rs:4-14`); JSON/Sheets witnesses only implement fact IDs (`skinny/crates/runtime/src/grammars/json/event_grammar_witness.rs:10-24`, `skinny/crates/runtime/src/grammars/sheets_witness/event_grammar_witness.rs:10-23`); `Tape` lacks event-cell vector (`skinny/crates/runtime/src/tape/mod.rs:94-100`). | Unimplemented |
| `CollapsedStage` | None in runtime | Runtime `rg` found no `CollapsedStage`; architecture itself places the x86 asm file under `bbnf-simd`, not runtime (`restart/ARCHITECTURE.md:1107-1108`). | Unknown for runtime |

# Lock 14 Grammar-Name Leak Audit

| Leak class | Evidence | Verdict | LOC/risk estimate |
|---|---|---|---|
| Root grammar-named modules in generic runtime | `skinny/crates/runtime/src/lib.rs:3-7` declares `generated_json` and `generated_css_l4_declaration_values`; `skinny/crates/runtime/src/lib.rs:17-19` reexports them as `grammars::{css_l4_declaration_values,json}`. | Divergence | 120-250 LOC, medium risk: replace hardcoded root aliases with generated manifest or metadata registration. |
| Proof-only grammar witnesses in root | `skinny/crates/runtime/src/lib.rs:9-15` exposes `json_event_grammar_witness` and `sheets_witness` behind test/proof cfg. | Divergence | 50-120 LOC, low/medium risk: move to generated proof fixtures or grammar dirs not root aliases. |
| Hand-written JSON per-grammar files | `skinny/crates/runtime/src/grammars/json/sink.rs:1` and `skinny/crates/runtime/src/grammars/json/scan.rs:1` explicitly state they are JSON-owned and not generated. | Divergence | 400-800 LOC, high risk: sink/scan semantics affect performance and correctness. |
| Hand-written CSS per-grammar sink | `skinny/crates/runtime/src/grammars/css_l4_declaration_values/sink.rs:4-18` defines `CssFactError`; `skinny/crates/runtime/src/grammars/css_l4_declaration_values/sink.rs:18-99` defines `FactSink`; file lacks generated marker. | Divergence | 100-200 LOC, medium risk: fact output hash/schema behavior must be preserved. |
| Generated per-grammar type names inside generated files | `skinny/crates/runtime/src/grammars/json/value.rs:12-24` defines `JsonNodeKind`; `skinny/crates/runtime/src/grammars/json/view.rs:12-30` defines `JsonRoot`; `skinny/crates/runtime/src/grammars/css_l4_declaration_values/config.rs:2-4` defines CSS row/schema constants. | Allowed generated surface | 0 LOC: allowed if generated from metadata/source. |

# Divergences Catalogued

| ID | Divergence | Evidence | LOC estimate | Risk |
|---|---|---|---:|---|
| 1C-D1 | Runtime root hardcodes grammar names instead of metadata-derived manifest. | `restart/locks/LOCKS.md:78`; `skinny/crates/runtime/src/lib.rs:3-19`. | 120-250 | Medium |
| 1C-D2 | Hand-written per-grammar runtime files remain. | `restart/locks/LOCKS.md:78`; `skinny/crates/runtime/src/grammars/json/sink.rs:1`, `skinny/crates/runtime/src/grammars/json/scan.rs:1`, `skinny/crates/runtime/src/grammars/css_l4_declaration_values/sink.rs:1-18`. | 500-1,000 | High |
| 1C-D3 | EventTape consumption is absent. | Spec `restart/ARCHITECTURE.md:1104-1108`; live `skinny/crates/runtime/src/tape/mod.rs:94-100`, `skinny/crates/runtime/src/tape/event_grammar.rs:4-14`. | 250-500 | Medium |
| 1C-D4 | V1 nine-grammar runtime matrix is not present. | Spec names nine extant grammars at `restart/ARCHITECTURE.md:764-767`; live dirs are 3. | 1,500-4,000 | High |
| 1C-D5 | CSS declaration-values module is a fact-stream implementation, not the claimed runtime artifact set. | Artifact-set claim `restart/ARCHITECTURE.md:1247-1250`; live CSS `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:4-6`, `skinny/crates/runtime/src/grammars/css_l4_declaration_values/sink.rs:18-99`. | 150-300 | Medium |
| 1C-D6 | OffsetTape implementation lacks the named `EventCursor` abstraction. | Spec `restart/ARCHITECTURE.md:1104-1106`; live cursor is `ValueRef` over offset cursor (`skinny/crates/runtime/src/tape/mod.rs:175-222`). | 100-250 | Low/medium |

# Gaps / Missing Primitives

| Gap | Evidence | Verify action |
|---|---|---|
| No compact event-cell storage API. | `Tape` fields are source, offsets, flag vectors, payload arena, id (`skinny/crates/runtime/src/tape/mod.rs:94-100`). | Add or locate generated EventTape runtime and run `rg -n "EventCell|push_event|event_cells|EventCursor" skinny/crates/runtime/src`. |
| No metadata-derived runtime grammar manifest. | Root modules are hardcoded with `#[path = "..."]` (`skinny/crates/runtime/src/lib.rs:3-15`). | Verify planned manifest owner in pipeline/codegen; runtime should consume generated manifest or metadata output, not hand-coded aliases. |
| No generated sink trait for JSON direct path. | `skinny/crates/runtime/src/grammars/json/sink.rs:1` says hand-owned; generated direct parser imports it (`skinny/crates/runtime/src/grammars/json/generated.rs:395`). | Regenerate from BIR/DirectFieldFacts and compare generated trait/receiver hooks to current tests. |
| CSS fact stream is not mapped to a formal `BackendShape`. | CSS scanner returns `String` fact stream (`skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:4-6`) and `FactSink` emits rows (`skinny/crates/runtime/src/grammars/css_l4_declaration_values/sink.rs:24-99`). | Determine whether this is SinkOnly, EventTape, or bench-only product plane; require codegen evidence. |

# Open Questions

| UNKNOWN | Why unknown | verify_action |
|---|---|---|
| 1C-U1: Is CSS declaration-values intended to be production runtime or bench/proof artifact? | It lives under runtime grammar dirs but emits a fact stream, not document/tape/direct typed values. | Inspect the generating codegen path and skinny RESULTS row that consumes `css_l4_declaration_value_fact_stream`; classify before synthesis. |
| 1C-U2: Is `EventGrammar` witness scaffolding a replacement for `EventTape`, or only compile-time proof? | Witnesses compile and tests pass, but no event-cell tape or parser consumer exists. | Require a generated parser that writes/reads payload event cells, then cite path:line and rerun `cargo test -p runtime`. |
| 1C-U3: Does architecture intentionally allow root-level generated grammar aliases despite Lock 14's "zero grammar-named modules" wording? | §4.3 allows per-grammar generated modules under `runtime/src/grammars/<name>/`, while Lock 14 forbids grammar-named modules in generic crates. | T-P3 should resolve wording: allow generated subdir names only, or require metadata manifest names outside generic root source. |

# Verification

`cargo test -p runtime` from `/Users/mkbabb/Programming/bbnf-lang/skinny` passed 11 runtime tests, including JSON retained parse/view, lazy offset tape, generated direct parser sink dispatch, CSS fact stream, and event-grammar proof tests.
