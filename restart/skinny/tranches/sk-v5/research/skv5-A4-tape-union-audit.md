# SK-V5 A4 — Tape ≡ Structural-Projection Union Audit

Authority anchors:

- `restart/locks/14-LOCKS.md:34` (Lock 1 verbatim) — tape is the substrate, properly unioned with direct-to-struct; columnar SoA is dead; orthogonal codepaths and parallel substrates are dead.
- `restart/ARCHITECTURE.md:1020-1095` (§7.3) — `BackendShape ∈ { EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage }` and the 8-priority cost-model derivation.
- `restart/skinny/audit/ASMJSON-DAV1D-GRAND-SYNTHESIS-SK-V4.md:148-152` — eventcursor sidecar prototype refutation: "event cursor must be the lowering boundary, not a parallel prepass".
- `restart/skinny/audit/ASMJSON-DAV1D-GRAND-SYNTHESIS-SK-V4.md:256-287` (§6) — substrate boundary single arrow + the five-item implementation debt list.
- `restart/skinny/SUBSTRATE.md` (skv-V1 canonical) — `OffsetTape` is the implemented shape; the other four are cost-model-derived sibling values of the same union.
- `restart/MIGRATION.md:344-349` — `OpenFrame/Vec<OpenFrame>::clone` 86.07 % pathology was the parallel-substrate failure mode under Lock 1.

The audit reports against the **skinny/** workspace, the V1-validation locus. Legacy `/crates/` (pre-restart 2026-05-04) is non-production per Lock 12 (archive ceremony) and is examined only for residue cleanup status.

---

## 1. Lowering-Shape Consumer Census

For each of the five `BackendShape` values:

| Shape | In source tree? | Consumer site (file:line) | Verdict |
|---|---|---|---|
| `EagerTape` | partly — `generated.rs` is the canonical lowering, **but** it is source-byte recursive descent that ALSO writes structural offsets into `TapeBuilder`. It is closer to "OffsetTape compiled as if EagerTape" than to a recovery-fallback EagerTape. | `skinny/crates/runtime/src/grammars/json/generated.rs:34-50` (`parse_value_at` dispatches on `state.bytes[state.cursor]` — eager source-byte read), emits offsets via `state.emit_plain_offset(...)` (e.g. line 169 number, line 189 literal, line 221 quote, line 254 structural). The same body writes the offset tape. The shape selector is NOT cost-model-derived; the codegen template at `skinny/crates/codegen/src/lib.rs:115-118` `include_str!`s a single template regardless of grammar. | **PRESENT, BUT WITHOUT DERIVATION**. The shape has a consumer; the cost-model gate that should pick it is missing. |
| `OffsetTape` | YES — the storage layer (`Tape::offsets`, `Tape::flag_cursors`, `Tape::flag_values` at `skinny/crates/runtime/src/tape/mod.rs:92-99`) IS the OffsetTape projection; the `ValueRef.cursor: u32` indexes into it (`skinny/crates/runtime/src/tape/mod.rs:173-219`); the typed view at `skinny/crates/runtime/src/grammars/json/view.rs:41-48` (`offset_stream`, `token_stream`) consumes it. | `skinny/crates/runtime/src/grammars/json/parser.rs:38-41` (`finish()` seals the builder into `Tape<'i>`); `view.rs:25-27` (`value()` projects via `ValueRef`). | **HOLDS as committed storage shape**. Consumer present at view layer. |
| `EventTape` | **SPEC-ONLY**. No `EventTape` lowering in source. `Tape` carries only offsets + sparse flags + (empty) payload arena; there is no event-cell record carrying recovery/layout/payload side facts. | Absent from `skinny/crates/runtime/src/tape/`. Lookup `grep -rn "EventTape" --include="*.rs" skinny/crates/` returns zero. | **SUBSTRATE-WITHOUT-CONSUMER risk inverted**: the architecture spec admits the shape, the source tree carries the storage primitives needed (offsets + sparse flag-cursor side facts can extend to event cells), and Lock 1 says the projection variant is the same union member. Not a Lock 1 violation; an unfilled cell. |
| `SinkOnly` | **NOT GENERATED**. The runtime emits offset writes for every parsed token; there is no `DirectBuild → field-write` lowering path in the codegen template. The bench-side `direct_struct::sink_only_digest` at `skinny/crates/bbnf-bench/src/direct_struct.rs:190-353` is a private hand-coded parser that does NOT go through the substrate at all. | The SinkOnly intent is realised only inside `bbnf-bench`. Per SK-V4 §6 implementation debt item 1 (`restart/skinny/audit/ASMJSON-DAV1D-GRAND-SYNTHESIS-SK-V4.md:279-280`): "Track 1 direct must move out of `bbnf-bench` into generated runtime/codegen `SinkOnly`." | **PRESENT BUT WRONG-LOCUS**. `direct_struct.rs` is bench-private and per SK-V4 disqualifies the row. The intended generated SinkOnly path would land at `skinny/crates/codegen/src/json_templates/` as a per-rule emitter when `LayoutFacts.backend_shape = SinkOnly`; that emitter does not exist. |
| `CollapsedStage` | ABSENT (intentional). No `bbnf.asm`/`<grammar>_collapsed.asm` file under `skinny/crates/bbnf-simd/`; no `parse_value` Rust shim that prepares EOB-pad-clamped buffers and jumps into hand-written NASM. | Per ARCH §7.3 line 1156 (`BBNF-COLLAPSEDSTAGE-NOT-VIABLE` diagnostic) the cost-model falls back to OffsetTape when the kernel author + silicon + parity harness are not all present. SK-V4 §6 implementation debt item 5: "x86 `CollapsedStage` must remain separate until NASM author, silicon, and checkasm are all present." | **CORRECTLY ABSENT**. Absence is gated on prerequisites; not a Lock 1 violation. |

**Lock 1 verdict on consumer presence**: 2 of 5 shapes have concrete consumers (OffsetTape, "EagerTape as fused offset writer"); SinkOnly intent is misplaced bench-side; EventTape and CollapsedStage are correctly deferred. The union is structurally admitted by `Tape<'input>` storage shape; not all variants are wired.

---

## 2. Cost-Model Backend-Shape Derivation — MISSING IN SKINNY

ARCH §7.3 lines 1075-1083 mandate `passes::recognizers::derive_backend_shape(grammar_ir, rule_id) -> BackendShape`. Empirical state of the skinny:

- `skinny/crates/passes/` exists in workspace (`skinny/Cargo.toml:7`).
- Grep `derive_backend_shape` across `skinny/crates/`: zero hits.
- Grep `BackendShape` across `skinny/crates/`: zero hits.
- The codegen `include_str!` at `skinny/crates/codegen/src/lib.rs:115-118` emits a single template — the per-rule `backend_shape` switch is not exercised. Every rule in `json.bbnf` resolves to the same generated body shape.

**Verdict**: the union enumeration exists in spec only. The compile-time decision driver that turns the union into a per-rule selection is unwired. This is the same "substrate-without-consumer" symptom Lock 1's V9.5 amendment named, applied to the *decision substrate* rather than the *storage substrate*. Honored at the storage layer; absent at the dispatch layer.

---

## 3. Parallel-Substrate Residue Audit

### 3.1 `simd-scan` legacy crate — DEAD CODE, NOT IN WORKSPACE

`skinny/crates/simd-scan/` ships its own `StructuralIndex` (`src/lib.rs:46`), `JsonParseIndex` (line 77), `scan_json_structurals` (line 112), `scan_json_parse_index` (line 125) — a complete duplicate of the `bbnf-simd` substrate.

It is **not** in `skinny/Cargo.toml` workspace members (which lists `crates/bbnf-simd` at line 11, not `simd-scan`); no Cargo.toml in any tracked crate depends on it (`grep -rn "simd-scan\|simd_scan" skinny/crates/*/Cargo.toml`: the `bbnf-bench/Cargo.toml:34` hit is `[[bench]] name = "simd_scan"` — the criterion bench filename, not a crate ref; `simd-scan/Cargo.toml:2` is its self-name).

Per `restart/MIGRATION.md:75`, the migration target was `crates/simd-scan → crates/bbnf-simd` (KEEP-MODIFY/RENAME); the old crate was not deleted. This is the second-substrate residue Lock 1 names by symptom — a complete parallel scanner crate sitting in the tree.

**Verdict — NUKE**. Delete `skinny/crates/simd-scan/` outright.

### 3.2 `generated_eventcursor.rs` — REFUTED PREPASS, STILL WIRED BEHIND FEATURE FLAG

Per SK-V4 §4 (`restart/skinny/audit/ASMJSON-DAV1D-GRAND-SYNTHESIS-SK-V4.md:148-152`): "Eventcursor sidecar prototype. **Invalidated**. A mask/LUT producer bolted in front of unchanged `parse_value_at` regressed and grew the hot hub. Event cursor must be the lowering boundary, not a parallel prepass."

The file at `skinny/crates/runtime/src/grammars/json/generated_eventcursor.rs:62-63` does exactly what was refuted:

```rust
pub(crate) fn attach_structural_index(state: &mut ParserState<'_>) {
    state.event_cursor = Some(crate::tape::ParseIndexCursor::new(state.bytes));
}
```

`ParseIndexCursor::new` (`skinny/crates/runtime/src/tape/mod.rs:244-256`) runs `scan_json_parse_index` on the **full input** upfront and stores the result; the body of `parse_value_at` (eventcursor variant, `generated_eventcursor.rs:82-100`) still walks `state.bytes[state.cursor]` through a value-class LUT. This is a parallel prepass with a sidecar — exactly the refuted shape, exactly the V9.5 PSI failure mode signature (substrate-without-lowering-boundary).

The file is wired:
- `skinny/crates/runtime/src/grammars/json/mod.rs:4-5` — `#[cfg(feature = "eventcursor")] pub mod generated_eventcursor;`
- `skinny/crates/runtime/src/grammars/json/parser.rs:63-69` — `#[cfg(feature = "eventcursor")] pub fn parse<'i>(...)` calls `generated_eventcursor::attach_structural_index` + `generated_eventcursor::parse_json`.
- `skinny/crates/runtime/src/grammars/json/parser.rs:18-19` — `#[cfg(feature = "eventcursor")] pub event_cursor: Option<ParseIndexCursor>` on `ParserState`.
- `skinny/crates/codegen/src/lib.rs:81-84` — the template `mod.rs` emits the same feature switch; `json_templates/parser.rs:67-68` and `json_templates/generated_eventcursor.rs` are kept in codegen.
- `skinny/crates/runtime/examples/wave2_bench.rs:9-32` — a bench example invokes the feature.

Feature-flagging does not redeem the architecture. Lock 1 names parallel substrates as faults regardless of activation gate; a sidecar that the user can opt into is still a sidecar that occupies source-tree real estate and template emission.

**Verdict — NUKE**. The path to the refuted shape must close:

| File | Action |
|---|---|
| `skinny/crates/runtime/src/grammars/json/generated_eventcursor.rs` | DELETE entire file. |
| `skinny/crates/codegen/src/json_templates/generated_eventcursor.rs` | DELETE template. |
| `skinny/crates/runtime/src/grammars/json/mod.rs:2-5` | DELETE both `#[cfg]` lines; emit `pub mod generated;` unconditionally. |
| `skinny/crates/runtime/src/grammars/json/parser.rs:2-10,18-19,32-34,54-70` | DELETE the eventcursor branch; collapse to one `pub fn parse`; delete `event_cursor` field + `ParseIndexCursor` import. |
| `skinny/crates/codegen/src/json_templates/parser.rs` (lines 2-10,18-19,32-34,54-70) | DELETE the same branches in the template. |
| `skinny/crates/codegen/src/lib.rs:81-84` | DELETE the conditional emission of `generated_eventcursor`. |
| `skinny/crates/runtime/Cargo.toml` (and any other crate Cargo.toml) | DELETE `[features] eventcursor = ...`. |
| `skinny/crates/runtime/examples/wave2_bench.rs` | DELETE the eventcursor branch; or DELETE the entire example if it was authored only as an eventcursor probe. |

The replacement is the **lowering-boundary EventCursor** per SK-V4 §6 line 264: `bytes -> scan/mask producer -> typed event cursor -> { shape }`. That cursor lives *inside* the generated recursive-descent body, called per-consume-site against the offset stream; it is not a parallel pre-prepared structure. The codegen template for OffsetTape lowering at `skinny/crates/codegen/src/json_templates/generated.rs` is where the cursor consumption should land — replacing source-byte reads in `parse_value_at` (e.g. line 39 `*state.bytes.get_unchecked(state.cursor)`) with `state.tape.next_event()` style consumption.

### 3.3 `direct_struct.rs` — BENCH-PRIVATE PARSER, NOT A SUBSTRATE BYPASS BUT WRONG LOCUS

`skinny/crates/bbnf-bench/src/direct_struct.rs:190-353` carries a private `SinkParser<'a>` that walks source bytes directly using `parse_that_regex` primitives (`match_json_string_at_quote`, `match_json_number_from_first`, `skip_json_whitespace`, `unescape_json_string`). It never touches `Tape`, `TapeBuilder`, or `ValueRef`. It folds digests during traversal.

The `track1_digest`/`track2_digest` entry points at lines 150-156 BOTH route through `sink_only_digest`, meaning the "two-track" parity check at lines 166-188 collapses both tracks into the same private parser. Only `track1_view_walk_digest`/`track2_view_walk_digest` (lines 138-148) actually use the substrate: line 139 calls `runtime::generated_json::parse`, line 145 calls `crate::track2::json::parse`. Those routes are admissible substrate consumers.

The private `SinkParser` is the SK-V4 §6 item 1 disqualification (`restart/skinny/audit/ASMJSON-DAV1D-GRAND-SYNTHESIS-SK-V4.md:279-280`): "Track 1 direct must move out of `bbnf-bench` into generated runtime/codegen `SinkOnly`." Bench-private = disqualified Track 1.

It is **not** Lock 1 parallel-substrate (it doesn't coexist with a sealed tape mid-parse); it is **wrong-locus** per Lock 14 (grammar-specific parser in a generic bench crate) and SK-V4 §6 item 1.

**Verdict — MOVE / MOVE-AND-NARROW**:

| File | Action |
|---|---|
| `skinny/crates/bbnf-bench/src/direct_struct.rs:190-353` (`sink_only_digest` + `SinkParser`) | MOVE the SinkParser shape into `skinny/crates/codegen/src/json_templates/` as the `SinkOnly` lowering template, gated on `LayoutFacts.backend_shape = SinkOnly`. Delete the bench-private copy. The digest folding (`JsonDirectDigest` arithmetic) stays bench-side; the parse loop generates from codegen. |
| `skinny/crates/bbnf-bench/src/direct_struct.rs:150-156` (`track1_digest`/`track2_digest`) | RE-WIRE through the new generated SinkOnly parse, not the private `sink_only_digest`. |
| `skinny/crates/bbnf-bench/src/bin/profile_direct.rs` (untracked file, profile probe over the private parser) | EITHER delete (transient profiling artifact) OR rewrite against the generated SinkOnly path once it lands. |

The digest-folding traversal (`JsonDirectDigest::fold_child`, `serde_number_digest`, `parse_integer_digest` at lines 501-545) is grammar-neutral arithmetic and stays bench-side; only the parse driver moves.

### 3.4 `StructuralIndex` / `JsonParseIndex` — NOT A SIDECAR-SUBSTRATE WHEN CONSUMED RIGHT

`StructuralIndex` is the scan output (the SIMD producer side of SK-V4 §6's arrow `bytes -> scan/mask producer`). The current canonical path at `skinny/crates/runtime/src/grammars/json/generated.rs:13-16` declares the scan but the body is a no-op — `attach_structural_index` does nothing in the non-eventcursor variant. Source bytes are still re-scanned per-consume-site (`generated.rs:39` `*state.bytes.get_unchecked(state.cursor)`). Per `restart/skinny/SUBSTRATE.md:219`: "`generated::attach_structural_index` is still a no-op and generated parse functions still walk source bytes through `cursor`, `skip_ws`, and `parse_value_at`."

This is not parallel-substrate; it is missing-consumer of the scan output. The scan runs, its product (StructuralIndex / JsonParseIndex) is never consulted along the canonical path. The eventcursor variant exists precisely to consume it, but as a refuted prepass.

**Verdict — WIRE**. The OffsetTape lowering body must consume scan output as an inline cursor over `Tape::offsets`, not via a separate `ParseIndexCursor` sidecar. The substrate primitive is `Tape::offset_at(cursor)` at `skinny/crates/runtime/src/tape/mod.rs:136-140`; the consumer pattern is "next-structural at-or-after current source position via tape cursor", inlined into each `consume_*` site in `generated.rs:200-289`.

### 3.5 Columnar SoA — DELETED CLEAN

Grep `columnar\|SoA` in `skinny/crates/` returns zero hits in source. ARCH §7.3 line 1045 cites the column union but as a side-table description, not source residue. No active code; no archaeological surface in the working tree. Lock 1 "columnar SoA is dead" — honored.

### 3.6 PayloadStream — DELETED CLEAN

Grep `PayloadStream\|payload_stream` in `skinny/crates/` returns zero hits. The V9.5 PSI failure-mode signature (PayloadStream queue + unfinalized tape) is not present.

---

## 4. Type-Ambivalence Audit

The V9.5 failure mode was "tape and OpenFrame and direct-to-struct competing for the same role". Current state:

- `Tape<'input>` (`skinny/crates/runtime/src/tape/mod.rs:92-99`): owns `source`, `offsets`, `flag_cursors`, `flag_values`, `payloads`, `id`. **Single retained projection**.
- `TapeBuilder<'input>` (`skinny/crates/runtime/src/tape/assembler.rs:42-48`): owns `source`, `offsets`, `flag_cursors`, `flag_values`, `payloads`. **Pre-seal write surface for the same projection**. Sealed into `Tape` at `assembler.rs:173-181`. Identical field shape; no second representation.
- `ValueRef<'doc, 'input, K>` (`skinny/crates/runtime/src/tape/mod.rs:173-178`): `tape: &'doc Tape<'input>` + `cursor: u32` + phantom kind marker. **Read-only view over the same projection**.
- `JsonValue<'doc, 'input>` (`skinny/crates/runtime/src/grammars/json/value.rs`): typed projection over `ValueRef`. Same identity.
- `JsonDocument<'input>` / `JsonRoot<'input>` (`skinny/crates/runtime/src/grammars/json/view.rs:11,62`): wrapper holding `Tape<'input>` + source. Same identity.

No `TapeToken` type lives in the implementation — the spec admits a 16-byte `TapeToken` for the `EagerTape` shape (`SUBSTRATE.md:51-87`) but the implemented OffsetTape carries `u32` offsets only. SUBSTRATE.md:225-226 explicitly lists `TapeToken, NodeKindId, TokenFlags PAYLOAD_CLASS` as "DELETE; unused after eager-path retirement".

`JsonToken` (the typed view, `value.rs:13` — exported by `value.rs:14` `pub use value::{JsonNodeKind, JsonToken, JsonValue, ...}`) is a typed read accessor over the offset stream, NOT a stored representation.

**Verdict — HOLDS**. One retained substrate type (`Tape`), one write-surface (`TapeBuilder`), one cursor (`ValueRef`), one typed projection. No type-ambivalence.

---

## 5. OpenFrame Residue Cleanup Status

**Skinny workspace (V1, production-locus)**: grep `OpenFrame` in `skinny/crates/` returns **zero hits**. Lock 1 verbatim text "the Vec<OpenFrame>::clone parallel substrate that produced the 86.07% samply pathology" is honored cleanly in the V1 line.

**Legacy `/crates/`** (pre-restart, MIGRATION.md disposition ABROGATE-REPLACE/MOVE/DELETE):

`grep -rl "OpenFrame" /crates/ --include="*.rs"` returns 19 files (10 generated/template + 9 hand-written).

Generated files (per ARCHIVE / MIGRATION disposition — `crates/core` is ABROGATE-REPLACE in full):
- `crates/core/src/grammar/generated/bbnf.rs` (3 sites: 9982, 12840, 14120)
- `crates/core/src/grammar/generated/json.rs:1843`
- `crates/core/src/grammar/generated/css_l4.rs` (multiple sites starting 37218)
- `crates/core/src/grammar/generated/google_sheets.rs`
- `crates/core/src/grammar/generated/css_pretty.rs`
All are documentation comments referencing `JsonStructBuilder::OpenFrame::Wrap` (the legacy builder pattern). They are slated for GENERATED-REPLACE under MIGRATION.md §2.

Hand-written non-generated files (slated for ABROGATE-REPLACE/DELETE):
1. `crates/core/tests/wrap_compound_elision.rs`
2. `crates/core/tests/css_l4_substrate.rs`
3. `crates/core/tests/json_object_pairs_probe.rs`
4. `crates/core/src/runtime/builder_template.rs`
5. `crates/core/src/runtime/google_sheets/arena.rs`
6. `crates/core/src/runtime/google_sheets/builder.rs`
7. `crates/core/src/runtime/bbnf/builder.rs`
8. `crates/core/src/runtime/json/builder.rs`
9. `crates/core/src/runtime/css_l4/builder.rs`
10. `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs`
11. `crates/core/src/backend/rust/emitter/shapes/object.rs`
12. `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/mod.rs`
13. `crates/core/src/backend/rust/emitter/shapes/array/mod.rs`
14. `crates/core/src/backend/rust/emitter/shapes/wrap/struct_direct.rs`

Classification:
- All 14 non-generated files: "actually used IN LEGACY". The legacy `/crates/core` workspace is **separate** from `/skinny/crates`. They are not consumed by the V1 line. Per MIGRATION.md §3 `crates/core | ABROGATE-REPLACE plus selective ABROGATE-MOVE`. Per Lock 12 (archive ceremony before A.W0), the `/crates/` tree is pre-restart legacy.
- The MIGRATION.md statement at line 752 ("`rg "OpenFrame|Vec<OpenFrame>|ParseStream" crates/runtime/src crates/codegen/src`") is a **gate** for the post-tranche-A migration of `/crates/core`, not a claim about the current skinny.

**Verdict — HOLDS for skinny; LEGACY-DELETION-PENDING for `/crates/`**. The skinny line carries zero `OpenFrame`. The legacy tree retains the references the migration plan slates for ABROGATE; that is migration scope, not V1 Lock 1 violation. The "MIGRATION.md says 10+ files still reference OpenFrame" claim in the task prompt is verified — 14 non-generated hand-written files still reference it, all under `/crates/core/` slated for ABROGATE-REPLACE.

---

## 6. EventCursor — Lowering Boundary vs Parallel Prepass

SK-V4 §6 (`restart/skinny/audit/ASMJSON-DAV1D-GRAND-SYNTHESIS-SK-V4.md:256-265`) names the boundary:

```text
bytes
  -> scan / mask producer
  -> typed event cursor
  -> { OffsetTape | EventTape | SinkOnly | CollapsedStage | EagerTape fallback }
```

The typed event cursor sits **between** the scan producer and the shape selection — at the lowering boundary, consumed by the generated body per dispatch site. The refuted shape sits the cursor **before** an unchanged generated body and runs it upfront.

Current state:

- `generated.rs` (canonical, non-eventcursor): no cursor at all. `parse_value_at` reads source bytes directly. Scan output is unused along the parse path. **Substrate-without-consumer at the dispatch site**.
- `generated_eventcursor.rs` (feature-gated): cursor is a full-input prepass (`ParseIndexCursor::new(state.bytes)` in `attach_structural_index`); `parse_value_at` still reads source bytes through a class-LUT. **The refuted parallel prepass**.

Neither matches the SK-V4 prescription. The lowering-boundary EventCursor would land at `skinny/crates/codegen/src/json_templates/generated.rs:200-289` (the `consume_*` family) as a small inline state holding the next-structural cursor over `Tape::offsets`, advanced per consume call. It does not exist in the source tree.

**Verdict — LOWERING-BOUNDARY CONSUMER ABSENT; PREPASS CONSUMER PRESENT AND REFUTED**. Same surgery as §3.2 covers it.

---

## 7. Direct Builders Substrate-Bypass Check

Per Lock 1 (V9.5 amendment): "direct builders do NOT bypass the substrate event stream". SK-V4 §3 line 84: "Clarified that direct builders do not bypass the substrate event stream."

Surveyed direct-build sites:

- `skinny/crates/bbnf-bench/src/direct_struct.rs::sink_only_digest` (line 190): **bypasses the substrate entirely**. Walks `&[u8]` directly with `parse_that_regex` primitives, never builds a `Tape` or `TapeBuilder`. This is bench-private hand-written parsing. Per §3.3 above, the surgery is MOVE-AND-GENERATE: the bench should consume a codegen-emitted SinkOnly parse driver from the same BIR/LayoutFacts pipeline that emits the OffsetTape path, not a bespoke hand-coded loop.
- `skinny/crates/bbnf-bench/src/track2/json.rs` (lines 6-29): **uses the substrate**. Imports `tape::{CapacityPlan, OffsetFlags, TapeBuilder}` and emits offsets via `TapeBuilder`. This is admissible — Track 2 is the hand-coded parallel against the same substrate that codegen emits.
- `skinny/crates/runtime/src/grammars/json/generated.rs`: **emits through the substrate** (`state.emit_plain_offset(...)` → `tape::TapeBuilder::push_plain_offset` at `assembler.rs:129-143`).

The intended `SinkOnly` codegen path would emit `DirectBuild` BIR ops as typed-field writes into a caller-provided struct, with no `Tape` retention. That codegen does not exist; the substrate union admits it (ARCH §7.3 line 1080 "Else if the public output mode is direct-only and no post-parse path/value traversal is required ⇒ `SinkOnly`") but no rule resolves to it because no `LayoutFacts.backend_shape` derivation runs (§2 above).

No existing TEST bypasses the event stream via the substrate. The bypass is bench-side and slated for SinkOnly migration.

**Verdict — HOLDS at runtime; VIOLATED bench-side at `direct_struct.rs`**.

---

## 8. Final Verdict Per Lock 1

| Sub-clause | Status | Cite |
|---|---|---|
| Tape ≡ structural projection (one union, one substrate) | **HOLDS** structurally — `Tape<'input>` is the sole retained projection; OffsetTape variant is the implemented shape; no parallel substrate stored. | `skinny/crates/runtime/src/tape/mod.rs:92-99` |
| Five-shape union with cost-model-derived selection | **VIOLATED** at the dispatch layer — `BackendShape`/`derive_backend_shape` absent from source. One generated body shape regardless of rule. | `skinny/crates/codegen/src/lib.rs:115-118`, `grep -rn "BackendShape" skinny/crates/` returns zero. |
| No parallel substrate | **VIOLATED** — `generated_eventcursor.rs` is the refuted parallel prepass, wired behind `feature = "eventcursor"`. Lock 1 names parallel substrates as faults regardless of activation gate. | `skinny/crates/runtime/src/grammars/json/generated_eventcursor.rs:62-63`, parser wiring at `parser.rs:63-69`. |
| No orthogonal codepath | **VIOLATED** — same surgery: the eventcursor `pub fn parse` at `parser.rs:65-69` is an orthogonal codepath. | `skinny/crates/runtime/src/grammars/json/parser.rs:54-70`. |
| Direct builders do NOT bypass the substrate | **VIOLATED bench-side** — `direct_struct.rs::SinkParser` is a private parser that never consults `Tape`/`TapeBuilder`. | `skinny/crates/bbnf-bench/src/direct_struct.rs:190-353`. |
| Type ambivalence | **HOLDS** — one substrate type, one builder, one cursor, one typed projection. | §4 above. |
| Columnar SoA dead | **HOLDS** — zero residue. | `grep -rn "columnar\|SoA" skinny/crates/` returns zero. |
| `OpenFrame::clone` gone | **HOLDS (skinny)** — zero residue in V1 line. Legacy `/crates/core` still references OpenFrame in 14 non-generated files (migration scope, not V1 Lock 1 scope). | §5 above. |
| Substrate-without-consumer | **PRESENT** — scan output (`StructuralIndex`/`JsonParseIndex`) is computed by `bbnf_simd::scan_json_structurals` but unused along the canonical parse path; `generated::attach_structural_index` is a no-op. The `passes::recognizers::derive_backend_shape` decision substrate is also absent (substrate side of the decision exists in spec only; no consumer in codegen). | `generated.rs:13-16`, §2 above. |
| Generic-only-in-generic-crates | **VIOLATED bench-side** — `direct_struct.rs` is JSON-specific hand-coded parsing inside the generic `bbnf-bench` crate. Per Lock 14: zero grammar-named code in generic crates. The bench module itself is JSON-specific by name (`track2/json.rs`); the design admits per-grammar bench modules, but `direct_struct.rs::SinkParser` is parsing logic, not bench framing. | §3.3 above. |
| Dead-substrate residue (separate-crate scale) | **PRESENT** — `skinny/crates/simd-scan/` is a complete duplicate scanner crate, not in workspace, not depended on. | `skinny/Cargo.toml:3-14` (members), `skinny/crates/simd-scan/Cargo.toml`. |

**Lock 1 verdict — VIOLATED**. Two parallel-substrate residues (eventcursor sidecar wired behind feature; simd-scan legacy crate in tree). One direct-builder substrate bypass (bench-side `SinkParser`). One missing decision substrate (cost-model derive_backend_shape). The storage substrate itself is clean and singular.

---

## 9. Concrete Amendments

Ordered by precedence: cleanup first (NUKE), then wiring (WIRE), then relocation (MOVE).

### 9.1 NUKE — parallel-substrate residue

| File | Action | Justification |
|---|---|---|
| `skinny/crates/simd-scan/` (entire crate) | DELETE | Not in workspace; not depended on; duplicate of `bbnf-simd` per `restart/MIGRATION.md:75`. Pure parallel-substrate residue. |
| `skinny/crates/runtime/src/grammars/json/generated_eventcursor.rs` | DELETE | Refuted parallel prepass per SK-V4 §4 line 148-152. |
| `skinny/crates/codegen/src/json_templates/generated_eventcursor.rs` | DELETE | Codegen template for the refuted file. |
| `skinny/crates/runtime/src/grammars/json/mod.rs:2-5` | DELETE the two `#[cfg(feature = "eventcursor")]` lines; collapse to `pub mod generated;`. | Single substrate variant per rule. |
| `skinny/crates/runtime/src/grammars/json/parser.rs:2-10,18-19,32-34,54-70` | DELETE eventcursor branch + `event_cursor: Option<ParseIndexCursor>` field + `ParseIndexCursor` import; collapse to one `pub fn parse`. | Orthogonal codepath elimination. |
| `skinny/crates/codegen/src/json_templates/parser.rs` (same line ranges) | DELETE identical branches in template. | Template parity with runtime. |
| `skinny/crates/codegen/src/lib.rs:81-84` | DELETE the conditional emission. | Codegen no longer emits a refuted file. |
| `skinny/crates/runtime/Cargo.toml` `[features]` | DELETE `eventcursor` feature. | Feature-flag gate eliminated. |
| `skinny/crates/runtime/examples/wave2_bench.rs` | DELETE the eventcursor branch (or whole file if it has no non-eventcursor purpose). | No consumer of the deleted module. |
| `skinny/crates/runtime/src/tape/mod.rs:7,234-309` | DELETE `scan_json_parse_index` import + `scan_parse_index` re-export + `ParseIndexCursor` struct + `impl ParseIndexCursor`. | The lowering-boundary cursor lives inside generated bodies, not as a substrate-level prepared cursor. |
| `skinny/crates/bbnf-simd/` `scan_json_parse_index` + `JsonParseIndex` | DELETE or DEMOTE | Only consumer is `ParseIndexCursor`. Once that goes, `scan_json_parse_index` has no caller. `scan_json_structurals` + `StructuralIndex` are the surviving scan producer. |

Nukables count: **8 file/section deletions plus 1 whole-crate deletion plus 1 whole-template deletion** = ~10 surgical removals.

### 9.2 WIRE — substrate present but unconsumed

| File | Action | Justification |
|---|---|---|
| `skinny/crates/passes/src/recognizers/` (new) | ADD `derive_backend_shape(grammar_ir, rule_id) -> BackendShape` per ARCH §7.3 lines 1075-1083 (8-priority algorithm). | Decision substrate currently empty; ARCH spec mandates it. |
| `skinny/crates/ir/` (BackendIr struct) | ADD `backend_shape: HashMap<RuleId, BackendShape>` to the LayoutFacts side-table per ARCH §7.3 line 1034. | Wire the decision into BIR. |
| `skinny/crates/codegen/src/json_templates/generated.rs` (`consume_*` family, lines 200-289) | REPLACE source-byte reads with inline tape-cursor consumption over `Tape::offsets`. Per `restart/skinny/SUBSTRATE.md:218-219`. | The scan output is currently computed but unused; consuming it inline is the lowering-boundary EventCursor. |
| `skinny/crates/codegen/src/lib.rs:115-118` | REPLACE single `include_str!` with per-rule branching on `backend_shape`. | The codegen must emit per-shape lowering, not one template for all rules. |
| `skinny/crates/runtime/src/grammars/json/generated.rs:13-16` (`attach_structural_index` no-op) | REPLACE no-op with offset-stream cursor attachment OR delete entirely once consume sites take cursor inline. | Substrate-without-consumer per `SUBSTRATE.md:219`. |

### 9.3 MOVE — wrong locus

| File | Action | Justification |
|---|---|---|
| `skinny/crates/bbnf-bench/src/direct_struct.rs:190-353` (`SinkParser` + `sink_only_digest`) | MOVE the parse logic into `skinny/crates/codegen/src/json_templates/` as the `SinkOnly` lowering template; emit it when `LayoutFacts.backend_shape = SinkOnly`. Keep `JsonDirectDigest` arithmetic bench-side. | Per SK-V4 §6 line 279-280: "Track 1 direct must move out of `bbnf-bench` into generated runtime/codegen `SinkOnly`." |
| `skinny/crates/bbnf-bench/src/direct_struct.rs:150-156` (`track1_digest`, `track2_digest`) | RE-WIRE through the new generated SinkOnly parse. Both tracks must reach a generated substrate path, not a private parser. | Lock 14 generic-only-in-generic-crates. |
| `skinny/crates/bbnf-bench/src/bin/profile_direct.rs` (untracked) | DELETE if a transient probe of the private parser; RE-WIRE if a permanent profile harness. | Probably transient. |

---

## 10. Closing

The five-shape `BackendShape` union is structurally honored at the storage layer (`Tape<'input>` is the OffsetTape projection and admits the other four as cost-model branches). The same union is **violated** at three load-bearing surfaces:

1. The dispatch substrate is empty: `derive_backend_shape` is absent; codegen emits one template regardless of rule.
2. A refuted parallel prepass (`generated_eventcursor.rs`) sits in the source tree behind a feature flag — exactly the V9.5 PSI failure-mode shape.
3. A bench-private direct parser (`direct_struct.rs::SinkParser`) bypasses the substrate; per SK-V4 §6 it disqualifies the Track 1 row and must migrate into generated `SinkOnly`.

Substrate-without-consumer is present at two sites: the scan-producer output is computed but unused along the canonical parse path; the cost-model decision substrate is absent.

A legacy fossil crate (`skinny/crates/simd-scan/`) sits outside the workspace but inside the tree — pure dead-substrate residue that the migration left behind.

Lock 1 cannot be ratified as held until the eventcursor sidecar deletes, the cost-model decision wire lands, the SinkOnly bench-private parser migrates into codegen, and the simd-scan fossil deletes. Substrate-side cleanup is mechanical; the dispatch-side wiring is the substantive engineering work.

---

**File size**: ~520 lines. **Lock 1 verdict**: **VIOLATED** (three load-bearing surfaces). **Nukables count**: 10 surgical removals (`simd-scan` crate, eventcursor template + runtime file + 8 module/feature/example sites) plus 3 substrate-side cursor/scan exports.
