# SK-V14 P2-D: Substrate + Tape Design

Pass: S-P2 Research. Cycle: V2 (V1 fold: C-P2D-3 demoted to §1.6(d) substrate-side observation per HARDENING-S-P2-V1-CONSOLIDATED §3.2 Fold-2).
Date: 2026-05-23.
Scope: interrogate the offset-tape substrate — lazy-materialisation counters per `skinny/RESULTS.md` Notes block, logical-vs-allocated tape ratios across 17 corpora, structural-projection union (Lock 1). Conclude whether tape + structural projection are one substrate; identify where a tape-shape change moves a hot leaf. No parallel substrate proposals.
Output: this file.
P1 hot-leaf antecedents: P1-B `runtime::generated_json::generated::parse_object_value_at_direct::<JsonDigestSink>` (81.13 % twitter direct Track 1, `skinny/crates/runtime/src/grammars/json/generated.rs:466`); P1-B `parse_array_element_at_direct::<JsonDigestSink>` (85.55 % canada direct Track 1, `generated.rs:506`); P1-B `<bbnf_bench::generated_real_typed::DirectParser>::skip_value` (72.50 % twitter typed Track 1; 76.12 % citm_catalog typed Track 1; `bbnf-bench/src/generated_real_typed.rs:2949`); P1-E §4.4 substrate-union framing (`p1e-hot-leaf-attribution.md:246`); P1-A §4 `copy_nonoverlapping` Lock-1 same-substrate signal; P1-C ANOM-2 view-boundary materialization; CH5 V3 substrate-union verdict at `research/p1/hardening/V3/CH5.md` (6/6 ACCEPT).
Lock surface: **Lock 1** (substrate union — primary); Lock 14 (grammar-neutrality of the substrate surface); Lock 16 (any tape-shape change touching SIMD scan or `bulk_emit_positions_64_neon` re-enters the admissibility allowlist).

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

### §1.1 — One substrate at HEAD: source-tree layout

`Tape<'input>` carries five backing fields and is the sole retained projection structure (`skinny/crates/runtime/src/tape/mod.rs:94-101`):

```
pub struct Tape<'input> {
    source: &'input [u8],
    offsets: Vec<u32>,
    flag_cursors: Vec<u32>,
    flag_values: Vec<u8>,
    payloads: PayloadArena,
    id: TapeId,
}
```

The single tape module (`skinny/crates/runtime/src/tape/`) carries five files totalling 532 lines: `mod.rs` (237; `Tape`, `ValueRef`, `PayloadArena`, `OffsetFlags`), `assembler.rs` (124; `TapeBuilder` + `CapacityPlan`), `offsets.rs` (6; `OffsetTapeStats`), `event_grammar.rs` (31; typed-grammar cursor types), `event_grammar_tests.rs` (134). There is no sibling substrate crate, no parallel offset stream, no second tape, no retained `Vec<JsonEvent>`. Cross-checked: `grep -rn "struct.*Tape\b" skinny/crates/runtime/src/` returns three hits — `Tape<'input>` (the canonical retained substrate at `tape/mod.rs:94`), `TapeBuilder<'input>` (the parse-time builder facade at `tape/assembler.rs:42`), `TapeId` (the identity newtype at `tape/mod.rs:92`). The three are one substrate: `TapeBuilder` writes into the fields that `Tape::from_offsets` then takes ownership of (`tape/assembler.rs:115-122`). There is no shadow substrate.

`ParserState<'i>` at `skinny/crates/runtime/src/grammars/json/parser.rs:7-12` carries `(input, bytes, cursor, tape: TapeBuilder<'i>)`. The Track 1 parse path's runtime state is a single tuple: one source slice, one byte cursor, one tape-builder. The scan output (`scan_structurals` at `skinny/crates/runtime/src/grammars/json/scan.rs:22`) returns `StructuralIndex` (a `Vec<u32>` of positions plus a backend tag), which is then folded into the same `TapeBuilder.offsets` by `attach_structural_index` (`scan.rs` plus the generated wire). There is no parallel `structural_offsets` field retained on `ParserState` between scan and parse — the scan output flows into the tape, then into the parser, then into the sealed `Tape`. The pre-lazy three-buffer drift documented at `restart/skinny/SUBSTRATE.md:239` (`ParserState.structural_offsets` + `TapeAssembler.offsets` + `Tape.offsets`) is **closed** at HEAD: `ParserState` carries only `tape: TapeBuilder`, and `TapeBuilder` carries the offsets that `Tape` then owns.

### §1.2 — Lazy-materialisation counters: 17-corpus tape-shape census

`skinny/RESULTS.md` §"Notes" carries one lazy-tape-materialisation row per corpus (lines 137-180). Per-row format: `N offsets, L logical offset bytes + F sparse flag bytes (R₁x input), A allocated tape bytes (R₂x input), P payload bytes; object opens O, array opens R, closes C, string quotes Q, numbers M, literals U, separators S`. The 17 rows form the empirical floor for any tape-shape interrogation.

Logical-vs-allocated ratio table (sorted by `R₂` allocated-tape-bytes / input-bytes):

| Corpus | Input bytes | Offsets count | Logical (L) | Allocated (A) | A/L | A/input (R₂) | Notes |
|---|---:|---:|---:|---:|---:|---:|---|
| gsoc-2018 | 3,327,831 | 41,714 | 166,856 | 272,384 | 1.63× | 0.08× | sparsest; large object-of-strings shape |
| github_events | 65,132 | 2,526 | 10,104 | 16,424 | 1.63× | 0.25× | small + balanced |
| apache_builds | 127,275 | 7,068 | 28,272 | 32,792 | 1.16× | 0.26× | object-of-strings (lowest A/L) |
| unicode_mixed | 1,053,086 | 41,870 | 167,480 | 272,384 | 1.63× | 0.26× | object-of-strings with unicode |
| numbers | 150,124 | 10,003 | 40,012 | 65,536 | 1.64× | 0.44× | flat number list |
| canada | 2,251,051 | 223,236 | 892,944 | 1,048,576 | 1.17× | 0.47× | nested floats |
| unicode_basic | 1,048,586 | 92,146 | 368,584 | 524,288 | 1.42× | 0.50× | balanced unicode |
| random | 510,476 | 49,011 | 196,044 | 262,144 | 1.34× | 0.51× | mixed shapes |
| update_center | 533,178 | 35,281 | 141,124 | 263,424 | 1.87× | 0.49× | nested objects + strings |
| marine_ik | 2,983,466 | 359,563 | 1,438,252 | 2,097,152 | 1.46× | 0.70× | deeply nested floats |
| mesh | 723,597 | 80,250 | 321,000 | 524,288 | 1.63× | 0.72× | nested floats (densest) |
| y_string_unicode | 35,601 | 2,202 | 8,808 | 26,624 | 3.02× | 0.75× | sparse-flag dominated |
| twitter | 631,515 | 29,573 | 118,292 | 133,632 | 1.13× | 0.21× | strings + small objects |
| citm_catalog | 1,727,204 | 85,035 | 340,140 | 524,312 | 1.54× | 0.30× | balanced objects |
| unicode_escapes | 1,050,797 | 11,274 | 45,096 | 75,776 | 1.68× | 0.07× | sparsest escapes-only |
| instruments | 220,346 | 14,793 | 59,172 | 65,536 | 1.11× | 0.30× | balanced |
| distinct_values | 153,630 | 11,118 | 44,472 | 65,536 | 1.47× | 0.43× | string-table shape |

Three observations:

1. **A/L ratio is overwhelmingly in 1.11×–1.87× band** (15 of 17 corpora). Two outliers: `y_string_unicode` at 3.02× (small corpus with very sparse `flag_values` band — `flag_cursors`/`flag_values` capacity dominates), `apache_builds` at 1.16× (close-to-exact reserve). The geometric-grow `CapacityPlan::GrowOnly` (`tape/assembler.rs:18,89-91`, production default per `from_env`) holds within 2× of the realised structural count on every non-toy corpus. No corpus exhibits the >3× capacity overrun that would justify revisiting the `OneShotSimd` plan (`assembler.rs:17, 25-26`) — which reserves `simd_count + 8` and trades a pre-scan for capacity exactness. Plan C is **available but not load-bearing**.

2. **R₂ (A/input) spans 0.07× – 0.75×, never approaches 1×.** Per-input tape footprint is ≤ 0.75× input on every corpus and ≤ 0.50× on 13 of 17. This is the substrate-density truth: the tape never approaches the input size, even on `mesh`/`marine_ik` where the input is mostly numeric digits and the offset-per-structural-byte ratio is highest. The substrate-union claim is consistent with this density floor — offset projection IS strictly smaller than source, and the parser walks both the source slice and the offset projection.

3. **Sparse-flag band is empirically near-empty.** Per-corpus `flag_values` (the `needs_decode` / unicode-escape annotation lane) is non-zero on 8 of 17 corpora and ≤ 0.50 % of `logical offset bytes` on 7 of those 8. Only `y_string_unicode` (9000 sparse flag bytes vs 8808 logical offset bytes — 102 %), `unicode_escapes` (9385 vs 45096 — 21 %), `unicode_mixed` (9795 vs 167480 — 5.8 %), `gsoc-2018` (8545 vs 166856 — 5.1 %), and `update_center` (1045 vs 141124 — 0.74 %) carry non-trivial sparse-flag pressure. The remaining 12 corpora have 0–25 bytes of flag annotation — `binary_search` over `flag_cursors` (`tape/mod.rs:144-150`) is doing essentially nothing on the JSON corpus mass.

### §1.3 — Substrate-projection union: two cursors, one substrate

CH5 V3 (`research/p1/hardening/V3/CH5.md:78-83`) verified **two structurally independent cursors** at HEAD:

- **Track 1 cursor**: `ParserState::cursor: usize` (`parser.rs:10`) + the 12 inline `cursor: &mut usize` parameter signatures across `generated.rs` (`grep -c "cursor: &mut usize" runtime/src/grammars/json/generated.rs` → 12). Lives entirely inside `runtime::generated_json::*`.
- **Track 2 cursor**: `DirectParser::cursor: usize` (`bbnf-bench/src/generated_real_typed.rs:2742-2746`). Lives entirely inside `bbnf_bench::generated_real_typed::*`.

The two cursors **do not share a module path beyond the crate-graph root**. Neither cursor calls into the other's parse path. Both cursors index the same source slice (`&'i [u8]`) — they share the substrate, not the producer. This is the substrate-union shape verbatim per Lock 1's 2026-05-04 reframe (`restart/locks/LOCKS.md:48`): "tape is the greenfield's parsed event projection, unioned with direct-to-struct typed values that borrow into it (`&'i Tape<'i>` + cursor) when a retained document exists."

The asymmetry is load-bearing: Track 1 (`parse_object_value_at_direct::<JsonDigestSink>`) walks source bytes AND writes offsets into `TapeBuilder` (the retained-tape path). Track 2 (`DirectParser::skip_value`) walks source bytes ONLY (the SinkOnly direct-to-struct path). Both consume the same substrate token stream conceptually — the difference is that Track 1's substrate is materialised into the offset tape, and Track 2's substrate is materialised into the typed product (and dropped on the floor for the unselected subtrees, which is why `skip_value` dominates: 76.12 % `citm_catalog`, 72.50 % `twitter`, 41.70 % `marine_ik`). The substrate is one — the projection plane is per-row.

### §1.4 — Cross-section: `parse_object_value_at_direct` and `DirectParser::skip_value` against the tape

`parse_object_value_at_direct::<S>` at `runtime/src/grammars/json/generated.rs:466-502` is the Track 1 direct envelope. Body shape: byte peek at `bytes[*cursor]`, match into 7 arms (`{` → recurse object, `[` → recurse array, `"` → parse string + `sink.object_string_source(...)`, `-`/digit → `parse_number_object_direct`, `t`/`f`/`n` → consume literal + `sink.object_bool(...)` / `sink.object_null()`). Six of the seven arms produce a sink call; the structural arms recurse without a sink call (the sink sees the object/array close from the parent frame's emit). This envelope is **not** itself a primitive — it is the dispatch-step inner loop that consumes one substrate event. Per P1-E §4.1 (`p1e-hot-leaf-attribution.md:234`): "the hot loop is a branch-on-first-byte dispatch step under the substrate-union scheme — the primitive *behind* the envelope is the `dispatch` primitive."

`DirectParser::skip_value` at `bbnf-bench/src/generated_real_typed.rs:2949-2964` is the typed-plane structural-skip primitive. Body shape: byte peek at `self.bytes[self.cursor]`, match into 7 arms (`{` → `skip_object`, `[` → `skip_array`, `"` → `skip_string_raw`, `-`/digit → `number_span`, `t`/`f`/`n` → `consume_literal`). It is structurally identical to `parse_object_value_at_direct` MINUS the sink-emit calls — typed-plane sees only the typed-subset, every unselected subtree resolves to a skip. This is the substrate-union truth: typed-plane is **substrate-walk-with-shape-validation**, not typed-decode (P1-E §4.4: `skip_value` is `substrate` + `dispatch` in equal parts).

`skip_object` (`generated_real_typed.rs:2966-2985`) and `skip_array` (`:2987-3003`) recurse mutually with `skip_value`. The skip walks the source slice byte by byte; it does NOT consult the offset tape. Track 2's cursor IS independent of Track 1's tape projection — but both are valid substrate walks against the same source identity.

### §1.5 — The Lock 1 union variant question (Pass Omega V1.1 receiver)

`restart/skinny/SUBSTRATE.md:34-41` records the SK-V13 receiver: "one substrate remains binding, but SK-V13 must admit or architecturally block a new union variant distinct from REDRESS 96/97/98." This obligation is **inherited at SK-V14 and is the load-bearing tape-shape question for S-P2-D**. The two prior union-variant proposals are pre-blocked:

- **REDRESS 96** (`skinny/REDRESS.md:2797`): full class-column substrate + move-consumed `scan_structurals` vector. Correctness-green + parity-green; missed every W3 must-improve row + every W10b maintain floor. REJECT.
- **REDRESS 97** (`skinny/REDRESS.md:2852-2906`): allocation-free streaming cursor over the aarch64 scanner. Correctness-green + parity-green; missed every W3 must-improve row + every W10b maintain floor. REJECT.
- **REDRESS 98** (`skinny/REDRESS.md:2910-2950`): retires `G-W3-UNION-SUBSTRATE`. "On the M5 Max wide-issue core, the scalar `consume_structural`/delimiter path that profile attribution flagged as structural rediscovery is cheaper than materializing or streaming a SIMD structural cursor through retained parsing. The SIMD scan looked discarded because consuming it adds memory traffic and cursor indirection that the current branch-predictable, cache-hot scalar loop does not pay."

**S-P2-D's substrate-union verdict** is therefore not "find a new union variant"; it is "**the substrate union holds at HEAD; tape + structural projection ARE one substrate; the producer asymmetry between Track 1 (writes tape) and Track 2 (skips, no tape write) is the projection-plane discriminant Lock 1 admits, not a substrate split.**" Per Pass Omega V1.1 / SK-V13 substrate receiver (`SUBSTRATE.md:33-41`), the receiver may be discharged by **architectural block** of a new union variant — and the substrate-walk-with-shape-validation framing IS that architectural block: there is no new union variant to admit; the two existing planes (retained-tape + direct-skip-with-shape) are substrate-union projections, not a third candidate.

### §1.6 — Substrate-side observations

Per the §1.2 census and §1.3 union, four concrete tape-shape perturbations are evaluable against the §1.4 hot leaves. Each is named here as a substrate-side observation, NOT a candidate primitive (P2-D scope is interrogation; primitive design is P2-B/C/E):

(a) **Capacity-plan switch**. `CapacityPlan::GrowOnly` (the default; geometric grow at `assembler.rs:89-91`) vs `OneShotSimd` (one-shot reserve from `simd_count + 8` at `scan.rs:51`). The 17-corpus A/L band is 1.11–1.87× under `GrowOnly`; `OneShotSimd` would crush A/L to ~1.00× at the cost of a full SIMD pre-scan per parse. Hot-leaf consequence: `parse_object_value_at_direct` would see fewer `reserve_offsets_cold` jumps (`assembler.rs:87-91`) — but P1-A's `copy_nonoverlapping` 9.5-11.4 % rows (`p1a-samply-mode-1.md:142,143,150`) are the only tape-commit pressure currently visible, and that pressure is tape-write (the `unsafe { offsets.as_mut_ptr().add(len).write(...) }` at `assembler.rs:76-82`), not capacity-grow. **Moving A/L closer to 1× moves zero hot-leaf cycles** because the grow path is already cold (`#[cold] #[inline(never)]` at `assembler.rs:87-88`).

(b) **Sparse-flag elimination**. 12 of 17 corpora have ≤ 25 bytes of `flag_values`. A tape-shape that gates `flag_cursors`/`flag_values` Vec construction behind a "needs_unescape exists" flag, defaulting to a zero-length pair, would save the `Vec::new()` (2 × `mem::size_of::<Vec>` = 48 bytes) per parse on the 5 corpora that exhibit no flag pressure (`canada`, `mesh`, `numbers`, `marine_ik`, `unicode_basic`, `random`, `distinct_values`, `instruments`). Hot-leaf consequence: zero — `binary_search` over an empty `flag_cursors` is already O(1); the only savings is the parser-state-init 48 B and the `payloads.is_empty()` check. **No hot-leaf moves**.

(c) **Tape elision under direct-only paths**. Per §1.3 asymmetry, the Track 2 direct-only path (Track 2 / `DirectParser::skip_value` / `JsonDigestSink`) does NOT write into a tape at all. The Track 1 direct-only path (`parse_direct` at `generated.rs:407`) builds a `ParserState` with a `TapeBuilder` AND walks the source. The `BackendShape::SinkOnly` value (per `restart/skinny/SUBSTRATE.md:286-293` and ARCH §7.3) is the formal name of this elision: "Sink-only direct-to-struct paths have no document identity because they do not retain a queryable document" (`SUBSTRATE.md:284`). At HEAD, the Track 1 direct path still constructs a `TapeBuilder` even when it will not be consulted — the `JsonDigestSink` consumer (`bbnf-bench/src/direct_struct.rs:48-110` per P1-B §1.4) never asks for `document.root_value()`. Hot-leaf consequence: `parse_object_value_at_direct::<JsonDigestSink>` (the 81.13 % twitter top-1 leaf per P1-B §2 direct row) would skip the `attach_structural_index` + `TapeBuilder::push_offset` calls if `SinkOnly` were the per-rule materialisation plan. **This is the single tape-shape perturbation that moves a P1 hot leaf** — and per Lock 10's auto-detect mandate (`LOCKS.md:164`), the perturbation is gated on the cost-model derivation of `LayoutFacts.backend_shape` per `BackendShape ∈ {EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` per ARCH §7.3. The substrate change is the existing five-shape taxonomy, not a new substrate.

(d) **Sparse-flag-band gating on `Tape::flag_cursors`/`flag_values` construction** (V2-demoted; formerly C-P2D-3 in V1 §2 enumeration). Defer `Vec<u32>::new()` and `Vec<u8>::new()` allocations for `flag_cursors` / `flag_values` (`tape/mod.rs:97-98`) until the first `patch_flags` call writes a non-zero flag (`assembler.rs:94`). Per §1.2 census, 12 of 17 corpora have ≤ 25 bytes of flag pressure — the two `Vec` headers (48 bytes on 64-bit) plus their default reserve are zero-utility on those rows. The substrate primitive is `Option<(Vec<u32>, Vec<u8>)>` or a `SmallVec`-style inline-2 store; structurally a one-liner change at `TapeBuilder::new` (`assembler.rs:50-59`). The existing `patch_flags` (`assembler.rs:94-113`) is the scalar reference; the change is wrapping the `flag_*` field access in an `Option::get_or_insert_with(Default::default)`. The `binary_search` consumer at `tape/mod.rs:144-150` is unchanged (`Option::as_ref().map(|v| v.binary_search(...))`). Arch: substrate-side / allocation discipline; no SIMD/ASM. Architecture-independent. Grammar-neutrality: HIGH — the substrate field is already grammar-neutral; the gating is grammar-policy-free. **Disposition stamp: Demoted V2: zero hot-leaf consumer at SK-V14; re-elevate to candidate if S-P3 finds same-wave consumer.** Per (b) above the hot-leaf consequence is zero, restating the same finding from the allocation-side perspective; (b) is the load-bearing observation, (d) names the concrete substrate-edit shape that would actuate (b) if a same-wave consumer materialises (e.g. the C-2 reprofile re-attributes a non-trivial parser-state-init self-time on small corpora — currently absent).

Translation: the substrate union holds; the *projection plane* (whether tape is materialised at all for a given parse) is the per-rule decision. Where a tape-shape change moves a hot leaf is **only at the per-rule `backend_shape` boundary** — and that boundary is already declared in the five-shape vocabulary. There is no second substrate to add.

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

P2-D's scope is substrate interrogation, **not parallel substrate proposals**. The candidate-primitive list below is therefore **substrate-side primitives only**: observations + measurement hooks whose purpose is to keep the substrate union honest and to let S-P3 wire the per-rule `backend_shape` cost model against concrete substrate evidence. Per §1.5 + §1.6, the load-bearing substrate work is **per-rule `backend_shape` activation against the five-shape vocabulary**, not a sixth shape. V2 cycle: C-P2D-3 (sparse-flag-band gating) demoted to §1.6(d) substrate-side observation per CH1 V1 §3.2 + CH4 V1 §3 CF-2; the identifier is held in a gap-note row below for cross-tranche reference stability.

### C-P2D-1 — `BackendShape::SinkOnly` activation on JSON `parse_direct` direct-to-struct path

- **Shape**: a per-rule `LayoutFacts.backend_shape[rule_id] = SinkOnly` selection that causes the lowering at `crates/codegen/src/lower/rust.rs` to emit `parse_object_direct` + `parse_array_direct` (`generated.rs:436,437`) WITHOUT constructing `TapeBuilder` / calling `attach_structural_index`. The `ParserState::tape: TapeBuilder<'i>` field becomes optional (or replaced by `()` under `SinkOnly`). The substrate primitive is the existing `Tape<'input>` / `TapeBuilder<'input>` types; the change is the *absence* of construction at parse entry.
- **Scalar-reference status**: PRESENT. The scalar reference IS the current `parse_value_direct` at `generated.rs:425-462` — it already takes `(input, bytes, cursor, sink)` with no tape parameter. The reference is the existing direct-emit body MINUS the (currently still constructed-but-unused) `TapeBuilder` in `ParserState`.
- **Arch**: substrate-side / lowering-pass; no SIMD/ASM. Architecture-independent.
- **P1 antecedent**: P1-B `parse_object_value_at_direct::<JsonDigestSink>` (81.13 % twitter direct Track 1; 86.64 % update_center; 80.78 % gsoc-2018; 87.16 % github_events — `p1b-samply-mode-2.md:117,126,132,124`). The 7 P1-B direct rows where the envelope is 70 % + top-1 carry the elidable tape-init cost. The hot-leaf-class re-attribution per P1-E §2.2 is `dispatch` (branch-on-first-byte under the substrate-union); the tape-elision shifts the substrate footprint without changing the dispatch primitive itself.
- **Grammar-neutrality**: HIGH — `BackendShape::SinkOnly` is already grammar-neutral by construction (one of five enum values at ARCH §7.3; the per-rule derivation is auto from existing Grammar IR facts). The primitive is a substrate-mode selection, not a JSON-specific shape. CSS L4 declaration-values, BBNF-self, and Sheets formulas all admit `SinkOnly` when the consumer is a fact-stream or row sink with no retained-document query.

### C-P2D-2 — Lazy-tape-materialisation column extension in `skinny/RESULTS.md`

- **Shape**: a per-row schema-column addition `lazy_tape_materialisation_ratio = (A/input, A/L, flag_pressure_bytes)` exposed alongside the existing Mbps + c/B + verdict columns, so the consumer (`xtask gate-json` per SYNTHESIS.md §3) can mechanically refuse a row whose A/input crosses a configurable threshold or whose A/L exceeds the 2× empirical band. The substrate primitive is the existing `OffsetTapeStats` at `runtime/src/tape/offsets.rs:1-6` (currently 3 fields: `offset_count`, `offset_bytes`, `offset_capacity_bytes`); the change is plumbing it through the Notes-block emit at the bench harness and into the typed parse-result schema.
- **Scalar-reference status**: PRESENT. The fields are already computed at parse-end: `Tape::offset_bytes()` (`tape/mod.rs:152-154`), `Tape::flag_bytes()` (`:156-158`), `Tape::offset_capacity_bytes()` (`:160-164`). The current `RESULTS.md` Notes block (`skinny/RESULTS.md:137-180`) prints them as prose. The change is a column, not a new computation.
- **Arch**: substrate-side / measurement; no SIMD/ASM. Architecture-independent.
- **P1 antecedent**: P1-A §4 (`p1a-samply-mode-1.md:318-321`) — the `copy_nonoverlapping` 9.5-11.4 % rows are the only currently-visible tape-commit signal, but per §1.6(a) above the grow path is `#[cold]`; the column-based gate would let the bench-harness refuse a row whose tape footprint grew between cycles (substrate-regression guard) without requiring re-profiling. Same primitive supports P1-D PMU c/B re-attribution against tape footprint.
- **Grammar-neutrality**: HIGH — `OffsetTapeStats` is already grammar-neutral (lives in `runtime::tape::*`, not `runtime::generated_json::*`). The column primitive is a schema-level measurement, not a JSON-specific lever. CSS L4 / Sheets / BBNF-self all share the same tape substrate (the substrate is grammar-neutral per `SUBSTRATE.md:7`), so the same `OffsetTapeStats` column applies to every grammar that lowers to `OffsetTape` or `EagerTape`.

### C-P2D-3 — [DEMOTED V2 → §1.6(d) substrate-side observation]

Formerly *Sparse-flag-band gating on `Tape::flag_cursors`/`flag_values` construction*. Demoted V2: zero hot-leaf consumer at SK-V14; re-elevate to candidate if S-P3 finds same-wave consumer. Technical content preserved verbatim at §1.6(d). Candidate identifier retained as gap-note for cross-tranche reference stability (CH1 V1 §3.2 fold target; CH4 V1 §3 CF-2 ACCEPT-as-honest-completeness).

### C-P2D-4 — `BackendShape::EventTape` interrogation (NOT proposed; documented as pre-blocked by REDRESS 96/97/98)

- **Shape**: a per-rule materialisation plan that retains the SIMD structural cursor as a queryable substrate (the "EventTape" enum value of ARCH §7.3). **NOT a candidate at S-P2-D**: REDRESS 96 and REDRESS 97 are the two faithful implementations (full class-column substrate; allocation-free streaming cursor). Both regressed uniformly across W3 and W10b (`REDRESS.md:2917-2922`). REDRESS 98 retires the union-substrate thesis on this host (`REDRESS.md:2910-2950`).
- **Scalar-reference status**: N/A — REJECT-by-history.
- **Arch**: would be SIMD-coupled (aarch64 NEON `bulk_emit_positions_64_neon` cursor retention); host arm64 M5 Max.
- **P1 antecedent**: there is no SK-V14 P1 evidence that would re-open the route per CH3 substrate pre-block. P1-D §4 anomaly 7 (per P1-E §4.7) — "the SIMD ratio is a substrate truth, not a prompt for parallel-substrate redress" — explicitly closes this door.
- **Grammar-neutrality**: would be HIGH if admissible; but **not admissible**. Listed here as the canonical "what would a parallel-substrate proposal look like, and why is it pre-blocked" reference for CH3 / CH5 cross-checking.

### Candidate-list discipline footnote

P2-D's candidate enumeration is **2 active + 1 demoted-to-§1.6(d) + 1 pre-blocked** (V2). The dispatch prompt forbids parallel substrate proposals; the active candidates are (a) per-rule shape selection (re-using the existing `BackendShape` enum), (b) substrate-measurement plumbing. Neither proposes a new substrate, a new buffer, a new sidecar producer, or a renamed scanner. Both respect the substrate union per Lock 1. The C-P2D-3 sparse-flag allocation discipline migrated to §1.6(d) at V2 because it had zero hot-leaf consumer at SK-V14 (the candidate's own §1.6(b) cross-reference; CH1 V1 §3.2 + CH4 V1 §3 CF-2 fold targets); the identifier is preserved as a gap-note for cross-tranche stability and may re-elevate to candidate status if S-P3 finds a same-wave consumer.

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

| Candidate | Grammar-neutral verdict | JSON-specific dependency | CSS L4 admissibility | BBNF-self / Sheets admissibility |
|---|---|---|---|---|
| C-P2D-1 (`BackendShape::SinkOnly` activation) | **YES** | none — `BackendShape` is the five-shape vocabulary at ARCH §7.3; per-rule derivation is auto from Grammar IR facts | CSS L4 declaration-values lowers to a fact-stream sink consumer; `SinkOnly` applies directly per `SUBSTRATE.md:284-292` | BBNF-self has fact-stream consumers (LSP/diagnostics); Sheets formulas have row-sink consumers — both admit `SinkOnly` |
| C-P2D-2 (`OffsetTapeStats` column extension) | **YES** | none — `OffsetTapeStats` lives at `runtime::tape::*`, not `runtime::generated_json::*` | applies verbatim — any grammar lowering to `OffsetTape` or `EagerTape` emits the same stats | applies verbatim — substrate field is grammar-agnostic |
| C-P2D-3 (sparse-flag gating) | **N/A — DEMOTED V2 → §1.6(d)** | n/a (was: none — `flag_cursors`/`flag_values` are substrate fields; their semantics are per-grammar-policy) | n/a (was: CSS L4 selector-combinator annotations would benefit if sparse) | n/a (was: any grammar with sparse mid-parse annotations admits the gating) |
| C-P2D-4 (`EventTape` — pre-blocked) | **N/A** (REJECT-by-REDRESS-96/97/98) | n/a — would have been grammar-neutral if admissible | n/a | n/a |

Per CH2 / Lock 14: none of the two active candidates carries a JSON-grammar match arm, JSON-named module, JSON-specific type in a generic-crate public API, or JSON-keyed feature flag. The substrate primitives are already grammar-neutral by construction; the candidate list is a per-substrate-mechanism activation/measurement set, not a per-grammar fork. CSS L4 spec evidence per CH2 F2 binding (zero CSS L4 grammar-neutral primitive evidence at SK-V14 profile — only `declaration_values` renders) is consistent: the substrate-side candidates apply to CSS L4 *by construction of the substrate*, not by per-grammar profile-derived admission.

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

CH3 pre-block surface per the dispatch context §2 + PASS-2-RESEARCH §3 CH3. P2-D's interrogation MUST refuse to re-open any of the following:

### §4.1 — REDRESS 96 / REDRESS 97 / REDRESS 98 substrate-ceiling family

- **REDRESS 96** (`skinny/REDRESS.md:2797-2849`): W3 class-column + move-consumed structural-index substrate. REJECT.
- **REDRESS 97** (`skinny/REDRESS.md:2852-2906`): W3 V2 streaming-cursor implementation (allocation-free; aarch64-scanner-anchored). REJECT.
- **REDRESS 98** (`skinny/REDRESS.md:2910-2950`): `G-W3-UNION-SUBSTRATE` retirement; on M5 Max the scalar `consume_structural` path beats SIMD cursor retention.

**P2-D mitigation**: §1.5 explicitly closes the "new union variant" question by architectural-block per Pass Omega V1.1 receiver (`SUBSTRATE.md:33-41`). C-P2D-1/2/3 do **not** propose retaining a SIMD structural cursor, do **not** propose a class-column substrate, do **not** propose a streaming cursor. C-P2D-4 is listed as the pre-blocked anti-pattern reference, NOT as a candidate. CH3 dispose: ACCEPT.

### §4.2 — REDRESS 50–55 SK-V5 UTF-8 fusion family

`SUBSTRATE.md:254-266` documents the SK-V5 redress items 51/53 (`JsonEventCursor` whitespace-skip centralisation; `JsonStructuralCursor` consuming live per-stripe punctuation/quote masks) — both measured + REJECTed. The admissible H.W1 cursor "is not a renamed whitespace skipper and not a second parser-local scanner."

**P2-D mitigation**: C-P2D-1 (SinkOnly activation) does not introduce a new cursor; it elides the TapeBuilder construction at parse entry. C-P2D-2/3 are measurement/allocation candidates with no cursor implication. CH3 dispose: ACCEPT.

### §4.3 — REDRESS 60–72 SK-V6 retained-parse + sidecar producers + digest cap-16

`skinny/REDRESS.md:206` documents the historical sidecar consolidation ("`Box<[u32]>` plus packed `Box<[u8]>` flags"); the lazy-offset tape migration (Item 20 at `skinny/REDRESS.md:246`) is the current shape. A retained sidecar classifier is pre-blocked (`SUBSTRATE.md:38-41`).

**P2-D mitigation**: C-P2D-2 (column extension) does NOT retain a new sidecar — it surfaces the existing `OffsetTapeStats` fields. C-P2D-3 (sparse-flag gating) does NOT retain a new sidecar — it gates the construction of fields the substrate already carries. CH3 dispose: ACCEPT.

### §4.4 — REDRESS 80, 82-84 (canada mantissa-widen; tiny single-quartet unicode classifier; StringBlock16 tiny probe; object-pair compaction)

These are primitive-class pre-blocks for the canada/mantissa lane and tiny-string lookup variants. P2-D's substrate-interrogation candidates do not touch the mantissa or tiny-string primitives; the C-P2D-3 sparse-flag gating affects the `needs_decode` flag-band only.

**P2-D mitigation**: no overlap. CH3 dispose: ACCEPT.

### §4.5 — REDRESS 88 (PMULL prefix-XOR as hot body) + REDRESS 89 (CSSC CTZ next-bit bulk consumer)

These are SIMD/instruction-class pre-blocks per P2-C's scope (host-arch ASM/SIMD esoterica). P2-D is substrate-side; the substrate-side candidates do not invoke PMULL or CSSC.

**P2-D mitigation**: no overlap. CH3 dispose: ACCEPT.

### §4.6 — Lock 1 substrate-union ceiling (the binding constraint)

The dispatch prompt's binding: "No parallel substrate proposals." P2-D's two active candidates respect this:
- C-P2D-1: re-uses existing `BackendShape::SinkOnly`; does not introduce a new shape.
- C-P2D-2: re-uses existing `OffsetTapeStats`; does not introduce a new substrate field.

C-P2D-3 (V2-demoted to §1.6(d)) re-uses existing `flag_cursors`/`flag_values`; even if re-elevated by a future same-wave consumer it does not introduce a new substrate field. C-P2D-4 is explicitly REJECT-by-history and is listed as a paper-trail anchor for CH5 cross-checking, not as a candidate.

### §4.7 — CH5 hidden-coupling cross-check

Per dispatch context CH5 binding ("substrate union holds; P2-D concludes whether the tape + structural projection are one substrate"): §1.1 + §1.3 + §1.5 jointly conclude **YES, the substrate union holds at HEAD**. The Track 1 ≡ Track 2 dishonesty risk is closed by CH5 V3's two-cursor independence verification (`research/p1/hardening/V3/CH5.md:78-83`). The retained-cursor risk is closed by §1.1 + §1.6(c) — C-P2D-1's `SinkOnly` activation *removes* a retained-substrate path (elides `TapeBuilder` construction) rather than adding one. CH5 dispose: ACCEPT for both active candidates (C-P2D-1, C-P2D-2); the V2-demoted §1.6(d) sparse-flag observation (formerly C-P2D-3) is CH5-clean by construction (re-uses existing substrate field, no new sidecar); the pre-blocked C-P2D-4 documents the anti-pattern for cross-checking.

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

### §5.1 — bbnf source-tree authority (HEAD-verified)

- `skinny/crates/runtime/src/tape/mod.rs:1-237` — `Tape<'input>`, `ValueRef`, `PayloadArena`, `OffsetFlags`, `DocumentView` trait, `TapeId` newtype. The canonical retained substrate definition.
- `skinny/crates/runtime/src/tape/assembler.rs:1-124` — `TapeBuilder<'input>`, `CapacityPlan` enum (4 variants: `Sampled` / `Exact` / `OneShotSimd` / `GrowOnly`), `push_offset` / `push_plain_offset` / `patch_flags` / `finish` API + the `#[cold] reserve_offsets_cold` grow path.
- `skinny/crates/runtime/src/tape/offsets.rs:1-6` — `OffsetTapeStats` (3 fields: `offset_count`, `offset_bytes`, `offset_capacity_bytes`).
- `skinny/crates/runtime/src/tape/event_grammar.rs:1-31` — `EventGrammar` trait + `AnyGrammar` marker.
- `skinny/crates/runtime/src/grammars/json/parser.rs:1-69` — `ParserState<'i>` (4 fields: `input`, `bytes`, `cursor`, `tape: TapeBuilder<'i>`); `parse` entry at `:47-52` calls `attach_structural_index` then `generated::parse_json`.
- `skinny/crates/runtime/src/grammars/json/generated.rs:35-56` — `parse_value_at` + `dispatch_value` (the parse-attribution-gated envelope per `#[cfg_attr(feature = "parse-attribution", inline(never))]`).
- `skinny/crates/runtime/src/grammars/json/generated.rs:407-462` — `parse_direct` + `parse_value_direct` (the Track 1 direct envelope WITHOUT typed-product schema).
- `skinny/crates/runtime/src/grammars/json/generated.rs:466-502` — `parse_object_value_at_direct::<S: JsonSink>` (the 81.13 % twitter direct Track 1 hot leaf per P1-B).
- `skinny/crates/runtime/src/grammars/json/generated.rs:506-545` — `parse_array_element_at_direct::<S: JsonSink>` (the 85.55 % canada direct Track 1 hot leaf per P1-B).
- `skinny/crates/runtime/src/grammars/json/scan.rs:22-38` — `scan_structurals` SIMD entry; `scan_structurals_scalar` scalar parity reference.
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:2742-2756` — `struct DirectParser<'i> { input, bytes, cursor }` + `DirectParser::new`.
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:2949-3003` — `DirectParser::skip_value` / `skip_object` / `skip_array` (the 72.50 % twitter typed Track 1 hot leaf per P1-B).
- `skinny/RESULTS.md:137-180` — the lazy-tape-materialisation Notes block (17 corpora; logical-vs-allocated tape ratios; per-token category counts).
- `skinny/RESULTS.md:5-39` — the per-row parse-result table (17 corpora × 3 planes; hot-leaf cite per row).

### §5.2 — Prior-tranche substrate authority

- `restart/locks/LOCKS.md:48-90` — Lock 1 verbatim + the 2026-05-04 reframe + the 2026-05-21 v+1 substrate-ceiling fold + the substrate_target/retention_lifetime/policy_owner manifest requirement (`:75-82`) + the REDRESS 96/97/98 binding-history clause (`:84-90`).
- `restart/skinny/SUBSTRATE.md:1-303` — the SK-V13 substrate slice (canonical layout for `TapeToken`/`Tape`/`ValueRef`/`DocumentView`); `:33-41` Pass Omega V1.1 / SK-V13 substrate receiver; `:233-303` SK-V5 `OffsetTape` evidence + the SK-V5 REDRESS-50/55 typed-event-cursor REJECT.
- `restart/skinny/SUBSTRATE.md:268-303` — typed event cursor + the four falsifiable SOTA-BEAT levers (Lock 15 enforcement, NEON intrinsic upgrade, typed-event lowering, CollapsedStage).
- `skinny/REDRESS.md:246-256` — Item 20 lazy-offset tape-union migration commit-log.
- `skinny/REDRESS.md:2797-2849` — REDRESS 96 class-column + move-consumed substrate (REJECT).
- `skinny/REDRESS.md:2852-2906` — REDRESS 97 streaming-cursor implementation (REJECT).
- `skinny/REDRESS.md:2910-2950` — REDRESS 98 `G-W3-UNION-SUBSTRATE` retirement.
- `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md:142,143,150,318-321` — `copy_nonoverlapping` 9.5-11.4 % tape-commit pressure rows + the Lock-1 same-substrate union signal paragraph.
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md:117-149,155-176,275-277` — direct-plane + typed-plane hot-leaf tables; `DirectParser::skip_value` 72.50/76.12 % envelope dominance; anomaly 4 substrate-skip-primitive framing.
- `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md:450,455` — ANOM-2 view-boundary materialization framing (substrate-union forces second pass to lift offset-tape).
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md:155-167,246` — typed-plane hot-leaf table + §4.4 substrate-union paragraph ("`skip_value` is `substrate` + `dispatch` in equal parts").
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH5.md:1-279` — substrate-union 6/6 ACCEPT verdict at V3; §3Z two-cycle-chain closure on CH5; two-cursor independence executable-verification at `:78-83`.
- `restart/skinny/tranches/sk-v14/research/p2/S-P2-DISPATCH-CONTEXT.md:50-58` — P2-D scope binding + the no-parallel-substrate constraint.

### §5.3 — Authority bindings

- `restart/prompts/skinny/PASS-2-RESEARCH.md:51` — P2-D scope row verbatim ("Whether tape and structural projection are one substrate (Lock 1) and where a tape-shape change would move a hot leaf. No parallel substrate proposals.").
- `restart/prompts/skinny/PASS-2-RESEARCH.md:125-131` — CH5 lens binding ("a candidate that proposes a second source scan, a retained cursor, an aux density table, or a parser-owned structural projection violates Lock 1 and CH5 REJECTs it").
- `restart/prompts/skinny/PASS-2-RESEARCH.md:236-240` — §8.6 substrate union closing pin ("P2-D concludes whether the tape + structural projection are one substrate; a candidate that splits them, or adds a sidecar event vector, violates Lock 1 and S-P3 may not shortlist it").
- `restart/prompts/ORCHESTRATOR.md §8` non-negotiables — Lock 1 substrate union; same-wave consumer rule; scalar-reference + checkasm discipline.
- `restart/ARCHITECTURE.md §7.3` — the five `BackendShape` values (`EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, `CollapsedStage`); the per-rule auto-derivation 8-step algorithm.
- `restart/ARCHITECTURE.md §9` (lines 1373-1426 per `SUBSTRATE.md:45`) — tape + direct-to-struct union; tape invariants.

### §5.4 — External (SOTA-comparator) substrate-shape evidence

- sonic-rs `src/lazyvalue/` (canonical LazyValue path) — "the slice IS the projection" pattern per `SUBSTRATE.md:237`. No retained structural-index buffer post-parse on the LazyValue path.
- simdjson `dom_parser_implementation::structural_indexes` + `document::tape` — the **only** SOTA comparator that retains two post-parse buffers per `SUBSTRATE.md:237`. simdjson is the architecture-pressure reference for what NOT to do at the SK-V14 substrate union (per REDRESS 96/97/98 — the union substrate thesis was the simdjson-shaped retention; it falsified on M5 Max).
- yyjson — single-pass into the parse-time-output buffer (one substrate); reference for the no-retained-sidecar discipline.
- asmjson — direct-emit into parse-time-output buffer; reference for one-substrate discipline.
- RapidJSON — direct-emit; reference for one-substrate discipline.
- All four comparator citations carry through P2-A's full teardown; P2-D's substrate-union claim is consistent with 4 of the 5 comparators (simdjson is the substrate-multi-buffer outlier and REDRESS 96/97/98 are the M5-Max-confirmed empirical floor against re-adopting that pattern).
