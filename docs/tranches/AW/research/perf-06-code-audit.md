# AW-III code audit — optimisation firing / precedence / dead code / direct-to-struct / type inference

## Methodology

- **Inputs (prebuilt, no cargo expand reruns)**:
  - `/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/prebuild/expand/bbnf_monolithic/expand.rs` (401 lines — bench binary only, consumes workspace `generated.rs`)
  - `/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/prebuild/expand/css_l4/expand.rs` (162,534 lines — CSS L4 full macro expansion)
  - `/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/prebuild/expand/google_sheets_monolithic/expand.rs` (15,139 lines)
  - `/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/prebuild/expand/json_monolithic/expand.rs` (3,742 lines)
  - `/Users/mkbabb/Programming/bbnf-lang/.profiles/samply/prebuild/expand/json_monolithic_value/expand.rs` (3,761 lines)
- **Source of truth**: `/Users/mkbabb/Programming/bbnf-wt-aw-a1/crates/core/src/grammar/generated.rs` (21,198 lines — BBNF-self bootstrap) plus the full `crates/**` tree.
- **Approach**: targeted `grep` + `Read` + artefact citation. Every claim below carries a file:line pointer.
- **Commit (HEAD at audit start)**: `b58d1461423acba6963343dc306e5029cc3cd66c`.

## Part 1 — AW-IV lever firing status

Every lever's evidence is grounded in either (a) the emitter's static substrate construction (`profile.rs`), (b) the 5 prebuilt expand artefacts, and (c) the walker's runtime dispatch paths (`driver.rs`).

| Lever | Status | Evidence |
|-------|--------|----------|
| **PSI rayon stage-B** | NOT-FIRING | Code present at `crates/bbnf-tape/src/psi.rs:70-73` (gated on `#[cfg(feature = "rayon")]`; `fill_parallel` at `:401`). Activation gate at `:312-315` returns `false` whenever `profile.parallel_break_even_bytes == 0`. Emitter hard-codes the value to **`0u32`** at `crates/core/src/backend/rust/emitter/profile.rs:139` (via `#parallel_break_even_bytes` interpolation which is not populated). All 4 emitted grammars in the expand artefacts confirm: `parallel_break_even_bytes: 0u32` at `css_l4/expand.rs:340`, `google_sheets_monolithic/expand.rs:94`, `json_monolithic/expand.rs:90`, `json_monolithic_value/expand.rs:220`. Per-grammar calibration is AW-IV.W3.4 scope. |
| **ShapeRef runtime dispatch** | PARTIAL (CSS-only static, no consumer) | `SHAPE_DICT` is populated for CSS L4 with 13 entries at `.profiles/samply/prebuild/expand/css_l4/expand.rs:53290-53415` (`__SHAPE_DICT_TABLE[13]` + `SHAPE_DICT: &[::bbnf::runtime::tape::ShapeEntry] = &__SHAPE_DICT_TABLE;` at `:53416`). Empty for the other three emitted grammars: `json_monolithic/expand.rs:928`, `google_sheets_monolithic/expand.rs:2869`, `json_monolithic_value/expand.rs:1058` all show `SHAPE_DICT: &[...] = &[];`. `BBNF_SHAPE_DICT` is empty for every emitted grammar (`css_l4/expand.rs:53450` etc.). Consumer side is completely absent: `grep push_shape_ref` in `crates/bbnf-tape/src/driver.rs` returns **zero** hits; the only `push_shape_ref` call sites are in `crates/bbnf-tape/tests/tape_basic.rs` (unit-test constructions) and `crates/bbnf-tape/src/builder.rs:589` (the builder method definition itself). No walker dispatch consults `SHAPE_DICT` at compound-emit time. |
| **PHF + SIMD keyword** | NOT-FIRING | `keyword_tables: &[]` is hard-coded at `crates/core/src/backend/rust/emitter/profile.rs:144` and shows up as `&[]` in every emitted profile (`css_l4/expand.rs:345`, `google_sheets_monolithic/expand.rs:99`, `json_monolithic/expand.rs:95`, `json_monolithic_value/expand.rs:225`). No `NAMED_COLOR_PHF` / `FREQUENCY_ORDER` / `phf_map!` macro invocation appears anywhere in the 4 emitted bench artefacts. No `keyword_dispatch.rs` exists under `crates/core/src/backend/rust/emitter/` (see `ls` of that directory: `dta.rs`, `grammar.rs`, `mod.rs`, `prettify/`, `profile.rs`, `visitor.rs`). No `keyword_stats.rs` exists under `crates/ir/src/passes/recognizers/`. |
| **CSS selector classifier** | NOT-FIRING | No `selector_classifier.rs` at `crates/core/src/backend/rust/emitter/`. No 256-entry byte-to-selector-kind LUT emitted anywhere. `grep selector_classifier` over `crates/**` returns zero matches. AW-I.W2.7's structural bitmap (`AU.2.7` carry) populates `GRAMMAR_PROFILE.structural_alphabet` — for CSS L4 it is populated (`css_l4/expand.rs:341` references `__GRAMMAR_PROFILE_ALPHABET`) — but the classifier that reads the bitmap at compound-selector positions does not exist. `__compoundSelector` dispatch in the walker remains byte-level alt dispatch. |
| **Scanner closure (`active_columns`)** | NOT-FIRING | `active_columns: &[]` hard-coded at `crates/core/src/backend/rust/emitter/profile.rs:142`. Confirmed in every emitted profile: `css_l4/expand.rs:343`, `google_sheets_monolithic/expand.rs:97`, `json_monolithic/expand.rs:93`, `json_monolithic_value/expand.rs:223`. Scanner `PaddedView` migration has not landed — no `find_next_structural_from` paired migration (AW-IV.W2.4 scope). |
| **Bloom + GADT runtime dedup** | NOT-FIRING | `dedup_eligible_rules: &[]` hard-coded at `crates/core/src/backend/rust/emitter/profile.rs:147`. Confirmed in every emitted profile: `css_l4/expand.rs:348`, `google_sheets_monolithic/expand.rs:102`, `json_monolithic/expand.rs:98`, `json_monolithic_value/expand.rs:228`. No `dedup.rs` exists under `crates/bbnf-tape/src/`. No `dedup_eligibility.rs` nor `pattern_dedup.rs` under `crates/ir/src/passes/`. Walker's `push_compound` arm (`crates/bbnf-tape/src/driver.rs`) contains no bloom filter probe. |
| **Pratt generalisation** | PARTIAL (Sheets + BBNF self only; CSS `calc()` and BBNF `binary_factor`-at-grammar-use do not lift) | Detection pass is present at `crates/ir/src/passes/recognizers/dta.rs:391-402` (`collect_precedence_chain` dispatch inside `emit_rule`). Sheets has 2 `DtaState::ShuntingYard` + 2 `__DTA_SY_*_PREC` arrays emitted (`google_sheets_monolithic/expand.rs:995-1044` for the 6-op arithmetic + `:1961-1978` for the 2-op statement separator chain). BBNF-self also lifts: `crates/core/src/grammar/generated.rs:186-227` shows a 5-entry `__DTA_SY_82_PREC` for `+ - * / %`, and `:1202-1205` emits the corresponding `DtaState::ShuntingYard`. JSON (`json_monolithic`, `json_monolithic_value`) and CSS L4 have `shunting_yard_rules: &[]` (`json_monolithic/expand.rs:920`, `css_l4/expand.rs:53285`, `json_monolithic_value/expand.rs:1050`). **CSS `calc()` does not lift** despite the grammar (`grammar/css/l4/func-body.bbnf:40-41` defines the 2-rung chain `mathProduct = mathValue , (mathProductOp >> mathValue) *` / `mathExpr = mathProduct , (mathSumOp >> mathProduct) *`): the recognizer at `crates/ir/src/passes/recognizers/dta.rs:756-763` expects `IrNode::Seq(2-children)`, but the `>>` operator in BBNF lowers to `IrNode::Next(a, b)` (see `crates/ir/src/passes/recognizers/dta.rs:622`), which is NOT a `Seq` — so `match_operator_chain_rule` declines and the rule falls back to the deep Alt+Seq+Repeat tower (cf. the many `DtaState::Seq` / `DtaState::Repeat` / `DtaState::AltLinear` entries around `css_l4/expand.rs:52000-52100`). |
| **`reduce_column<C,R>` visitor API** | NOT-FIRING | No `reduce_column` method anywhere in `crates/bbnf-tape/**` (`grep reduce_column` returns zero runtime hits; only the emitter comment in `profile.rs` and `lib.rs`). `reorder_unroll_visitors: &[]` in every emitted profile (`css_l4/expand.rs:349`, `google_sheets_monolithic/expand.rs:103`, `json_monolithic/expand.rs:99`, `json_monolithic_value/expand.rs:229`). The codegen module `crates/core/src/backend/rust/emitter/visitor.rs:72` has `emit_visitor_kernels(&[])` short-circuit: `if visitors.is_empty() { return TokenStream::new(); }` — every grammar delivers an empty slice because no `@visitor` directive exists in any grammar. |

### Summary

Of 8 load-bearing AW-IV levers, **0 are fully firing**, **2 are partial** (ShapeRef has static data for CSS L4 but no runtime consumer; Pratt is detected for Sheets + BBNF-self but not propagated into CSS `calc()` or into JSON which has no operator chain), and **6 are not firing at all**. This matches AW-III's scoping ("most NOT-FIRING per AW-II close; optimisation is AW-IV's scope") — the levers emit their substrate slot as `&[]` through a single concentrated point at `crates/core/src/backend/rust/emitter/profile.rs:142-148`. Activating any lever means (a) populating that slot with a non-empty slice via an IR recognizer pass, and (b) landing a walker-side consumer that reads it.

## Part 2 — Precedence collapsing

**Is the precedence-collapsing IR pass actually collapsing expression rules? Where does it succeed, where does it fail, and why?**

### Where the pass lives

- **Detection** — `crates/ir/src/passes/recognizers/dta.rs:670-738` (`collect_precedence_chain`) + `:752-800` (`match_operator_chain_rule`) + `:818-870` (`extract_operator_set`).
- **Dispatch into lift** — `crates/ir/src/passes/recognizers/dta.rs:391-402`: for each rule, before lifting the body, the lifter probes for a chain head; on hit it allocates a single `DtaState::ShuntingYard { head, precedence }` and registers every chain rule as pointing at that state.
- **Gate** — detected chain must have **≥ 2 rungs** (`:696-698`) and **pairwise-disjoint dispatch bytes across rungs** (`:715-726`).
- **Runtime** — walker consumes `DtaState::ShuntingYard` via its dedicated arm; see `crates/bbnf-tape/src/driver.rs` (grep for `ShuntingYard` confirms presence at 8 sites inside the runtime loop).

### Where it succeeds

1. **Sheets `formula` / `statement`** — 2 ShuntingYard states emitted:
   - `__DTA_SY_73_PREC` (6 entries: `&`, `+`, `-`, `*`, `/`, `^`) at `google_sheets_monolithic/expand.rs:995-1044`; dispatch `:2250` (`DtaState::ShuntingYard { precedence: &__DTA_SY_73_PREC, ... }`).
   - `__DTA_SY_180_PREC` (2 entries: `;`, `,`) at `:1961-1978`; dispatch `:2659`.
   - `shunting_yard_rules: &__DTA_SHUNTING_YARD_RULES` at `:2861` (populated non-empty).

2. **BBNF-self bootstrap** — 1 ShuntingYard state emitted for the grammar's imported `value_expr` arithmetic operators:
   - `__DTA_SY_82_PREC` (5 entries: `+`, `-`, `*`, `/`, `%`) at `crates/core/src/grammar/generated.rs:186-227`; dispatch `:1202-1205`.
   - Corresponds to the `value_or → value_and → value_concat → value_unary → value_factor → value_atom` tower in `grammar/expressions.bbnf` (imported via `@import { value_expr, type_annotation } from "expressions"`).

### Where it fails (and why)

1. **CSS L4 `calc()` / `min` / `max` / `clamp`** — `grammar/css/l4/func-body.bbnf:40-41` defines the textbook 2-rung chain:
   ```
   mathProduct = mathValue , (mathProductOp >> mathValue) *
   mathExpr    = mathProduct , (mathSumOp  >> mathProduct) *
   ```
   But `mathExpr`'s body is lowered as `IrNode::Seq(Ref(mathProduct), Repeat(Next(Ref(mathSumOp), Ref(mathProduct))))` — the `>>` operator produces `IrNode::Next(a, b)` (see the enumeration at `crates/ir/src/passes/recognizers/dta.rs:622`). The recognizer at `crates/ir/src/passes/recognizers/dta.rs:756-763` asserts that the inner body's tail is `IrNode::Seq(_)`:
   ```rust
   let inner_stripped = strip_transparent_owned(tail);
   let inner = match inner_stripped {
       IrNode::Repeat { inner, lo: 0, hi: u32::MAX } => *inner,
       _ => return None,
   };
   // ... later uses `match_operator_chain_rule` with `IrNode::Seq(c)` required.
   ```
   The `Next` wrapper is not peeled by `strip_transparent_owned` (`:885-890` only peels `OptionalWhitespace` and `Map`), so the `match_operator_chain_rule` probe declines. Result: CSS L4 falls back to the deep nested-Alt tower (confirmed by `shunting_yard_rules: &[]` at `css_l4/expand.rs:53285` and the visible Seq/Repeat/AltLinear structure around `:52000-52100`).

2. **BBNF `binary_factor` (non-bootstrap path)** — the grammar itself is the self-hosted BBNF, so BBNF-self gets the Pratt lowering (as above), but `binary_factor = mapped_factor , (binary_operators ?w , mapped_factor) *` uses standard `,` separators (not `>>`) and has a **single rung** (`binary_factor`'s body is the operand; there is no `a_n = a_{n+1} (op a_{n+1})*` chain cascade). Detection at `:696-698` requires ≥ 2 rungs — so `binary_factor` alone does not qualify. In practice, the BBNF-self generated.rs lifts `binary_factor` as `DtaState::Seq(Ref, Repeat(Seq(Ref, Ref)))` (see the `DtaRuleId(14) → DtaStateId(82)` chain near `generated.rs:1206-1213`).

### Evidence of the deep-Alt tower in CSS

Walker dispatch for CSS `__compoundSelector` / mathExpr / value regions runs through `DtaState::AltLinear` + `DtaState::Seq` + `DtaState::Repeat`, not `DtaState::ShuntingYard`. Sampled lines `css_l4/expand.rs:52000-52100` show the classic lowered form with no precedence metadata — each operator is simply another Alt branch tried linearly.

### Bottom line

The Pratt lowering pass is a real, landed IR pass (detection + dispatch + runtime arm). It fires on 2 of the 4 compiled-from-grammar bench targets (Sheets, BBNF self) and **does not fire** on CSS L4 nor any JSON grammar. The CSS gap is load-bearing for perf on `bootstrap` / `tailwind` (every `calc()` / `min()` / `max()` value currently walks a deep Alt tower). AW-IV.W3.3 explicitly names Pratt generalisation to CSS `calc`/`min`/`max`/`clamp` as the fix; the blocker is that the `>>` (Next) wrapper sits between `Seq` and the inner operator pair, defeating `strip_transparent_owned`'s peel set.

## Part 3 — Dead / legacy code audit

Signal-to-noise assessment across four hand-audit markers.

### Counts by category

| Marker | Count | Primary locations |
|--------|-------|-------------------|
| `#[allow(dead_code)]` in `src/` | 3 | `crates/ir/src/passes/recognizers/dta.rs:1005, 1008, 1011` — three `_sentinel` functions forcing type-availability for `PushFingerprint` / `TypeDesc` / `StringId`. Load-bearing for compile but contribute no runtime code. |
| `#[allow(dead_code)]` in `tests/` + `examples/` + `benches/` | 15 | Every CSS L4 test file has a `mod css_types { pub fn parse_hex_color(...) { ... } }` stub — `tests/css_l4.rs:7`, `tests/css_l4_color_view.rs:34`, `tests/css_l4_parity.rs:40`, `tests/css_l4_named_color_parity.rs:12`, `tests/css_l4_dimensions.rs:17`, `tests/tape_parity.rs:50`, `tests/serialize_roundtrip.rs:45`, `benches/css/l4.rs:11`, `examples/test_l4.rs:4`. Plus `tests/sheets_parity.rs:69`, `tests/css_l4_parity.rs:108` (`collect_typed_leaves` helper), `benches/json/competitors.rs:151, 281, 395` (JsonValue enum variants for competitor harnesses). Load-bearing: every `css_types` module supplies host fns the generated parser references; the allow suppresses the inevitable "module imported but function X not called" warning in that specific test. |
| `#[allow(unused, ...)]` broad | 2 | `crates/core/src/grammar/mod.rs:24` (blanket allow over `generated.rs` — load-bearing, the auto-generated module contains large stretches of identifier-only code paths). `crates/egraph/src/rewrite.rs:57` (trait default `should_apply(...)` with unused params). |
| `// TODO` | 1 | `crates/core/src/grammar/host.rs:194`: `// TODO-postAW: collapse these structural-invisible Seqs at the lifter (or mark them transparent) so consumers don't re-peel.` — real future-work marker, not legacy. |
| `// XXX` / `// FIXME` | 0 | None. |
| `// legacy` / `// removed` / `// deprecated` | 5 comment sites | `crates/bbnf-tape/src/builder.rs:176` ("legacy fn-per-rule path"), `crates/bbnf-tape/src/dta.rs:27, 29` ("legacy fn-per-rule deletion"), `crates/bbnf-tape/src/finaliser.rs:94, 207` ("legacy fn-per-rule" + "post-order tapes (legacy fn-per-rule)"), `crates/core/src/lower/expression.rs:491` ("legacy fn-per-rule tape"), `crates/core/src/grammar/host.rs:200` ("legacy fn-per-rule emission"), `crates/core/src/backend/rust/emitter/grammar.rs:176` ("legacy fn-per-rule path"), `crates/core/src/backend/rust/emitter/dta.rs:37, 80, 90` (3 sites, "legacy fn-per-rule" references in doc comments). All are stale doc strings from before AW-I landed the DTA-primary path — the fn-per-rule path itself was **deleted**; these are documentation remnants, not shim code. |
| Files named `*_old.rs` / `*_legacy.rs` / `*_v1.rs` | 0 | Confirmed via `find crates -name '*_old.rs' -o -name '*_legacy.rs' -o -name '*_v1.rs'` — zero results. |
| `todo!()` / `unimplemented!()` | 0 in `crates/**/src/` | Zero `todo!()` or `unimplemented!()` macros. 10 `unreachable!()` marks exist but are classification-exhaustiveness assertions (8 in egraph-derive proc macros, 2 in bench harnesses). |

### Classification

- **(a) Load-bearing placeholders** — 3 sentinel fns in `dta.rs`; all `css_types` host-fn modules across tests/benches; the generated module's blanket allow; the 1 TODO in `host.rs:194`. Total ≈ 30 lines of markers plus ~200 lines of body code (the `css_types` modules repeat a ~15-line `parse_hex_color` across 7 files and several smaller stubs). Recommend DRY consolidation into a shared fixture — not delete.
- **(b) Genuinely dead** — 0 files identified. Every `#[allow(dead_code)]` encountered has a live-consumer justification or is a compile-time sentinel.
- **(c) Legacy shim (stale documentation referring to deleted paths)** — 11 comment lines across 7 files all referencing "legacy fn-per-rule", a path that was deleted in AW-I. These are stale prose in doc-comments (`//!` / `///` blocks), not live code. Total: **~11 lines of stale prose** that should be rewritten to say "pre-DTA" or dropped.

### Net line count

Genuinely dead code: **0 lines**. Stale-documentation legacy prose: **~11 lines** (all in doc-comments referring to the pre-DTA path). Load-bearing markers: **~30 attribute lines** + ~200 lines of replicated `css_types` stubs that want consolidation but are not dead.

## Part 4 — Direct-to-struct system audit

### Where it lives (and where it does not)

- `grep direct_to_struct` across `crates/**` — **zero matches**. No code module named `direct_to_struct` or `DirectToStruct` exists.
- `grep as_value` hits `crates/core/src/grammar/generated.rs` (many `as_value_<variant>()` accessors generated for BBNF-self view types) and `crates/analysis/src/state/diagnostics/ir_analysis.rs` (unrelated helper). No shared `as_json_value()` fast-path; no `JsonValue` materialisation API on the tape.

### What the system *is* in practice

The "direct-to-struct" terminology is AR/AS-era (see `docs/tranches/AR/critique.md`, `docs/tranches/AP/AP.md`) and refers to the View-layer code in `crates/core/src/backend/rust/view/`:

- **Short-circuit scalar projections** at view-read time: `leaves.rs` emits `.as_f64()`, `.as_u32()`, `.text()` per rule based on `TypeDesc`. Lazy, not eager.
- **Named-type projections** (AW.0.5): `crates/core/src/backend/rust/view/named_types.rs:52-80` and `color.rs`. `RustNamedTypes` resolves `TypeDesc::Named("Color")` / `TypeDesc::Named("ColorMix")` → `(U8, F64, F64, F64, F64)` (40-byte tuple layout). The emitter writes into `PayloadData::LargeAggregate` via the planner admission at `crates/ir/src/passes/payload/layout.rs:194-206` (`LARGE_PAYLOAD_MAX = 64` cap). The view-layer `color::Color::decode` reconstructs the typed struct.
- **Universal named-type fallback** (AW-II.W5c.1, landed at commit `4f030b15`): `Named("String")` / `"str"` / `"Bytes"` → `Tuple([U32, U32])` at `crates/ir/src/passes/payload/layout.rs:194-205`.

### Qualifying grammars

- **CSS L4**: `Color` / `ColorMix` named-type resolvers present at `crates/core/src/backend/rust/view/named_types.rs:77-89`. The `colorFunction` / `colorFn` / `colorMix` rules declare `-> input : Color` in `grammar/css/l4/color.bbnf`; the planner admits them at `LARGE_PAYLOAD_MAX`. **This is firing** — see AW-II.W5c.1 and earlier AW.0.5 work.
- **JSON** (two variants, `json_monolithic` + `json_monolithic_value`): no named-type resolvers; no direct-to-struct projection. Every value is projected through the view's `.as_<variant>()` discriminated accessors. There is no short-circuit "parse directly into `JsonValue`" path — the tape is always materialised first, and a consumer who wants `JsonValue` walks the view.
- **Sheets**: no named-type resolvers. The only direct-to-struct-like concept is `TypeDesc::Tuple(scalar_fields)` compiled to an `Aggregate` payload (≤ 16 bytes) via `plan_layout` at `crates/ir/src/passes/payload/layout.rs:139-144`.
- **BBNF-self**: same as Sheets — no named-type bypass; the AST is reconstructed through view-layer accessors.

### Is it firing for grammars that qualify?

**Yes — for CSS L4's `Color` / `ColorMix`.** The named-type resolver is active; the planner admits at `LARGE_PAYLOAD_MAX`; the view-layer `color::Color::decode` parses the 40-byte blob. Verified by the presence of `ColorPayloadBytes` in `view::color::COLOR_PAYLOAD_BYTES` and the re-exports in `crates/core/src/backend/rust/view/mod.rs:73-74`.

**No — for JSON**, but this is *by design* and not a regression: JSON's `value = object | array | string | number | ...` is a recursive Alt, and the runtime materialises a full tape precisely because the typed-view layer is the extraction surface. A "direct-to-struct" path for JSON would require either a separate eager projection codegen (a parallel subsystem — violates KISS) or speculative AoS layout (not in the plan).

### Why not emitted more broadly?

The named-type admission gate is `TypeDesc::Named(sid)` that the backend's `resolve_named` returns `Some(TypeDesc::Tuple(...))` for. Today that resolver has exactly 2 entries (`"Color"`, `"ColorMix"`). Three factors keep the list short:

1. **AU.4.2's stated architecture**: "codegen handles struct projections via per-backend type tables, not a central registry" (see the doc at `crates/core/src/backend/rust/view/named_types.rs:6-9`). Each entry is a conscious addition.
2. **Backend parity gate**: TS / WASM backends (noted at `:23-26`) have no `NamedTypeResolver` impl yet; adding CSS value types here without TS parity breaks the multi-backend invariant.
3. **The universal fallback** at W5c.1 already covers `"String"` / `"str"` / `"Bytes"` — the most common generic projections — without per-backend name tables.

## Part 5 — Type inference projection audit

End-to-end trace: grammar `-> Ty` annotation → IR TypeDesc → materialization → emitter payload writes → walker payload writes.

### The 5-step path

1. **Grammar** — `grammar/css/l4/color.bbnf` declares `hex_color_6digit = /#[0-9a-fA-F]{6}/ -> parse_hex_color(input) : u32`. Source: `.bbnf` file parsed by BBNF self-bootstrap.
2. **IR `TypeDesc`** — the constraint solver (`crates/ir/src/passes/types/constraint/`) infers `TypeDesc::U32` for `hex_color_6digit`. AW-II.W5c.1 fix at `crates/ir/src/passes/types/constraint/helpers.rs:143-153` (recursive `effective_payload_type` unwrapping `Tuple([Span, T])` chains) — **fix landed**; `join_types` preserves deeply-nested scalar payloads across the 148-branch CSS `namedColor` Alt join.
3. **Payload layout planning** — `crates/ir/src/passes/payload/layout.rs:128-206` (`compute_payload_layouts_with_resolver`) produces a `PayloadLayout` for rules whose `TypeDesc` is `Tuple(scalars)`, a scalar-Alt, a bare `Span`, or a `Named(_)` that resolves to a scalar tuple. The universal-fallback clause at `:194-205` closes the W5c named-type gap.
4. **DTA lifter** — `crates/ir/src/passes/recognizers/dta.rs:525` lifts `IrNode::Map { inner, .. }` by recursing into `inner` alone. **This is the open hole**: the `..` rest pattern discards `fn_id`, `type_annotation`, and any carried `PayloadKind`. The `DtaState::Regex` and `DtaState::Literal` variants at `crates/bbnf-tape/src/dta.rs:93-104` have **no `payload: PayloadKind` field**, so even if the lifter threaded through the type, the wire contract has no slot to carry it.
5. **Walker payload writes** — `crates/bbnf-tape/src/driver.rs:893-914` (`DtaState::Regex` arm) hard-codes `PayloadKind::F64`: `psi.push(PayloadJob::new(rec_idx, lo, *pos, PayloadKind::F64, 0));` at `:912`. The in-source comment at `:907-911` acknowledges the hole: `// Enqueue a PSI job — the PayloadKind classification is the emitter's responsibility; without a per-state kind annotation on the table we default to F64 as the most common numeric payload. The emitter-driven lowering in AW.1.2 threads the right kind through.` The `DtaState::Literal` arm at `:875-891` emits no `PayloadJob::push` at all — no payload bytes for any literal-matched rule. Aggregate / LargeAggregate paths continue to work because they run through `push_compound` not the Regex/Literal leaves.

### Is W5c's "partial fix" now fully sound?

**No.** W5c (commit `4f030b15` + `c7791075`) landed:

- Recursive Span-prefix unwrap in `effective_payload_type` (`helpers.rs:143-153`).
- Universal-name fallback for `Named("String")` / `"str"` / `"Bytes"` projecting to `Tuple([U32, U32])` (`layout.rs:194-205`).
- Span-text disambiguator for `bool_lit` / `int_lit` under sentinel `rule_kind` (`lower/expression.rs` updates around the `lower_map_arrow` body).

These fixes are **layout-pass soundness**. The remaining hole is between the layout pass and the wire. Specifically:

1. **Hole #1 (named in W5c scope-reveal)**: `crates/ir/src/passes/recognizers/dta.rs:525` — `IrNode::Map { inner, .. } => self.lift_node(inner)` strips the entire Map envelope wholesale. The `fn_id` (pointing at the host fn / MapExpr that classifies the payload kind) and the `type_annotation` (carrying the user-declared `: u32` / `: f64` / `: Bool`) are both discarded. Confirmed at `b58d1461`.
2. **Hole #2 (corollary to #1)**: `crates/bbnf-tape/src/dta.rs:93-104` — `DtaState::Regex { pattern: &'static str }` and `DtaState::Literal { text: &'static str }` have no `payload` field. Even if the lifter preserved the PayloadKind, there is nowhere to stamp it on the wire. Adding a `PayloadKind` field to both variants is the concrete W1 fix in AW-III's plan.
3. **Hole #3 (walker-side)**: `crates/bbnf-tape/src/driver.rs:912` — hardcoded `PayloadKind::F64` for every regex match.
4. **Hole #4 (walker-side)**: `crates/bbnf-tape/src/driver.rs:875-891` — `DtaState::Literal` arm has no payload emission at all. For a rule like `true_kw = "true" -> true : Bool`, nothing lands on the tape beyond the span.
5. **Hole #5 (Seq → KvPair promotion)**: `frame_to_tape_kind` at the walker does not promote a `Seq` compound to `KvPair` when the enclosing rule's layout is a 2-field KV shape. The existing AR.9 KV-pair detection lives in `crates/ir/src/passes/payload/layout.rs:137-143` (the `is_kv_pair_shape` gate) but the walker doesn't consume the decision. Named explicitly in the AW-III.W1 plan.

Each of these is producer-side (lifter/emitter/walker/wire) — consumer-side types and layouts are already well-formed. The 36 Cluster C payload-activation failures at AW-II close trace to this producer surface.

### Auxiliary observation — Literal payload absent at runtime

`emit_leaf` at `crates/bbnf-tape/src/driver.rs:1366-1394` writes only span metadata + a variant-stamp on `flags`; it never pushes a `PayloadJob`. Every `DtaState::Literal` arm at `:875-891` calls `emit_leaf` and then `advance_or_pop_with` — the payload pipeline is bypassed entirely. For scalar-literal rules (`true`, `false`, unit suffixes `px`, `em`, ... that project to `u8` discriminants), the tape carries no payload bytes. The 3 CSS percentage `InlineScalar` reader tests (AV.0.9 / AW-IV.W1.3 scope) are the surface-visible manifestation; the root cause is this architectural omission.

## Synthesis

### Highest-leverage remaining work

**The 20× bench regression's attribution sits on the dispatch baseline, not on un-activated optimisations.** The AW-III plan's architectural thesis (AW-III.md:37-53) is correct: every input byte visits ≥ 1 walker state; every transition costs ~10s of cycles (match dispatch + Frame update + PSI/counter bookkeeping + branch-predictor miss). Activating the 6 not-firing levers would each shave incremental %ages:

- **PSI rayon stage-B** (calibration only) — no real code work; a one-commit calibration pass + per-grammar `parallel_break_even_bytes` constant populates. Biggest win on **canada.json** (zero-sharing f64-heavy input that straight-line parallelises).
- **ShapeRef runtime dispatch** — static data exists for CSS L4; walker-side consumer missing. Delivering `push_shape_ref` in the compound-emit branch converts 13 CSS L4 recogniser patterns from multi-record Seq chains to single records. Biggest win on **bootstrap.css / tailwind.css** per record-count drop ≥ 30% target.
- **PHF + SIMD keyword** — no emitter pass exists; substrate slot is hardcoded `&[]`. Biggest per-branch gain but most implementation cost; touches ~148 CSS named colours + ~150 Sheets function names.

### What lever, if activated, would most reduce the 20× regression?

**Neither — the load-bearing work is the payload-activation closure (AW-III.W1) + the Pratt generalisation to CSS `calc()` (AW-III resequenced from AW-IV.W3.3).** The 6 AW-IV levers listed above collectively sum to a meaningful fraction of the regression but **each is amortising dispatch overhead**, not eliminating it. To close a 20× gap, the *structural* work is:

1. **Pratt lowering extended to `IrNode::Next`** (Part 2 evidence) — peel the `Next(a, b)` wrapper in `strip_transparent_owned` or add a matching arm to `match_operator_chain_rule`. CSS L4's mathExpr + mathProduct (and by extension every `min` / `max` / `clamp` body) collapses from a deep `Seq + Repeat + AltLinear` tower to a single `DtaState::ShuntingYard`. State count drops; dispatch depth drops; operator precedence becomes a byte-indexed LUT lookup.
2. **Complete producer-side payload wiring** (Part 5 holes #1-#5) — this is the AW-III.W1 wave's scope; closing it green would cut 36 test failures *and* restore the payload-activation short-circuit for every scalar leaf (InlineScalar, WideScalar, Bool, U8). The walker's payload path would then amortise the PSI enqueue cost across every leaf, not only the F64 subset.

Only after those structural items close should the AW-IV levers be activated — otherwise the levers are layered over a baseline that is still architecturally incorrect.

### What is architecturally incorrect (beyond un-activated)?

1. **`IrNode::Map { inner, .. }` wholesale strip in DTA lifter** (`dta.rs:525`) — the Map envelope carries the type annotation that informs payload routing. Discarding it at lift time forces the walker into a hard-coded F64 default for every regex leaf. This is not un-activation; it is a type-system correctness hole.
2. **`DtaState::Regex` / `DtaState::Literal` have no `payload: PayloadKind` field** (`dta.rs:93-104`) — even if the lifter preserved the PayloadKind, the wire contract provides no slot for it. A fully-typed walker requires extending the DtaState enum.
3. **Walker `DtaState::Literal` arm emits no payload** (`driver.rs:875-891`) — every literal-dispatched scalar (true/false/unit-suffix) lands on the tape as bare span with no discriminant bytes. This is a silent data-loss path that consumers work around by peeking span text at view-read time.
4. **`strip_transparent_owned` doesn't peel `Next`** (`dta.rs:885-890`) — architecturally the `>>` operator is a parser-combinator "run A, discard B, return A" pair, structurally equivalent to a constrained `Seq`. Failing to peel it breaks the Pratt recogniser on every grammar that uses `>>` for operator pairing. CSS L4 pays the full cost.
5. **`SHAPE_DICT` emitted without consumer** (CSS L4 has 13 entries at `css_l4/expand.rs:53290-53415` that are never read) — this is an architectural half-landing: emitter side done at AV.5.6 / AV.6.1-6.3, walker side never wired. Either activate the walker consumer or drop the emitter pass until the consumer is ready. Carrying emitted-but-unconsumed data is the classic orthogonal-codepath smell.

### Bottom line

Of the 8 enumerated AW-IV levers, **6 are fully un-activated and will remain so until AW-IV opens; 2 are partial (ShapeRef partial-CSS, Pratt partial-Sheets/BBNF)**. The precedence-collapsing pass is real and fires where its shape gate admits (Sheets, BBNF-self), but fails on CSS `calc()` because of a one-wrapper peel gap. The dead-code audit is clean (0 lines genuinely dead, ~11 lines of stale doc-prose). The direct-to-struct system is CSS-Color-only by design; no JSON fast-path exists (nor should, under KISS). The type-inference projection has W5c's layout-pass fix in place, but the producer-side wire (lifter → DtaState → walker → tape) has five concrete holes that AW-III.W1 explicitly names and plans to close — closing them is both correctness (36 failing tests) and performance (full payload pipeline activation). **The highest-leverage wins are structural (Pratt extension to `Next`; payload wire completion), not lever-activation.**
