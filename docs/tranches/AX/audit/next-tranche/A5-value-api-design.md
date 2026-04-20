# A5 — Value API Apples-to-Apples Design Audit

Read-only audit. Baseline HEAD `9074a685` (master). Scope: characterise
bbnf's current Value / view surface, compare with sonic-rs,
lightningcss, and cssparser, and decide what "apples-to-apples" means
for next-tranche benchmarks — in light of AX invariant 21.

Nothing in this doc recommends adding a hand-coded `bbnf::json::Value`.
Every proposal routes through the shape emitter.

## 1. Current bbnf Value surface

### 1.1 `Parsed<'p, R>` — the ownership seam

`crates/core/src/runtime/parsed.rs:76-87` defines:

```text
pub struct Parsed<'p, R> {
    tape: Tape,               // owned
    input: &'p str,           // borrowed
    root_offset: TapeOffset,
    _root_marker: PhantomData<R>,
}
```

The ownership contract is `(Tape, &'p str)`, NOT `(Tape, Arc<str>)`.
The prompt's `Arc<str>` framing is not in tree; the input is a plain
`&'p str` borrow scoped to the caller's buffer (parsed.rs:8-13).

Public surface (parsed.rs:95-137):

- `new(tape, input, root_offset) -> Self` (line 95)
- `tape() -> &Tape` (line 106)
- `input() -> &'p str` (line 112)
- `root_offset() -> TapeOffset` (line 118)
- `into_tape() -> Tape` (line 124)
- `view() -> R::View<'_>` (line 135) — resolves root view via `Root`
  GAT in `Parsed<'p, R: Root>`.

The `Root` trait (parsed.rs:53-65) is the per-grammar binding; the
GAT `type View<'p>` is wired by `generate_views` to the grammar's
root-rule `<Rule>View<'p>` struct (view/mod.rs:344-363).

### 1.2 `<Grammar>NodeView<'p>` + per-rule `<Rule>View<'p>`

Both are zero-sized wrappers around `(TapeCursor<'p>, &'p str)`
(view/mod.rs:283-286, 301-304). A view is `Clone + Copy`; a
`TapeCursor` is effectively `(&'p Tape, TapeOffset)`. No allocation;
no decode; no materialisation until an accessor is called.

Universal accessors emitted by `emit_common_accessors` on every view
(view/mod.rs:176-266):

| Method | Return | File:line |
|---|---|---|
| `new`, `from_cursor` | `Self` | mod.rs:182-200 |
| `cursor` | `TapeCursor<'p>` | mod.rs:203 |
| `input` | `&'p str` | mod.rs:208 |
| `kind` | `TapeKind` | mod.rs:211 |
| `span` | `(u32, u32)` | mod.rs:216 |
| `span_text` | `&'p str` | mod.rs:219 |
| `variant_idx` | `u8` | mod.rs:225 |
| `rule_kind` | `<Grammar>RuleKind` | mod.rs:230 |
| `children` | `impl Iterator<Item = NodeView<'p>>` | mod.rs:239 |
| `child(i)` | `Option<NodeView<'p>>` | mod.rs:246 |
| `is_recovered` | `bool` | mod.rs:251 |
| `identifier_span` | `parse_that::Span<'p>` | mod.rs:260 |

The rule-kind discriminator enum is emitted per-grammar with one
variant per non-transparent rule, plus a sub-variant tail for
heterogeneous-alt coercion (view/mod.rs:114-167, 313-319).

### 1.3 Typed per-kind accessors

Dispatched in `emit_typed_accessors` (view/mod.rs:392-447) after
peeling `Map`/`OptionalWhitespace` wrappers. Routing:

- **Leaves** (`view/leaves.rs:29-134` for scalar, :145+ for aggregate)
  — `.text()` (line 39), `.byte_range()` (line 118), plus:
  - `TypeDesc::Span` → `.value()` / `.as_span()` → `&'p str`
    decoded from payload (leaves.rs:47-69).
  - Scalar payload (`is_scalar_payload()`) → `.value()` +
    `.as_<rust_ident>()` reading the typed `tape.payload_<T>(rec)`
    with a span-parse fallback (leaves.rs:70-108).
  - Aggregate payload (`PayloadLayout` present, non-KV) →
    `emit_aggregate_accessors` reads `layout.total_bytes` via
    `tape.payload_bytes` and projects typed fields with
    `<T>::from_le_bytes` at recorded offsets (leaves.rs:145+).
  - KV-pair shape `[Span, scalar]` → `emit_kv_pair_accessors`
    (mod.rs:413-414, delegates to seq.rs).

- **Seq** (`view/seq.rs:19+`) — `.child_N()` positional + named
  accessors derived from `Ref` targets (seq.rs:47-66). Named
  accessor elides when the `Ref` target is transparent.

- **Alt** (`view/alt.rs:27-`) — per-branch `.as_<variant>()` +
  `.is_<variant>()` predicates dispatched on `meta_idx`
  (alt.rs:68-81). When every branch is payload-eligible, a
  `<RuleName>Value` enum is **already emitted** via AQ.6.C (alt.rs
  header comment lines 8-15), with a `.value()` returning the
  chosen branch's typed value — this is a *grammar-derived* typed
  enum scoped to a single Alt rule, not a hand-coded tree.
  Invariant 21 admits this; the enum is emitted by the shape
  emitter, not hand-written.

- **Repeat** (`view/repeat.rs:17-61`) — `.iter()`,
  `.len()`, `.is_empty()`, `.get(i)`. Element view type resolves
  through `resolve_elem_view` (repeat.rs:68-82): `Ref` target
  name when non-transparent, else `NodeView`.

### 1.4 Serialize + prettify

`generate/serialize/mod.rs:15-105` emits `serialize_<rule>` functions
that default to `__ser.text(__v.span_text())` (line 38), plus a
`__dispatch_serialize` that routes on `variant_idx` (lines 47-59),
plus entry points `serialize_compact` (line 76) and `serialize`
(line 81). Output is byte-identical to the source span by
construction — round-trip parity is fundamental to the emitter.

`#[parser(prettify)]` rewrites each rule's generated parser as an
`__<rule>_prettify` variant (generated.rs:22669+) composing pprint
groups. Mature at 295 rules × 4 grammars per W1r.6 typed-accessor
audit; `@pretty` directives at Sheets' `sep(X)` + CSS L4 list rules.

### 1.5 Invariant 21 restatement

From `docs/tranches/AX/AX.md:44`:

> **Grammar-derived view surface.** No hand-coded AST enum
> duplicates grammar structure. The user-facing AST is `NodeView<'p>`
> + `TapeCursor<'p>` + per-rule typed accessors emitted by the shape
> emitters from IR's `TypeDesc` inference, composed with
> `#[parser(serialize)]`-derived `serialize_compact` and
> `#[parser(prettify)]`-derived `_prettify` surfaces.
> External-comparator parity (sonic-rs, lightningcss, simdjson,
> serde_json, cssparser) holds via canonical-serialization byte
> equality on both sides — no `From<T>`, `PartialEq<T>`, or
> hand-written adapter module where `T` is a third-party type.

Key terms: "hand-coded", "duplicates grammar structure",
"adapter module". Grammar-emitted enums scoped to one rule — like
AQ.6.C's `<RuleName>Value` — are not hand-coded and do not
"duplicate grammar structure"; they *are* the grammar structure,
projected from IR.

## 2. sonic-rs API surface

Characterisation from sonic-rs's public API (comparator used in
`tests/json_canonical_parity.rs:46-51` and `benches/json_competitors`).

- `from_str::<Value>(src) -> Result<Value, Error>` — **eager**
  materialisation. `sonic_rs::Value` is a tagged-union enum with
  Object / Array / Number / String / Bool / Null variants. Allocates
  an owned tree; unescape runs eagerly; numeric decode runs eagerly.
- `get_by_path(src, path) -> Result<Value, Error>` — **lazy** tape
  scan. Walks sonic-rs's internal SIMD-scanned index without
  materialising the full tree; returns only the addressed sub-value.
  Closest semantic twin to `bbnf::Parsed` + `NodeView::child(i)`.
- `to_object(src) -> Result<Object, Error>` — **on-demand** typed
  projection of the root object.
- `to_string(&Value) -> Result<String>` — canonical emission (no
  whitespace outside strings). `json_canonical_parity.rs:48`
  establishes this as the shared bytes-normalised oracle.

sonic-rs's internal architecture (SIMD scan + arena-allocated DOM
with fused unescape) is one-pass: the tape IS the DOM, because the
DOM nodes point into the tape.

## 3. lightningcss API surface

- `StyleSheet::parse(src, options) -> Result<StyleSheet, Error>` —
  **eager** owned AST tree. `StyleSheet` holds owned rule vectors;
  every declaration, selector, at-rule is a concrete Rust enum /
  struct with owned fields. No cursor API.
- `stylesheet.to_css(options) -> Result<ToCssResult, Error>` —
  canonical text emission. `PrinterOptions { minify: false }` still
  performs arithmetic simplification (`calc()` evaluator),
  position-pair commutativity, multi-value shorthand reordering
  (next-tranche recap 00-session-recap.md:62-66).

No lazy-cursor API. No streaming interface. The shape is
"parse → owned tree → consumer walks tree".

## 4. cssparser API surface

- `Parser<'_>::next_including_whitespace() -> Result<&Token<'_>,
  ParseError>` — **token-level** streaming parser. No high-level
  AST. Every consumer builds its own tree on top of the token
  stream; servo-stylo and lightningcss both layer above cssparser.
- `Token<'_>` is an enum: Ident, AtKeyword, Hash, String, Number,
  Dimension, Percentage, WhiteSpace, Comment, Function, ParenBlock,
  …

The closest bbnf twin is `TapeCursor::children()` iteration over
scanner-emitted leaves — but bbnf is already one level above this:
the tape encodes parsed rule records, not raw tokens.

## 5. What is apples-to-apples?

Three distinct comparator lanes, each with different semantics:

| Lane | bbnf side | sonic-rs side | Status | Invariant 21 fit |
|---|---|---|---|---|
| **L-canonical** | `Parsed` + `serialize_compact` | `from_str::<Value>` + `to_string(&Value)` | **Live** (W1r.2) | Fits: canonical-bytes parity, not adapter |
| **L-lazy** | `Parsed` + `NodeView::child` / `rule_kind`-dispatch walk | `get_by_path(src, path)` | **Not landed** (W1r.7 measured AoS vs SoA on bbnf side only) | Fits: both sides lazy-decode their native tape |
| **L-eager** | `parsed.to_value::<T>()` (NOT IMPLEMENTED) | `from_str::<Value>` | **Blocked by emission** | Conditionally fits — see §6 |

`crates/core/benches/json_competitors` (per 03-value-api-json-perf.md
§2) currently measures bbnf's `parse()` against sonic-rs's
`from_str::<Value>()`. This is asymmetric work: bbnf produces only a
tape + view surface; sonic-rs produces a materialised tree. The
5.5–8.2× slowdown reported at 03-value-api §2 includes this
asymmetry, unquantified.

The W1r.7 bench `twitter_lazy_field.rs:199+` measures bbnf's
`NodeView` walk in isolation (AoS vs SoA random-access deltas) but
has no sonic-rs comparator. Symmetric comparator is missing.

## 6. Design: materialised-tree Value API

Proposal: emit a grammar-derived `<Grammar>Value` enum as a Rust
sibling of `<Grammar>NodeView`, constructed by `parsed.to_value()`
via O(n) tape walk.

Shape, from IR's `TypeDesc`:

- One variant per non-transparent rule (same enumeration as
  `<Grammar>RuleKind` at view/mod.rs:134-167).
- Payload per variant = `TypeDesc::to_rust_owned_type()`:
  - `Span` → `String` (owned) or `Box<str>`.
  - Scalar → the scalar (`f64`, `u32`, `bool`, …).
  - Tuple → named-field struct with per-field owned types.
  - Rule reference → `Box<<Grammar>Value>`.
  - Repeat → `Vec<<Grammar>Value>`.
  - Alt with all-scalar branches → the AQ.6.C already-emitted
    `<RuleName>Value` enum (reused, not duplicated).

Materialisation cost: O(tape-records) with one allocation per
compound. Payload decode reuses `leaves::emit_aggregate_accessors`'
`<T>::from_le_bytes` reads — no second decoder.

### 6.1 Invariant 21 resolution

Invariant 21 forbids "hand-coded AST enum duplicates grammar
structure" and "`From<T>`, `PartialEq<T>`, or hand-written adapter
module where `T` is a third-party type". The proposed emitter:

- **Not hand-coded**: emitted by a new `view/value.rs` sibling that
  consumes IR's `TypeDesc` inference — same substrate as
  `view/leaves.rs`, `view/seq.rs`, `view/alt.rs`.
- **Not a duplicate**: the enum variants enumerate the grammar's
  rules, not a third-party DOM's variants. For JSON that happens
  to parallel sonic-rs's shape, because JSON's grammar IS object /
  array / number / string / bool / null — but the enum is derived
  from `json.bbnf`'s rule set, not copy-pasted from `sonic_rs::Value`.
- **Not an adapter**: there is no `From<sonic_rs::Value>`,
  `PartialEq<sonic_rs::Value>`, or bridge module. Comparator parity
  flows the same way W1r.2 established: via `serialize_compact`
  (now additionally via `to_value` when the benchmark calls for
  an eager comparator).

The guard-rail: **the emitter reads from IR, not from a comparator
schema**. If CSS L4's `colorFunction`, `colorMix`, `colorFn` rules
shape `<CssL4Value>::ColorFunction`, `ColorMix`, `ColorFn`, that is
grammar-derived. If they shape `<CssL4Value>::Color { r, g, b, a }`
to match `lightningcss::values::color::CssColor`, that is a hand-
coded adapter — forbidden.

### 6.2 Counterargument, weighed

One could argue: "every `Value` enum IS an AST enum, and invariant
21 says no hand-coded AST enum duplicates grammar structure — so
emission IS an AST enum per rule." The invariant text literally
says "hand-coded", not "AST enum" without modifier. The AQ.6.C
emission of per-Alt-rule `<RuleName>Value` enums already exists at
alt.rs (header comment lines 8-15). If AQ.6.C's per-rule enum is
admitted, a grammar-scoped enum that aggregates them is admitted
by the same reasoning, since it is emitted by the same pass
reading the same IR.

## 7. Design: on-demand path projection

Alternative / complementary to §6. Emit `parsed.get::<T>(path) -> Option<T>`
where `path` is a const string (compile-time or runtime).
Semantics: O(lookup) via binary-search on the AoS sidecar's span_lo
column (W1.D populated `Columns::packed_cache`). Matches sonic-rs's
`get_by_path` contract; no full materialisation.

Design note: the AoS sidecar is **already populated** on first
random-access read in the W1r.7 bench (twitter_lazy_field.rs:199-228).
`parsed.get::<T>(path)` would route through the same infrastructure,
projecting the accessed record's typed value via the existing
`.value()` accessor. Low marginal code; high return as a
sonic-rs-parity lane.

## 8. Recommendation

**Pursue all three lanes, staged.** Each measures a different cost,
each fits invariant 21 under the emission-from-IR discipline.

1. **L-canonical (done):** `json_canonical_parity.rs` is the
   template. Extend to CSS L4 + Sheets + BBNF with the same
   symmetric-normaliser pattern. Not a new bench — a CI gate.

2. **L-lazy (short-term, W1r.7 extension):** Add a sonic-rs
   comparator lane to `twitter_lazy_field.rs`. sonic-rs side uses
   `get_by_path(src, "statuses[].text")` iteration; bbnf side is
   already written. This reveals the true lazy-decode ratio,
   decoupled from materialisation.

3. **L-eager (next-tranche scope):** Emit `<Grammar>Value` +
   `parsed.to_value::<T>()` via `view/value.rs`. Add an
   `eager_value` bench lane matching bbnf's `to_value::<Json>()` vs
   `sonic_rs::from_str::<Value>`. Same-shape work on both sides.
   Hard gate: within **3×** of sonic-rs on canada / twitter /
   citm / data / data_xl — tighter than W1.6's baseline floor
   once the §1 / §2 levers in 03-value-api (walker retirement +
   finaliser fusion) land.

Real users: lazy cursor for hot-path consumers (the tape-first
substrate's raison d'être); eager for ergonomics-seekers and
cross-language FFI (Python bindings need owned trees). Both
surfaces are legitimate; neither is optional once the comparator
story is honest.

## 9. Proposed next-tranche scope (AY candidate)

Per recap 00-session-recap.md:212-236 item 7 (Value API
materialisation, medium priority — elevated by this audit to
high given §8's symmetric-comparison imperative):

| Item | Where | Scope |
|---|---|---|
| Emit `<Grammar>Value` enum | `crates/core/src/backend/rust/view/value.rs` (new) | One variant per non-transparent rule; typed payload from `TypeDesc` |
| Emit `parsed.to_value()` | `crates/core/src/runtime/parsed.rs` additional impl | O(tape) walk, one allocation per compound |
| Emit `parsed.get::<T>(path)` | same + `view/path.rs` (new) | Binary-search on `packed_cache.span_lo`, typed return via existing `.value()` |
| L-lazy bench | `crates/core/benches/json_lazy_field.rs` (new) | bbnf `NodeView` walk vs sonic-rs `get_by_path` on twitter |
| L-eager bench | `crates/core/benches/json_monolithic_value.rs` (new) | bbnf `to_value::<Json>()` vs `sonic_rs::from_str::<Value>` |
| Hard gate | bench assertion | bbnf `to_value` within 3× sonic-rs post-W0b-walker-retirement |

Non-scope (per invariant 21): any `From<sonic_rs::Value>`,
`PartialEq<sonic_rs::Value>`, `bbnf-to-lightningcss` adapter
module, or per-comparator bridge. Canonical-serialization byte
equality remains the parity mechanism.

## 10. Artefact citations

- `crates/core/src/runtime/parsed.rs:53-138` — `Parsed<'p, R>` + `Root` GAT.
- `crates/core/src/backend/rust/view/mod.rs:176-374` — universal
  accessors, `<Grammar>NodeView`, `<Grammar>RuleKind`, root binding.
- `crates/core/src/backend/rust/view/mod.rs:392-447` — typed-accessor
  dispatch by peeled-body shape.
- `crates/core/src/backend/rust/view/leaves.rs:29-134` — scalar +
  Span leaf accessors; :145+ — aggregate payload accessors.
- `crates/core/src/backend/rust/view/seq.rs:19-66` — positional +
  named Seq child accessors.
- `crates/core/src/backend/rust/view/alt.rs:1-81` — per-branch
  `as_/is_` accessors; lines 8-15 document AQ.6.C's per-Alt
  grammar-emitted `<RuleName>Value` enum.
- `crates/core/src/backend/rust/view/repeat.rs:17-82` — Repeat
  iter/len/get + element-view resolution.
- `crates/core/src/generate/serialize/mod.rs:15-105` —
  `serialize_compact` emission via `span_text()` dispatch.
- `crates/core/src/backend/prettify/mod.rs:1-16` — prettify
  analysis/plan/sep_rewrite/types substrate.
- `crates/core/tests/json_canonical_parity.rs:29-56` —
  L-canonical harness template.
- `crates/core/benches/twitter_lazy_field.rs:199-228` — W1r.7
  AoS random-access kernel; L-lazy bbnf-side written, sonic-rs
  comparator missing.
- `docs/tranches/AX/AX.md:44` — invariant 21 full text.
- `docs/tranches/AX/audit/next-tranche/00-session-recap.md:212-236` —
  deferred-items ledger + Value API materialisation flagged.
- `docs/tranches/AX/audit/next-tranche/03-value-api-json-perf.md:152-190`
  — §4 gap analysis + §5 L3 levers (walker retirement, finaliser
  fusion, materialised-tree Value).
