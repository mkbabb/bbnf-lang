# A10 — Value API Deep Refinement (Concrete Emission Design)

Read-only audit. Baseline master HEAD `851aaebc`. Extends A5
(`A5-value-api-design.md`) with concrete emission shape, materialisation
strategy, bench lane shape, and corrections to the AY.W3 draft. A5
established three lanes (canonical / lazy / eager) and resolved
invariant 21 at the level of principle; A10 produces the emission and
API surface the implementation wave will ship.

## 1. Nine deep questions, answered

### 1.1 `<Grammar>Value` variant enumeration — grammar-rule count vs
comparator shape

Per-grammar rule counts (counted from the `.bbnf` corpus):

| Grammar | Non-transparent rule count (upper bound) |
|---|---:|
| JSON (`grammar/json/json.bbnf`) | 10 |
| BBNF (`grammar/bbnf/bbnf.bbnf`) | 31 |
| EBNF / BNF | 12 / 5 |
| Sheets (`grammar/google-sheets/google-sheets.bbnf`) | 39 |
| CSS L4 (`grammar/css/l4/*.bbnf` × 15 files) | ~275 (before pruning) |

The session recap (`00-session-recap.md` §2.1) records post-prune live
counts: CSS L4 lands 15 types post-pipeline, Sheets 31. So the
CSS L4 enum at emit time is ~15 variants, not 275 — the IR pipeline
collapses transparent + inline rules. Still, a 15-variant
`CssL4ParserValue` (`stylesheet`, `rule`, `declaration`, `selector`,
`atRule`, `mediaRule`, `keyframesRule`, `blockContent`, `value`,
`colorFn`, `string`, `number`, `ident`, `whitespace`, `comment`) is
legitimately grammar-shape.

The critical architecture question — **one variant per grammar rule vs
generic Object/Array/Leaf DAG** — resolves in favour of
**per-rule variants, with grammar-rule-bundle collapse for
structurally-identical rule families**. Two supporting arguments:

1. **Invariant 21 disposition.** Invariant 21 (AX.md:44) admits
   "grammar-derived" and rejects "hand-coded AST enum duplicates". A
   generic `Object(Vec<(Key, Value)>) | Array(Vec<Value>) | …` DAG is
   shape-borrowed from JSON/sonic-rs; applied to CSS L4 or Sheets it
   forces the emitter to project every rule onto Object/Array/Leaf,
   which *is* "duplicates grammar structure" inverted — flattening
   grammar rules into a generic shape. Rule-keyed variants project
   from the rule set directly.

2. **Typed-accessor parity.** `view/alt.rs:137-146` already emits
   `<RuleName>Value` for payload-eligible Alts (AQ.6.C). The aggregator
   enum is the natural sum of those per-rule projections; going
   generic would require a second lowering layer that re-flattens
   those AQ.6.C typed enums into Object/Array holes.

**JSON case bridges coincidentally.** JSON's grammar rules
(`object`, `array`, `string`, `number`, `true`/`false`, `null`)
happen to be isomorphic to sonic-rs's `Value`. So
`JsonParserValue::{Object, Array, String, Number, Bool, Null}` emerges
from the grammar and matches sonic-rs's shape. That is convergence
between "what JSON is" and "what sonic-rs encodes", not adapter
coincidence. Compare CSS L4: `CssL4ParserValue::{Stylesheet(Vec<Rule>),
StyleRule{selectors, declarations}, AtRule{…}, ColorFn(Color), …}`
bears no resemblance to any competitor's AST — precisely because CSS
L4's grammar is sui generis.

**Rule-family collapse** handles the sonic-shape concern cleanly:
when N rules share an identical projected `TypeDesc`, they collapse
to one variant. CSS L4's `number_int` / `number_float` / `percentage`
all project `f64`; they could share `CssL4ParserValue::Number(f64)`
rather than three variants. The emitter's existing `TypeDesc`
equivalence class is the mechanism. See §2 emission algorithm.

### 1.2 Materialisation strategy

Three alternatives weighed against bbnf's constraints (`Parsed<'p, R>`
already owns `Tape` + borrows `&'p str`, `parsed.rs:76-87`):

- **Arena (`bumpalo`-style)** — contiguous bump, cheap allocation,
  cheap drop, lifetime-scoped. Fits the existing
  `Parsed<'p, R>` lifetime story: a `parsed.to_value_in(&'v bump) -> &'v
  GrammarValue<'p>` would let the value share both the input lifetime
  and a caller-supplied arena. Zero individual allocations.
- **Box-owned (`Vec<T>` / `Box<T>` / `String`)** — standard Rust;
  every compound is a heap allocation + one per Vec growth cycle.
  sonic-rs's public `Value` presents as this shape but internally is
  (c) below. lightningcss's `StyleSheet` is genuinely this shape;
  profiling shows its parse-to-tree dominates allocator time.
- **Borrowed + `&'p str`** — references input; escape-free strings
  zero-copy; escape-bearing strings need a side arena. Matches the
  existing `view/leaves.rs:55-62 .value()` contract (`&'p str` from
  `payload_Span`).
- **Handle-into-Document (sonic-rs's actual architecture,
  `value_cursor.rs` archival in `crates/json-prototype/`)** —
  `Value` is a 24-byte tagged handle `{tag, offset, len}` pointing
  into a single `Document { nodes: Vec<Value>, arena: Vec<u8> }`.
  One allocation per document, O(1) per variant, cache-friendly
  packed storage.

**Recommendation: (d) Handle-into-Document, reusing the existing
Tape + AoS `packed_cache` as the Document.** Rationale chain:

1. bbnf's tape already IS a flat packed node stream with
   `(kind_meta, span_lo, span_hi, child_off, sib_skip, extra)`
   per record (`Columns` SoA at `crates/tape/src/columns.rs`). The
   AoS `packed_cache` hybrid (W1.D, populated lazily per
   `twitter_lazy_field.rs:206`) is *already* the 32-byte aligned
   parallel array sonic-rs calls "packed nodes".
2. `parsed.to_value::<T>()` need not materialise a second tree; it
   can materialise a **typed handle** that delegates reads back to
   the tape. The handle IS a `<Grammar>Value` enum; its payload
   variants carry `TapeOffset` + pre-decoded scalars.
3. The materialisation cost collapses to (a) one
   `packed_cache()` warm-up (already paid on first AoS access;
   ~170µs on twitter) plus (b) per-compound `Vec<...>`
   pre-sizing so the enum's aggregate variants (`Array(Vec<Value>)`)
   don't re-walk the tape.
4. Escape-bearing strings already have arena-backed
   `payload_string_with_source` on the tape (`json/value.rs:84`).
   The `<Grammar>Value::String` variant holds
   `StringPayload { borrow: u64, len: u32 }` with the high-bit tag
   convention mirroring `json-prototype/src/value.rs:81-121`. No
   second arena.

**Counter-argument:** handle-into-Document is more complex than
`Box<T>` / `Vec<T>` owned trees for CSS L4 / Sheets, where the
typed enums carry nested grammar-specific structs (ColorFn with
`(u8, f64, f64, f64, f64)`). Response: CSS L4's aggregate payloads
are already arena-backed (`LargeAggregate` 40 B slot via
`leaves::emit_aggregate_accessors`). The handle shape falls out of
that existing substrate.

**Owned-Vec fallback for compounds.** `Vec<<Grammar>Value>` for
Array/Repeat remains `Box`-owned (one allocation per compound);
scalars are inline in the handle. This is sonic-rs's actual
internal shape (`Document.nodes: Vec<Value>` is the Vec; per-node
handles are inline).

### 1.3 sonic-rs's value architecture (read from the archival
prototype, `crates/json-prototype/src/value.rs`)

Key architectural facts (mirrored in the prototype, isomorphic to
sonic-rs 0.5.8 per `Cargo.lock:3056-3076`):

- `Value` is a 24-byte `#[derive(Clone, Copy)]` enum.
  Scalars (Null, Bool, Number) inline. Strings carry a
  `StringSpan { loc: u64, len: u32 }` where the high bit of `loc`
  distinguishes input-borrow from arena-offset.
  Compounds carry a `NodeSpan { start: u32, subtree_len: u32,
  entries: u32 }` pointing into `Document.nodes`. Reading a compound's
  children walks `nodes[start..start + subtree_len]` and advances by
  `1 + subtree_len` on nested compounds.
  (`value.rs:39-57`)
- `Document` owns `nodes: Vec<Value>` + `arena: Vec<u8>`. Compound
  children live **inline in the nodes vector in pre-order**.
  (`value.rs:166-181`)
- Materialisation is one pass via visitor (`value.rs:244+
  parse_json<V: GrammarVisitor>(src, &mut visitor)`). The visitor
  pushes into `Document.nodes` during parse; string bytes either
  point into input (borrow) or get copied into arena (escape).
- No UTF-8 validation on the hot path.

**The fast "from_str::<Value>" is this shape.** bbnf's tape is
structurally the same (flat packed, O(tape-length) children-walk).
The gap is not architectural; it is (a) sonic-rs's SIMD structural
scan amortising several input bytes per token, and (b) sonic-rs's
one-pass emission vs bbnf's parse + finalise double-pass (A1 §4.2,
~25% of every JSON fixture's self-time).

**Implication for `to_value`:** if bbnf reuses the tape as the
Document, the materialisation cost is dominated by the typed enum
construction, not by a second tree allocation. The parse-to-value
gap converges to the parse-only gap (currently ~5.5-8.2×; tape
hot-path levers L1+L2 in A1 §5 target pushing this toward ~3×
post-AY). Handle-into-Document requires ZERO additional allocation
beyond the existing tape + arena; a grammar-derived `<Grammar>Value`
enum is pure decode at read time.

### 1.4 lightningcss's value architecture

Owned tree: `StyleSheet { rules: Vec<CssRule>, ... }` where every
`CssRule`, `Declaration`, `Selector`, `AtRule` variant is a typed
struct with owned fields (`String`, `Box<T>`, `Vec<T>`). Every
variant allocated. Round-trip via `stylesheet.to_css(opts)` walks
the tree and performs semantic canonicalisation
(`calc()` evaluator, position-pair commutativity, multi-value
shorthand reordering — session recap §2.2).

bbnf's `<CssL4Value>` should **not** replicate lightningcss's
shape. The AY.W3 scope (file bounds at W3.md:18-24) already says so;
this audit reaffirms. The handle-into-Document shape means bbnf's
CSS L4 value is a ~15-variant enum with per-variant handles
pointing into the tape. Materialisation cost is O(1) per node vs
lightningcss's O(tree-size) allocate + owned-copy.

### 1.5 Lazy-cursor (`get_by_path`) equivalent

sonic-rs exposes `sonic_rs::get_by_path(src, path) -> Value`. Bbnf's
`NodeView::child(i) / children()` is lower-level. The path-query API
bbnf should emit:

```rust
impl<'p, R: Root> Parsed<'p, R> where R::View<'p>: PathQuery {
    pub fn get(&self, path: &Path) -> Option<R::Value>;
}
```

Where `Path` is a typed breadcrumb (`Path::root().child("statuses")
.index(0).child("text")`). The implementation walks the same
substrate as `NodeView::child` but uses the `packed_cache` AoS
sidecar for O(log N) random access via binary-search on span_lo
(A5 §7). JSON Pointer syntax (`/statuses/0/text`) can be parsed to
`Path` as a convenience. Not blocked on emission — can wire from
existing typed accessors.

### 1.6 Round-trip invariant

```
to_value(parse(serialize_compact(to_value(parse(src))))) ==
to_value(parse(src))
```

This decomposes:

- `serialize_compact(value) -> String` via a new emitter
  `generate_value_serialize` that walks the `<Grammar>Value` enum
  and delegates to existing scalar encoders.
- `to_value(parse(s))` on the serialized output re-materialises the
  same handle tree (because handles reference the NEW input +
  NEW tape; equality compares by structural contents).

**Equality:** `<Grammar>Value` derives `PartialEq` where scalars
compare by value and handles compare by resolving their spans (not
by raw offset). This is the `Document::bytes` resolution pattern
(`value.rs:215-225`). Two `<Grammar>Value::String(sp)` values with
different offsets but identical resolved bytes compare equal.

### 1.7 Concrete emission source location + shape

**File:** `crates/core/src/backend/rust/view/value.rs` (new sibling
to `alt.rs`, `leaves.rs`, `seq.rs`, `repeat.rs`, `named_types.rs`).

**Entry point:**
```rust
pub fn generate_value_enum(ir: &GrammarIR, ctx: &IrCodegenCtx)
    -> TokenStream;
```
Called from `view/mod.rs::generate_views` after the per-rule view
block emission, so the `<Grammar>Value` enum and `ToValue`
impls are appended to the same module.

**Emission algorithm (pseudocode with concrete calls):**

```rust
fn generate_value_enum(ir: &GrammarIR, ctx: &IrCodegenCtx) -> TokenStream {
    let grammar = ctx.ident;
    let value_ident = format_ident!("{}Value", grammar);

    // 1. Collect TypeDesc equivalence classes over non-transparent rules.
    //    Rules with identical projected types collapse to one variant.
    let mut variants: Vec<ValueVariant> = Vec::new();
    let mut td_to_variant: FxHashMap<TypeDesc, usize> = FxHashMap::default();

    for rule in ir.rules.iter().filter(|r| !r.meta.is_transparent) {
        let td = ir.types.iter()
            .find_map(|(id, ty)| (*id == rule.id).then_some(ty.clone()))
            .unwrap_or(TypeDesc::Span);

        match td_to_variant.entry(td.clone()) {
            Entry::Occupied(e) => {
                // Append rule id to variant's source-rule list.
                variants[*e.get()].source_rules.push(rule.id);
            }
            Entry::Vacant(e) => {
                e.insert(variants.len());
                variants.push(ValueVariant::from_rule(rule, &td, ir));
            }
        }
    }

    // 2. Emit the enum. Each variant payload is derived from its TypeDesc:
    //      Span → Handle (StringSpan { loc: u64, len: u32 })
    //      scalar → native scalar (f64 / u32 / bool / ...)
    //      Tuple → named-field struct payload
    //      Vec<T> → Vec<<Grammar>Value>
    //      Named(sid) → resolved via RustNamedTypes (existing)
    //      Alt (AQ.6.C) → re-use the already-emitted <RuleName>Value enum
    let variant_defs: Vec<TokenStream> = variants.iter()
        .map(|v| emit_variant(v, ir)).collect();

    // 3. Emit ToValue trait impls. One impl per source rule,
    //    dispatching to the variant it collapsed into.
    let to_value_impls: Vec<TokenStream> = variants.iter()
        .flat_map(|v| v.source_rules.iter().map(|rid| emit_to_value(*rid, v, ir)))
        .collect();

    quote! {
        #[derive(Clone, Debug, PartialEq)]
        pub enum #value_ident<'p> {
            #(#variant_defs,)*
        }

        pub trait ToValue<'p, T> { fn to_value(self) -> T; }

        #(#to_value_impls)*
    }
}
```

**Sample output, JSON grammar:**
```rust
pub enum JsonParserValue<'p> {
    Null,
    Bool(bool),
    Number(f64),
    String(::bbnf::runtime::StringHandle<'p>),
    Array(Vec<JsonParserValue<'p>>),
    Object(Vec<(::bbnf::runtime::StringHandle<'p>, JsonParserValue<'p>)>),
}
impl<'p> ToValue<'p, JsonParserValue<'p>> for jsonView<'p> { ... }
impl<'p> ToValue<'p, JsonParserValue<'p>> for numberView<'p> { ... }
// etc.
```

`StringHandle<'p>` is a new runtime type at
`crates/core/src/runtime/handle.rs` encoding the `loc | len` pair with
the high-bit tag (mirroring `json-prototype::StringSpan`). Resolves
via `handle.as_str(&'p str input)` — zero copy for the borrow path,
arena read for escape path.

**Sample output, CSS L4 (reduced):**
```rust
pub enum CssL4ParserValue<'p> {
    Stylesheet(Vec<CssL4ParserValue<'p>>),        // variant for `stylesheet`
    StyleRule { selectors: Vec<CssL4ParserValue<'p>>, declarations: Vec<CssL4ParserValue<'p>> },
    Declaration { prop: StringHandle<'p>, value: Box<CssL4ParserValue<'p>> },
    ColorFn(::bbnf::runtime::Color),              // resolved from RustNamedTypes
    Number(f64),                                  // collapsed from number_int + number_float
    Ident(StringHandle<'p>),
    AtRule { name: StringHandle<'p>, prelude: Vec<CssL4ParserValue<'p>>, block: Box<CssL4ParserValue<'p>> },
    // ... ~10 more variants derived from live CSS L4 rules
}
```

### 1.8 Apples-to-apples bench matrix shape

Three concrete lanes. Each bench function pair produces matched work
on bbnf + sonic-rs sides (invariant 24 per A5 recommendation):

| Lane | bbnf fn | sonic-rs fn | Fixture count |
|---|---|---|---:|
| canonical | `bbnf_canonical_<fx>` = `JsonParser::parse + JsonParser::serialize_compact` | `sonic_canonical_<fx>` = `sonic_rs::from_str::<Value> + sonic_rs::to_string` | 5 |
| lazy | `bbnf_get_<fx>` = `Parsed::get("/statuses/0/text")` (new API) | `sonic_get_<fx>` = `sonic_rs::get_by_path(src, path)` | 1 (twitter) |
| eager | `bbnf_value_<fx>` = `Parsed::to_value::<JsonParserValue>()` | `sonic_value_<fx>` = `sonic_rs::from_str::<Value>(src)` | 5 |

Where the existing `crates/core/benches/json/value.rs` holds the
`walk_tape` proxy (A1 §6) — it's a lane-proxy, not real eager
materialisation. A10's design replaces it: `bbnf_value_<fx>` becomes
`Parsed::to_value::<JsonParserValue>()` + one deep walk to prevent
dead-code elim, directly comparable to
`sonic_rs::from_str::<Value>`. The visitor lane (`bbnf_visitor_<fx>`)
stays as the cold reference for "direct-to-struct, no tape"
baseline (A1 shows it already tracks prototype within ±5% per
`bench_bbnf_visitor`).

### 1.9 Invariant 21 — bridge or coincidence?

The JSON case looks like a bridge because JSON's grammar
(`object = "{" pair ("," pair)* "}"; array = "[" value ("," value)* "]";
…`) happens to have rule names that match sonic-rs's variants. If
someone renamed the grammar rule from `object` to `obj`, the emitted
variant would be `Obj` and this apparent-bridge collapses. The
dispositive test is: **is the rule set derived from the `.bbnf`
source, or from a comparator schema?** The emitter reads only
`ir.rules` + `ir.types` (no sonic-rs reference anywhere in
`view/value.rs`), so the answer is clearly the former. It is
coincidence born of JSON's simplicity, not an adapter.

Per-grammar contrast tables make this explicit:

| Grammar | `<Grammar>Value` variants (representative) | Maps onto competitor? |
|---|---|---|
| JSON | Null, Bool, Number, String, Array, Object | Coincidentally iso to sonic-rs `Value` |
| CSS L4 | Stylesheet, StyleRule, Declaration, ColorFn, Number, Ident, AtRule, MediaRule, KeyframesRule, … | No competitor has this shape |
| Sheets | Formula, FunctionCall, CellRef, Range, Expression, BinaryOp, UnaryOp, … | No competitor (google-sheets-formula-parser is private) |
| BBNF | Grammar, Rule, Expr, Alternation, Sequence, Ref, Literal, Regex, Repeat, Annotation, … | Ironically iso to its own self-bootstrapped internal |

## 2. Emission algorithm — concrete per-grammar shapes

Derived from §1.1's per-grammar rule count + §1.7's emission pseudo.

### JSON (10 rules → 6 variants after collapse)
```rust
pub enum JsonParserValue<'p> {
    Null, Bool(bool), Number(f64),
    String(StringHandle<'p>),
    Array(Vec<JsonParserValue<'p>>),
    Object(Vec<(StringHandle<'p>, JsonParserValue<'p>)>),
}
```
Rules `true_literal` + `false_literal` collapse into `Bool(bool)` via
TypeDesc equivalence. Rules `number` + `integer` (if present) collapse
into `Number(f64)`. `pair` collapses into `Object`'s KV-pair shape
(existing `is_kv_pair_shape` predicate, `view/mod.rs:408-414`).

### CSS L4 (~15 live rules → ~12 variants after collapse)
See §1.7 sample output; collapse `number_int + number_float +
percentage` into `Number(f64)`; ident-typed rules (`ident`, `name`,
`functionName`) collapse into `Ident(StringHandle<'p>)`.

### Sheets (31 live rules → ~22 variants)
Per the W1r.4a landing, Sheets has 51 serialize surfaces (commit
`53d99e4a`). The Value enum collapses syntactic variants into semantic
ones: `FunctionCall { name: StringHandle<'p>, args: Vec<Value<'p>> }`;
`CellRef { sheet: Option<_>, col: _, row: _ }`; `Range { a, b }`;
etc.

### BBNF self (17 live rules → ~14 variants)
Ironic case: `BbnfParserValue` is itself a bootstrap of the grammar
structure. Variants include `Grammar(Vec<Rule>)`, `Rule { name, body }`,
`Expr(Box<Expr>)`, etc. This is the self-hosting test — if the
emitted enum suffices to round-trip `.bbnf` source, the emission is
complete.

## 3. Materialisation strategy (one-sentence)

**Handle-into-Document reusing the existing tape + `packed_cache`
AoS sidecar** — `<Grammar>Value` is a 24-32-byte enum whose variants
are either inline scalars or handles pointing into the existing
`Tape` / string arena; no second arena, no per-node heap allocation
except for `Vec<Value>` compound children.

## 4. Bench lane additions

```rust
// lane 1 — canonical (already live at tests/json_canonical_parity.rs;
// promote to bench)
fn bbnf_canonical_twitter(b: &mut Bencher) {
    let src = load("twitter.json");
    b.iter(|| {
        let parsed = JsonParser::parse(black_box(&src)).unwrap();
        black_box(JsonParser::serialize_compact(parsed.view()));
    });
}
fn sonic_canonical_twitter(b: &mut Bencher) {
    let src = load("twitter.json");
    b.iter(|| {
        let v = sonic_rs::from_str::<sonic_rs::Value>(black_box(&src)).unwrap();
        black_box(sonic_rs::to_string(&v).unwrap());
    });
}

// lane 2 — lazy path-query (new)
fn bbnf_get_twitter(b: &mut Bencher) {
    let src = load("twitter.json");
    let path = Path::parse("/statuses/0/text").unwrap();
    let parsed = JsonParser::parse(&src).unwrap();  // hoist: path-query is a post-parse op
    b.iter(|| black_box(parsed.get::<JsonParserValue>(&path)));
}
fn sonic_get_twitter(b: &mut Bencher) {
    let src = load("twitter.json");
    let path = sonic_rs::JsonPointer::from("/statuses/0/text");
    b.iter(|| black_box(sonic_rs::get_by_path(black_box(&src), &path)));
}

// lane 3 — eager materialised (new)
fn bbnf_value_twitter(b: &mut Bencher) {
    let src = load("twitter.json");
    b.iter(|| {
        let parsed = JsonParser::parse(black_box(&src)).unwrap();
        black_box(parsed.to_value::<JsonParserValue>());
    });
}
fn sonic_value_twitter(b: &mut Bencher) {
    let src = load("twitter.json");
    b.iter(|| black_box(sonic_rs::from_str::<sonic_rs::Value>(black_box(&src)).unwrap()));
}
```

Bench file: `crates/core/benches/json/value.rs` (extend, retain
visitor + proto lanes for reference).

## 5. AY.W3 draft — deficiencies and corrections

W3.md was drafted before A10's emission design. Specific gaps and
corrections:

- **§Scope item 1: enum shape under-specified.** W3.md says "one
  variant per non-transparent rule, shape determined by `TypeDesc`".
  A10 §1.1 adds: rules with identical TypeDesc collapse to one
  variant; `is_kv_pair_shape` objects route to Object-KV; Alt-rooted
  rules reuse the existing AQ.6.C `<RuleName>Value` enum from
  `view/alt.rs:137-146` rather than re-emit. W3.md's "Alt-rooted
  rules → enum-in-enum: the variant holds a sub-enum named
  `<RuleName>Alt`" should become: the variant holds the existing
  `<RuleName>Value` type (AQ.6.C emission; do not emit a second
  sibling).
- **§Scope item 2: ownership story wrong.** W3.md says "Allocation
  via the existing arena hooks in `Parsed<R>`." There is no arena
  hook in `Parsed<R>` today (parsed.rs:76-87 is pure
  `(Tape, &'p str, TapeOffset)`; no arena). A10 §1.2 corrects: the
  arena is the tape's existing string arena (AY-V.W2's
  `payload_string_with_source`); compound children are `Vec<T>`
  owned; scalars inline. If bump-arena-hosted is desired, the
  emission signature is `Parsed::to_value_in<'v, T>(&self, bump:
  &'v Bump) -> &'v T` — distinct from `to_value()`; both valid,
  orthogonal.
- **§Hard gate #4: 3× ratio target prematurely tight.** A1 §1
  reports post-W0b JSON twitter bbnf/sonic ratio at **7.93×** (not
  5.96× as the stale doc 03 implied). The 3× gate presupposes L1
  (`push_structural` inlining) and L2 (finalise fusion) have
  landed. AY should sequence: W1 (tape levers) → W2 (Named
  preservation) → W3 (Value emission). W3.md does not state this
  ordering explicitly. Correct the gate to: "ratio ≤ 1.5× the
  parse-only ratio measured at W3 start" (i.e. eager should be no
  more than 1.5× worse than parse-only on the same fixture) — this
  removes the absolute-perf dependence.
- **§Sub-wave AY.W3.2 (`Parsed::to_value` method) elision.** W3.md
  proposes `type Value: ToValue<Self::Value>` on the `Root` GAT.
  This is recursive (Value bounded by ToValue<Value>). Correct:
  `type Value<'p>: 'p where Self: 'p; type ViewToValue<'p>:
  ToValue<'p, Self::Value<'p>>`. Actually simpler:
  ```rust
  pub trait Root {
      type View<'p> where Self: 'p;
      type Value<'p> where Self: 'p;
      fn make_view<'p>(...) -> Self::View<'p>;
      fn view_to_value<'p>(view: Self::View<'p>) -> Self::Value<'p>;
  }
  ```
  Then `Parsed::to_value(&self) -> R::Value<'_>` calls
  `R::view_to_value(self.view())`. No ToValue trait needed in user
  surface; grammar provides the impl directly.
- **§Archaeology closing paragraph misleads.** W3.md says "Lazy-
  cursor head-to-head is left as AY post-tranche review candidate
  OR AY scope item." A10 §1.5 pulls it in-scope: `Parsed::get(path)`
  is a small addition (≤ 100 LOC delta) using existing `packed_cache`
  binary-search. Deferring it leaves A5's three-lane matrix with a
  persistent hole.
- **§Dependencies "Blocks W5" is overstated.** W5 (compile-time
  levers) may restructure emit surfaces but `<Grammar>Value`
  emission is additive in view/value.rs — decoupled from
  compile-time work. The true blocker is W2 (Named preservation,
  §A6) so CSS L4's ColorFn variant carries the proper `Color`
  struct.
- **File bound should add `crates/core/src/runtime/handle.rs` (new)
  and `crates/core/src/runtime/path.rs` (new for lazy lane).** Both
  are runtime substrate the emitter consumes.

## 6. Three levers for AY.W3 execution

Per user directive "three levers; specific to the Value API, not
generic tape/parse levers":

**V1 — Collapse TypeDesc-equivalent variants.** §1.1 +
§2 call for this; W3.md doesn't. Without collapse, CSS L4 emits ~15
variants with four distinct "number" variants (`number_int`,
`number_float`, `percentage`, `dimension`) that all project f64.
Collapse cuts the enum size by ~30%, shrinks the `PartialEq` + `Clone`
impls proportionally, and matches sonic-rs's single `Number` variant
shape for JSON (coincidentally). Mechanism: `FxHashMap<TypeDesc,
usize>` during emission; aggregate source-rule ids per variant; emit
N `ToValue` impls all targeting the same variant. No runtime cost.

**V2 — Handle-into-Document reuse the tape.** §1.2 + §1.3. The
non-obvious win: bbnf's tape + `packed_cache` IS the Document;
`<Grammar>Value` is a typed accessor over that substrate, not a
second tree. Cuts materialisation cost from "O(tape) walks + O(tape)
allocations" to "O(tape) walks + O(compound) allocations". Mechanism:
`StringHandle<'p>` carries `(loc, len)` with high-bit tag; compound
variants carry `Vec<Value<'p>>` (standard) BUT scalars/handles inline
directly without indirection. See §1.7 sample output.

**V3 — Lazy path-query in the same wave.** §1.5. AY.W3 should land
all three lanes, not split eager-only + defer lazy. The lazy
implementation is < 100 LOC (Path parser + binary-search on
`packed_cache.span_lo`), it completes A5's three-lane matrix, and
it closes the "bbnf has no path-query equivalent to sonic-rs
`get_by_path`" gap that makes lazy comparisons untenable today.
Mechanism: `Parsed::get::<T>(path) -> Option<T>` walks the tape by
cursor traversal + typed decode at the terminus.

## 7. W3 sub-wave split recommendation

Yes — split. The three concerns are orthogonal enough to parallelise
and have different risk profiles. Proposed decomposition:

- **W3a — Runtime substrate** (1 agent). Adds
  `crates/core/src/runtime/handle.rs` (`StringHandle`,
  `CompoundHandle`), `crates/core/src/runtime/path.rs` (`Path`,
  JSON Pointer parser, binary-search on `packed_cache.span_lo`).
  No emitter changes. No test breakage risk. File bounds: two new
  files; can land independently. Gate: unit tests for handle
  resolve + path parse + path-walk against a hand-built tape
  fixture.
- **W3b — `<Grammar>Value` emitter + `Parsed::to_value`** (1 agent).
  Adds `view/value.rs`, modifies `view/mod.rs` (wire up), modifies
  `runtime/parsed.rs` (`to_value` method, `Root::Value` GAT), runs
  generated.rs regen. Gate: `cargo expand -p bbnf --test
  json_canonical_parity` shows `pub enum JsonParserValue` +
  `ToValue` impls per rule.
- **W3c — Bench lanes + parity tests** (1 agent). Modifies
  `crates/core/benches/json/value.rs` (three lanes), adds
  `crates/core/tests/json_value_parity.rs` (materialise → serialize
  → re-materialise round-trip). Gate: all three lanes produce
  comparable work-matched ratios; round-trip parity green on 5
  fixtures.

Dependencies: W3b depends on W3a (handle type + path type). W3c
depends on W3b (`to_value` method exists). Parallelisable with one
sequencing point (W3a lands first), not fully concurrent.

## 8. Artefact citations

- `crates/core/src/runtime/parsed.rs:53-138` — Parsed + Root GAT
  (A5 §1.1, A10 §1.2 corrected the Arc framing).
- `crates/core/src/backend/rust/view/mod.rs:97-373` — per-rule view
  emission; emission entry for `generate_value_enum` appends here.
- `crates/core/src/backend/rust/view/mod.rs:392-447` — typed-accessor
  dispatch; `<Grammar>Value` consumes the same IR body-peel logic.
- `crates/core/src/backend/rust/view/alt.rs:8-15, 137-146` — AQ.6.C
  `<RuleName>Value` enum emission for payload-eligible Alts; A10 §1.7
  reuses these in aggregate `<Grammar>Value` variants.
- `crates/core/src/backend/rust/view/leaves.rs:145-222` —
  `emit_aggregate_accessors`; A10 §1.3 notes the `Color` 40 B arena
  slot is already the handle substrate.
- `crates/core/src/backend/rust/view/named_types.rs:52-92` —
  `RustNamedTypes::from_ir`; A10 §1.1 + §2 resolves
  `TypeDesc::Named(sid)` through this.
- `crates/core/src/generate/serialize/mod.rs:15-105` — existing
  serialize substrate; A10 §1.6 proposes a parallel
  `generate_value_serialize` for the round-trip invariant.
- `crates/core/benches/twitter_lazy_field.rs:199-228` — AoS random
  access kernel; A10 §1.5 lazy path-query reuses `packed_cache`.
- `crates/core/benches/json/value.rs:65-130` — walk_tape proxy;
  A10 §1.8 replaces it with true `to_value` call.
- `crates/core/tests/json_canonical_parity.rs:29-56` — canonical
  parity template; A10 §4 lane 1 promotes to bench.
- `crates/json-prototype/src/value.rs:39-181` — sonic-rs-shape
  archival that A10 §1.3 mines for the handle pattern.
- `docs/tranches/AX/audit/next-tranche/A1-json-parse-fresh.md:38,
  200-234, 300-346` — fresh JSON gap attribution (7.93×, not
  5.96×) and L5 value lane motivation.
- `docs/tranches/AX/audit/next-tranche/A5-value-api-design.md:198-
  344` — three-lane thesis + invariant 21 resolution A10 builds on.
- `docs/tranches/AX/audit/next-tranche/A6-named-preservation-
  design.md:284-314` — A10 §1.7 CSS L4 ColorFn variant depends on
  A6's fix landing.
- `docs/tranches/AY/waves/W3.md:1-136` — draft A10 §5 corrects.

## 9. Summary

Per-rule variants with TypeDesc equivalence-class collapse, not a
generic Object/Array/Leaf DAG. Handle-into-Document materialisation
reusing the existing tape + `packed_cache`, not owned Box trees.
Three bench lanes (canonical / lazy / eager) landing together, not
eager-only. Sub-wave split W3a (runtime substrate) → W3b (emitter +
method) → W3c (benches + parity). Invariant 21 holds under
emission-from-IR discipline; JSON's apparent iso to sonic-rs is
coincidental, not adapter-shaped.
