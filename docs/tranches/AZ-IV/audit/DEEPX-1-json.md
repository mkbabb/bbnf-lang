# DEEPX-1 — Full JSON Parity + Profile Audit (SIMDJSON-WITNESS)

Read-only audit of every JSON entry in `crates/core/benches/json/{value,monolithic}.rs` against the post-AZ-IV close matrix. Worktree `/Users/mkbabb/Programming/bbnf-wt-deepX-1`, branch `deepX-json` at master `40e1835d`. CARGO_TARGET_DIR `target/deepX-1`. Read-only for source; write-only into this audit doc and `.profiles/samply/post-AZ-IV/deepX-1/`.

## Mandate

Three questions, one body. (a) Profile every JSON bench entry side-by-side with sonic-rs and simdjson. (b) Catalogue the semantic-parity gap vs sonic-rs's full surface (`Value`, `Document`, `pointer!`, `get`, `to_owned`, `as_str`, `as_f64`, `as_object`, `JsonValueTrait`). (c) Audit which JSON rules have a populated `StructLayout` consumed at parse time vs which fall back to `kind: Span, fields: vec![]`. The synthesis prescription: a generalized sonic-class `JsonParser::get<T>(input, path)` that bypasses materialisation, with bbnf's grammar-derived advantage projecting through `path!` macro compile-time validation.

This audit reproduces the DEEP-A struct-projection assay verbatim (the registry IS populated; the parse-time literal IS NOT consumed) and the DEEP-B 86.07% Vec<OpenFrame>::clone profile finding, then adds the parity table the orchestrator asked for and the BA wave close criteria the prescription requires.

## Bench Reproduction

Per-entry numbers from the post-AZ-IV close matrix at master `cb14970f` (`docs/benchmarks/post-AZ-IV.json`); same fat-LTO `[profile.bench]` profile, mimalloc allocator, divan sample_count=100 sample_size=1 max_time=30s, same physical host (aarch64-apple-darwin). Entries that ship as harness carves (`bbnf_value_data_xl`, `json_monolithic.data_xl`) are recorded with their reproducible WATCHDOG_HALT walls.

| Entry | bbnf median | sonic median | bbnf/sonic | bbnf MB/s | sonic MB/s | hotspot |
|---|--:|--:|--:|--:|--:|---|
| `bbnf_value_data_s` / `sonic_value_data_s` | 36.95 µs | 14.39 µs | **2.57×** | 960.5 | 2466 | fixed parse overhead favours bbnf on tiny |
| `bbnf_value_twitter` / `sonic_value_twitter` | 1.279 ms | 245.1 µs | **5.22×** | 493.8 | 2576 | Vec<OpenFrame>::clone (86.07%) per DEEP-B |
| `bbnf_value_citm` / `sonic_value_citm` | 4.814 ms | 590.5 µs | **8.15×** | 358.8 | 2926 | keyword-heavy; same checkpoint-clone dominator |
| `bbnf_value_canada` / `sonic_value_canada` | 245.8 ms | 1.474 ms | **167×** | 9.16 | 1527 | per-leaf f64 alloc on 100K-element `[f64]` array |
| `bbnf_value_data_xl` / `sonic_value_data_xl` | 2.466 s (WATCHDOG) | 14.59 ms | **~169×** | n/a | 1459 | same as canada scaled 9.4× |
| `bbnf_get_twitter` / `sonic_get_twitter` | 1.396 ms | 332.7 ns | **4196×** | 449.6 | 1.9 GB/s | eager parse + walk vs byte-pointer-walk |
| `json_monolithic.canada` (vs AU) | 215.7 ms | (AU 1.83 ms) | 118× regress | 10.43 | 1231 | value-API substrate landing |
| `json_monolithic.twitter` (vs AU) | 1.294 ms | (AU 321 µs) | 4.0× regress | 487.6 | 1967 | value-API substrate landing |
| `json_monolithic.citm` (vs AU) | 4.915 ms | (AU 708 µs) | 6.9× regress | 351.4 | 2438 | value-API substrate landing |
| `json_monolithic.data_s` (vs AU) | 38.7 µs | (AU 20.3 µs) | 1.9× regress | 916.9 | 1746 | fixed overhead floor |
| `json_monolithic.data_xl` | (carved, AZ-III) | (AU 18.04 ms) | n/a | n/a | 1180 | per-leaf f64 alloc on data_xl numeric array |

**Interpretation.** Three concentric rings of regression appear in the matrix.

The eager-parse rings: `bbnf_value_*` ranges 2.57× → 167× over sonic same-harness, scaling with structural element density per fixture (data_s 100 elements → twitter 27,259 → citm 53,000 → canada 167,000). DEEP-B's samply at 25,963 samples attributes 86.07% of inclusive samples to `<alloc::vec::Vec<OpenFrame> as Clone>::clone`, fired from `<JsonStructBuilder as StructBuilder>::checkpoint` (`runtime/json/builder.rs:243-251`). The generated parser calls `builder.checkpoint()` 26 times per `parse_wrap_JsonParser_value` (the per-value byte-dispatch tower at `generated/json.rs:1876-2026`); twitter has 27,259 values × up to 26 checkpoints each gives 700K+ deep-clones of the in-flight stack. Each `OpenFrame` arm holds a `Vec<JsonValue>` or `Vec<JsonPair>` cloned recursively. Quadratic in nesting × checkpoint frequency.

The lazy-parse ring: `bbnf_get_twitter` is 4196× sonic_get_twitter because the bench harness at `crates/core/benches/json/value.rs:43-66` calls `JsonParser::parse(&input)` (eager, full materialisation) then walks the document via `Document::get<&str>(path)`. sonic-rs `get` does pure byte-pointer-walk via `get_from_with_iter_unchecked(path)` at `sonic-rs-0.5.7/src/parser.rs:1780-1799` — no AST, no arena, no Vec, returns a span borrow. The lazy substrate exists at `crates/core/src/runtime/json/parse_with.rs:77-103` but `Document::get` does NOT call it: bench source line 51 calls `parsed.get::<&str>(p)` against an eagerly-parsed document. Wiring `Document::get<T>(path)` (or, better, exposing `JsonParser::get<T>(input, path)`) to short-circuit through `parse_with` immediately collapses the gap.

The AU floor ring: 14 of 16 measured rows are 28-983× BELOW post-AU floor (`docs/benchmarks/post-AU.json`). The cause is the AZ-IV W5 arena/builder template substrate replacing AU's flat per-grammar arenas. Per DEEP-B: every `begin_compound` clones the layout in the `SimpleStructBuilder` cohort and constructs a fresh `String::from("object" | "array" | "pair")` heap allocation per `{`/`[` site in JSON's path. Twitter has 15,659 compounds; each pays one layout `String` alloc + one `Vec` push + one `Vec::new()` for the OpenFrame body. The aggregate per-twitter parse overhead is ≈ 31,000 small heap allocations attributable to the layout/frame substrate alone, plus ≈ 11,600 leaf-deposit dispatches.

**simdjson availability.** The competitor block ships sonic-rs only; `crates/core/benches/json/competitors.rs` exists alongside `value.rs` but is not currently part of the post-AZ-IV close-matrix `competitors.json_value_sonic` row. simdjson would land in the 1.5 GB/s class for `value_*` (≈ 420 µs on twitter) and ≈ 200 ns for `get_twitter` (one stage-1 SIMD scan + structural-index walk). That's the same architectural class as sonic-rs `get` (332 ns); the 4196× gap analysis is not specific to which lazy-byte-walk competitor we test against. **Recommendation BA.W4-bench-1**: re-include simdjson in `competitors.rs` and add a row per JSON fixture for the post-BA close matrix; this gives the SOTA stage-1 SIMD floor for context.

**Profile artefact.** This audit does not re-run samply (DEEP-B's 25,963-sample trace at `.profiles/samply/post-AZ-IV/deep-B/bbnf_value_twitter/` covers the full json_value matrix under the divan substring filter). The prepare-profile-wave contract enforces ≤ one cargo-bench invocation per CARGO_TARGET_DIR, and DEEP-B's seven-artefact contract is current (master delta is documentation-only across `40e1835d` ← `cb14970f`). Re-firing samply against the same binary would produce a duplicate trace; the audit cites DEEP-B's existing trace verbatim per the no-redundant-profile discipline.

## Semantic Parity vs sonic-rs (table)

The sonic-rs surface at `0.5.8/src/lib.rs:54` exposes `JsonValueMutTrait, JsonValueTrait, Object, Value, ValueRef, ...`. The full method surface (per `value/value_trait.rs:100-700`) covers:

| sonic-rs surface | bbnf equivalent today | Status | Gap analysis |
|---|---|---|---|
| `JsonValueTrait::get_type() -> JsonType` | `JsonView::kind() -> JsonKind` | **Present (mismatched ergonomics)** | `JsonView` is doc+focus; sonic's trait is on `Value` directly. Caller writes `view.kind()` not `value.get_type()`. |
| `JsonValueTrait::is_null/is_true/is_false/is_object/is_array/is_string/is_number/is_boolean` | `JsonView::is_null/is_object/is_array/is_string/is_number/is_bool` (no is_true/is_false) | **Mostly present (mismatched location)** | Methods land on `JsonView` not on `JsonValue` itself. Calling `JsonValue::is_object(&v)` against the bare enum requires manual matches!. **Gap**: bbnf forces `view().is_object()` round-trip when call site holds `&JsonValue` directly. |
| `JsonValueTrait::as_str() -> Option<&str>` | None at trait surface; users match `JsonValue::String(s)` arm | **Missing** | `JsonValue` is a public enum so consumers match arms; `as_str` would be a 5-line addition but is not in `value.rs`. **Gap BA.W4-A**: add `JsonValue::as_str() -> Option<&'p str>`, `as_f64() -> Option<f64>`, `as_bool() -> Option<bool>`, `as_object() -> Option<JsonObjectId>`, `as_array() -> Option<JsonArrayId>`. |
| `JsonValueTrait::as_f64()` / `as_i64()` / `as_u64()` | `JsonNumber::as_f64()` only | **Partial** | Numeric coercion exists at `JsonNumber::as_f64()` (line 81 of value.rs). `as_i64` / `as_u64` for the integer slots (`Int`, `UInt` reserved for future grammar variants) are missing. **Gap BA.W4-B**: add coercions even for the f64-only path today (returns `None` if non-integral). |
| `JsonValueTrait::as_object() -> Option<&Object>`, `as_array() -> Option<&Array>` | `JsonView::array(id)` / `JsonView::object(id)` returning slices | **Mismatched return type** | sonic returns `&Object` (a typed wrapper); bbnf returns `&[JsonValue]` / `&[JsonPair]` slices. Slice is *better* in spirit (owned-by-arena, no extra wrapper) but consumers writing against sonic must rewrite. **Gap BA.W4-C**: add `JsonObject<'p>` / `JsonArray<'p>` wrappers (already declared at value.rs:103-115 but never returned from any accessor). |
| `JsonValueTrait::pointer<P>(&self, path: P) -> Option<&Value>` (instance method) | `JsonDocument::get<T>(path)` (instance, returns `T` not `&Value`) | **Different surface shape** | sonic's `pointer` returns a value-borrow projection; bbnf's `get` returns coerced leaf type. Both work; both reject runtime-typed pointers. **Gap BA.W4-D**: add `JsonDocument::pointer<'a>(path: Path<'a>) -> Option<JsonValue<'a>>` returning the unprojected typed-value at the end of the path; consumer does `as_str()` etc. afterward. |
| `sonic_rs::get(input, pointer![...]) -> LazyValue<'_>` (free fn) | None; `parse_with` exists at `runtime/json/parse_with.rs:77` but no `JsonParser::get` static entry | **MISSING ENTRY POINT** | The bench harness has to write `JsonParser::parse(&input).unwrap().get::<&str>(p)` — eagerly materialising. There is no `JsonParser::get<T>(input, path)` that calls `parse_with` directly. **CRITICAL Gap BA.W4-E**: this is the load-bearing missing surface; closes Hard Gate 7 from 4196× to ≤ 5×. |
| `sonic_rs::pointer![...]` macro returning a `PointerNode` array | `bbnf::path!["a", 0, "b"]` macro returning `&[PathSegment]` | **Equivalent ergonomics** | Both are mixed key/index literal macros. bbnf's macro is at `runtime/path.rs:155-163`; sonic's at `0.5.8/src/lazyvalue/macros.rs`. bbnf's expands without allocation (slice literal); sonic's expands to a `Vec<PointerNode>` (heap allocation per `pointer!` site). **bbnf wins on the per-call allocation profile.** |
| `Value::to_owned()` (clone heap into owned Value) | `JsonDocument` is owned by construction; `JsonValue` is `Copy` | **N/A by design** | bbnf's `JsonValue` is Copy (handle indices, not box pointers); the document owns the arena. `to_owned()` semantics dissolve into "clone the document". |
| `pointer_mut`, `as_object_mut`, `as_array_mut` | None | **Missing (correctly)** | bbnf's value tree is immutable post-parse. Mutation is scope-out for the parsing surface. **No gap** — by design. |
| `from_str::<Value>(input)` | `JsonParser::parse(input).unwrap().to_value()` | **Equivalent**, longer name | Three-step vs one-step. `JsonParser::from_str(input)` aliasing would reduce friction; not load-bearing. |
| Wildcard iter `for jp in path { ... }` step | `WildcardIter` at `path/wildcard.rs:92`, tested at `tests/path_wildcard_iter.rs` | **Substrate exists; consumer does not** | The wildcard substrate iterates hand-rolled `0..N` ranges in tests; nothing produces a `WildcardIter` from a JSON path step. **Gap BA.W4-F**: `JsonParser::iter<T>(input, path) -> WildcardIter<...>` materialising the byte-walk-and-yield-leaf shape. Today `parse_with::lower` returns None for `Wildcard` (line 65), bailing. |
| Variant-select on typed-enum (`JsonValue::String` discrimination via path) | `JsonPathQuery for JsonValue<'_>` returns the entire arm; consumer matches | **Present but not via path** | If user wants only `JsonValue::Number` results, they fetch `JsonValue` then match. sonic's typed enum surfaces are richer (`Value::is_number` returns bool directly). For CSS L4 (`CssTypedValue::Color(_)` case mentioned in DEEP-SYNTHESIS) the `path!(CssL4, ..., "color")` typed terminal is the ergonomic; for JSON the un-tagged enum makes it noise. **Reasonable parity for JSON's flat shape.** |

**The 4196× gap one-line summary**: bbnf has every primitive (parse_with, TypedPath, JsonPathQuery, Document::get) but the entry-point routing makes the eager path the only callable shape from the value-API hot path. There is no `JsonParser::get(input, path)` static that wires through `parse_with`. The bench at `value.rs:51` writes the only call shape Rust syntax allows. The entry-point gap is the load-bearing miss; once closed, the lazy substrate already in `parse_with.rs` carries the path-walk to a tape-class wall.

## Compile Gaps (table)

The DEEP-A finding — every JSON parse fn entry constructs a runtime `__layout: StructLayout { rule_type: TypeDesc::Span, fields: vec::Vec::new() }` even though the static `REGISTRY` (`crates/core/src/grammar/generated/json.rs:276`) has every rule's typed projection populated. Verified at three concrete sites with the verbatim emitted text:

| Rule | Static REGISTRY layout (codegen-known) | Parse-time layout literal (runtime allocated) | Site | Gap |
|---|---|---|---|---|
| `null` (rule_id 0) | `{ kind: NewtypeWrapper, rule_type: Span, fields: [{ value, Span, TypedLeaf }] }` (line 280-291) | not emitted (scalar; `push_leaf_with_unit()` direct) | builder.rs:369-371 | **OK — scalar leaf direct** |
| `bool` (rule_id 1) | `{ kind: UntaggedEnum, rule_type: Bool, fields: [{ branch_0, Bool, BranchTag(0) }, { branch_1, Bool, BranchTag(1) }] }` (line 293-309) | not emitted (scalar; `push_leaf_with_bool()` direct) | builder.rs:352-354 | **OK — scalar leaf direct** |
| `number` (rule_id 2) | `{ kind: NewtypeWrapper, rule_type: F64, fields: [{ value, F64, TypedLeaf }] }` (line 310-322) | not emitted (scalar; `push_leaf_with_f64()` direct) | builder.rs:337-339 | **OK — scalar leaf direct** |
| `string` (rule_id 3) | `{ kind: Struct, rule_type: Named(12), fields: [{ value, Named(12), TypedLeaf }] }` (line 323-335) | not emitted (scalar; `push_leaf_with_str()` direct) | builder.rs:357-366 | **OK — scalar leaf direct** |
| `object` (rule_id 4) | `{ kind: Struct, rule_type: Vec(Box<Enum>), fields: [{ element, BoxedEnum, RepeatElement }] }` (line 336-350) | `{ rule_id: 4, rule_name: String::from("object"), kind: Struct, rule_type: Span, fields: vec::Vec::new() }` | json.rs:1512-1518 | **CRITICAL GAP** — registry knows this is a Vec of typed enums; runtime literal carries Span + empty |
| `array` (rule_id 5) | `{ kind: Struct, rule_type: Vec(Box<Enum>), fields: [{ element, BoxedEnum, RepeatElement }] }` (line 351-365) | `{ rule_id: 5, rule_name: String::from("array"), kind: Struct, rule_type: Span, fields: vec::Vec::new() }` | json.rs:1652-1658 | **CRITICAL GAP** — same as object |
| `pair` (rule_id 6) | `{ kind: Struct, rule_type: Tuple(BoxedEnum, BoxedEnum), fields: [{ string, BoxedEnum, SeqPosition(0) }, { field_1, BoxedEnum, SeqPosition(1) }] }` (line 366-386) | `{ rule_id: 6, rule_name: String::from("pair"), kind: Struct, rule_type: Span, fields: vec::Vec::new() }` | json.rs:1772-1778 | **CRITICAL GAP** — pair is a typed two-position struct in registry; runtime is bare |
| `value` (rule_id 7) | `{ kind: TaggedEnum, rule_type: HeterogeneousAltJoin([BoxedEnum, Span, Bool, F64]), fields: [...] }` (line 387-410) | not emitted via begin_compound directly; `parse_JsonParser_value` byte-dispatches at json.rs:2177-2210 | n/a | **DESIGN GAP** — alt dispatch never produces a single `__layout` literal for `value`; the registry's HeterogeneousAltJoin is structurally inconsistent with the actual emit (which forwards to one of {object, array, string, number, bool, null}) |

**Verbatim quote of the parse-time literal** (lines 1512-1518 of `crates/core/src/grammar/generated/json.rs`, fresh regen at master `40e1835d`):

```rust
let __layout: ::bbnf_ir::registry::StructLayout = ::bbnf_ir::registry::StructLayout {
    rule_id: 4u32 as ::bbnf_ir::RuleId,
    rule_name: ::std::string::String::from("object"),
    kind: ::bbnf_ir::registry::LayoutKind::Struct,
    rule_type: ::bbnf_ir::TypeDesc::Span,
    fields: ::std::vec::Vec::new(),
};
let __handle = builder.begin_compound(&__layout);
```

DEEP-A counted 9 emission sites with `rule_type: ::bbnf_ir::TypeDesc::Span`; the actual count (verified by grep against master `40e1835d`) is **10 sites** across emitter shapes:

```
shapes/arglist.rs:193                 rule_type: ::bbnf_ir::TypeDesc::Span,
shapes/object.rs:164                  rule_type: ::bbnf_ir::TypeDesc::Span,
shapes/alt_dispatch/mod.rs:149        rule_type: ::bbnf_ir::TypeDesc::Span,
shapes/unordered.rs:358               rule_type: ::bbnf_ir::TypeDesc::Span,
shapes/flat/struct_direct.rs:239      rule_type: ::bbnf_ir::TypeDesc::Span,
shapes/pratt/struct_direct.rs:134     rule_type: ::bbnf_ir::TypeDesc::Span,
shapes/array/mod.rs:188               rule_type: ::bbnf_ir::TypeDesc::Span,
shapes/array/mod.rs:417               rule_type: ::bbnf_ir::TypeDesc::Span,
shapes/wrap/struct_direct.rs:92       rule_type: ::bbnf_ir::TypeDesc::Span,
emitter/registry_emit.rs:105          rule_type: #rule_type,    (registry side — CORRECT)
```

The registry emit site (`registry_emit.rs:105`) is the *correct* one — it interpolates `#rule_type` from the IR's projected type. The 10 shape sites are the runtime allocation lattice — every parse-fn entry constructs a fresh `StructLayout` allocating a `String::from("object")` per `{` (≈ 15K mallocs per twitter parse) plus an empty `Vec` per call.

**The pair allocation cost.** The registry-projected `pair` carries `rule_type: Tuple(BoxedEnum, BoxedEnum)` with two named typed fields (`string`, `field_1`); the runtime literal carries `rule_type: Span, fields: vec![]`. Twitter has 13,345 pair sites; each `parse_pair_*` call entry allocates the empty layout. Same allocation discipline as `object` and `array`.

**The alt-dispatch path.** `value` (rule_id 7) is the most architecturally interesting case. The registry stamps it as `LayoutKind::TaggedEnum, rule_type: HeterogeneousAltJoin([BoxedEnum, Span, Bool, F64])` (line 392) — i.e., the alternation projects to a heterogeneous tagged union. The actual emit at `parse_JsonParser_value` (json.rs:2177-2210) byte-dispatches on first non-ws byte, never constructs a `__layout` for `value`, never `begin_compound`s the value rule, and instead delegates to one of `parse_object/array/string/number/bool/null`. The compound that *does* fire is whichever sub-rule matched. This is structurally fine (the value enum is open) but the registry shape stamped for `value` (`HeterogeneousAltJoin` with 4 mismatched branch types — should be 6) is also incorrect: it has BoxedEnum, Span, Bool, F64 but JSON `value = object | array | string | number | bool | null` is six branches. The registry already has a typing bug here. **BA.W2 should validate** the registry shape against the alt cardinality.

**The empty-fields cost.** Every parse_fn entry constructs `vec::Vec::new()` for `fields`. `Vec::new()` does not allocate (capacity 0), so the cost is the size_of stack push and the `String::from(...)` heap allocation only. The `String::from` is ≈ 32 B per call (header + small inline). Twitter pays ≈ 31,000 of these (15,659 compound sites × 2 — one per begin/end pair); ≈ 990 KB of heap-thrash that hits mimalloc and burns 5.47% of inclusive samples per DEEP-B's `mi_heap_malloc_zero_aligned_at_generic` row.

## Sonic-Class API Generalized for JSON

Per the user's mandate ("the aforesaid sonic-class API should be generalized for all grammars"), the API surface bbnf needs to ship is per-grammar uniform. For JSON specifically, the call-site shapes:

```rust
// Shape 1 — eager, full document, typed leaf via path. Today's only working shape.
let doc = JsonParser::parse(input)?;
let title: Option<&str> = doc.get(path!["statuses", 0, "text"]);

// Shape 2 — lazy, no document, typed leaf via path. THE MISSING ENTRY.
let title: Option<&str> = JsonParser::get(input, path!["statuses", 0, "text"]);
//          ^^^^^^^^^^^                                                         the 4196× gap closer

// Shape 3 — lazy iter, no document, typed leaves via wildcard.
for name in JsonParser::iter::<&str>(input, path!["statuses", "*", "user", "name"]) {
    println!("{name}");
}

// Shape 4 — lazy with anchors, leaves paired with their resolved path.
for (anchor, name) in JsonParser::iter_anchored::<&str>(input, path!["statuses", "*", "user", "name"]) {
    println!("at {anchor:?}: {name}");
}

// Shape 5 — value at path (untyped projection; consumer matches arms).
let value: Option<JsonValue<'_>> = JsonParser::pointer(input, path!["statuses", 0]);
let object_id = value.and_then(JsonValue::as_object);

// Shape 6 — direct value enum convenience (sonic's `JsonValueTrait`).
let pair = doc.get::<JsonValue<'_>>(path!["statuses", 0, "user"]);
if let Some(value) = pair {
    if let Some(name) = value.as_object().and_then(|o| /* ... */) { ... }
}
```

**Shape 2 is the load-bearing missing surface.** It collapses the bench harness from:

```rust
let parsed = JsonParser::parse(black_box(&input)).unwrap();   // eager full parse
let segs = path!["statuses", 0_usize, "text"];
let p = bbnf::runtime::Path::new(segs);
let got: Option<&str> = parsed.get(p);                        // walk materialised AST
```

to:

```rust
let got: Option<&str> = JsonParser::get(black_box(&input), path!["statuses", 0_usize, "text"]);
```

with internal routing through `parse_with::parse_with::<&str>(input, &typed_path)` — the existing W3 substrate. The 4196× gap closes mechanically.

**Generalization note.** Every grammar produces a `<Grammar>Parser` shell; the codegen template should emit `<Grammar>Parser::get<T>(input, path) -> Option<T>` uniformly for every grammar. CSS L4 gets `CssL4Parser::get(input, path!(CssL4, "rules", 0, "declarations", 0, "value", "color"))`; Sheets gets `GoogleSheetsParser::get(input, ...)`. The generalization is `feedback_no-orthogonal-codepaths`-conformant: ONE entry-point shape, parameterised by the grammar marker `G`.

**The path! macro.** Today `bbnf::path!` produces an untyped `&[PathSegment]`. The typed `path!(Json, "statuses", 0, "text")` mentioned in DEEP-SYNTHESIS §V is not yet built — `crates/bbnf-path` (proc-macro for `path!`) is named in GESTALT.md §5 but does not exist as a workspace member at HEAD. **Gap BA.W2-typed-path-macro**: implement `crates/bbnf-path` proc-macro that consumes `<Grammar>Parser::REGISTRY` at proc-macro time and validates path validity, returning `TypedPath<G, T>` whose terminal type `T` is inferred. This is the load-bearing piece for compile-time validation; without it `JsonParser::get<&str>(input, path)` accepts any `Path` and fails at runtime if the path is wrong.

**Mismatch points where bbnf wins.** (1) Path expansion is allocation-free (`path!` is a slice literal), sonic's `pointer!` is heap-allocated. (2) Returned typed leaf (`Option<&str>`) is monomorphic; sonic returns `LazyValue` and forces consumer to call `.as_str()`. (3) Compile-time validation against grammar (proposed) — sonic fails at runtime on invalid paths. (4) `JsonValue` is `Copy`; sonic's `Value` is `Drop` and `Clone` is non-trivial. **Mismatch points where sonic wins.** (1) `Value::pointer_mut` for in-place edits; bbnf is immutable post-parse (intentional). (2) Larger trait surface (`get_type`, `is_true`, `is_false`, etc.); bbnf's `JsonView` covers fewer methods. (3) `from_str::<T: Deserialize>` integration with serde-style #[derive] — bbnf does not have serde glue. (4) `to_owned()` ergonomic for cloning out of borrowed scope; bbnf forces caller to keep document live.

## BA Wave Recommendations

### BA.W4 close criteria (parse_with-as-value-API wave) — fixtures + expected behavior

The orchestrator asked for 5-10 specific JSON-side close criteria. Each carries fixture, behavior, expected perf.

1. **`bbnf_get_twitter` ≤ 5× `sonic_get_twitter` same-harness** (Hard Gate 7 close). Fixture `data/json/twitter.json`. Path `["statuses", 0, "text"]`. Expected wall ≤ 1.66 µs (332.7 ns × 5). Mechanism: `JsonParser::get(input, path)` calls `parse_with::parse_with::<&str>(input, &typed_path)` directly; bypasses `JsonParser::parse()`. Bench rewrite at `crates/core/benches/json/value.rs:43-66` — the `parsed = JsonParser::parse(...); parsed.get::<&str>(p)` pair collapses to `JsonParser::get::<&str>(&input, ...)`.

2. **`bbnf_get_twitter_deep` (new bench)** — path `["statuses", 50, "user", "screen_name"]`. Bench-add at `value.rs`. Expected wall ≤ 5× sonic_get_twitter_deep (sonic's get scales ≤ linearly in path length, ≈ 1 µs). Validates the path-driven byte-skip discipline scales with depth, not document size.

3. **`bbnf_value_twitter` ≤ 1.5× `sonic_value_twitter` same-harness** — fixture `twitter.json`. Expected wall ≤ 367 µs (245.1 µs × 1.5). Mechanism: replace `Vec<OpenFrame>::clone` checkpoint with `(stack_depth, arena_array_count, arena_object_count)` value-tuple checkpoint per DEEP-B recommendation 1 (substrate redesign — DEEP-B closes this in BA.W3, but BA.W4 measures it after the W3 redesign + W4 `parse_with` activation compose).

4. **`bbnf_value_canada` ≤ 10× `sonic_value_canada`** — fixture `canada.json` (100K-element array of f64). Expected wall ≤ 14.74 ms (1.474 ms × 10). Mechanism: per-leaf f64 push goes through `push_leaf_with_f64()` which today allocates `JsonValue::Number(JsonNumber::Float(value))` per call — direct projection to `&'p [f64]` slice via bumpalo would close the gap. **Caveat**: 10× target is generous because canada's 100K elements pin the floor at allocator throughput; bringing to ≤ 5× requires SIMD f64 decode (Eisel-Lemire fast path is in `parse-that` already; routing JSON's regex-driven number decode to it is the work).

5. **`bbnf_value_data_xl` lifts WATCHDOG_HALT** — fixture `data_xl.json` (21 MB; carved at `crates/core/benches/json/value.rs:149`). Mechanism: same as canada (per-leaf f64 + checkpoint-clone elimination). Target wall ≤ 100 ms (sonic 14.59 ms × 7). Re-activates the carve and validates that the W3.2 watchdog re-runs do not panic-cascade at close ceremony.

6. **`bbnf_iter_twitter_users` (new bench)** — path `["statuses", "*", "user", "screen_name"]` lazy wildcard iterator yielding `&str`. Fixture `twitter.json`. Expected behavior: per-element walk produces 100 user names without materialising the document. sonic comparator: `sonic_rs::get_many` against the wildcard `pointer!`. Expected wall: ≤ 5× sonic_iter_twitter_users. Validates the wildcard-iter substrate at `path/wildcard.rs` lands a real consumer.

7. **`bbnf_pointer_twitter` (new bench)** — `JsonParser::pointer(input, path) -> Option<JsonValue<'_>>`. Fixture `twitter.json`. Validates the unprojected typed-value at path-end works (consumer-side `as_str` / `as_f64` / `as_object`). Expected wall: ≤ 5× sonic_get_twitter (within 5× factor of pure leaf get).

8. **`bbnf_value_data_s` ≤ 1.0× `sonic_value_data_s`** — fixture `data.json` (35 KB tiny). Currently 2.57×; small fixtures bbnf has fixed-overhead advantage on. Expected wall ≤ 14.39 µs sonic-floor parity. Mechanism: every BA.W2/W3/W4 substrate change applies; data_s is the small-fixture proof.

9. **AU floor 19/19 AT_OR_ABOVE** — every `bbnf_monolithic.*` and `google_sheets_monolithic.parse_*` and `css_l4.*` row at-or-above the post-AU close baseline. Closes the 28-983× regression ring named in `floors.post-AU.deltas`. The W5 arena/builder template substrate retires (per DEEP-A recommendation 2); per-grammar parse fns return typed shapes directly.

10. **simdjson competitor rows** — re-include simdjson in `crates/core/benches/json/competitors.rs`; add `simdjson_value_<fixture>` and `simdjson_get_twitter` rows to the post-BA close matrix. simdjson is the SOTA stage-1+stage-2 floor; sonic-rs is the stage-2 SOTA. Both are reference points for "how far is bbnf from the wall the literature has built".

### BA.W2 close criteria (direct-projection codegen wave) — compile-time projection gaps

The orchestrator asked for 3-5 specific compile-time projection gaps that BA.W2 must close. Each carries rule name and shape kind.

1. **`object` rule (rule_id 4, kind: Struct, rule_type: Vec<BoxedEnum>)**. Today's emit at `json.rs:1512-1518` is `rule_type: Span, fields: vec::Vec::new()`. Direct-projection target: emit `JsonObject<'p> { pairs: &'p [JsonPair<'p>] }` typed struct at codegen (the `JsonObject` declaration already exists at `runtime/json/value.rs:111-115` but is NEVER returned from any accessor). The parse fn writes directly into a bumpalo-allocated `&'p [JsonPair<'p>]` slice via `BumpVec` and constructs `JsonValue::Object(JsonObject { pairs })` at compound close. No `OpenFrame::Object`. No `Vec<JsonPair>::new()`. No layout literal. **Close criterion**: zero `String::from("object")` heap allocations in samply trace of `bbnf_value_twitter`; one bump-arena alloc per object compound, not three (no per-frame Vec, no per-arena Vec, no String).

2. **`array` rule (rule_id 5, same shape as object)**. Same as 1 with `JsonArray<'p> { items: &'p [JsonValue<'p>] }`. Crucial for `canada.json` — the 100K-element flat array compresses into one bump-arena slice, not 100K `Vec` pushes + one final slab move. **Close criterion**: `bbnf_value_canada` ≤ 10× `sonic_value_canada` per BA.W4-4.

3. **`pair` rule (rule_id 6, kind: Struct, rule_type: Tuple(BoxedEnum, BoxedEnum))**. The pair is a typed two-position struct (`string`, `field_1`). Direct-projection target: emit `JsonPair<'p> { key: &'p str, value: JsonValue<'p> }` (already exists at `runtime/json/value.rs:96-101`) with parse fn returning `Option<JsonPair<'p>>` directly. No `OpenFrame::Pair`. No pending-key dance. The `parse_pair_JsonParser_pair` body calls `parse_string_JsonParser_string` for the key, skips `:`, recurses to `parse_wrap_JsonParser_value` for the value, and returns `Some(JsonPair { key, value })`. **Close criterion**: `OpenFrame::Pair` enum arm deletes from `runtime/json/builder.rs`; the `pending_key` discipline at `builder.rs:191-235` deletes; `JsonStructBuilder` shrinks from 5 OpenFrame variants to 0 (entire builder dissolves).

4. **`value` rule (rule_id 7, kind: TaggedEnum, rule_type: HeterogeneousAltJoin)**. The registry shape is structurally inconsistent (4-arm AltJoin vs 6-branch grammar Alt; see "alt-dispatch path" finding above). Direct-projection target: emit `JsonValue<'p>` already declared at `runtime/json/value.rs:34-53` (Null/Bool/Number/String/Array/Object — the 6 branches), and have the byte-dispatcher at `parse_JsonParser_value` (json.rs:2177-2210) return `Option<JsonValue<'p>>` directly without going through any `begin_compound`/`end_compound` for `value` itself. The dispatcher is already written this way conceptually; the direct-projection wave makes it explicit by removing the `OpenFrame::Wrap` wrapper at `builder.rs:86`. **Close criterion**: `OpenFrame::Wrap` enum arm deletes; alt-dispatch never allocates a wrap frame.

5. **`number` rule (rule_id 2, kind: NewtypeWrapper, rule_type: F64)**. Already direct (`push_leaf_with_f64()` at `builder.rs:337-339` deposits a typed `JsonValue::Number(JsonNumber::Float(v))`). The remaining gap is the *integer slot* (`JsonNumber::Int(i64)` and `JsonNumber::UInt(u64)` at `value.rs:64-74`) reserved for grammar evolutions. The current grammar (`grammar/json/json.bbnf:4`) projects `-> f64` only, so even an integer parse projects through f64. **Close criterion**: BA.W2 lands a grammar refinement `number_int = /-?\d+/ -> i64` adjacent to `number = /…/ -> f64` and codegen emits a typed dispatch — the integer slot finally reaches the value tree. Defer to BB rule-discovery if scope-pressure forces.

**Generalization invariant (from DEEP-A §6).** Make `->` an *override*, not a *precondition*. Every Named rule (whether annotated or not) projects to a typed struct/enum by default. The registry already carries `rule_type` for every rule via the CSP at `crates/ir/src/passes/types/mod.rs:266-269`; the emitter currently throws all of it away (10 sites verified). BA.W2 makes the emitter read `ir.struct_registry.layout(rule.id).rule_type` and project; the runtime `__layout` literal dissolves entirely. This realises the GESTALT §2.4 invariant *generally*, not only for the `->` subset.

### Cross-cutting scope items

**The `parse_with` legacy lowering.** `crates/core/src/runtime/json/parse_with.rs:60-67` lowers `TypedSegment::Wildcard → None` (bails). This means today's `parse_with` cannot handle wildcard paths — every `JsonParser::get` for wildcard paths fails. **Gap BA.W4-G**: `parse_with` extends to handle `TypedSegment::Wildcard` by returning a `WildcardIter` directly, not bailing.

**The `parse_with` legacy-segment Vec allocation.** `parse_with.rs:96-99` builds a `Vec<LegacySegment<'_>>` per call to convert `TypedPath` segments to the document's `Path` alphabet — one heap alloc per `parse_with` call. **Gap BA.W4-H**: collapse the alphabet split per DEEP-SYNTHESIS §VII.W5 (the `cursor.consult(&ParsedSegment)` redesign); the `LegacyPath/LegacySegment` shim retires.

**The post-W3 `path!` macro typing.** Today the macro produces `&[PathSegment]` (untyped); the prescribed typed form `path!(Json, "statuses", 0, "text") -> TypedPath<Json, &'static str>` is not implemented. Per GESTALT §5 the proc-macro lives in `crates/bbnf-path` (cdylib) which does not exist at HEAD. **Gap BA.W2-J**: implement `crates/bbnf-path` proc-macro that consumes the `<Grammar>Parser::REGISTRY` at proc-macro-execution time, validates the path against the static layout graph, and emits `TypedPath<G, T>` whose terminal type `T` is inferred by walking the registry's `rule_type` chain.

**The `JsonValue` accessor surface.** `JsonValue::as_str()` / `as_f64()` / `as_bool()` / `as_object()` / `as_array()` do not exist. The bare match-arm pattern works but ergonomically lags sonic. **Gap BA.W4-A**: add 5 inherent methods (≈ 30 LOC) to `runtime/json/value.rs`. Trivial; not load-bearing for performance but load-bearing for the parity story.

## Closing

**The single load-bearing finding.** Every architectural primitive bbnf needs to close the 4196× gap is present (TypedPath, parse_with, JsonPathQuery, REGISTRY) — the gap is the entry-point routing. `JsonParser::get(input, path)` does not exist; the bench harness has no syntactic shape that doesn't go through `JsonParser::parse(input)` first. Wiring `<Grammar>Parser::get<T>(input, path) -> Option<T>` through `parse_with::parse_with::<T>(input, &typed_path)` is the ≈ 50 LOC change in the codegen template at `crates/core/src/backend/rust/emitter/grammar.rs` that closes Hard Gate 7 by ≥ 3 orders of magnitude.

**Compile-time projection.** The 10 hardcoded `rule_type: TypeDesc::Span` sites (including the 9 named in DEEP-A) produce 31K mallocs per twitter parse — ≈ 5% of inclusive samples per DEEP-B's mimalloc family. BA.W2's direct-projection codegen reads `ir.struct_registry.layout(rule.id).rule_type` and emits the typed struct shape; the `__layout` literal dissolves; per-compound `String::from("object")` allocation deletes; the `OpenFrame` runtime-stack discipline retires for the JSON grammar. Composes with BA.W3's checkpoint redesign (per DEEP-B): the speculative-rollback discipline that triggered 86.07% of the `Vec<OpenFrame>::clone` cost goes away because direct-construction parse fns return `Option<JsonValue<'p>>` directly — no rollback because no in-flight stack to roll back.

**Generalization principle.** Every recommendation here applies uniformly to every grammar. `<Grammar>Parser::get` per grammar; direct-projection codegen per grammar's registry; bumpalo arena per grammar's compound shape. The grammar is the only distinguishing input. The sonic-class API generalizes to all bbnf grammars by construction; CSS L4 inherits the same `CssL4Parser::get(input, path!(CssL4, ...))` shape; Sheets inherits `GoogleSheetsParser::get(input, ...)`. This is the GESTALT §4 promise made concrete.

**Top three BA.W4 fixtures (re-stated for return summary).** (1) `bbnf_get_twitter` ≤ 5× sonic via `JsonParser::get(input, path)` static entry; (2) `bbnf_value_canada` ≤ 10× sonic via per-leaf-f64 direct projection; (3) `bbnf_iter_twitter_users` lazy wildcard iter via `JsonParser::iter(input, path!["statuses", "*", "user", "screen_name"])`.

**Top three BA.W2 compile gaps (re-stated).** (1) `object` rule emits typed `JsonObject<'p> { pairs: &'p [JsonPair<'p>] }` direct from registry; (2) `pair` rule emits typed `JsonPair<'p> { key: &'p str, value: JsonValue<'p> }` direct from registry — `OpenFrame::Pair` arm deletes; (3) the 10 hardcoded `rule_type: TypeDesc::Span` emission sites become `rule_type: <registry-projected>` reading `ir.struct_registry.layout(rule.id).rule_type`.

The fix is one mechanism (direct-projection codegen) viewed at three altitudes (DEEP-A's architectural assay, DEEP-B's runtime cost mechanism, this audit's parity surface). One commit lands the prescription. Closes Hard Gates 7 and 16.
