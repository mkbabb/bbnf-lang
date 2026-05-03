# DEEP-A — Struct-Projection Assay

## The User's Question

> "Why is struct projection not wired up? The user reads the post-AZ-IV
> state as having a runtime arena/builder template registry indirection
> where direct-projection codegen should live."

> "Type inference for `->`-less rules: even without an explicit `->`
> annotation, the type inference system should infer the type and project
> into a struct."

This audit traces the parse-to-typed-value path end-to-end, identifies
the indirection layer, and states the architectural defect. Rust only.

## End-to-End Data Flow (one fixture)

Input: `{"title":"hi"}`. Target: a typed `JsonValue::Object(_)` rooted in
a `JsonDocument<'_>`. The hops below are the actual journey at HEAD
(commit `15e1e5a1`, post-AZ-IV close).

1. **Entry point** — `JsonParser::parse(input)` lives in the emitter
   shell at
   `crates/core/src/backend/rust/emitter/grammar.rs:354-361`
   (`pub fn parse(input: &str) -> ::core::result::Result<JsonDocument<'_>, ParseErr>`).
   Generated body lives at
   `crates/core/src/grammar/generated/json.rs:3434-3470`.
2. **Parse-body construction** — the body emits `let mut builder =
   JsonStructBuilder::new();` at
   `crates/core/src/backend/rust/emitter/grammar.rs:404` and constructs a
   `PathCursor` over the eager (always-`ParseFully`) static empty path
   at `grammar.rs:419-425`.
3. **Dispatcher entry** — control routes into
   `parse_JsonParser_value` at `json.rs:2177-2210` which byte-dispatches
   on the first non-ws byte: `b'{' → parse_object_JsonParser_object`
   (`json.rs:2210`).
4. **Per-rule parse fn — runtime layout literal** —
   `parse_object_JsonParser_object` at `json.rs:1495-1623` constructs an
   inline `__layout` literal at `json.rs:1512-1518`:
   ```rust
   let __layout: ::bbnf_ir::registry::StructLayout =
       ::bbnf_ir::registry::StructLayout {
           rule_id: 4u32 as ::bbnf_ir::RuleId,
           rule_name: ::std::string::String::from("object"),
           kind: ::bbnf_ir::registry::LayoutKind::Struct,
           rule_type: ::bbnf_ir::TypeDesc::Span,    // <-- hard-coded SPAN; not the projected type
           fields: ::std::vec::Vec::new(),          // <-- empty; not the projected fields
       };
   let __handle = builder.begin_compound(&__layout);
   ```
   This literal is constructed **on every parse call**, allocating a
   `String::from("object")` per `{`.
5. **Builder dispatch** — `JsonStructBuilder::begin_compound` at
   `crates/core/src/runtime/json/builder.rs:261-294` matches on
   `(layout.kind, layout.rule_id)` integer literals (`5 = array`,
   `4 = object`, `6 = pair`) to push the matching `OpenFrame` variant
   onto a runtime stack. The registry-projected `rule_type` and
   `fields` are **never read** here — the dispatch consults only `kind`
   and `rule_id`.
6. **Body parse — key push** — `parse_string_JsonParser_string`
   (`json.rs:1387-1481`) byte-scans the string body, decodes escapes
   into the arena (or borrows zero-copy from input), and calls
   `builder.push_leaf_with_str(body)` (`json.rs:1420`).
7. **Builder leaf deposit** — `push_leaf_with_str` at
   `builder.rs:357-366` performs an `unsafe transmute` to lifetime-extend
   the slice to `'p` and routes through
   `deposit(JsonValue::String(s))` at `builder.rs:181-235`.
   `deposit` walks the open-frame stack — for the topmost
   `OpenFrame::Object`, the string promotes to `pending_key`
   (builder.rs:191-207). At `:`, value parsing recurses through
   `parse_wrap_JsonParser_value` (`json.rs:1587`) → `parse_string_…` for
   the inner `"hi"`. The inner string lands in the same `Object` frame,
   pairing the pending key into `JsonPair { key, value }`
   (builder.rs:212-214).
8. **Compound close** — `parse_object_JsonParser_object` reaches the
   matching `}` at `json.rs:1597-1599`, calls
   `builder.end_compound(__handle)`. `end_compound` at `builder.rs:296-334`
   pops the `OpenFrame::Object`, pushes the pair `Vec` into the arena
   (`arena.push_object(pairs)` → `JsonObjectId(1)`), constructs
   `JsonValue::Object(id)`, and `deposit`s the value to the parent —
   here the empty stack, so it lands as `self.root`.
9. **Finalisation** — back in the emitter shell at `grammar.rs:466`,
   `builder.finalise(input)` (`builder.rs:164-175`) returns
   `JsonDocument::new(self.arena, root, input)`.
10. **`Document::get` projection (path query)** — for `bbnf_get_twitter`
    at `crates/core/benches/json/value.rs:44-66`,
    `parsed.get::<&str>(path)` walks the **already-materialised**
    document via `walk_path` at `document.rs:370-392`. The walker linearly
    scans `pairs.iter().find(|p| p.key == name)` per object segment.

**Indirections counted between input bytes and `JsonValue::Object(_)`:**

- One runtime `String::from("object")` allocation **per `{`**
  (`json.rs:1514`).
- One `Vec<OpenFrame>` push **per compound** (`builder.rs:291`).
- One `Vec<JsonValue>` / `Vec<JsonPair>` allocation **per compound body**
  (the OpenFrame's `items`/`pairs` Vec, `builder.rs:63-71`).
- One arena `Vec` push **per closed compound** (`arena.rs:126-146`).
- One `JsonValue::Object(JsonObjectId(_))` indirection on read — every
  walker step calls `doc.object(id) -> &[JsonPair]` (`document.rs:108-111`)
  to resolve the handle through the arena slab. Rust cannot inline the
  resolution past the arena's `Vec<Vec<JsonPair>>` boundary; the typed
  enum is **handle-rooted**, not direct.

## The `project_types` Pass — Type Inference Coverage

`project_types` (`crates/ir/src/passes/types/mod.rs:51-491`) runs a CSP
over every IR node, populating four sidecar maps on `GrammarIR`:

- `ir.types: Vec<(RuleId, TypeDesc)>` — rule-level projected types
  (mod.rs:489-490).
- `ir.type_map: TypeMap` — per-NodeId type, vec-elem type, seq-result
  type, structural-type-pre-collapse (mod.rs:488).
- `ir.type_obligations: Vec<TypeObligation>` — named diagnostics for
  under-determined Refs/Alts (mod.rs:486).
- `ir.struct_registry: StructRegistry` — per-rule `StructLayout`
  with discriminator + fields + projected `rule_type`
  (mod.rs:451 → registry.rs:64-97).

### `->` annotated rules

Annotated rules close the projection:
`number = /…/ -> f64` resolves to `TypeDesc::F64` via
`fn_descriptor_return_type` (registry.rs:458-473), reaches the registry
as `StructLayout { kind: NewtypeWrapper, rule_type: F64,
fields: [{ name: "value", type_desc: F64, source: TypedLeaf }] }`.
The number-shape emitter at
`crates/core/src/backend/rust/emitter/shapes/number.rs:182` does emit
`builder.push_leaf_with_f64(value)` — so for **scalar** annotated rules
the f64 reaches the JSON builder's deposit path. **But it does not
reach the layout literal** — `rule_type` in the runtime literal is
hard-coded `TypeDesc::Span` regardless (see "Layout literal" §
following). The push side projects, the begin-compound side does not.

### `->`-less rules

The user's question. **Type inference DOES produce a TypeDesc for
every rule, annotated or not.** registry.rs:114:

```rust
let rule_type = rule_types.get(&rule.id).cloned().unwrap_or(TypeDesc::Span);
```

The CSP solves rule_vars for every rule (mod.rs:266-269). Compound
rules without `->` resolve through the structural CSP — `Seq` produces
a `Tuple`, `Alt` produces an `Enum` / `BoxedEnum` / `HeterogeneousAltJoin`,
`Repeat` produces a `Vec`, etc. The fallback to `TypeDesc::Span` in
registry.rs:114 fires only when the CSP completely fails to solve, which
the cycle-break grounding (mod.rs:83-129) prevents.

**The gap is on the emitter side, not the inference side.** The
registry is populated, every rule has a `StructLayout`, every layout
carries a `rule_type` — but the emitter does not consume them. The
hard-coded `TypeDesc::Span` in the layout literal at nine emitter sites
(see grep `rule_type: ::bbnf_ir::TypeDesc::Span` across
`backend/rust/emitter/shapes/{object,array,arglist,unordered,
flat/struct_direct,pratt/struct_direct,wrap/struct_direct,
alt_dispatch/mod}.rs`) is the smoking gun. Inference produces the
type; the emitter discards it.

### Compound rules (Object/Array)

`object` and `array` rules both project to compound layouts with
typed children. The registry classifies them as
`LayoutKind::Struct` with one `RepeatElement` field (for `array`) or
multiple `SeqPosition` fields (for `object`'s `pair`). This metadata
is **never consulted at parse time** — `JsonStructBuilder::begin_compound`
only inspects `(kind, rule_id)`. A regenerated grammar that changed the
field shape would require code regen *and* a manual edit to
`builder.rs:270-290` to keep in sync.

### Alt branches

`Alt` rules project to `LayoutKind::TaggedEnum` (heterogeneous) or
`LayoutKind::UntaggedEnum` (homogeneous). For JSON's `value = object | array | string | number | bool | null`, every branch's
typed projection is in the registry. The emitter handles this through
the **per-shape dispatcher** (`json.rs:2177-2210`) which byte-dispatches
on the first character to the matching shape fn. The branch tag reaches
the builder via `push_branch_tag(idx)` (`builder.rs:374-381`) but for
JSON the tag is **discarded** because `JsonValue` is untagged — the
sum is structural (one variant per `JsonValue` arm).

### Pratt rules

`pratt/struct_direct.rs:130-135` emits a layout literal with
`rule_type: TypeDesc::Span`. Pratt's emitter walks the precedence LUT;
the operator/operand projection lives in
`pratt/...` shape code, not in the layout. Same divorced shape: the
inference produces a Pratt-typed `TypeDesc`, the emitter ignores it.

### Repeat rules

`Repeat { lo, hi }` rules project to `LayoutKind::Struct` with one
`RepeatElement` field carrying `TypeDesc::Vec(inner)`. The registry
captures this. The shape emitters under
`crates/core/src/backend/rust/emitter/shapes/{flat,wrap}/struct_direct.rs`
emit Vec-iteration loops, but the loop body's per-element push goes
through `builder.deposit` (a runtime stack push) — not into a typed
struct field whose offset is known at compile time.

## The Arena/Builder Template Indirection (W5.3)

Two template modules embody the fault:

- `crates/core/src/runtime/arena_template.rs:80-134` — `CompoundSlabArena<C>`
  with one `Vec<C>` per compound, generic over `CompoundEntry`.
- `crates/core/src/runtime/builder_template.rs:121-286` — generic
  `SimpleStructBuilder<'p, V, C>` whose `begin_compound` clones the
  layout (`builder_template.rs:222-228`), pushes a `Frame { layout,
  branch_tag, children }` onto a runtime stack, and on `end_compound`
  builds a `C::new_entry(layout, branch_tag, children)` and pushes it
  to the slab.

**The disqualifying defect:** `SimpleStructBuilder::push_leaf_with_f64`
(`builder_template.rs:243-245`) is

```rust
fn push_leaf_with_f64(&mut self, _v: f64) {
    self.deposit(V::unit());
}
```

The f64 value is **discarded** — the simple-cohort template deposits
`V::unit()` for every typed leaf push (f64, i64, u64, bool — only str
preserves the value via the `Span` arm). Five grammars (BNF, EBNF, CSV,
CSS Pretty, Math) are routed through this template and **lose every
typed leaf payload at parse time**. The grammar's `->` annotations are
syntactically valid, type-inferred, and enrolled in the registry, but
the runtime erases them.

### Why the indirection costs 28-65×

`docs/tranches/AZ-IV/audit/W6-fat-lto.txt:62-67` names the regression:
"the AZ-IV W5 arena/builder template substrate replaced AU's flat
per-grammar arenas; the registry indirection costs 28-65× on
bbnf_self/sheets and 1.9-118× on json_monolithic". The cost has three
components:

1. **Layout-literal allocation per parse fn entry** — every
   `parse_object_*` / `parse_array_*` / `parse_pair_*` call constructs
   a fresh `String::from("object")` (`json.rs:1514`,
   `json.rs:1653`). On `canada.json` (100k-element float array), the
   array variant fires 100k times → 100k pointless `String`
   allocations.
2. **Per-compound `Vec<OpenFrame>`/`Vec<Frame>` push + per-body
   `Vec<JsonValue>`/`Vec<Frame::children>`** — every depth-N nested
   compound costs N `Vec` allocations on the stack and one slab `Vec`
   push at `end_compound`. simdjson uses a single arena tape; bbnf
   uses N+1 per-compound vectors.
3. **Read-side handle resolution** — `JsonValue::Object(id)` forces
   a `arena.objects[id-1].as_slice()` indirection on every read
   (`arena.rs:161-166`). The compiler cannot inline past the
   `Vec<Vec<JsonPair>>` boundary; cache locality is destroyed by the
   double indirection.

### Could the per-grammar arena/builder be REPLACED?

Yes, and this is the architectural transposition the user is asking
about. A **direct emission** would generate, per grammar, a typed
struct shape per registered `StructLayout`:

```rust
// derived from registry::StructLayout for `pair`
struct JsonPair<'p> { key: &'p str, value: JsonValue<'p> }
```

The parse function would write directly into the typed struct's fields
**by offset**:

```rust
fn parse_pair(input, p, state) -> Option<JsonPair<'p>> {
    let key = parse_string(input, p, state)?;
    skip_colon(input, p)?;
    let value = parse_value(input, p, state)?;
    Some(JsonPair { key, value })  // direct struct construction
}
```

No `OpenFrame`. No `Vec<OpenFrame>`. No `__layout` allocation. No
`begin_compound`/`end_compound` indirection through a builder trait.
No handle/arena resolution on read — the typed struct holds the value
directly (with sub-tree allocations going into a single arena pool).

The concrete blocker today: every shape emitter in
`crates/core/src/backend/rust/emitter/shapes/{object,array,wrap,…}/`
emits a `builder.begin_compound(&__layout)` /
`builder.push_leaf_with_*` / `builder.end_compound(__handle)` triple
— a uniform "build into runtime stack" template. Rewiring requires the
emitter to read `ir.struct_registry.layout(rule.id)` and emit a struct
literal per rule, threading the typed `TypeDesc` through every
shape-fn signature so the parser fns return the typed shape directly.
**No file currently emits a `RuleStructName { field: parsed_value, … }`
literal.** Every one of the nine `rule_type: TypeDesc::Span` sites
must be replaced with the registry-projected type, and the layout
literal must dissolve entirely (the shape is determined at codegen
time, not runtime).

## The Intermediate Untyped Phase — Identified

GESTALT.md §2.3-2.4 ("typed materialisation requires direct-to-struct,
because anything else re-derives shape after inference has already
composed it"). The intermediate untyped phase exists in three concrete
data structures:

1. **The runtime `__layout: StructLayout` literal** —
   `json.rs:1512-1518`, emitted at every parse-fn entry. This is
   *runtime data describing static structure*. The structure is known
   at codegen time; passing it through a runtime value is the
   indirection. It also carries `rule_type: TypeDesc::Span` and
   `fields: vec![]`, *deliberately* erasing the registry-projected
   data.

2. **`OpenFrame<'p>` (and the simple-cohort `Frame<'p, V>`)** —
   `builder.rs:60-87` and `builder_template.rs:92-106`. A runtime sum
   over `{ Array, Object, Pair, Wrap }` that mirrors the static
   `LayoutKind` enum. Every begin_compound walks the runtime stack to
   pick the matching variant. The static shape that codegen knows is
   re-derived at every parse step.

3. **`JsonValue::Object(JsonObjectId)` handle indirection** —
   `value.rs:34-53` and `arena.rs:32-85`. The typed enum stores arena
   handles, not direct slices. Read-side projection requires a
   `doc.object(id) -> &[JsonPair]` resolution against a `Vec<Vec<…>>`
   slab. Rust cannot inline past the slab; the cache misses are
   structural. simdjson stores compact tape offsets; sonic-rs stores
   raw spans; bbnf stores `Vec<Vec<…>>` indices.

The simple-cohort template's value-erasure
(`builder_template.rs:243-245`) is the **fourth** intermediate phase —
the typed leaf is discarded at deposit time. This is the most stark
violation of the typed-materialisation invariant: the f64 produced by
the parse-time scanner reaches `push_leaf_with_f64`, then is
**replaced with `V::unit()`** before deposit. For five grammars, the
typed value never makes it into the document tree at all.

## SOTA Comparison

### sonic-rs `pointer!` mechanism

`sonic_rs::get(input, sonic_rs::pointer!["statuses", 0, "text"])` at
`/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/sonic-rs-0.5.8/src/lazyvalue/get.rs:398`
returns a `LazyValue<'_>` that **borrows from input bytes directly**.
sonic-rs does not build a document; it scans forward in bytes, skipping
matched-but-uninteresting subtrees, until the path resolves. The
returned `LazyValue` is a span pointer into the input — `as_raw_str()`
yields the raw bytes; `as_str()` decodes only on demand.

**Why 332ns vs bbnf's 1.396ms (4196×):** sonic-rs does not *parse* in
bbnf's sense. It byte-scans with simdjson-style skip routines. bbnf's
`bbnf_get_twitter` calls `JsonParser::parse(input)` *first* (a full
materialisation), *then* walks the document tree. The W3 lazy
bail-out parse exists (`crates/core/src/runtime/json/parse_with.rs`)
but `JsonDocument::get` does not call it — `bench/value.rs:51` calls
`parsed.get::<&str>(p)` against an eagerly-parsed document. Even
without the eager-vs-lazy seam, the cost of building the
`Vec<JsonValue>`/`Vec<JsonPair>` arena slabs dominates over a single
byte-scan extraction.

### simdjson OnDemand mechanism

simdjson's OnDemand surface streams across the input bytes, returning
a `Value` cursor that resolves keys/indices on demand. The internal
representation is a 16-byte tape-record stream (compound-open,
compound-close, scalar with payload), parsed with SIMD lookahead. No
allocation per element. bbnf's pre-AZ-II tape achieved this; AZ-II
deleted it in favour of struct-direct, but the struct-direct
substrate (W5 arena/builder template) reintroduced the indirection in
a less efficient form (per-compound `Vec`s instead of one tape).

### What we should mirror with better ergonomics

The grammar-derived advantage is that we know the static struct
shape at codegen time. sonic-rs / simdjson do not — they're written
for one shape (JSON) and use byte-walk discipline because the static
type is unknown. bbnf has the registry; it can emit *typed structs
per rule, with parse functions that construct them directly*. This is
the GESTALT §2.4 promise. The current state has the registry but no
emitter consumer.

## The Architectural Defect — Stated Plainly

**The struct registry is populated at codegen time but never consumed
at parse time.** The emitter emits a `__layout: StructLayout` runtime
literal carrying `rule_type: TypeDesc::Span` and `fields: vec![]`
(every emit site, hard-coded), then dispatches it through a
`begin_compound` builder method that consults only `(kind, rule_id)`
integer literals — discarding the projected types and fields entirely.
The registry is, in the current emission path, a **vestigial
substrate**: populated by `project_types`, serialised across the IR
boundary, used by the audit pass, and then ignored by the runtime
parse code. Type inference produces a usable `TypeDesc` for every
rule, annotated or not; the emitter throws every one of them away
nine times over.

## Concrete Mechanism for Direct-Projection Codegen

The new codegen must emit, per grammar, **one typed struct per
registered `StructLayout`** whose fields directly carry the
`TypeDesc`-projected types. Per the JSON grammar:

```rust
// emitted from StructRegistry — no runtime construction
pub struct JsonObject<'p> { pub pairs: &'p [JsonPair<'p>] }
pub struct JsonPair<'p>   { pub key: &'p str, pub value: JsonValue<'p> }
pub struct JsonArray<'p>  { pub items: &'p [JsonValue<'p>] }
pub enum   JsonValue<'p>  { Null, Bool(bool), Number(JsonNumber),
                            String(&'p str),
                            Object(JsonObject<'p>),  // owned slice, not handle
                            Array(JsonArray<'p>) }
```

Per-rule parse functions return the typed shape directly:

```rust
fn parse_pair_JsonParser_pair<'p, 'a>(input: &'p [u8], p: &mut usize,
    state: &mut ScanState, arena: &'a Bump) -> Option<JsonPair<'p>>
{
    let key = parse_string_JsonParser_string(input, p, state)?;
    skip_colon(input, p)?;
    let value = parse_wrap_JsonParser_value(input, p, state, arena)?;
    Some(JsonPair { key, value })
}
```

Compound bodies allocate slices into a single bump arena (`bumpalo`
already a transitive dep per `arena.rs:21`). No `OpenFrame`. No
`Vec<OpenFrame>`. No `__layout` literal. No `begin_compound` /
`end_compound` builder dispatch.

### Files that change

- `crates/core/src/backend/rust/emitter/shapes/{object,array,arglist,
  unordered,flat/struct_direct,pratt/struct_direct,wrap/struct_direct,
  alt_dispatch/mod,keyword/struct_direct}.rs` — every `__layout`
  emission site rewires to read the registry layout and emit a typed
  struct literal at compound close.
- `crates/core/src/runtime/json/{builder.rs,arena.rs,
  builder_template.rs,arena_template.rs}` — `JsonStructBuilder`,
  `JsonArena`, `SimpleStructBuilder`, `CompoundSlabArena` all delete.
  Replaced by a thin per-grammar bump-arena allocator and direct
  struct construction in parse fns.
- `crates/core/src/runtime/json/value.rs` — `JsonValue::Object`/
  `Array` arms switch from handle to `&'p [JsonPair<'p>]` /
  `&'p [JsonValue<'p>]` (or stay enum-based but resolve through a
  bump-allocated slice, not a `Vec<Vec<…>>` slab).
- `crates/core/src/grammar/generated/json.rs` regenerates with the
  new shape; expected size delta is significantly smaller (no
  per-rule `__layout` constructor, no `String::from` per `{`).

### What stays

- `project_types` and `StructRegistry` — they ARE the source of
  truth; the new emitter consumes them. No registry change required.
- `JsonValue` enum surface (the typed sum) — stays.
- `JsonDocument::get<T>(path)` — stays (path-walking against a
  typed tree gets cheaper, not more expensive).

## Why `->`-less Rules Should Project Anyway

Inference covers them. `project_types` (mod.rs:51-491) populates
`ir.types` for every rule via the CSP — annotated or not. Compound
rules without `->` get a structural `TypeDesc` (Tuple from Seq,
Enum/BoxedEnum from Alt, Vec from Repeat). The CSP cycle-break
grounding (mod.rs:83-129) ensures every rule resolves to a usable
type; the `unwrap_or(TypeDesc::Span)` in registry.rs:114 fires only
in the truly-unsolvable case.

The gap is the emitter's hardcoded `TypeDesc::Span` in the layout
literal, which **defeats the inference layer downstream**. Fix: the
emitter reads `ir.struct_registry.layout(rule.id).rule_type` and emits
that into the typed struct shape per rule; `->` becomes purely a
grammar-author's *override* hint, not a precondition for typed
projection. A grammar without `->` annotations on compound rules still
derives a typed Pair / Object / Array struct per registry, with field
types inferred structurally from child rule projections. This matches
the GESTALT promise: "every `->` in the grammar projects directly to a
typed record at emission time" generalises to "every Named rule
projects to a typed record at emission time, with `->` overriding the
default structural projection on the leaf".

## Recommendations to DEEP-C

1. **Delete the `StructLayout` runtime literal pattern entirely.**
   Every `__layout: StructLayout = StructLayout { … }` emission site
   (nine in `crates/core/src/backend/rust/emitter/shapes/`) replaces
   with direct struct-shape codegen reading from
   `ir.struct_registry.layout(rule.id)`. The layout literal is
   *runtime data describing static structure* — pure overhead. Per
   `feedback_no-orthogonal-codepaths`, this is the orthogonal-codepath
   the audit identifies.

2. **Replace `JsonStructBuilder` + `OpenFrame` runtime stack with
   parse-fn direct struct return values.** Per-rule parse fns return
   `Option<JsonPair<'p>>`, `Option<JsonObject<'p>>`, etc., constructed
   in-place. No builder trait. No `begin_compound` / `end_compound`
   dispatch. The `crates/core/src/runtime/builder.rs` `StructBuilder`
   trait deletes; the `crates/core/src/runtime/{json,sheets,…}/
   builder.rs` per-grammar files delete; `arena_template.rs` /
   `builder_template.rs` delete (the dedup target dissolves because
   the per-grammar parse fns emit the typed shape directly).

3. **Switch the arena from `Vec<Vec<…>>` slabs to a single `bumpalo`
   bump arena per parse.** Replace
   `JsonArrayId(u32)`/`JsonObjectId(u32)` with `&'p [JsonValue<'p>]` /
   `&'p [JsonPair<'p>]` allocated inside the bump. Read-side
   projection is one pointer dereference instead of a slab index +
   inner-Vec-deref. `bumpalo` is already a transitive dep
   (`arena.rs:21`).

4. **Activate the `simple-cohort` typed-leaf push.**
   `crates/core/src/runtime/builder_template.rs:243-260` — every typed
   leaf push (`push_leaf_with_f64` / `_i64` / `_u64` / `_bool`)
   currently calls `self.deposit(V::unit())`, **discarding the value
   entirely**. This is the most stark violation of the typed-materialisation
   invariant in the entire codebase: 5 grammars (BNF, EBNF, CSV, CSS
   Pretty, Math) lose every typed `->` leaf at parse time. Even before
   the larger architectural fix, this trivial replacement (deposit a
   typed `V` arm carrying the value) closes the leak. Better: dissolve
   the template entirely per recommendation 2.

5. **Reroute `JsonDocument::get` through the lazy parse_with for
   bbnf_get_twitter parity.** `parse_with` exists at
   `crates/core/src/runtime/json/parse_with.rs:77-103` but
   `Document::get` does NOT use it — the bench at
   `crates/core/benches/json/value.rs:44-66` calls
   `JsonParser::parse(input)` (eager, full-tree) then walks the
   document. The 4196× sonic gap is structural to this seam. Wiring
   `Document::get<T>(path)` to the lazy path internally is independent
   of the larger struct-direct refactor and closes Hard Gate 7
   immediately.

6. **(Optional, encoded for completeness)** Make `->` an OVERRIDE,
   not a PRECONDITION. The registry already carries a `rule_type` for
   every rule, annotated or not. The new emitter projects every rule
   to a typed struct by default, with `->` annotations overriding the
   default structural projection at the leaf. This realises the GESTALT
   §2.4 invariant generally, not only for the `->` subset.

