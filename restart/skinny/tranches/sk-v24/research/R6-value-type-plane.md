# R6 — The Value/Type Plane at HEAD

Recon cartography of skinny's value materialization + type-projection surfaces.
All paths absolute-anchored under `/Users/mkbabb/Programming/bbnf-lang/skinny/`
(the `restart/skinny/**/*.rs` are research harnesses, NOT the live tree). HEAD =
`7f3be27c2`. Every `path:line` below was opened and verified.

---

## 0. Orientation — the three materialization paths

There is no single value plane. Three distinct paths coexist, selected by a
**shape gate on a rule named `value`** at `crates/codegen/src/runtime_generator.rs:99`
(`if !sink_only.has_rule("value")`):

| Path | Trigger | Emitter | Value surface | Types? |
|---|---|---|---|---|
| **JSON offset-tape (lazy view)** | grammar has rule `value` | `runtime_generator::emit_sink_only` splices 8 **static** `json_templates/*` files via `include_str!` (`runtime_generator.rs:104-133`) | lazy `ValueRef<K>` cursor over one offset tape | none — pure structural fold |
| **JSON direct-sink (eager)** | same; appended into `generated.rs` | `json_sink_direct::render(sink_only)` (`runtime_generator.rs:106`) — grammar-derived recursive descent | `JsonSink` trait callbacks | none |
| **General-descent (Sheets, non-`value`)** | no rule `value` | `general_descent::render(sink_only, profile_id)` (`runtime_generator.rs:159`) | lazy `GoogleSheetsNode` flat-sweep view | **yes**, lossy 4-way fold |
| **Typed direct-to-struct** | opt-in, schema supplied | `emit_typed_from_source(name, src, &DirectSchemaSet)` (`codegen/src/lib.rs:130`) | user struct | yes — but schema is **hand-authored**, not grammar-derived |

Crux finding: `grammars/json.bbnf` (all 18 lines) contains **zero `->` projections**
— it is pure structural EBNF. So the entire JSON `JsonValue` taxonomy is
*recovered structurally* (role classification, §6), never declared. The `->`
type machinery is exercised **only** by the Sheets grammar on the general-descent
path. Verify: `grammars/json.bbnf:1-18`; Sheets projections at
`/Users/mkbabb/Programming/bbnf-lang/grammar/google-sheets/google-sheets.bbnf:6,12,17-18,34-42,97`.

---

## 1. The tape node layout (it is SoA, NOT AoS)

`crates/runtime/src/tape/mod.rs:92-99` — `Tape<'input>`:

```
source:       &'input [u8]      // borrowed input
offsets:      Vec<u32>          // ONE u32 per structural token = its src byte offset
flag_cursors: Vec<u32>          // sparse side-channel: cursors that carry flag bits
flag_values:  Vec<u8>           // parallel to flag_cursors (binary-searched)
payloads:     PayloadArena      // Vec<u8> blob arena; UNUSED on live JSON path
id:           TapeId
```

There is **no packed AoS node record**. A "node" = an index (`cursor: u32`) into
`offsets`. Kind/span/flags are all *re-derived lazily* from `source[offset]`, not
stored. Consequences:

- **Flags** are a sorted sparse array, not per-node bytes. `OffsetFlags(u8)` with
  `GRAMMAR_BIT0/BIT1` (`tape/mod.rs:14-34`). Lookup is a **binary search**:
  `Tape::flags_at` `tape/mod.rs:142-148`. Only one bit is live: `STRING_NEEDS_DECODE
  = GRAMMAR_BIT0` (`grammars/json/config.rs:5`).
- **Builder**: `TapeBuilder` `tape/assembler.rs:42-123`. Hot emit is
  `push_plain_offset` (`assembler.rs:71-85`, unchecked ptr write + `set_len`);
  flags patched out-of-band via `patch_flags` (`assembler.rs:94-113`, append-or-
  update-last, must be cursor-ordered).
- `JsonToken` (`grammars/json/value.rs:104-111`) is a **materialized** AoS-ish
  struct (`kind, payload_class, start, end, payload_or_skip`) but it is produced
  on-demand by `token_from_cursor` (`view.rs:416-442`) for the `token_stream`
  iterator — it is a *view export*, never the storage.
- Capacity: `CapacityPlan` (`assembler.rs:13-40`), env `BBNF_CAPACITY_PLAN`,
  live default `GrowOnly` → constant `256` (`scan.rs:64`).

The lazy-cursor contract is abstracted by the `Cursor` trait
`crates/runtime/src/tape/cursor.rs:20-36` (`tape/cursor/offset/kind`, `type Kind`
per-grammar). JSON impls it at `grammars/json/view.rs:466-488`; the doc-comment
there and in `cursor.rs:1-19` deliberately keeps navigation OFF the trait so
grammars cannot be structurally collapsed into each other.

---

## 2. The Classify / NodeClass surface (lazy, source-driven)

There is **no stored discriminator**. Kind is decoded from the first source byte
at a cursor's offset:

- `JsonNodeKind` enum `#[repr(u16)]` — `grammars/json/value.rs:10-26` (13 variants).
- `JsonNodeKind::at_cursor(tape, cursor)` — `value.rs:28-47` — the **single classify
  primitive**: `match source[offset] { b'{'=>ObjectOpen, b'"'=>String, b'-'|b'0'..=b'9'
  =>Number, ... }`. Called everywhere navigation touches a node.
- `value_from_ref` — `value.rs:143-171` — the classify→typed-wrapper switch
  producing `JsonValue::{Object,Array,String,Number,Bool,Null}`.
- Sheets analogue: `GoogleSheetsKind::at_cursor` (generated) at
  `crates/runtime/src/grammars/sheets/view.rs:122`.

This byte-reclassification is on the hot navigation path (e.g. `next_sibling_cursor`
re-decodes every node it skips, `view.rs:355-382`). A stored 1-byte class column
would eliminate it — see §8 attach sketch.

---

## 3. The eager sink

`JsonSink` trait — `crates/runtime/src/grammars/json/sink.rs:4-119` — 11 required
callbacks (`begin_object`, `key`, `string`, `i64/u64/f64`, `bool`, `null`, ...) +
sink-prefix variants (`array_*`, `object_*`) + `key_source`/`string_source`
(escape-aware, gated on `needs_unescape`, `sink.rs:17-37`).

The eager driver is `parse_direct` — generated, in `grammars/json/generated.rs:73-91`
(entry) → `parse_value_direct` (`generated.rs:95-...`, `match byte` dispatch at
`:105`) → `parse_object_direct`/`parse_array_direct` (`:664`/`:698`). No tape is
built; sink callbacks fire inline. Numbers materialize via
`parse_that_regex::number::{materialize_f64,materialize_i64,materialize_u64}`
(`generated.rs:62-64`). This IS the "direct-to-sink" emitter, produced by
`json_sink_direct` (`crates/codegen/src/json_sink_direct/mod.rs:29-40` render
pipeline; walk over `SinkOnlyExpr`).

The tape-building parse (`parse_json`, `generated.rs:18-25`) is the OTHER eager
sink: it eagerly emits the full offset tape (every structural + string + scalar
gets an offset) before any view access. There is no partial/bounded parse — §8.

---

## 4. The lazy cursor / view

`crates/runtime/src/grammars/json/view.rs` (488 lines, a **static template**
copied from `crates/codegen/src/json_templates/view.rs`, also 488). Surface:

- `JsonRoot` `view.rs:12-61` — owns `JsonDocument{ tape, source_str }`; `.value()`
  = `value_from_ref(ValueRef::new(&tape, 0))` (`view.rs:26-28`).
- `ValueRef<'doc,'input,K>` — the cursor — `tape/mod.rs:173-219`; phantom-typed by
  `K` (RootKind/ObjectKind/... zero-size enums `view.rs:453-460`).
- Navigation (all OFF the `Cursor` trait, all forward-scan, all O(n)):
  - `JsonObject::pairs()` `view.rs:90-96` → `JsonObjectPairs` iterator
    `view.rs:259-299` (forward walk, `next_sibling_cursor` to skip values).
  - **`JsonObject::get(key)` `view.rs:106-114`** — linear `pairs().find_map`, string
    compare per pair. No index. **The pointer/path-get attach point.**
  - `JsonArray::values()` `view.rs:147-153` → `JsonArrayValues` `view.rs:301-331`.
  - `next_sibling_cursor(tape, cursor)` `view.rs:355-382` — the tree-skip: for a
    container, depth-counts forward re-decoding every node. **O(subtree) skip.**
  - Spans re-derived lazily: `span_for_value` `view.rs:333-353`, `string_body_range`
    `view.rs:384-390` (re-runs `match_string`), `scalar_span` `view.rs:392-414`.
- Leaf decode: `JsonString::as_str` `view.rs:206-217` (Cow; unescape only if
  `flags_at` has `STRING_NEEDS_DECODE`), `JsonNumber::as_f64/as_i64` `view.rs:231-239`
  (via `serde_json` — a real cost).
- Recursive visitor: `visitor.rs:16-38` (`walk_value`), driven by `JsonRoot::visit`.

Sheets' lazy view is **generated** (not templated): `general_descent/view.rs`
emits `GoogleSheetsNode` with a flat `nodes()` sweep + `as_f64/as_bool/as_tag/span`
accessors (`sheets/view.rs:154-258`).

---

## 5. TypeDesc? — there is none. Types are a monotone structural fold.

**There is no `TypeDesc` type anywhere in skinny** (`rg TypeDesc` → 0 hits). The
question "do types reach emission, or is it a monotone fold?" resolves to: **a
monotone fold on every path; the richest, the general-descent path, collapses `->`
into a 4-category enum; the JSON path has no types at all.**

### 5a. The frontend `->` carrier (syntactic, then discarded)
- `ExprKind::Project { body, target: String, ty: Option<String> }` —
  `crates/ir/src/lib.rs:261-265`. `ty` is a bare `Option<String>`, not a resolved type.
- **Type inference discards it**: `Infer::infer_expr` for `Project` returns
  `self.infer_expr(*body)?` — `crates/passes/src/lib.rs:203` — the projection type
  is thrown away; the body's structural `Type` (`Type::Seq/Alt/List/...`,
  `passes/src/lib.rs` inference enum) survives. So the type lattice never sees `->`.

### 5b. `->` survives *syntactically* into the backend, then folds
- Lowering `ExprKind::Project → BackendExpr::ValueProject { body, target, ty }` —
  `passes/src/lib.rs:1349-1353` (in `lower_expr`). `ty` carried verbatim.
- On the JSON path `ValueProject` is a no-op passthrough (`json_sink_direct/
  first_set.rs:84` recurses into body; nothing reads `ty`).
- On the general-descent path `ty` IS read — but **folded to 4 categories**:
  `classify_projection(target, ty.as_deref())` → `enum ProjectionKind {Float, Bool,
  Tag, Span}` — `general_descent/view.rs:20-46`. Rule: `ty==Some("Span")→Span`;
  `target "f64"/"f32"→Float`; `"true"/"false"→Bool`; `"Nu8"→Tag`; else `Span`. This
  is the *entire* type expressiveness that reaches emission. It produces accessors
  `as_f64/as_bool/as_tag/span` (`view.rs:build_tag_table` `:190-235`), nothing more
  — no struct fields, no nesting, no aggregate types.

### 5c. The JSON struct taxonomy is *invented* by role classification
The `JsonValue`/`JsonObject`/... shapes are NOT from the grammar (json.bbnf has no
`->`). They are synthesized in `passes::extract`:
- `derive_materialization_roles(grammar)` — `passes/src/lib.rs:1403-1490` — classifies
  rules **by byte-literal / regex-kind pattern**: `{`+`}` → container
  (`:1433-1440`), `[`+`]` → sequence (`:1441-1448`), regex `SpanKind::String` →
  string (`:1404-1408`), `SpanKind::Number` → number (`:1409-1413`), literals
  `true`&`false` → bool (`:1414-1424`), `null` → null (`:1425-1432`), a rule with
  ≥4 value-target refs → `value` (`:1451-1465`), a rule with string+value+`:` →
  pair (`:1468-1479`). **This is the JSON taxonomy hardcoded as structural
  heuristics.**
- `derive_materialization_plan` — `passes/src/lib.rs:1136-1287` — turns roles into
  `MaterializationDescriptor{ kind: TapeKind, label, shape: "{Prefix}Object"..,
  fields: Vec<DirectBuildField> }`. Shape names are `format!("{prefix}Object")` etc.
- `materialize_rule` — `passes/src/lib.rs:1357-1391` — splices `SpanMark(Start) +
  body + SpanMark(End) + TapeEmit{kind} + DirectBuild{shape,fields} + Return`.
- `TapeEmit{kind: TapeKind}` (`ir/src/lib.rs` `TapeKind` 9 variants:
  Container/Sequence/KeyValuePair/StringValue/NumberValue/BoolValue/NullValue/
  Member/Element) is the coarse tag that reaches the tape emitter. The rich
  per-field `DirectBuildTarget{ type_ref, presence, cardinality, representation,
  decode }` schema (`ir/src/lib.rs:483-541`) exists but on the offset-tape path the
  `target` field is `None` for every descriptor (`passes/src/lib.rs:1149-1287`) —
  the typed layer is NOT populated from the grammar.

### 5d. The typed layer IS full-fidelity — but fed by a hand-authored schema
- `DirectSchemaSet` / `DirectTypeRef` / `DirectFieldSchema` — a real type IR —
  `crates/codegen/src/direct_schema.rs:4-120` (Struct/Vec/MapString/MapEntriesVec/
  StringEnum/Option/Scalar; presence, duplicate, capacity hints).
- `emit_typed_from_source(name, src, schema)` — `codegen/src/lib.rs:130-146` — the
  schema is an **input parameter**. `schema_direct::lower_program(sink_only, schema)`
  (`lower/schema_direct.rs:11-33`) merely `schema.validate()?` + copies the
  `direct_shape_roster` from the sink program; it does **not** derive the schema.
- The only schema producer is **hand-written**: `xtask/src/real_typed_schema.rs:7`
  (`pub fn schema() -> DirectSchemaSet`) — literal `DirectRootSchema::struct_root(
  "parse_citm_catalog", "crate::real_typed_struct::CitmCatalog<'i>", ...)` rows.
- Renderer: `json_typed_direct::render(program)` — `crates/codegen/src/
  json_typed_direct.rs:10` (1245 LOC). This is a working **direct-to-struct emitter**
  whose only missing input is a grammar-derived schema (§8b).

**Net**: types never compose. `->` is a leaf tag folded to `ProjectionKind`;
struct shape is heuristic; the one honest type IR (`DirectSchemaSet`) is authored
by hand outside the compiler.

---

## 6. Hardcode / name-gate census (verified sites)

| Site | File:line | What it gates |
|---|---|---|
| **`sink_only.has_rule("value")`** | `runtime_generator.rs:99` | THE central gate: rule named `value` → JSON templates; else general-descent. Comment claims "shape-based" but keys off the literal name `value`. |
| `has_rule` def | `lower/sink_only.rs:31-33` | `rules.iter().any(|r| r.name == name)` |
| `shape_for(...).unwrap_or("Object"/"Array"/"Pair"/"String"/"Number"/"Bool"/"Null")` | `passes/src/lib.rs:277-283` | hardcoded 7-name JSON value taxonomy fallback |
| Hardcoded `JsonValue` variant list | `passes/src/lib.rs:285-291` | `Object(..)/Array(..)/String(..)/Number(..)/Bool(bool)/Null` — the 6-arm enum baked into shape-facts |
| role classifier literal gates | `passes/src/lib.rs:1417-1431` | `b"true"`,`b"false"`,`b"null"`,`b"{"`,`b"}"`,`b"["`,`b"]"`,`b":"` byte-literals define the value kinds |
| `>= 4` value-ref heuristic | `passes/src/lib.rs:1462` | "a rule referencing ≥4 value targets is the `value` rule" |
| `classify_projection` string-match | `general_descent/view.rs:35-45` | `"f64"/"f32"/"true"/"false"/"Nu8"/"Span"` literal projection vocabulary |
| `include_str!("json_templates/{value,view,visitor}.rs")` | `runtime_generator.rs:120-133` | JSON lazy surfaces are **static files**, not grammar-derived |
| grammar-name → PascalCase prefix | `runtime_generator.rs:154`, `general_descent::grammar_pascal` | `profile_id` IS grammar name on general path |
| CSS routed by name/entry | `runtime_generator.rs:57` (`css_scan::derive_program(&grammar_name, &entry_rule, ...)`) | separate CSS scan path |

Frontend-capability gate (declarative, not name): `RuntimeFrontendRequirements`
`grammar_provider.rs:38-81` (bools: `projection`, `typed_projection`,
`whitespace_directive`, `discard_operator`, ...).

---

## 7. The ws-skip surface

- Grammar: `ws = /[ \t\n\r]*/` (`grammars/json.bbnf:6`), threaded explicitly through
  `value`/`pair`/`array`/`object`/`json` (`json.bbnf:11-18`).
- Inference collapses it: regex `SpanKind::Whitespace` → `BuiltinTy::Unit`
  (`passes/src/lib.rs:225-231`, `regex_type` → `RegexKind::Whitespace` → `Unit`).
- Live JSON emission: a single free function `skip_ws(state)` =
  `skip_ascii_whitespace(bytes, cursor)` — `grammars/json/generated.rs:238-242`;
  called at ~20 sites (every value/delimiter boundary: `generated.rs:20,29,64,108,113,
  123,250,296,321,355,362,529,549,672,...`). `skip_ascii_whitespace` is imported from
  `parse_that_regex` (`generated.rs:6`).
- Direct path: same `skip_ascii_whitespace` inline (`generated.rs:81,101,529,549,...`).
- General-descent: the emitter **generates** a blanket `skip_ws` helper
  (`byte.is_ascii_whitespace()`) — `general_descent/emit.rs:126-137`, doc at
  `emit.rs:123-125` ("the grammar's optional `?w` markers, uniform ... whitespace-
  insensitive grammar"). Inserted at rule entry (`emit.rs:272`) and each Seq element
  (`emit.rs:303`).

So ws is a **uniform blanket skip**, recovered from the `SpanKind::Whitespace`
regex, never a per-position `?w` decision. There is no whitespace-preservation path.

---

## 8. Concrete attach points

### 8a. Path API — pointer `get` + stop-early lazy parse

**Where it hurts today**: `JsonObject::get` (`view.rs:106-114`) is O(members) string
compare; a nested pointer `/a/b/2` is O(sum of subtree sizes) because each hop
re-runs `pairs()`/`values()` + `next_sibling_cursor` (`view.rs:355-382`) which
re-decodes every skipped node's kind byte. And `parse` (`parser.rs:47-52`) always
builds the FULL tape before any get.

**Attach 1 — a `Path`/pointer verb over the existing cursor** (additive to `view.rs`;
the `Cursor` trait is intentionally navigation-free so this lives grammar-side):

```rust
// grammars/json/view.rs — new impl, reuses next_sibling_cursor + at_cursor
impl<'doc,'input> JsonValue<'doc,'input> {
    pub fn pointer(self, path: &[Seg]) -> Option<JsonValue<'doc,'input>> {
        path.iter().try_fold(self, |v, seg| match (v, seg) {
            (JsonValue::Object(o), Seg::Key(k)) => o.get(k),      // view.rs:106
            (JsonValue::Array(a),  Seg::Idx(i)) => a.values().nth(*i),
            _ => None,
        })
    }
}
```

This is a pure win only if paired with a stored kind column (kills the re-decode in
`next_sibling_cursor`) — otherwise it inherits the O(subtree) skip. Cheapest real
speedup: add a parallel `Vec<u8>` kind column filled at `push_plain_offset`
(`assembler.rs:71`) and read it in `at_cursor` (`value.rs:29`) instead of
`source[offset]`.

**Attach 2 — stop-early / bounded lazy parse** at the tape builder. Today `parse_json`
(`generated.rs:18`) recurses the whole document. A "parse until pointer resolves"
needs a *skip-parse* that emits only container-open/close offsets (depth tracking)
and defers leaf spans:

```rust
// generated.rs — a second entry beside parse_json
pub fn parse_prefix<'i>(state: &mut ParserState<'i>, want: &[Seg])
    -> Result<u32 /*cursor of target*/, ParseError<'i>> {
    // reuse dispatch_value (generated.rs:45) but at each container, if the
    // current path is not a prefix of `want`, call a skip_value that advances
    // state.cursor via structural scan WITHOUT emit_plain_offset (parser.rs:35).
}
```

The structural scanner already exists to power this: `scan::scan_structurals`
(`grammars/json/scan.rs:34`, NEON) — currently **neutralized** on the live path
(`scan.rs:3-13`) but is exactly the "jump to matching `}`/`]`" primitive a
skip-parse needs. Attach = re-activate it as the skip engine.

### 8b. Direct-to-struct emitter — the schema is the only missing wire

The emitter already exists and works: `json_typed_direct::render` (`json_typed_direct.rs:10`),
fed by `schema_direct::lower_program` (`lower/schema_direct.rs:11`), fed by a
`DirectSchemaSet` (`direct_schema.rs:4`). The ONLY hand-authored piece is the schema
(`xtask/src/real_typed_schema.rs:7`). The attach is a **schema-derivation pass** that
consumes the `->` projections + role plan the compiler already computes:

```rust
// new: passes::extract::derive_direct_schema(grammar, &materialization_plan) -> DirectSchemaSet
//  - one DirectTypeSchema per role in MaterializationRoles (passes/src/lib.rs:1403)
//  - field type_ref from ExprKind::Project.ty (ir/src/lib.rs:263) instead of dropping it
//    at infer_expr (passes/src/lib.rs:203) — carry ty into DirectBuildTarget.type_ref
//  - cardinality from Repeat/Optional (Vec/Option), decode from SpanKind (Escaped/NumberScalar)
//  - then emit_typed_with_layout (codegen/src/lib.rs:213) takes the derived schema
```

Two edits unlock it: (1) stop discarding `Project.ty` at `passes/src/lib.rs:203` —
thread it into `DirectBuildField.target` (currently always `None`,
`passes/src/lib.rs:1149-1287`); (2) add the `derive_direct_schema` producer so
`emit_typed_from_source` (`codegen/src/lib.rs:130`) no longer needs a caller-supplied
`DirectSchemaSet`. The renderer, validator, and `DirectTypeRef` lattice are already
built.

---

## 9. Pointer appendix (fast index)

- Tape storage: `crates/runtime/src/tape/mod.rs:92-171` · builder `tape/assembler.rs:42-123`
  · cursor trait `tape/cursor.rs:20-36` · flags `tape/mod.rs:14-34,142-148`
- Classify: `grammars/json/value.rs:28-47` (`at_cursor`) · `value.rs:143-171` (`value_from_ref`)
- Eager sinks: trait `grammars/json/sink.rs:4-119` · direct driver `generated.rs:73-91,95-` ·
  tape driver `generated.rs:18-25` · codegen `json_sink_direct/mod.rs:29`
- Lazy view: `grammars/json/view.rs` (get `:106`, sibling `:355`, iters `:259,301`)
- Type/projection: IR `ir/src/lib.rs:261-265` (Project), `:361-541` (BackendShape/
  DirectBuild schema) · infer-drop `passes/src/lib.rs:203` · lower `passes/src/lib.rs:1349` ·
  roles `passes/src/lib.rs:1403-1490` · plan `passes/src/lib.rs:1136-1391` · fold
  `general_descent/view.rs:20-46`
- Typed direct: schema IR `direct_schema.rs:4-120` · bridge `lower/schema_direct.rs:11` ·
  render `json_typed_direct.rs:10` · hand schema `xtask/src/real_typed_schema.rs:7`
- Name-gate: `runtime_generator.rs:99` · shape fallbacks `passes/src/lib.rs:277-291`
- ws-skip: grammar `grammars/json.bbnf:6` · live `generated.rs:238-242` · general
  `general_descent/emit.rs:126-137`
- Structural scanner (skip-parse engine, neutralized): `grammars/json/scan.rs:34-48`
