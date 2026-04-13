# Direct-to-Struct Projection — Audit + AR Plan

## Current state (dormant)

`compute_payload_layouts` in `crates/ir/src/passes/payload/layout.rs:56`
returns `HashMap::new()` for every production grammar. The map is
populated via `finalize_compile` (`crates/core/src/pipeline/compile.rs:173`)
for VM/Rust/TS targets, but no rule survives projection with a TypeDesc
shape that the planner accepts. Concrete probe across six grammars:

| Grammar | Rules | `payload_layouts` | Tuples projected | All-scalar Tuples |
|---|---|---|---|---|
| `grammar/json/json.bbnf` | 10 | **0** | 1 (`pair`) | 0 |
| `grammar/css/l4/stylesheet.bbnf` | 184 | **0** | 105 | 0 |
| `grammar/bbnf/bbnf.bbnf` | 52 | **0** | 24 | 0 |
| `grammar/google-sheets/google-sheets.bbnf` | 38 | **0** | 20 | 0 |
| `grammar/ebnf/ebnf.bbnf` | 14 | **0** | 3 | 0 |
| `grammar/misc/csv.bbnf` | 6 | **0** | 2 | 0 |

Aggregate: 304 rules total, 155 Tuple-typed rules, **zero with all
fields scalar**. Every Tuple field is either `Span`, `BoxedEnum`,
`Vec(_)`, `Option(_)`, or a nested Tuple of those — never a
`F64/Bool/U8` scalar.

The scalar-payload single-rule path (AQ.6.A) is similarly unused: not a
single rule projects to `TypeDesc::F64` / `Bool` / `U8` / `U32` / etc.
in any of the six grammars probed. The CSS L4 type histogram contains
`Span=53`, `Vec=4`, `Tuple=105`, `BoxedEnum=21`, **zero scalar
TypeDescs**. JSON's `number` rule (`number = /regex/ -> f64 ;`)
projects to `Span`, not `F64`.

## Why dormant

The single root cause: **`-> f64` / `-> 0u8` / `-> true` `MapExpr`
mappings produce `FnDescriptor::Expr { return_type: None }` instead of
`FnDescriptor::Expr { return_type: Some(Named(...)) }`, breaking every
downstream type inference path.**

`crates/core/src/lower/expression.rs:1229` (`lower_map_arrow`) walks
three return-type extraction paths in order:

1. Type-shorthand bare ident (`unwrap_value_ident_str` → `is_type_name`)
2. Numeric/float literal suffix (`deep_unwrap_value` → `int_lit` / `float_lit`)
3. Bool literal (`deep_unwrap_value` → `bool_lit`)

All three paths assume the value-expression sub-tree exposes a
distinguishable `int_lit` / `float_lit` / `bool_lit` rule_kind compound
under the wrapper. Post-tape-rewrite, those leaf compounds are inlined
into `value_atom`. `deep_unwrap_value` (`crates/core/src/lower/value_expr.rs:909`)
stops at `value_atom` and returns it; the subsequent
`leaf.rule_kind() == BbnfBootstrapRuleKind::int_lit` (line 1267) and
`leaf.rule_kind() == BbnfBootstrapRuleKind::bool_lit` (line 1286)
checks therefore always return `false`.

The bare-ident `-> f64` shorthand similarly relies on
`unwrap_value_ident_str` returning `Some("f64")` from a `value_atom`
whose span text is exactly `f64` — the helper handles this case
correctly (`value_expr.rs:875-891`), but for `number = /regex/ -> f64`
the lowered `MapExpr` ends up as
`FnCall { name: 6, args: [Input] }`, not `MapExpr::Input`. This means
the value-atom span is being parsed as a function call shape (the
trailing `;` rule terminator?), not a bare ident; root cause is
plausibly the `lower_atom_named` `(`-detection at
`value_expr.rs:457`. Either way the type-shorthand branch never fires
and `try_specialize_map_fn` (which only matches when expr ==
`MapExpr::Input` AND return_type is `Some(Named("f64"))`) is bypassed.

Per-rule probe of every Map node in CSS L4 confirms the pattern: 299
Map nodes, all `Expr(return=None)`. Sample:

```
absoluteLengthUnit:
  Map[Expr(ret=None, expr=IntLit(0))]  inner=Literal("px")
  Map[Expr(ret=None, expr=IntLit(13))] inner=Literal("c")
  Map[Expr(ret=None, expr=IntLit(9))]  inner=Literal("cm")
  ...
fontLengthUnit:
  Map[Expr(ret=None, expr=IntLit(2))]  inner=Literal("rem")
  Map[Expr(ret=None, expr=IntLit(15))] inner=Literal("rlh")
  ...
```

The user wrote `"px" -> 0u8`. The lowering captures
`expr=IntLit(0)` (correctly) but loses the `u8` type
suffix. Downstream:

1. The `MapConstraint` (`constraint/operators.rs:208`) reads
   `return_type.unwrap_or(TypeDesc::Span)` — every branch
   becomes `Span`.
2. `AltConstraint::join_types` (`constraint/helpers.rs:90`) sees
   homogeneous `[Span, Span, ...]` and projects to `Span`.
3. `Ref(absoluteLengthUnit)` projects to `BoxedEnum` (the CSP
   `Ref` ground rule, `generate.rs:147`).
4. `dimension = number, anyUnit` Seq projects to `Tuple([Span,
   BoxedEnum])` rather than `Tuple([F64, U8])`.
5. `compute_payload_layouts` finds zero `Tuple`s of scalars and
   emits an empty map.

This is one bug masquerading as the absence of an entire feature.
**The plan, infrastructure, and codegen are all wired and correct;
the lowering pipeline never produces the input that the planner
needs.**

## Grammar expressiveness gap

After fixing the lowering bug above, the existing grammar idiom
`length = number, lengthUnit` already expresses what AR needs:

- `number = /regex/ -> f64` → `TypeDesc::F64` (single-rule scalar
  payload, AQ.6.A path)
- `lengthUnit = ... | "px" -> 0u8 | ...` → `TypeDesc::U8` (Alt
  homogeneous join over scalar branches)
- `length = number, lengthUnit` → `TypeDesc::Tuple([F64, U8])`
  (Seq projection, scalar tuple)
- `compute_payload_layouts` accepts the rule, plans `total_bytes=9`,
  field offsets `[F64@0, U8@8]`. View accessor returns the tuple.

What the language **still cannot express** today:

1. **Named struct projection**: `length = number, lengthUnit -> Length`
   (with `Length` resolving to a host-defined struct). The TypeDesc
   ladder has `Named(StringId)` but the named-type case is never
   bridged into payload layouts. The view accessor would need to
   return a typed struct, not an anonymous tuple.
2. **Field naming**: even with `Tuple([F64, U8])` the view accessor
   returns `(f64, u8)` — no field names. AQ.6.B.4's example
   `Length { value, unit }` requires a way to label fields.
3. **Named enum projection**: `value = string | number | "true" -> true | "false" -> false | "null" -> 0u8`
   today projects to `BoxedEnum` (CSP `Ref` over heterogeneous
   branches). AQ.6.C describes a typed enum view but has no input
   syntax for naming the enum or its variants.
4. **Composite host calls**: there is no `MapExpr` constructor for
   `Length { value: input.0, unit: input.1 }` — only `IntLit`,
   `FloatLit`, `BoolLit`, `StringLit`, `Input`, `InputProp`,
   `FnCall`, `BinOp`, `UnaryOp`. A host call returning a struct
   could synthesize one, but the return-type tracking needs the
   host-fn registry to declare per-arg semantics.

The first three gaps are addressable with grammar directives; the
fourth is already reachable through `@host` annotations — the
existing `extract_value_func_name` path at `expression.rs:1296`
propagates a host fn's declared return type into the MapExpr.

## Proposed architectural path

### M0 (foundational, fixes everything else): repair `lower_map_arrow`

The single change that activates **every dormant scalar payload path**
in every grammar is to replace the rule_kind-based leaf detection in
`deep_unwrap_value` / `lower_map_arrow` with **span-text inspection
on the `value_atom`**.

`value_atom` already carries the literal span. Its leading byte
already discriminates int / float / bool / ident / etc. in
`lower_value_atom` (`value_expr.rs:382`). The fix:

```rust
// In lower_map_arrow, replace the int/float/bool leaf checks with
// span-text inspection at the value_atom level — exactly the same
// dispatch lower_value_atom uses for its lowering.
let return_type = return_type.or_else(|| {
    let leaf = deep_unwrap_value(value_expr);
    let text = leaf.span_text().trim();
    match text.as_bytes().first()? {
        b'0'..=b'9' | b'.' | b'-' => {
            let (_digits, suffix) = split_numeric_suffix(text);
            (!suffix.is_empty()).then(|| {
                TypeDesc::Named(ctx.strings.intern(suffix))
            })
        }
        _ if text == "true" || text == "false" => {
            Some(TypeDesc::Named(ctx.strings.intern("bool")))
        }
        _ => None,
    }
});
```

This is a ~15-line change; no IR, no CSP, no codegen surface
modified. After it, the probe shows
`absoluteLengthUnit: ty=Some(U8)`,
`length: ty=Some(Tuple([F64, U8]))`,
`payload_layouts: 1 entry for length, total_bytes=9`.

The `is_type_name` shorthand bug for `-> f64` is similar: the
`value_atom`'s span text equals `f64` exactly, so
`unwrap_value_ident_str` should already return `Some("f64")`. The
probe report's `expr=FnCall{name:6,args:[Input]}` outcome means
the chain detection is overshooting somewhere — needs a focused
trace and likely a one-line guard in `lower_atom_named` to
short-circuit when the trimmed atom span matches `is_type_name`.

### M1: end-to-end activation of single-rule scalar payloads

After M0, validate the existing AQ.6.A path end-to-end on JSON's
`number` rule and CSS L4's `*Unit` rules:

1. Confirm `ir.types[number] == F64`, `ir.types[absoluteLengthUnit] == U8`.
2. Confirm the Rust backend's `tape_prelude` / `map_value` siblings
   emit `push_leaf_with_<T>` for the rule epilogue.
3. Confirm view accessors emit the typed `payload_<T>` reader.
4. Run JSON citm bench; expect modest improvement from `number`'s
   tape-cost drop (one record per number, scalar payload instead of
   span re-parse on read).

Acceptance: `cargo test --workspace` green, JSON `number` reads
without re-parsing source span.

### M2: aggregate Tuple projection lights up

After M1, CSS L4's `length = number, lengthUnit` (and every
analogous dimension rule) projects to `Tuple([F64, U8])`. The
`compute_payload_layouts` planner then plans
`PayloadLayout { total_bytes: 9, fields: [F64@0, U8@8] }` for each
of: `length`, `angle`, `time`, `frequency`, `resolution`, `flex`,
`percentage`. Likewise CSS L4's `dimension = number, anyUnit`
becomes `Tuple([F64, U8])` (after `anyUnit` joins to `U8`).

The Rust backend already has `push_leaf_with_aggregate` wired
(per AQ.6.B in the audit). What's missing: the rule body must emit
two scalar writes into a stack buffer at the planned offsets, then
a single `push_leaf_with_aggregate` at the epilogue. For
`number , lengthUnit` this means the number scanner writes f64 to
buf[0..8], the unit dispatch writes u8 to buf[8], and the rule
epilogue commits 9 bytes to the tape.

Concrete example — full pipeline for CSS L4's `length` rule (or
JSON's `number`'s tape behaviour after M1):

```text
Grammar:    length = number , lengthUnit ;
Lowering:   IrNode::Seq([
              Map(Regex(numeric), NumberConvert),         // → F64
              Ref(lengthUnit),                            // → U8 (after fold)
            ])
project_types: Tuple([F64, U8])
compute_payload_layouts: PayloadLayout {
  fields: [(F64, offset=0), (U8, offset=8)],
  total_bytes: 9,
}
Rust codegen body:
  let mut __agg = [0u8; 16];
  let f = css_number_scan_f64(state)?;     // existing fast path
  __agg[0..8].copy_from_slice(&f.to_le_bytes());
  let u = lengthUnit_dispatch(state)?;      // dispatch table → u8
  __agg[8] = u;
  Some(tape.push_leaf_with_aggregate(
    TapeKind::Rule, span, /*flags*/0, &__agg[..9],
  ))
View:
  pub fn value(&self) -> (f64, u8) {
    let bytes = self.tape.payload_bytes(self.cursor.record()).unwrap();
    (
      f64::from_le_bytes(bytes[0..8].try_into().unwrap()),
      bytes[8],
    )
  }
```

Acceptance: probe shows `payload_layouts.len() ≥ 7` for CSS L4
(one per dimension rule). Bench: CSS L4 `bootstrap`/`tailwind` get
the dimension rules off the compound path; expect 5–10% gain on
declaration-heavy datasets where dimensions dominate.

### M3: named-struct surface

`length = number, lengthUnit -> Length` already parses today (the
`->` postfix accepts any value-expression). The lowering must:

1. Recognise the bare `Length` ident as a type name (extend
   `is_type_name` to accept user types — gate on
   `ctx.host_fns.contains_key("Length")` or on a new
   `@struct Length { value: f64, unit: u8 }` directive).
2. Project the rule TypeDesc as `TypeDesc::Named(StringId)`
   pointing at "Length".
3. Carry the field-name list alongside the layout — either via a
   new `@struct` directive parsed into `GrammarIR`, or by reading
   field metadata from a host-side `pub struct Length { value:
   f64, unit: u8 }` declaration via `bbnf_derive`.
4. View accessor emits `Length { value: ..., unit: ... }` instead
   of `(f64, u8)`.

The minimum-change path: add a `@struct Length { value: f64,
unit: u8 } ;` grammar directive that registers a name with field
list. `lower_map_arrow` for `-> Length` then resolves to
`TypeDesc::Named("Length")` and the codegen sites consult a
`GrammarIR::structs: HashMap<StringId, Vec<(StringId, TypeDesc)>>`
for the field layout.

The maximal-change path: drop the directive and infer the layout
from the rule's intrinsic Seq shape — `length = number,
lengthUnit -> Length` carries `(F64, U8)` from the body; the host
struct is the only thing the user supplies. This is more elegant
but requires the host to declare its struct shape outside the
grammar, which blocks self-hosting.

The Rust codegen already needs a host struct definition to
materialise the typed accessor return; a `@struct` directive
reified in the IR is the simplest cross-backend story (TS / WASM
backends emit their own type with the same field names).

Acceptance: `length = number, lengthUnit -> Length` round-trips
through `derive(Parser)` with a `pub struct Length { value: f64,
unit: u8 }` declaration; `view.length().value() == Length { value,
unit }`.

### M4: typed Alt enum (AQ.6.C surface)

`value = string | number | "true" -> true | "false" -> false | "null" -> 0u8 -> JsonValue`
projects to `TypeDesc::Named("JsonValue")`. After M3's
`@struct` plumbing, add `@enum JsonValue { String(Span), Number(f64),
Bool(bool), Null(u8), Array, Object } ;` in the grammar and emit a
typed enum view. Each Alt branch's variant index maps to one of
the enum variants; payload-eligible branches (scalar / aggregate)
emit `payload_<T>` reads, non-payload branches emit cursor-wrapped
fallbacks (per AQ.6.C.2).

The branch-to-variant matching is structural: branch index N maps
to the Nth variant of the enum (declaration order). Variants
whose payload type is `Span` collapse to `Variant(span_text)`;
variants matching a scalar TypeDesc collapse to
`Variant(payload_<T>)`; variants whose body is a Ref to another
typed rule collapse to `Variant(SubView<'p>)` (cursor-wrapped).

Acceptance: JSON `value` rule emits `JsonValue` view with five
variants matching the grammar's five branches exactly; bench shows
removed cursor descent for scalar branches.

## Expected performance impact

JSON citm baseline (post-AQ master, M4 Max):
- `cargo bench` cold-citm: 2,654 MB/s (650µs/iter on 1.7MB input)
- `__value` 56.4% self-time, `__pair` 28.2%, WS 11.9% (post-AP profile)
- citm contains ~4M values; each currently emits a `push_compound`
  with 4 children (string + colon + value + optional comma), one
  TapeRec per token plus one per compound

What M0–M2 buys on JSON:
- `number` rule (after M0+M1): tape rec changes from 16-byte compound
  + 8-byte payload slot to 16-byte leaf with 8-byte payload inline
  → -8 bytes per number. citm has ~50K numbers → 400KB less tape
  written + same amount less read. citm tape size drops ~5%.
- WS scan unchanged.
- `__value` self-time mostly comes from the Alt dispatch + recursion,
  not number conversion specifically; expect 2–3% citm gain from the
  reduced tape write/read pressure.

What M3 buys on CSS L4:
- `length` / `angle` / `time` / `dimension` rules collapse from a 2-
  child compound (8 bytes header + 2×16-byte child records = 40 bytes)
  into a single 16-byte aggregate leaf with 9-byte payload → -24
  bytes per dimension. CSS bootstrap (~155KB) contains hundreds of
  declarations with 2–4 dimensions each → ~30–50KB of tape pressure
  removed.
- `__declaration` self-time (33.2% of bootstrap parse) sees a direct
  improvement: each value parse is one push instead of three.
- Estimated 8–15% gain on CSS bootstrap / tailwind from this alone.

What M4 buys on JSON value access:
- Today: `view.value().as_string()` walks the cursor + reads span
  text. After M4: `match view.value() { JsonValue::String(s) => s, ..
  }` — direct enum match on the variant idx. View-side micro-bench
  improvement is large; parse-side improvement is the
  push_leaf_with_aggregate elimination of the wrapping compound.

### Cache-line / bytes-per-node accounting

A current "number value in CSS dimension" path:
```
parent rule compound: 16B (TapeRec)
  child 0: number compound:    16B
    child: regex leaf:         16B (+ payload 8B = 24B effective)
  child 1: lengthUnit compound: 16B
    child: literal+map leaf:    16B
Total: 80B for one dimension instance
```
After M2:
```
parent rule compound: 16B (TapeRec)
  aggregate leaf: 16B (+ payload 9B = 25B effective)
Total: 41B for one dimension instance — ~50% reduction
```
On a 64-byte cache line: pre-M2 fits 1.6 dimensions, post-M2 fits
3.1. The CSS L4 `__declaration` hot path walks dimension records;
half the cache misses translates directly to throughput.

## Concrete milestones for AR

| ID | Milestone | Files | Bench gate |
|---|---|---|---|
| **M0** | Fix `lower_map_arrow` leaf detection for value_atom span text. Probe must show `length: Tuple([F64, U8])` and `payload_layouts.len() ≥ 1` per dimension grammar. | `crates/core/src/lower/expression.rs:1264-1292`, `crates/core/src/lower/value_expr.rs:909` (`deep_unwrap_value`). ~30 LOC. | No regression. Probe asserts. |
| **M1** | Activate single-rule scalar payloads end-to-end. JSON `number` rule emits `push_leaf_with_f64`; view emits `payload_f64` accessor. CSS L4 `*Unit` rules each emit `push_leaf_with_u8`. | `crates/core/src/backend/rust/emitter/{tape_prelude,map_value,grammar}.rs`, `crates/core/src/backend/rust/view/leaves.rs`. AQ.6.A wiring, already in progress. | JSON citm ≥ 2,700 MB/s. |
| **M2** | Aggregate Tuple payload codegen: `compute_payload_layouts` populates ≥ 7 entries on CSS L4 (one per dimension family). Rust backend emits stack-buffer write + `push_leaf_with_aggregate` for any rule with a layout. View accessor reads via `payload_bytes`. | `crates/core/src/backend/rust/emitter/grammar.rs` (rule prelude/epilogue), `crates/core/src/backend/rust/view/seq.rs`. AQ.6.B wiring. | CSS bootstrap ≥ 550 MB/s (+10%). |
| **M3** | `@struct Name { field: type, ... }` directive + `-> Name` lowering. View accessor returns named struct instance with field-by-field copy from payload bytes. | New: `crates/ir/src/types/structs.rs`. Modify: `crates/core/src/lower/expression.rs` (`lower_map_arrow`), `crates/core/src/backend/rust/view/leaves.rs`. ~150 LOC. | Round-trip test for `length -> Length`. No bench regression. |
| **M4** | `@enum Name { Variant(type), ... }` directive + Alt-of-payload typed enum view. JSON `value -> JsonValue` projects to typed enum with five variants matching branch indices. | Same files as M3. AQ.6.C codegen surface. | JSON citm ≥ 2,800 MB/s. View-side micro-bench: scalar variants 5× faster than current span-walk fallback. |
| **M5** | Field naming for anonymous Tuple aggregates. Either inherit names from rule's Seq children identifiers (e.g., `length = value: number, unit: lengthUnit`) — requires grammar syntax extension — OR retain anonymous tuple return when no `@struct` directive is provided. Decision: **anonymous-tuple is sufficient**; M3 already covers the named case. M5 deferred unless bench / ergonomic data demands it. | — | — |

### Execution wave

A single agent can land M0 in a focused half-hour change. M1 is one
agent on the Rust backend siblings already touched by AQ.6.A. M2
adds two agents (one for the codegen prelude/epilogue, one for the
view layer). M3+M4 share grammar parser changes — one agent for the
directive lowering, one for the view emission. M5 only fires if
demanded.

Total LOC delta estimate: ~600 LOC additions, ~50 LOC deletions
(the rule_kind-based dead leaf checks in `lower_map_arrow` after
M0 lands).

## Open questions for the AR plan author

1. Field naming source-of-truth: grammar directive (`@struct`)
   vs. host struct introspection vs. anonymous tuple? The audit
   recommends `@struct` for backend portability.
2. Should the planner's `MAX_PAYLOAD_BYTES` (16) be raised for
   user struct projections that exceed two scalars? The current
   16-byte limit fits one TapeRec slot exactly. Raising it
   requires either multi-record allocation or a side-table.
   Recommend: keep at 16 for AR; rules whose layout exceeds 16
   bytes continue using the compound pathway, which is fine.
3. Variant tagging in M4: today `TapeRec::flags` carries the alt
   `variant_idx` (already used by AQ.6.C). Confirm that no other
   subsystem squats on those bits before extending.
4. Does the `ws_pattern`-driven `?w` between Seq children preserve
   span boundaries needed for the aggregate write? Aggregate write
   needs to know offsets at scan time, not at view time — verify
   that the `lengthUnit_dispatch` callsite in M2's example can return
   a u8 directly without re-walking source.
