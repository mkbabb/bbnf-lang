# PROVE-08 — TYPE PLANE: SCHEMA FROM GRAMMAR (F2)

Verdict: **PARTIAL** — the mechanism (HM core + `->`→`type_ref` threading + schema
derivation) is built, measured, and runs perf-neutral; the gate's *equality*
conjunct ("derived schema structurally equals `real_typed_schema.rs`") is **refuted
as architecturally unreachable** by three independent walls. Measured, one gate met
(perf-neutral / free), one gate impossible.

Worktree: `/Users/mkbabb/Programming/bbnf-lang/.claude/worktrees/wf_386900ed-462-8`
Base commit: `d2b32f6e3`. Build root: `<worktree>/skinny`.

---

## 1. What I built

A colocated directory-module `crates/codegen/src/type_plane/` (lives with its
consumer, `direct_schema` + the typed direct-to-struct emitter) — **448 impl LOC +
109 test LOC**:

- `hm.rs` (221) — a compact Hindley–Milner unification core re-authored from the
  SK-V21 PROTO-P6 spike: union-find over type variables (path-halving `find`,
  constructor-binding `unify` with an honest clash error), a closed leaf
  constructor vocabulary `TyCon {Bool, F64, U8Tag, StringDecoded, StringSpan}`, a
  bidirectional-check leaf lexer `classify_arrow` (ascription `: T` *checks*, bare
  target *synthesizes*), and `infer(arrows) -> Inference` that mints one var per
  rule and **unifies every arrow of a rule into it**.
- `derive.rs` (211) — reads the *real* front-end `->` facts via
  `grammar::parse_runtime_source_facts` (the `RuntimeTypedProjection` /
  `RuntimeProjection` capture the compiler already computes), binds each arrow to
  its owning rule by source offset, runs `hm`, and **threads the resolved leaf
  types into the `DirectBuildScalar` slots the materialization plan drops as
  `None`** — plus emits a validating grammar-derived `DirectSchemaSet`.
- `mod.rs` (16) — glue; `pub use derive::{derive, DerivedTypePlane, DeriveError}`.
- `tests/type_plane.rs` (109) — drives derivation on a typed json.bbnf, asserts the
  census + leaf bindings + schema validity, prints the artifact.

One-line registration in `crates/codegen/src/lib.rs` (`pub mod type_plane;`).
No `.bbnf` regen was performed (see §4 — the base front-end rejects `->`, so the
derivation reads the arrow facts directly from the typed source string).

## 2. Diff summary

| File | ± | role |
|---|---|---|
| `crates/codegen/src/type_plane/hm.rs` | +221 | HM unification core (P6 re-author) |
| `crates/codegen/src/type_plane/derive.rs` | +211 | `->` facts → HM → `DirectSchemaSet` + `type_ref` fill |
| `crates/codegen/src/type_plane/mod.rs` | +16 | module glue |
| `crates/codegen/tests/type_plane.rs` | +109 | measured integration test |
| `crates/codegen/src/lib.rs` | +1 | `pub mod type_plane;` |

Total +558 lines, all additive; zero hand-patches to `@generated` files; no
name-gates; no x86.

## 3. Measured table

Metric certified: the type plane is a **compile-time** construction — it runs in the
generator, adds **zero** instructions to the runtime parse path. There is therefore
no cold-parse cyc/B *delta* to measure; the honest certified number is the
**derivation wall-time** and the structural census.

| corpus / lane | N | loadavg | derive wall | result |
|---|---|---|---|---|
| json.bbnf (typed, 5 arrows) — `type_plane::derive` | 1 | 15.02 | **167.458 µs** | schema derived + validated |

Census (SK-V21 P6 invariant reproduced):

```
arrows=5  projected_rules=4  unify_events=1
resolved leaf types (FROM grammar `->`, none hardcoded):
  bool   -> Bool          (two arrows "true"|"false" UNIFY to one Bool)
  null   -> U8Tag         (-> 0u8)
  number -> F64           (-> f64)
  string -> StringDecoded (-> decode_json_string_to_arena(input) : String)
DirectBuild type_ref None-slot fill (the dropped wire, now populated):
  bool -> Bool   null -> Null   number -> F64   string -> String
```

Runtime cyc/B: **unchanged by construction.** The emitted parser is a pure function
of the schema's *shape*; the type plane is a schema *producer* running at generation
time. No runtime binary was modified, so no paired PMU parse run applies — the
derived schema is not structurally swappable into the existing `regen-real-typed`
lane (see §4 wall 1), so there is no altered-lane binary to bench. The free-ness is
established by (a) the change touching only compile-time code, and (b) the 167 µs
generation cost, not by a parse-time measurement that would be identically zero.

## 4. The gate refutation — three independent walls

Gate: "derived schema structurally equals the hand schema in `real_typed_schema.rs`
AND the direct-to-struct lane compiles + parity passes with zero runtime cost."

The **perf conjunct is met** (compile-time only). The **equality conjunct is
unreachable** — the hand schema is document-shaped, not grammar-shaped:

- `xtask/src/real_typed_schema.rs` = **45 struct types across 19 roots, 1014 LOC**
  (TwitterSearch{statuses:Vec<Tweet>}, CitmCatalog, CanadaGeometry{coordinates:
  Vec<Vec<Vec<NumberString>>>}, Instrument's 31 fields, …).
- The generic `json.bbnf` derives **1 struct + 4 leaf scalars**.

**Wall 1 — information.** Those 45 struct shapes (field names `id`/`text`/
`statuses`, nesting, capacity hints like `875`, `184`, `10_800`) encode the shape of
specific *documents*. A generic JSON grammar contains none of it: json objects are
`pair*` with runtime string keys. The information lives in `real_typed_struct.rs`,
not in any json grammar. No `->` addition to `json.bbnf` can recover it.

**Wall 2 — IR expressiveness.** `DirectTypeKind` has exactly **one variant,
`Struct`** (`direct_schema.rs:52-58`). The grammar's *natural* output is a recursive
**sum** — `value = object | array | string | number | bool | null` — which the
schema IR **cannot represent**. Only the leaf slice is IR-expressible; the composite
`JsonValue` node has no home. Deriving a schema equal to the hand schema would
require the hand schema to already be sum-free (it is — because it is
document-specialized), which is exactly Wall 1 restated.

**Wall 3 — front-end at base.** At `d2b32f6e3` the `->` projection is **not in
`ExprKind`** at all (no `Project` variant; `ExprKind` = Seq/Alt/Repeat/Optional/
Literal/Regex/Ref/Annotation). It is captured in a *parallel* `RuntimeSourceFacts`
(`RuntimeTypedProjection`), **flagged `BBNF-UNSUPPORTED-PROJECTION`**, and never
reaches `infer_expr` or the materialization plan — every `DirectBuildField.target`
is `None` (`passes/src/lib.rs:1113-1223`). R6's premise ("`Project.ty` discarded at
`passes/src/lib.rs:203`; thread it into `DirectBuildTarget.type_ref`") describes a
**later commit `7f3be27c2`**, not the worktree base. This prototype threads the type
by reading the real facts and populating the `None` slots — the honest minimal wire
that the base IR permits.

**Consequence for the campaign:** "schema from grammar" as an equality target is a
category error against a generic grammar. It is reachable only by *per-document
typed grammars* (e.g. `twitter.bbnf` declaring `-> Tweet` struct projections) plus a
schema IR extended with a sum-type node — both are net-new architecture, not a
threading fix. The type-plane *mechanism* proven here (arrows → HM → validated
schema + `type_ref` fill) is the reusable half; the document-shape half is not
grammar-derivable.

## 5. Visual verification (the derived artifact)

The generator would consume this — emitted verbatim by the test (`-- --nocapture`),
every field `ty` populated from a grammar `->`, zero hardcoding:

```
DirectSchemaSet {
    module_name: "generated_json_type_plane",
    roots: [ DirectRootSchema { function_name: "parse_json",
             rust_type: "crate::json::JsonLeaves", ty: Type("JsonLeaves") } ],
    types: [ DirectTypeSchema { type_id: "JsonLeaves", kind: Struct { fields: [
        { key_literal: "bool",   ty: Scalar(Bool) },
        { key_literal: "null",   ty: Scalar(U32) },              // U8Tag -> u32 width
        { key_literal: "number", ty: Scalar(F64) },
        { key_literal: "string", ty: Scalar(DecodedJsonString) },
    ], ignored_fields: [], unknown_fields: Skip } } ],
    schema_hash: "bd0ed1b60b1978e3",
}
```

There is no runtime-hot-path disassembly to show because the change adds no runtime
code — the "hot path" of a compile-time schema producer is the 167 µs derivation,
whose product (above) is the visual verification.

## 6. Integration cost

**Low, additive, but blocked at the seam by Wall 3.** The module is a self-contained
`codegen` submodule (compiles clean, test green under `ax-iter`). To wire it into the
live generator: (1) `codegen::emit_typed_from_source` already takes a
`&DirectSchemaSet` — swap the hand `real_typed_schema::schema()` call for
`type_plane::derive(&grammar, typed_source)?.schema`; (2) that swap only serves the
grammar-derivable leaf slice, so it cannot replace the 45 document structs the
`regen-real-typed` lane needs. Full integration requires the campaign to first (a)
lift `->` into `ExprKind::Project` (the `7f3be27c2` work) so inference sees it, and
(b) add a sum-type node to `DirectTypeKind`. Until then the prover stands as the
proven *mechanism*, not a live-lane swap.

## 7. Test reproduction

```
cd <worktree>/skinny
CARGO_TARGET_DIR=target-tp cargo test -p codegen --profile ax-iter \
  --test type_plane -- --nocapture
# test result: ok. 1 passed; derive_wall=167.458µs
```
