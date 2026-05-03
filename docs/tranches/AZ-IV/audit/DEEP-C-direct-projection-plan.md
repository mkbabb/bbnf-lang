# DEEP-C — Direct-Projection Path Forward

**Date**: 2026-05-02
**Auditor**: DEEP-C (read-only deep architectural plan; direct-projection lane)
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-deepaudit-C`
**Base**: `master 15e1e5a1` (AZ-IV closed `complete_with_misses` at `cb14970f`; doc-recycle commits through `15e1e5a1`)
**Read-first**: `GESTALT.md`, `codegen-paths.md`, `AZ-IV.md` §Invariants, `AZ-IV/FINAL.md` §Hard Gates Closure, `BA/BA.md` (recycled), `audit/POST-CLOSE-SYNTHESIS.md` (RETRACTED), `audit/POST-CLOSE-D-pathforward.md`, `crates/ir/src/passes/types/{mod,type_map,registry}.rs`, `crates/ir/src/registry/struct.rs`, `crates/ir/src/types/grammar.rs`, `crates/core/src/runtime/json/{value,arena,builder,document,parse_with}.rs`, `crates/core/src/runtime/{arena_template,builder_template}.rs`

## Mandate (verbatim)

> *"Why is struct projection not wired up?"*
>
> *"Even without an explicit `->` annotation, we should use our type inference system to infer the type thereof and project into a struct."*
>
> *"We should mirror, though with superior ergonomics, sonic-rs's, simdjson's, etc — the SOTA — their get API to be as performant and UX friendly."*
>
> *"Ignore our TS and WASM backends for now, these are not relevant and will likely need to be fully re-engineered at some point (or can we leverage a shared ABI?)."*
>
> *"This must be implemented."*

## What Direct-Projection Means (Mechanism)

**Definition**: the emitter generates one typed Rust struct/enum per `<Grammar>Document` shape; the parse function writes directly to those typed fields without an intervening untyped runtime. `Document::get<T>(path)` is a typed accessor on the typed document, the path's resolved type `T` decided at compile time by `path!`. The lazy lane (`parse_with`) short-circuits at the first leaf the path requests; eager mode is `parse_with(input, &EMPTY_PATH)` — the same codepath, `&EMPTY_PATH` selecting "build the whole tree".

**Concrete**: today's runtime is one parse-time abstraction stack:

```
recognizer (parse_*) → builder (FooStructBuilder) → arena (FooArena slabs) → finalise() → FooDocument { arena, root, input }
                                                            └── compound_kind_for_layout: registry lookup PER LEAF
```

Direct-projection collapses every node here. The recognizer becomes the writer; the builder, arena, and finalise step retire from the value-API hot path; `FooDocument`'s typed sum is what the recognizer materialises directly. Compound layouts that today dispatch through `OpenFrame::from_layout(layout, &registry)` (`StructRegistry::compound_kind_for_layout` at runtime) become const projections at codegen — `compound_kind_for_layout` is known at xtask regen time from the layout's `rule_name`.

**Why "struct projection is not wired up" today**: the substrate exists end-to-end, but two layers of indirection survive:

1. `project_types` produces a complete `TypeDesc` for every rule (annotated or not — see §"`->`-less Rules"). It also populates `StructRegistry` with one `StructLayout` per Named rule (kind / fields / field source / projected `TypeDesc`).
2. The emitter consumes `StructRegistry` for compound *layout decisions* (which fields, which kind, which branch tag) — but the *generated parse path still routes through* `FooStructBuilder::begin_compound(layout: &StructLayout)` *with a runtime registry resolution to determine the compound kind*. `compound_kind_for_layout` is `pub fn compound_kind_for_layout(layout: &StructLayout) -> &str { layout.rule_name.as_str() }` — a one-line method that executes at runtime per leaf push (`crates/ir/src/registry/struct.rs:387-390`). The W6.1 fat-LTO bench rows traced 14 BELOW-AU regressions to this lookup (28-65× on bbnf_self/sheets_parse_*; 1.9-118× on json_monolithic).
3. `Document::get<T>(path)` walks the materialised tree post-parse via a `walk_path` helper (`crates/core/src/runtime/json/document.rs:370-392`) and a `JsonPathQuery` trait family per leaf type. The W3 lazy substrate `parse_with` exists, but `Document::get<T>` does not consume it: callers write `JsonParser::parse(input)?.get(path)` — *two operations* — and pay the full eager arena materialisation even when they want a single leaf. That is the architectural source of the 4196× `bbnf_get_twitter` gap vs sonic-rs.

The "direct-projection" thesis is the architectural inversion: the `TypeDesc` and `StructLayout` known at codegen time are projected into a typed Rust struct/enum tree at the emit boundary, not at runtime through registry indirection. Every `->` annotation reaches a typed field; every `->`-less rule reaches a typed field via inferred `TypeDesc`; every compound layout becomes a known-at-codegen const; `Document::get<T>(path)` reroutes through `parse_with(input, path)` for narrow paths, never building the arena.

## Type Inference for `->`-less Rules

**The user's specific concern**: "Even without an explicit `->` annotation, we should use our type inference system to infer the type thereof and project into a struct."

**State of the substrate (verified)**: `project_types` (`crates/ir/src/passes/types/mod.rs:51-491`) runs a CSP-propagation pass over every rule. Phase 4 emits `types_map: HashMap<RuleId, TypeDesc>` from rule variables — *every rule that the constraint system saw produces a `TypeDesc`*. Phase 6 corrects Repeat vec_elem types. Phase 7 collects scratch types. The closure runs *unconditionally* for annotated and `->`-less rules; the constraint system propagates structural shape from leaves upward through Seq/Alt/Repeat without an explicit annotation requirement.

**What kinds of `TypeDesc` `->`-less rules produce**:

- **Pure structural Seq with leaf children**: `TypeDesc::Span` (post-collapse) or `TypeDesc::Tuple([Span, Span, ...])` (when children types differ). The collapse-vs-tuple decision is `collapse_simple_spans` in `GrammarIR` set by absence of `@pretty`.
- **Alt of homogeneous branches**: `TypeDesc::Span` if every branch projects to `Span`; `TypeDesc::Named(_)` if every branch references the same other rule; `TypeDesc::HeterogeneousAltJoin([T1, T2, ...])` when branches disagree (a NAMED obligation, not a silent collapse, per AZ-III invariant 7).
- **Repeat of leaves**: `TypeDesc::Vec(Box<inner>)` — the inner type comes from `vec_elem_types` (vec-context CSP variable).
- **Ref to another rule**: `TypeDesc::Named(rule_name)` if scalar; `TypeDesc::BoxedEnum` if the target is a recursive compound; `TypeDesc::Enum` in vec-context (where the Vec already provides indirection).
- **Cyclic Refs grounded by the cycle-break loop**: `TypeDesc::BoxedEnum` with NAMED `UnresolvedCompoundRef { cyclic: true }` obligation (`crates/ir/src/passes/types/mod.rs:84-129`) — never a silent fallback.

**Where the gap lives** (the answer to "why isn't this wired up"): `StructRegistry::populate_struct_registry` is called only over `Named` rules. The decision "should this rule have a `StructLayout`?" is not "did the rule type-check?"; it is "is the rule's body shape structural enough to project a `StructLayout::Struct` / `TaggedEnum` / `UntaggedEnum` / `NewtypeWrapper`?" Today this excludes:

- Rules whose body is a single `Ref` (the `TypeDesc` is `Named(other_rule)`; the layout would be redundant).
- Rules whose `TypeDesc` is `Span` (no payload structure to project).
- Rules whose body is `OptionalWhitespace` / `Negate` / `Skip` / `Next` only (purely structural, no payload).
- Rules absorbed by `inline_acyclic` / `fuse_single_use` (resolved through `inline_trace` + `path_check_resolver`).

The first three categories produce a typed scalar (or scalar-of-scalar) at the consumer, not a `StructLayout` entry. This is correct as the layout's own definition: a `StructLayout` describes a *compound* shape. But for `->`-less rules whose `TypeDesc` is *compound* (`Tuple`, `Vec(inner)`, `Option(inner)` of a struct, `HeterogeneousAltJoin`), the layout *is* projected — the user's intuition is correct that inference reaches them.

**The fix (W1's mechanism)**: explicit invariant audit. Every rule whose projected `TypeDesc` is a compound shape (i.e., not `Span`/`F64`/`Bool`/etc.) MUST have a `StructLayout`. The audit fails-closed if a compound-typed rule has no layout. The opposite invariant — every `StructLayout` has a `TypeDesc` reachable through Phase 4 — is already true. The gap is the audit; one IR pass after `populate_struct_registry` runs the inverse: enumerate compound-typed rules; assert each has a layout. The pass also surfaces `->`-less rules whose layout would be deficient (e.g. a Seq of three Spans collapses to a single Span; the current registry skips it; the inverse audit confirms either (a) the collapse is intentional, or (b) the rule needs `@no_collapse` for prettify identity).

**The codegen consumption (W2's mechanism)**: the emitter reads `StructLayout` for every Named compound rule and emits a typed Rust struct/enum (today this is partially in place — `JsonValue` enum, `CssTypedValue` enum, `BbnfValue` enum are hand-derived from grammar in W1.A/B and revert-restored at AY/AZ-I; they exist but are not what the typed `TypeDesc` directly projects to). Direct-projection codegen makes the typed value a *generated* artefact: `<Grammar>Value` is emitted from `StructRegistry`'s entries, with `TypeDesc::Vec(inner)` projecting to `Vec<inner>`, `TypeDesc::Option(inner)` to `Option<inner>`, `TypeDesc::Tuple([T1, T2, ...])` to `(T1, T2, ...)` or a generated struct, `TypeDesc::HeterogeneousAltJoin([T1, T2, ...])` to a generated `enum`. The hand-curated `JsonValue` survives only as the *facade type* (`JsonValue<'p> = Generated::JsonValue<'p>` re-export); generation is the source of truth.

## SOTA `get` API (sonic-rs / simdjson) — Mirror with Superior Ergonomics

**The current value-API surface**:

```rust
let doc = JsonParser::parse(input)?;             // eager; full arena materialisation
let title: Option<&str> = doc.get(path);         // walks materialised tree; runtime path borrowed alphabet
```

Two operations; the first is O(n) input bytes; the second is O(path-depth). sonic-rs is one operation, O(path-depth × selective-byte-scan). The 4196× gap is structural.

**The new API surface (after direct-projection lands)**:

```rust
// 1. Compile-time-typed entry; sonic-rs's pointer! equivalent — superior:
//    - the path expression resolves T at parse compile time via the StructRegistry
//    - invalid paths fail to compile with grammar-aware diagnostics
//    - no runtime registry lookup
let title: Option<&str> = JsonParser::get(input, path!(Json, "title"));

// 2. Runtime path (less common; flexible; for dynamically-constructed paths):
//    - same backend (parse_with) consumes a runtime PathSchema
//    - the type-checked entry is preferred wherever the path is statically known
let v: Option<JsonValue<'_>> = JsonParser::get_dyn(input, &runtime_path);

// 3. Eager-as-degenerate-lazy collapse (one codepath):
//    - parse(input) is sugar for parse_with(input, &EMPTY_PATH)
//    - generated body is one branch; emitter shrinks; __EAGER_EMPTY_PATH lie dies
let doc: JsonDocument = JsonParser::parse(input)?;

// 4. Wildcard streaming (sonic-rs's get_iter equivalent):
//    - path!(..., "*", ...) returns Iter<Item = T>; zero-allocation default
//    - .with_anchors() yields (Path<'_>, T) for re-anchorable usage
//    - .collect() materialises if caller wants
for tweet in JsonParser::iter(input, path!(Json, "statuses", "*", "text")) {
    process(tweet);
}
```

**Why this beats sonic-rs in ergonomics** (per the user's mandate "superior ergonomics"):

| sonic-rs / simdjson OnDemand | bbnf direct-projection |
|---|---|
| `pointer![...]` is JSON-only | `path!(Json, ...)`, `path!(CssL4, ...)`, `path!(Sheets, ...)` for any grammar |
| Path validation at runtime; bad pointer = `None` with no diagnostic | Path validation at parse compile time; bad path = `cargo build` error naming the segment, the resolved struct type, and valid alternatives |
| Return type is JSON-typed (`&str`, `f64`, `Value`); user must know JSON shape | Return type inferred from path: `path!(CssL4, ..., "color")` returns `Option<&CssColor>` — grammar-aware |
| simdjson OnDemand requires a `Document` cursor + manual iterator state | bbnf wildcard returns `Iter<Item = T>` directly; the path expression IS the iterator |
| No type-safe variant selection on JSON sums | `path!(CssL4, "rules", 0, "declarations", 0, "value", "color")` selects `CssTypedValue::Color(_)` by name |

**Why this beats sonic-rs in performance** (the architectural reason): sonic does not parse; it scans bytes selectively. bbnf's `parse + get` materialises everything before walking. Direct-projection's `JsonParser::get(input, path)` *short-circuits* at the first leaf the path requests — the cursor's `Decision::ProjectLeaf(reader_fn)` writes the typed leaf into the caller's `Option<T>` slot at the moment of the leaf-recognise; the parser never finishes the document. Mechanism inversion, not constant-factor optimisation. Architectural target: `bbnf_get_twitter ≤ 5×` sonic on same-harness; stretch to ≤ 1× routes only with profile evidence.

## Tranche Shape — 6 Waves + Close

The next code tranche (letter to be assigned by DEEP-D synthesis; provisional reference: **the direct-projection tranche**, candidate letters BD/BE per current trajectory analysis). Six waves + measurement & close, all named, hard gates each.

### W0 — Truth, Regen Baseline, Cleanup Absorption

- **Mechanism**: fresh `cargo xtask regen --check` 9/9 GREEN; workspace nextest baseline; W6.1 watchdog rows reproduced; absorb the three immediate cleanup commits Audit-D Section IV identified — DELETE 18 verified zero-caller substrates + populate `SANCTIONED_SUBSTRATES`; retire 3 module clusters (`generate/serialize/`, `generate/regex/phf.rs`, `backend/strategy/` survivors collocated); WIRE OR DELETE `merge_path_seed` (Audit-B's missing wire). These reduce surface debt before the architectural transpositions land.
- **Hard gate**: `cargo xtask regen --check` 9/9 GREEN; nextest 100% pass; `audit/W0-regen.txt` + `audit/W0-substrate-cleanup.txt`; `audit/W0-failing-test-census.txt` triplet for any deferred ignore.

### W1 — Type Inference Audit + Compound-Type Layout Coverage

- **Mechanism**: implement the *inverse* `StructRegistry` audit. New IR pass `audit_compound_layout_coverage` runs after `populate_struct_registry`; enumerates `(rule_id, TypeDesc)` from `ir.types`; for every `TypeDesc` that is `Tuple` / `Vec(compound)` / `Option(compound)` / `HeterogeneousAltJoin`, asserts a `StructLayout` exists in the registry. The pass surfaces `->`-less compound-typed rules that today have no layout entry. Generates layouts for surfaced rules using the same projection logic `populate_struct_registry` already implements (no new heuristics — the existing pass extends to cover the audit's surfaced rules).
- **Hard gate**: every compound-typed rule has a registry layout; the inverse-audit pass passes; new test `crates/ir/src/passes/tests/inverse_layout_audit.rs` enumerates surfaced rules pre-pass and verifies post-pass coverage; `audit/W1-typed-coverage.txt` archives the rule-by-rule audit.

### W2 — Direct-Projection Codegen for Compound Shapes

- **Mechanism**: emitter generates `<Grammar>Document` typed struct + `<Grammar>Value` typed enum from `StructRegistry` entries. Per-rule layouts project: `LayoutKind::Struct` → `pub struct <Rule><'p> { field_0: T0, field_1: T1, ... }`; `LayoutKind::TaggedEnum` → `pub enum <Rule><'p> { <Branch0>(T0), <Branch1>(T1), ... }`; `LayoutKind::UntaggedEnum` → unitary payload type at consumers (with sub-variant audit metadata preserved); `LayoutKind::NewtypeWrapper` → `pub struct <Rule>(T)`. The parse fn writes directly to typed fields — no `OpenFrame::from_layout` runtime dispatch; the compound-kind discriminator is a const projected at codegen from `layout.rule_name` (the registry's `compound_kind_for_layout` retires to the eager-degenerate-Document-construction lane only).
- **Hard gate**: per-grammar arena/builder template retires from the value-API hot path (eager Document construction lane retains as the `parse_with(input, &EMPTY_PATH)` degenerate case); `compound_kind_for_layout` runtime call retires from the value-API hot path; codegen emits one `<Grammar>Document` struct + one `<Grammar>Value` enum per grammar; `audit/W2-direct-projection-coverage.md` archives the per-grammar typed shape.

### W3 — Eager-as-Degenerate-Lazy Collapse

- **Mechanism**: rewrite `parse(input)` to `parse_with::<EagerSchema>(input, &EMPTY_EAGER)` where `EagerSchema` is a per-grammar marker (`pub enum <Grammar>EagerSchema {}`) implementing `PathSchema` with `Output = <Grammar>Document<'p>` and `decision_for(_, _, _) = Decision::ParseFully`. The dispatcher signature collapses to `parse_with`; `parse` becomes a 5-line surface alias. The dishonest cross-grammar `__EAGER_EMPTY_PATH: LazyLock<TypedPath<Json, &'static str>>` literal (replicated in 9 generated bodies; survives W1 grammar-overfit static scan because the scan inspects `runtime/**` only, not `generated/`) DELETES. Per-grammar phantom marker types replace it; honest typing.
- **Hard gate**: `crate::path::markers::Json` no longer appears in 8/9 generated bodies (only json.rs's eager scaffold uses it); `__EAGER_EMPTY_PATH` LazyLock literal DELETED; `cargo expand -p bbnf-core --lib` shows one `parse_with` body per grammar with the eager body collapsed; `audit/W3-collapse-evidence.txt` archives the asm parity check (`cargo asm bbnf::grammar::generated::json::JsonParser::parse` shows zero codegen change vs pre-W3 baseline — the optimiser inlines the empty path away).

### W4 — `Document::get<T>(path)` Reroute Through `parse_with`; Sonic-Class API Lands

- **Mechanism**: `JsonParser::get<T>(input, path)` becomes the hot-path entry. Trait-style: `JsonParser::get<T>(input, path) -> Option<T>` reroutes through `parse_with::<T>(input, &path)` directly — the value-API never builds the arena for narrow paths. The eager `Document::get<T>(path)` walker survives only for already-materialised documents (e.g. a caller who wants to issue many `get` against one document; rare, but legitimate). The 4196× `bbnf_get_twitter` gap closes through reroute, not through optimisation. The W2 typed-leaf reader writes the leaf directly into the caller's `Option<T>` slot; cursor's `Decision::ProjectLeaf(reader_fn)` lands as a new `Decision` variant projected from the path's terminal segment.
- **Hard gate**: `bbnf_get_twitter ≤ 5× sonic_get_twitter` MET on same-harness comparison (Hard Gate 7 close); `bbnf_value_canada ≤ 5× sonic_value_canada` MET via Eisel-Lemire fast-path numeric decoding (substrate exists in `fast_float2`); `audit/W4-sonic-floor.txt` archives the matrix; samply 7-artefact contract per `PROFILING.md` for every claim.

### W5 — Cursor Consult Unification (Audit-D T2) + AU Floor Closure

- **Mechanism**: `PathCursor` `match_field(&str)` + `match_index(usize)` + `decide(rule_id)` collapse into one polymorphic method `cursor.consult(&ParsedSegment) -> Decision` where `ParsedSegment::{Field(&'a str), Index(usize), VariantTag(&'a str)}` is the recognizer's parsed-segment ADT. Generated parsers call `cursor.consult(seg)` once per shape-decision site instead of dispatching across three method names. The 348 generated cursor-call sites uniformise. AU floor 19/19 closes (the 14 BELOW-AU rows that traced to `compound_kind_for_layout` close in W2 + W4; W5 closes the remaining call-shape orthogonality).
- **Hard gate**: `cursor.match_field` / `match_index` / `decide` DELETED; `cursor.consult(&ParsedSegment)` is the only call surface; AU floor 19/19 rows at-or-better than `post-AU.json`; `audit/W5-cursor-consult.txt` + `docs/benchmarks/post-direct-projection.json` `floors` block.

### W6 — Measurement & Close

- **Mechanism**: full bench matrix re-run; samply 7-artefact contract per profiled entry per `docs/instructions/PROFILING.md`; close-honesty checklist; FINAL.md.
- **Hard gate**: `bbnf_get_twitter ≤ 5× sonic_get_twitter` (Hard Gate 7 — the architectural target); `bbnf_value_*` parity-or-better against `sonic_value_*` (Hard Gate 16 — close); AU floor 19/19; tailwind WATCHDOG resolves through direct-projection (Pratt + unordered shape paths) or routes EXPLICITLY to BA rule-discovery's regex-rewrite enumeration with named close criterion; samply 7-artefact contract MET for every Hard Gate 10/11/12 claim.

## Deletion Bias — Targets

Per AZ-IV §Deletion Bias, the next tranche must DELETE before adding. Enumerated targets with file:line + estimated LOC:

1. **`crates/core/src/runtime/arena_template.rs`** (134 LOC) — RETIRE from value-API hot path after W2 lands; the eager Document construction lane (W3 degenerate case) retains it. Net delta: keep but sever from value-API; the runtime registry indirection retires.
2. **`crates/core/src/runtime/builder_template.rs`** (286 LOC) — same disposition as #1; per-grammar instantiations across 5 simple grammars (~165 LOC each, ~825 LOC total) retire when direct-projection writes typed fields.
3. **`crates/core/src/runtime/{ebnf,csv,bnf,bbnf,google_sheets,css_pretty}/kind.rs`** (~80 LOC across 6 files) — `compound_kind_for_layout` runtime registry consumers retire; codegen-emitted const projections replace.
4. **`__EAGER_EMPTY_PATH: LazyLock<TypedPath<Json, &'static str>>`** literal (9 generated grammars × ~10 lines = 90 LOC; `crates/core/src/grammar/generated/{json,bnf,csv,math,bbnf,google_sheets,css_pretty,ebnf,css_l4}.rs` line 3443-3452 family) — DELETE; per-grammar phantom marker types replace.
5. **`LegacyPath` / `LegacySegment` shim across 4 W3 grammars** (`crates/core/src/runtime/{json,bbnf,css_l4,google_sheets}/parse_with.rs:60-67`; ~80 LOC + per-parse `Vec<LegacySegment>` allocation) — DELETE when `Document::get` consumes `TypedPath` natively.
6. **`cursor.match_field` + `cursor.match_index` + `cursor.decide`** (3 methods, ~16 LOC + 348 generated dispatch sites) — DELETE; replaced by single `cursor.consult(&ParsedSegment)`.
7. **Per-grammar `__path_plan { pub use crate::path::cursor::{Decision, SegmentKind}; ... }`** wrapper modules (9 grammars × 4 lines `pub use` = 36 LOC + path-plan emitter wrapper ~30 LOC) — DELETE; consumers import `Decision`/`SegmentKind` from `crate::path::cursor` directly.
8. **Eager `Document::get<T>` walker for narrow paths** (`crates/core/src/runtime/json/document.rs:144-159` + the per-leaf-type `JsonPathQuery` impls's `walk_path` consumption ~40 LOC) — REROUTE through `parse_with`; the walker survives only for already-materialised documents.
9. **The 32 zero-caller `pub` substrates** (per `audit/W5-substrate-denominator.md`) — DELETE; permanent `substrate_audit.rs` test gates against regression. Estimated LOC delta: -300 to -800 (mid: -500).
10. **`compound_kind_for_layout(layout: &StructLayout) -> &str`** (`crates/ir/src/registry/struct.rs:387-390`) — RETIRE from value-API hot path; eager Document lane keeps it as a single-call site, codegen emits per-layout const projections elsewhere.
11. **`AscentStrategy` trait + 3 impls + `DefaultAscent` typedef** (`crates/core/src/path/ascent.rs`, 277 LOC) — Audit-A-flagged: `with_ascent()` setter never called outside its own definition; only consumer is W2 micro-bench. EVALUATE for deletion in W5 unless a consumer surfaces.

**Stale plan docs to DELETE or supersede explicitly**:

- `docs/tranches/AZ-IV/audit/POST-CLOSE-SYNTHESIS.md` — RETRACT; replaced by the synthesis this DEEP cohort produces. The current file references a "BD" and "BE" trajectory that DEEP-D's synthesis will resolve. Delete on landing the new synthesis.
- `docs/tranches/REMAINING-TRAJECTORY.md` — already SUPERSEDED per project memory; DELETE in W0's cleanup absorption (the trajectory is now `docs/tranches/AZ-IV/FINAL.md` § + the DEEP synthesis output).

**Total estimated LOC delta**: -1700 to -2200 across W0+W2+W3+W4+W5 (per Audit-D §X "Ten deletions" estimate; consistent with this enumeration).

## TS/WASM Punt + Shared ABI Question

**The user's directive**: *"Ignore our TS and WASM backends for now, these are not relevant and will likely need to be fully re-engineered at some point (or can we leverage a shared ABI?)."*

**Punt explicit**: TS and WASM backends are out of scope for the direct-projection tranche. The W5 TS Node-execute carry (F5; W1 backend-ts aggregate gap) and the WASM cdylib (`crates/bbnf-path-ts`) survive AZ-IV in their current state. They will be re-engineered or routed to a shared ABI in a separate tranche letter (after rule-discovery closes, OR concurrently if the shared-ABI mechanism is found to be cheap).

**Shared-ABI feasibility analysis**:

Three candidate ABIs warrant evaluation in a future tranche:

1. **`wasm-bindgen-shared`**: standard wasm-bindgen ABI; what `crates/bbnf-path-ts` already uses for its cdylib. Pro: standard, mature. Con: JS-specific; doesn't bridge to native TS interpreter (Node) AND wasm boundary uniformly; the W5 Node-execute gap surfaces because the wasm boundary returns Spans (input bytes) rather than aggregated typed objects. Direct-projection's typed `<Grammar>Document` could be emitted as a wasm-bindgen-compatible JS class hierarchy, but that adds a backend per direct-projection emission — the value-API IR seam we land in this tranche is the right boundary.

2. **`abi_stable`**: stable Rust ABI for cross-crate dynamic dispatch. Pro: lets a TS/WASM/native backend consume the same Rust trait surface. Con: the ABI has its own type system constraints (no lifetimes; arena handles must serialise to opaque tokens); mismatch with the borrowed `<Grammar>Document<'p>` shape. Plausibility: medium. Would require a `<Grammar>OwnedDocument` shadow type that copies arena contents. Not viable for the value-API hot path; viable for a "TS interop" lane that is explicitly slower.

3. **Custom IR-based ABI**: emit `<Grammar>Document` once via the IR, then per-backend (Rust/native, TS/WASM, future C ABI) instantiate from the same IR projection. Pro: the cleanest extension of direct-projection; the IR's `StructLayout` is already the canonical shape; backends consume it as their input. Con: requires the `Emitter` trait expansion AUDIT-F T2 deferred. Plausibility: HIGH for the next-after-direct-projection tranche, because direct-projection's W2 mechanism establishes the IR seam. This is the recommended approach.

**Recommendation**: do not pre-design the shared ABI in the direct-projection tranche. Direct-projection establishes the IR seam (`StructRegistry` → `<Grammar>Document` typed shape) and the value-API surface (`JsonParser::get`, `JsonParser::iter`, `JsonParser::parse`). The shared-ABI tranche (after rule-discovery) consumes the IR seam to instantiate non-Rust backends. The W5 TS Node-execute gap (F5) will close in that tranche, not the direct-projection tranche.

**Operational note**: the direct-projection tranche must NOT regress the existing TS cdylib build (`audit/W5-bbnf-path-ts-build.txt` shows green at AZ-IV close). The cdylib's existing surface is grandfathered; W2's emitter changes write only to `crates/core/src/grammar/generated/<ident>.rs` and the runtime modules; the TS bindings consume the path/value-API at the same surface they consume today.

## Implementation Order — 3 Cleanup Commits + 6 Waves + Close

Concrete commit list:

**Pre-tranche (cleanup; absorbed into W0)**:

1. **`chore(generate/serialize+regex/phf+backend/strategy): retire dead module clusters`** — Audit-B's three module-level death clusters; ~-350 LOC.
2. **`chore(substrate-audit): delete 18 zero-caller substrates + populate SANCTIONED_SUBSTRATES`** — Audit-B's verified deletion bucket; ~-400 LOC.
3. **`feat(ir/passes/path-seed): wire merge_path_seed at canonical egraph saturation site`** OR **`chore(ir/passes/path-seed): retire unconsumed merge_path_seed loader`** — Audit-B's missing wire; LOC delta dispatches on chosen path.

**Wave 0 — Truth & cleanup**:

4. `docs(direct-projection/W0): land DEEP synthesis-derived plan` (this doc + the DEEP-A/B/D/synthesis cohort's joint output).
5. `chore(direct-projection/W0): regen baseline + fixture archives` (`audit/W0-regen.txt`, `audit/W0-failing-test-census.txt`).

**Wave 1 — Type inference audit**:

6. `feat(ir/passes/inverse-layout-audit): enumerate compound-typed rules; assert StructLayout coverage`.
7. `feat(ir/registry/populate): cover surfaced ->-less compound-typed rules`.

**Wave 2 — Direct-projection codegen**:

8. `feat(emitter/struct-direct): emit <Grammar>Document typed struct from StructLayout entries` — the headline transposition.
9. `feat(emitter/value-typed): emit <Grammar>Value typed enum; hand-curated value enums become re-exports of the generated artefact`.
10. `chore(runtime/<grammar>/kind): retire compound_kind_for_layout runtime call from value-API; codegen-emit const projections`.

**Wave 3 — Eager collapse**:

11. `feat(emitter/parse-eager): rewrite parse(input) as parse_with::<EagerSchema>(input, &EMPTY_EAGER)`.
12. `chore(emitter/empty-path): retire __EAGER_EMPTY_PATH cross-grammar literal; emit per-grammar phantom marker`.

**Wave 4 — `Document::get` reroute**:

13. `feat(value-api/get-reroute): JsonParser::get<T>(input, path) reroutes through parse_with` — the architectural target for the 4196× gap close.
14. `feat(path/cursor): Decision::ProjectLeaf(reader_fn) variant for terminal-segment leaf write-through`.

**Wave 5 — Cursor consult unification**:

15. `feat(path/cursor): cursor.consult(&ParsedSegment) -> Decision; retire match_field/match_index/decide`.
16. `chore(generated): regen 9/9 with consult call shape`.

**Wave 6 — Measurement & close**:

17. `bench(direct-projection/post): post-direct-projection.json fat-LTO matrix`.
18. `docs(direct-projection/FINAL): close-honesty + Hard Gates Closure + samply 7-artefact contract evidence`.

## Hard Gates (≥ 20)

1. `cargo xtask regen --check` 9/9 GREEN at every wave close.
2. Workspace nextest 100% pass; ignores carry triplet.
3. `audit_compound_layout_coverage` IR pass passes — every compound-typed rule has a `StructLayout`.
4. `<Grammar>Document` typed struct + `<Grammar>Value` typed enum emitted from `StructRegistry` per-grammar; hand-curated value enums become re-exports.
5. `compound_kind_for_layout` runtime call retires from the value-API hot path (eager Document construction lane retains as one call site; codegen emits const projections elsewhere).
6. `parse(input)` becomes a thin wrapper around `parse_with::<EagerSchema>(input, &EMPTY_EAGER)`.
7. `__EAGER_EMPTY_PATH: LazyLock<TypedPath<Json, _>>` cross-grammar literal DELETED — the dishonest type ends.
8. Per-grammar `__path_plan { pub use ... }` re-exports DELETED; one canonical `Decision`/`SegmentKind` import path.
9. `cursor.consult(&ParsedSegment) -> Decision` is the only call surface; `match_field`/`match_index`/`decide` DELETED.
10. `Decision::ProjectLeaf(reader_fn)` variant lands; cursor terminal-segment writes leaf into caller's `Option<T>` slot.
11. `JsonParser::get<T>(input, path)` reroutes through `parse_with::<T>(input, &path)`; arena never built for narrow paths.
12. `bbnf_get_twitter ≤ 5× sonic_get_twitter` same-harness (the architectural target — 4196× → ≤ 5×).
13. `bbnf_value_twitter ≤ 1.5× sonic_value_twitter` (5.2× → ≤ 1.5×).
14. `bbnf_value_canada ≤ 5× sonic_value_canada` (167× → ≤ 5× via Eisel-Lemire fast-path numeric leaves).
15. `bbnf_value_data_xl` MEASURED (no WATCHDOG) under fat-LTO; floor anchored.
16. `bbnf_value_data_s ≤ 1.5× sonic_value_data_s` (2.6× → ≤ 1.5×).
17. AU floor 19/19 rows at-or-better than `post-AU.json` (the W5 arena/builder template registry indirection regression closes here).
18. Permanent `substrate_audit.rs` test passes; zero-caller substrate count = 0 (32 → 0 via consume-or-delete).
19. `samply --unstable-presymbolicate` 7-artefact contract per `PROFILING.md` for every Hard Gate 12/13/14/15/16 claim.
20. `LegacyPath` / `LegacySegment` shim DELETED across all 4 W3 `parse_with.rs` modules; `Document::get` consumes `TypedPath` natively.
21. `<Grammar>Value` typed enum is generation-derived; the hand-curated `JsonValue` / `CssTypedValue` / `BbnfValue` / `SheetsValue` are re-exports of the generated artefact (semantic richness preserved per `feedback_preserve-rich-ast`).
22. `JsonParser::iter<T>(input, path!(..., "*", ...))` lazy wildcard surface MET — `Iter<Item = T>` zero-allocation default; `.with_anchors()` + `.collect()` adapters.
23. Tailwind perf timeout closes via either (a) regex-engine consumption of the CSP-selected scanner, or (b) named hotspot routed with samply 7-artefact contract evidence.

## Non-Routable Carries

Every Audit-C MASKED-DEFERRAL bound to a wave; every Audit-A LegacyPath/`__EAGER_EMPTY_PATH`/AscentStrategy finding bound; every Audit-D T1-T7 transposition bound:

| # | Carry | Wave | Mechanism |
|---|---|---|---|
| C-F2 | `bbnf_get_twitter ≤ 5× sonic` | W4 | `JsonParser::get<T>` reroute through `parse_with` |
| C-F5 | TS Node-execute (W1 backend-ts gap) | OUT-OF-SCOPE (TS punt) | Future shared-ABI tranche |
| C-AF | AU floor 18/19 BELOW | W2 + W4 | `compound_kind_for_layout` runtime lookup retires from value-API hot path |
| C-F4 | Tailwind regex_scan timeout | W6 (or routes to BA rule-discovery if BA's regex-rewrite enumeration closes it first) | Per direct-projection (Pratt + unordered shape paths) OR explicit named route to BA |
| C-F8 | 32 zero-caller substrates | W0 | DELETE in cleanup absorption |
| C-F10 | 3 watchdog rows (bbnf_value_data_xl, json_monolithic.data_xl, css_l4.tailwind) | W6 | Direct-projection mechanism for narrow paths; tailwind routes to BA rule-discovery if needed |
| A-LegacyPath | `LegacyPath` / `LegacySegment` shim across 4 W3 grammars | W4 | `Document::get` consumes `TypedPath` natively |
| A-EmptyPath | `__EAGER_EMPTY_PATH` cross-grammar `markers::Json` literal | W3 | Per-grammar phantom marker types replace |
| A-AscentStrategy | `AscentStrategy` substrate-without-consumer (277 LOC) | W5 | Evaluate for deletion; consumer surface in W2 micro-bench only |
| D-T1 | Eager-as-degenerate-lazy collapse | W3 | `parse_with::<EagerSchema>` wrapper |
| D-T2 | Cursor consult unification | W5 | `cursor.consult(&ParsedSegment)` |
| D-T3 | Per-grammar `__path_plan` re-exports DELETE | W3 | Single canonical alphabet at `crate::path::cursor` |
| D-T4 | Arena/builder template DELETION on value-API | W2 | Direct-projection codegen subsumes |
| D-T5 | `Document::get<T>` reroute through `parse_with` | W4 | The architectural target |
| D-T6 | TS aggregate-projection IR pass | OUT-OF-SCOPE (TS punt) | Future shared-ABI tranche |
| D-T7 | Zero-caller substrate cleanup | W0 | DELETE in cleanup absorption |

No carry routes to a fictional successor letter. Every row resolves to a wave, an explicit OUT-OF-SCOPE punt with a named future destination, OR a routed follow-on against a real tranche letter (BA rule-discovery for tailwind regex enumeration; the shared-ABI tranche for TS/WASM).

The trajectory becomes: **AZ-IV (closed) → direct-projection tranche → BA (rule-discovery, recycled) → shared-ABI tranche → cleanup pass tranche**. DEEP-D's synthesis assigns canonical letters; this plan stays letter-agnostic until that synthesis lands.
