# AX.W1r planning — `TypeDesc::Named` preservation audit

**Scope.** Trace the `TypeDesc::Named(sid)` collapse observed by
W1r.1's empirical probe (`docs/tranches/AX/audit/W1r1-diag.md`):
`ir.types` carries zero Named entries on every Rust-target compile,
leaving W1r.1's IR-derived resolver with an empty bindings map and
the direct-to-struct projection inert.

## 1. Data flow — `-> input : Name` from grammar to emit

| Stage | File : line | Handling of `Named(sid)` |
|---|---|---|
| **Grammar parse** | `grammar/css/l4/color.bbnf:228, 255, 303` | Author writes `colorFunction = @{ … } -> input : Color ;`, `colorFn = … -> input : Color ;`, `colorMix = … -> input : Color ;`. |
| **Lower → FnDescriptor** | `crates/core/src/lower/expression.rs:1879–1885, 2040–2055` | `resolve_type_name("Color")` returns `TypeDesc::Named(sid)` because `"Color"` ∉ `from_scalar_name`. The FnDescriptor is `Expr { expr: Input, return_type: Some(Named(sid)) }`. `try_specialize_map_fn` (line 2024) only rewrites when `inner` is `IrNode::Regex` — colour rules have `Seq` / nested `Map` inners, so the annotation survives. |
| **Map IR node** | `crates/ir/src/passes/types/generate.rs:341–358` | `IrNode::Map { inner, fn_id }` emits `MapConstraint::new(var, Named(Color))` from `FnDescriptor::Expr::return_type`. The outer Map node's CSP variable is bound to Named(sid). |
| **Rule-level binding** | `crates/ir/src/passes/types/generate.rs:72–86` | `EqualConstraint::new(rule_var, body_var)` ties the rule's type to its body's outermost node. Rule-level projection is therefore Named(Color) at CSP-solve time. |
| **Structural normalizer loop** | `crates/core/src/pipeline/compile.rs:510–568` | Runs under `!options.structural`; iterates `canonicalize_aliases → prune_unreachable → inline_acyclic → prune_unreachable → fuse_single_use → prune_unreachable → eliminate_epsilon → merge_literals → factor_common_prefixes` up to 64× until fingerprint fixes. Each pass rewrites bodies; `body_has_map` guards on `inline.rs:49` + `fuse.rs:73` protect Map-bodied rules from fusion. |
| **project_types (CSP)** | `crates/ir/src/passes/types/mod.rs:47–397` | Solves the constraint system, writes `ir.types = types_map.into_iter().collect()` at line 397. The only producer of `ir.types`. |
| **Rust prepare_grammar** | `crates/core/src/backend/driver/analysis.rs:79–154` | Additional Rust-only passes: `compute_sp_method_rules` (line 135), `project_types` (line 136), `compute_payload_layouts_with_resolver(RustNamedTypes)` (lines 149–153). |
| **Emit — direct-to-struct** | `crates/core/src/backend/rust/emitter/grammar.rs:49–139` | Walks `ir.rules`, filters on `TypeDesc::Named(sid)` (line 63). W1r.1 probe: zero admissions on every grammar. |
| **Emit — aggregate view** | `crates/core/src/backend/rust/view/leaves.rs:145–243` | `emit_aggregate_accessors` reads `type_desc = ir.types[rule.id]`; `.as_color()` gates on `named_type_name(...) == Some("Color" \| "ColorMix")` + `is_color_layout` (lines 215–216). Current grammars hit neither arm. |

## 2. Collapse site

**The collapse is NOT in `project_types`.** The CSP's
`MapConstraint` (`operators.rs:208–213`) unconditionally assigns
`return_type` to the Map node's variable; nothing downgrades it to
the inner Seq's structural tuple.

**The collapse IS in the structural normalizer loop**
(`compile.rs:510`) — more precisely in `compute_sp_method_rules` +
its downstream interaction with `Ref`-in-`Seq` span-override during
`project_types`. Mechanism:

1. `compute_sp_method_rules` (`crates/ir/src/passes/span.rs:168–194`)
   walks every rule, calls `unwrap_map_node` on the body (strips the
   outer `Expr { return_type: Named }` Map wrapper), then tests
   `can_be_span_parser(inner, sp_set)`. Colour-function inner bodies
   (Seq of literals / regex refs / colour-value refs) satisfy the
   predicate in sufficient iterations.
2. When a rule's `has_sp_method = true`, `generate.rs:170–175` (Ref
   generation) stamps `SpOverrideRef` on parent Seq children,
   overriding the child's solved type to `Span` at Seq-revise time.
3. Seq-revise then folds all-Span children into a single Span via
   `SeqConstraint::revise` (`seq.rs:78–130`) + the all-Span / all-
   simple-span compression in `generate.rs:154–189`.
4. The outer Map's `return_type: Named(Color)` is preserved on the
   Map node's own variable, but the **rule-level** variable is tied
   via `EqualConstraint(rule_var, body_var)` where `body_var` is
   the outer Map. This should keep Named on rule_var.

**Suspected actual site.** The probe observation — `ir.types`
contains a structural `Tuple([Span, U8, BoxedEnum, …])` where a
Named was expected — is only consistent with the Map node
*itself* being rewritten. Candidate passes: `fuse_single_use` if
`body_has_map` returns false (it shouldn't for nested Maps, but the
`Repeat { inner }` / `Skip` / `Next` traversal arms at
`fuse.rs:155–169` need audit); `factor_common_prefixes` which
rewrites Alt branches and may hoist a Map above a common prefix
Seq, inverting the Map/Seq order; `eliminate_epsilon` which
rewrites optional tails.

**Architecturally-primary suspect:** `try_flatten_pair` in
`types/utils.rs` (re-exported at `types/mod.rs:29`) — called during
Seq type projection to collapse `(T, Vec<T>)` → `Vec<T>`. If a Map
wrapper projects through this code path rather than through
`MapConstraint`, the Named is lost. The probe panic should be
expanded to discriminate *which* pass produces the observed Tuple
— W1r.1 did not instrument the loop iterations.

## 3. Semantic analysis — preserve or collapse?

Invariant 21 (AX.md §Invariants) mandates: "No hand-coded AST enum
duplicates grammar structure. The user-facing AST is `NodeView<'p>`
+ `TapeCursor<'p>` + per-rule typed accessors emitted by the shape
emitters from IR's `TypeDesc` inference". The typed-materialization
invariant (README.md §Code discipline): "Every `->` annotation in a
grammar reaches the tape emitter; inference composes types and
never loses them."

**Preservation is correct.** A grammar author writing
`-> input : Color` is declaring the rule's contract: *this rule
produces a Color, regardless of which Seq children compose it.*
Collapsing to `Tuple([Span, U8, BoxedEnum, …])` destroys the
declared type, forcing the author to manually re-project via
`.value()` tuple decoding — the exact surface invariant 21 rejects.

The VM/Rust asymmetry documented in W1r.1 (VM preserves
`Named("String")` for JSON via `universal_named_shape` fallback;
Rust collapses because its `compute_sp_method_rules` sp-override
rewires refs) is a **Rust-target bug**, not an intentional
divergence. The backend-specific resolver was introduced at AW.0.5
(AU.4.2 stated path) to keep name tables per-backend; losing the
`Named` annotation upstream defeats the entire mechanism.

## 4. Preservation — concrete proposal

**Minimum-invasive change list** (5 files, 1 crate):

| File | Change |
|---|---|
| `crates/ir/src/passes/span.rs` | `compute_sp_method_rules::unwrap_map_node` must NOT unwrap `FnDescriptor::Expr { return_type: Some(Named(_)) }` Maps. The return-type annotation pins the rule to a concrete non-Span shape; sp-override is semantically incorrect for it. |
| `crates/ir/src/passes/types/constraint/reference.rs` | Ref-to-named-rule path must not engage span-override. Read the target rule's `return_type` before applying `SpOverrideRef`. |
| `crates/ir/src/passes/types/mod.rs` | Add a post-solve assertion: for every rule whose body is `Map(_, FnDescriptor::Expr { return_type: Some(td) })`, `types_map[rule.id] == td`. Fail loud on violation — this is the typed-materialization invariant. |
| `crates/core/src/backend/rust/emitter/grammar.rs` | Once Named survives, `emit_direct_to_struct_projection` lights up unchanged. W1r.1's refactor is already consumer-ready. |
| `crates/core/tests/css_color_parity.rs` | Add an end-to-end test: compile CSS L4 through `prepare_grammar`, assert `ir.types` contains `(colorFn_id, Named("Color"))`. Wire-contract gate. |

**Grammars regen required:** yes — CSS L4's `generated.rs` will
emit `.as_color()` / `PROJECTION_DIRECT_TO_STRUCT` entries. JSON
will emit `.as_string()` or similar for its `Named("String")`
paths. BBNF / Sheets unaffected (no `-> input : <Name>`
annotations).

**Runtime impact:** neutral-to-positive. The `LargeAggregate` 40 B
layout already exists (`compute_payload_layouts_with_resolver` at
`LARGE_PAYLOAD_MAX`); Rust emitter already routes through
`PayloadData::LargeAggregate`. Preserving Named simply re-enables
the decoder wiring downstream consumers use.

## 5. Direct-to-struct mechanism — current behaviour

`emit_direct_to_struct_projection` emits a
`pub const PROJECTION_DIRECT_TO_STRUCT: &[(&str, &str); N]` +
N `__named_type_shim_<name>` marker functions, one per distinct
admitted name. **On every current grammar** (JSON / CSS / BBNF /
Sheets / Ebnf / Bnf / GoogleSheets): `N = 0`, no const, no shims.

The `.as_color()` method in `view/leaves.rs:204–243` gates on
`named_type_name(type_desc, ir) == Some("Color" | "ColorMix")` +
`is_color_layout(layout) == true`. With Named collapsed, the gate
returns `None` on every compile — `.as_color()` never fires in
generated view code. **Tests that currently pass**
(`css_l4_color_view.rs` 23/23, `css_color_parity.rs` 4/4) exercise
`Color::decode` on hand-fabricated 40 B arrays and
`RustNamedTypes::from_ir` on synthetic single-rule IRs — neither
reaches the real CSS L4 view's `.as_color()` path. The supposedly-
green end-to-end path is a decoder-unit-test alibi, not a working
view projection.

**Generalizing to arbitrary grammar-declared structs** is feasible
with the preservation fix (§4) and incurs only the layout planner's
existing `LARGE_PAYLOAD_MAX = 64 B` cap. Beyond that cap the
resolver declines; the aggregate falls back to compound-children
storage (unchanged behaviour). No hand-coded per-grammar bridge;
no third-party-comparator shim. Performance is paid at emit-time,
not parse-time — the decoder is a single
`Color::from_tuple(self.value())` call, layout-directed and
LLVM-visible at the caller.

## 6. Recommendation

**Preserve.** The collapse violates invariants 20, 21, and the
typed-materialization invariant simultaneously; the fix is
localized (5 files); the consumer wiring (W1r.1's
`RustNamedTypes` + `emit_direct_to_struct_projection`) already
landed and is waiting for upstream data. The alternative
(collapse-is-correct) would require deleting AW.0.5's entire
resolver trait surface + the `LargeAggregate` 64 B admission cap
+ the `.as_color()` view shim — scope equivalent to reverting four
tranches.

The Rust-target divergence from VM is unintended and should be
repaired. The `compute_sp_method_rules` unwrap-Map-wrapper
behaviour at `span.rs:178–179` is the first surgical cut.
