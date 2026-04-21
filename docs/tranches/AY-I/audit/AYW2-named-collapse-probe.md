# AYW2 — Named-collapse probe

Empirical discrimination of A6's H1/H2 hypotheses for `TypeDesc::Named(_)`
collapse on CSS L4 (`colorFunction`, `colorFn`, `colorMix`) + JSON
(`string`). Probe author: AY.W2.1 sub-agent.

## Probe shape

**Option A** — test in `crates/core/tests/named_pipeline_probe.rs`.
Two `#[test]` functions:
- `css_l4_named_pipeline_probe` — runs CSS L4.
- `json_named_pipeline_probe` — runs JSON.

Each test runs `compile_paths_request(CompileTarget::Vm)` twice per
grammar — once with `PipelineOptions { structural: true, .. }` (pre-opt
snapshot) and once with default options (post-opt). The structural-mode
snapshot bypasses the structural normalizer loop and the e-graph
saturation block (both gated on `!options.structural` in
`crates/core/src/pipeline/compile.rs:504,591`). Cross-comparing the
two snapshots discriminates pipeline-pass causes from upstream
(lowering / grammar-source) causes.

Reproduce:

```bash
cargo test -p bbnf --test named_pipeline_probe -- --nocapture --test-threads=1
```

The probe always exits 0; it's an instrumentation surface, not a
gate. The wire-contract test `named_type_preservation.rs` (W2.7
deliverable) is the actual gate.

## Per-rule trace

Five checkpoints captured per rule:

1. **Pre-opt body root** — `IrNode` discriminator at the rule's body root
   in structural mode (preserves source-lowered shape).
2. **Pre-opt `ir.types[rule.id]`** — CSP projection in structural mode.
3. **Post-opt survival** — whether the rule still appears in `ir.rules`
   after the production pipeline.
4. **Post-opt body root** — `IrNode` discriminator post-pipeline.
5. **Post-opt `ir.types[rule.id]`** — final CSP projection consumed by
   the Rust emitter via `emit_direct_to_struct_projection` at
   `crates/core/src/backend/rust/emitter/grammar.rs:49-72`.

| Rule | Pre-opt body root | Pre-opt `ir.types` | Post-opt survives? | Post-opt body root | Post-opt `ir.types` |
|------|-------------------|-------------------|--------------------|--------------------|---------------------|
| CSS `colorFunction` | `Map[Expr → Named("Color")]` | `Named("Color")` | ✗ (pruned) | — | — |
| CSS `colorFn`       | `Seq` | `Tuple([Span, U8, BoxedEnum, BoxedEnum, BoxedEnum, Option(BoxedEnum)])` | ✓ | `Seq` | same Tuple (Map remains a buried child) |
| CSS `colorMix`      | `Seq` | `Tuple([Span, U8, Option(BoxedEnum), Span, BoxedEnum, Option(BoxedEnum), Span, BoxedEnum, Option(BoxedEnum)])` | ✗ (pruned) | — | — |
| JSON `string`       | `Map[Expr → Named("String")]` | `Named("String")` | ✓ | `Map[Expr → Named("String")]` | `Named("String")` |

## Collapse discrimination

A6 §2 conjectured **H1** (egraph cost-guided extraction unwraps the
outer Map) or **H2** (`compute_aliases` / `compute_transparent` stamps
the rule before `canonicalize_aliases` + `prune_unreachable` drop the
shell). Neither hypothesis fits the empirical evidence.

### Finding 1 — JSON `string` is fully healthy

`string`'s lowered body is `Map[Expr → Named("String")] { Regex }`.
The Map root grounds the CSP `MapConstraint`'s `var` to `Named("String")`,
which the rule_var inherits via `EqualConstraint(rule_var, body_var)`
(`crates/ir/src/passes/types/generate.rs:84-85`). Both pre-opt and
post-opt observe `ir.types[string] = Named("String")`. The probe's
"Named-bearing rules surviving" pass count for JSON is `1` (string),
matching expectation. **No fix needed for JSON.**

A6 §1's claim "ir.types admits zero `Named(_)` entries on the
Rust-target path for all six grammars" is *partially incorrect*: VM
target's `ir.types` for JSON does carry `Named("String")`, and the
Rust-target path adds nothing that drops it (the `prepare_grammar →
analyze_grammar → project_types` triplet is a superset of VM's path,
which already projects Named).

### Finding 2 — `colorFn` (and `colorMix`) is a grammar-precedence issue

The pre-opt structural snapshot already shows `colorFn`'s body root as
`Seq`, not `Map`. The Map appears only as a deeply-nested child wrapping
the closing `")"` literal:

```
- Seq
    - Literal ("color")
    - Next ("(", colorSpace)
    - Ref(colorValue)
    - Ref(colorValue)
    - Ref(colorValue)
    - Skip
        - Repeat[0..=1] (Next("/", colorValue))
        - Map[Expr → Named("Color")]
            - Literal (")")
```

This is the BBNF grammar's expression-precedence definition exposing a
mismatch with user intent. The grammar in `grammar/bbnf/bbnf.bbnf`:

```
mapped_factor = factor , ( "->" ?w , ( value_expr , type_annotation ? ) ) ? ;
binary_factor = mapped_factor , ( binary_operators ?w , mapped_factor ) * ;
concatenation = ( binary_factor ?w , "," ? ) + ;
```

So `->` binds at the **mapped_factor** level — it attaches to the
single nearest factor on its left, not to the whole concatenation /
binary chain. In:

```bbnf
colorFn = "color" , "(" >>
    colorSpace , colorValue , colorValue , colorValue , ("/" >> colorValue)?
    << ")" -> input : Color ;
```

The `-> input : Color` only wraps the rightmost factor `")"`. The
intended behaviour — wrap the whole rule body — requires either:
- explicit grouping: `colorFn = ( "color" , ... << ")" ) -> input : Color ;`
- or `@{...}` capture (the colorFunction shape).

`colorFunction` already uses `@{...}` (per grammar `colorFunction = @{
... } -> input : Color ;`), and its pre-opt body root IS
`Map[Expr → Named("Color")] { Map[SpanCapture] { Seq } }` — Named at
root. So the same fix repeated for colorFn / colorMix would let the
CSP propagate Named end-to-end.

### Finding 3 — `colorFunction` and `colorMix` are pruned as unreachable

The post-opt snapshot shows `colorFunction` and `colorMix` eliminated
from `ir.rules`, with zero live referrers. The pre-opt snapshot (which
sets `preserve_identity = true` for every rule) preserves them.

The cause is `prune_unreachable`
(`crates/ir/src/passes/transform/prune.rs:11-78`) doing its job on a
truly unreachable cycle. Tracing:

- The entry rule `stylesheet` reaches `value` via `qualifiedRule →
  ruleBlock → blockContent → declaration → value`.
- `value`'s definition (in `properties.bbnf:39-43`) references
  `colorFn`, `hex`, `namedColor`, `varFunction`, etc. directly — but
  **NOT** `color`. The `value` rule from `values.bbnf:84` (which
  *does* include `color`) is in a file not imported by the entry
  point.
- `color = colorMix | colorFn | hex | colorFunction | namedColor`
  (in `color.bbnf:321`) — only `colorFn`, `hex`, `namedColor` survive
  via direct Refs from the entry-reachable `value`.
- `colorMix` is referenced only by `color` (its `color, percentage?
  ,",", color, percentage?` body inside `color.bbnf:299-303`).
- `colorFunction` is referenced only by `color`.
- `color` is referenced only by `colorMix` (the cycle: `color →
  colorMix → color`).

So `color`, `colorMix`, and `colorFunction` form an island unreachable
from `stylesheet`. `prune_unreachable`'s DFS from `ir.entry` correctly
drops the whole island.

`gradients.bbnf:22 (colorStop = color, …)` would have referenced
`color`, but `gradients.bbnf` is not transitively imported by
`stylesheet.bbnf` — only `properties.bbnf`, `selectors.bbnf`, and
`media.bbnf` are imported, and none of those re-imports `gradients`.

## Collapse-site identification

There is **no IR-pipeline collapse site** for the four named rules.
The empirical observations decompose as:

- `string` (JSON): no collapse, Named survives.
- `colorFunction`, `colorMix` (CSS L4): pruned by
  `prune_unreachable` because no entry-reachable rule references
  them. Pruning happens at every iteration of the structural
  normalizer loop (`crates/core/src/pipeline/compile.rs:552, 554, 557`),
  but the prune is *correct* — these rules genuinely have no
  consumers from the entry rule.
- `colorFn` (CSS L4): outer `Map[Expr → Named("Color")]` was
  never the body root, even pre-lowering. This is the BBNF
  grammar's expression-precedence binding `->` at the
  `mapped_factor` level rather than wrapping the whole rule
  body (`grammar/bbnf/bbnf.bbnf:42`).

## Hypothesis verdict

| Hypothesis | Verdict | Evidence |
|------------|---------|----------|
| **H1** (e-graph extraction unwraps Map) | REJECTED | colorFn's pre-opt body root is also `Seq`, not `Map`. The e-graph runs *after* the structural normalizer loop, so any post-opt body shape that already exists in pre-opt cannot be the e-graph's doing. |
| **H2** (alias/transparent stamping drops shell) | REJECTED | All four rules show `is_alias = None`, `is_transparent = false` in both snapshots. The metadata passes do NOT stamp the rules. |
| **GRAMMAR-PRECEDENCE** (BBNF `->` binds at mapped_factor) | CONFIRMED for colorFn / colorMix | Pre-opt structural snapshot shows the Map at the rightmost factor, not at body root. Source-grammar fix surface. |
| **DEAD-RULE-PRUNE** (entry-unreachable cycle) | CONFIRMED for colorFunction / colorMix | Entry-reachable `value` does not reference `color`; the `color → colorMix → color` cycle is unreachable. `prune_unreachable` correctly drops the island. |

## Recommended fix surface

W2.2 should reframe the fix from "preserve Named through pipeline
passes" to addressing the actual two root causes:

### Fix A — `colorFn` / `colorMix` precedence (grammar-source)

Wrap the rule body in parentheses or `@{...}` so the `->` annotation
applies to the whole expression. Either:

```bbnf
colorFn = ( "color" , "(" >>
    colorSpace , colorValue , colorValue , colorValue , ("/" >> colorValue)?
    << ")" ) -> input : Color ;
```

Or (matching the colorFunction shape):

```bbnf
colorFn = @{
    "color" , "(" >>
        colorSpace , colorValue , colorValue , colorValue , ("/" >> colorValue)?
    << ")"
} -> input : Color ;
```

The first form preserves the existing semantics (no SpanCapture wrapper);
the second form mirrors colorFunction's `@{...}` capture. Per AY
invariant 4 (no new grammar directives), this is a pure source-edit
within existing syntax. **Estimated change: 2 grammar lines per rule
(colorFn + colorMix), ~6 LOC total.**

### Fix B — `colorFunction` / `colorMix` reachability (grammar-source)

Add `color` as a branch of `value` in `properties.bbnf`. Currently:

```bbnf
value = varFunction | calcFunction | urlFunction | colorFn
      | genericFunction | hex
      | dimension | number | cssString | namedColor | globalKeyword
      | "," | "/"
      | dashIdent | ident ;
```

Should become:

```bbnf
value = varFunction | calcFunction | urlFunction | color
      | genericFunction
      | dimension | number | cssString | globalKeyword
      | "," | "/"
      | dashIdent | ident ;
```

(Note: colorFn / hex / namedColor become reachable via `color`.) This
makes `colorFunction`, `colorFn`, `colorMix`, `hex`, `namedColor` all
reachable through `color`'s Alt. **Estimated change: 1 grammar line
edited (~4 LOC).**

Both fixes together restore the colour rule reachability + Named
preservation that A6 expected. Wire-contract test
`named_type_preservation.rs` (W2.7) will then assert each colour rule
projects as `Named("Color")` in `ir.types`.

### Optional belt-and-braces — pipeline-side guards

Per A6 §4, the proposed pipeline-side guards (predicate
`has_named_return_type` in `metadata.rs`, cost-model side constraint
in `cost.rs`, `unwrap_map_node` skip in `span.rs`) remain valuable as
**defensive guarantees** — they prevent any *future* grammar that does
correctly bind Named at the body root from losing it through pipeline
churn. None of them needs to land in W2.2 to fix the three observed
rules; they're invariant insurance, not bug-fix code.

If user / orchestrator chooses to add the guards anyway:

- `metadata.rs:compute_aliases` — guard `extract_alias_target` to
  return `None` when the Map's `FnDescriptor::Expr.return_type` is
  `Some(Named(_))`. Currently the function only matches `IrNode::Ref(id)`
  so the guard is a no-op for the rule shapes observed here, but adding
  a `has_named_return_type` predicate documents the invariant.
- `metadata.rs:compute_transparent` — same.
- `egraph/cost.rs:GrammarCostModel::cost` — add infinite-cost penalty
  on extracting a Map node whose `FnDescriptor::Expr.return_type` is
  `Some(Named(_))` if the candidate form does not preserve the Map.
  Currently the e-graph rules don't generate Map-stripping rewrites, so
  this is an invariant-preservation belt-and-braces.
- `passes/span.rs:unwrap_map_node` — skip the unwrap when the Map's
  `FnDescriptor::Expr.return_type` is `Some(Named(_))`. Already noted
  in A6 §4.3.

## Estimated fix LOC

- **Grammar-source fix A (colorFn/colorMix precedence)**: ~6 LOC
  in `grammar/css/l4/color.bbnf`.
- **Grammar-source fix B (color reachability via value)**: ~4 LOC
  in `grammar/css/l4/properties.bbnf`.
- **Pipeline guards (defensive, optional)**: ~50 LOC across
  `crates/ir/src/passes/metadata.rs` (predicate + 2 guards),
  `crates/ir/src/egraph/cost.rs` (cost penalty), and
  `crates/ir/src/passes/span.rs` (unwrap guard).
- **Wire-contract test (W2.7)**: ~80 LOC in
  `crates/core/tests/named_type_preservation.rs`.

Total work-unit cost (grammar fix + wire-contract test): **~90 LOC**.
With optional defensive guards: **~140 LOC**.

## Caveat — Pratt-style Map nesting

The probe focused on the four W2.1-spec rules. Other Named-annotated
rules in the grammar suite may exhibit different shapes (e.g. Pratt
operator chains where Named can attach to operator nodes inside
nested precedence levels). The probe binary is left in the codebase
as a pinned reproducer; W2.2 / W2.7 should re-run it after grammar
edits to confirm the fix, and extend it with new probe targets as
named annotations are added to other grammars.
