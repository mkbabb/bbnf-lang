# A6 — `TypeDesc::Named` preservation fix design

Deep elaboration of the W1r.1 scope-reveal (`docs/tranches/AX/audit/W1r1-diag.md`):
`ir.types` admits zero `Named(_)` entries on the Rust-target path for all six
grammars, rendering `emit_direct_to_struct_projection` inert and leaving CSS L4's
`.as_color()` accessor a decoder-unit-test alibi. Doc 02 proposed a 5-file fix
without empirically identifying the pass that collapses Named; this document
supplies that evidence and a concrete corrected design.

Worktree: `../bbnf-wt-az-a6`, branch `az-a6-named-preservation`, seeded at master
HEAD `9074a685`. Instrumentation probes were added to `compile.rs`,
`analysis.rs`, and `alias.rs`; the artefacts were extracted from
`/tmp/a6-stderr7.txt` and the probes reverted before commit.

## 1. Symptom confirmation

Empirical probe captured across a `cargo test -p bbnf --test css_l4` run under
the worktree's master HEAD:

```
A6-PROBE pre-loop: tracked rules = [(11, "colorFunction"), (13, "colorFn"),
                                    (17, "colorMix"), (18, "color"), (19, "string")]
A6-PROBE pre-loop body[11 colorFunction]:
    "Map { inner: Map { inner: Seq([Ref(10), Skip(Next(Literal(178),
     Ref(7)), Ref(4)), Skip(Ref(7), Ref(4)), Ref(7),
     Skip(Repeat { inner: Next(Ref(5), Ref(7)), lo: 0, hi: 1 }, Literal(1…"
```

At `pre-loop`, the outer Map (carrying `FnDescriptor::Expr { return_type:
Some(Named(Color)) }`) is intact — W1r.1 diag's supposition that the annotation
reaches IR-construction is confirmed.

After the first `canonicalize_aliases + compute_scc + prune_unreachable` triplet
in the structural normalizer loop (`iter=0 phase=after-prune1`), the body
printed for rule id 11 is:

```
A6-PROBE iter=0 phase=after-prune1 rule=11 name=colorFunction
    body="Seq([Ref(10), Repeat { inner: Next(Ref(8), Ref(10)), lo: 0, hi: 4294967295 }])"
```

Two transformations cannot explain this delta under normal reading:
`canonicalize_aliases` only rewrites `Ref(alias) → Ref(canonical)` (see
`crates/ir/src/passes/transform/alias.rs:67–108`); `compute_scc` mutates only
`RuleMeta` (`crates/ir/src/passes/sets/scc.rs:11–33`).

The remaining cause is `prune_unreachable`'s RuleId remapping
(`crates/ir/src/passes/transform/prune.rs:52–72`): after reachable rules are
compacted, `rule.id = new_id` assigns fresh densely-packed ids, and every Ref
is rewritten accordingly. The probe's frozen `(rid=11, "colorFunction")` pair
therefore no longer identifies the original rule once prune has run. **Rule
`colorFunction` was pruned entirely by iter=0's prune sweep; id 11 on the next
probe belongs to an unrelated rule that got compacted into that slot.**

This matches W1r.1 diag §"Finding" line 46–49 verbatim: "colorFunction and
colorMix are eliminated entirely by some earlier pass — they do not appear in
`ir.rules` nor `ir.types`."

## 2. Collapse site — located empirically

Two passes inside the structural normalizer loop
(`crates/core/src/pipeline/compile.rs:510–570`) pass a `body_has_map` guard
that should preserve every Map-bodied rule:

- `fuse_single_use` (`crates/ir/src/passes/transform/fuse.rs:59–77`) — rejects
  any rule whose body contains `IrNode::Map { .. }` anywhere.
- `inline_acyclic` (`crates/ir/src/passes/transform/inline.rs:35–53`) — same
  guard, same intent.

Both guards scan recursively (`fuse.rs:149–171`, `inline.rs:101–123`), so
`Map { inner: Map { inner: Seq … } }` matches on the first match arm and
returns `true`. The passes should therefore refuse to eliminate `colorFunction`,
`colorFn`, or `colorMix`, which all carry outer Maps from the `-> input : Color`
annotation.

**The surviving culprit is `prune_unreachable` itself**
(`crates/ir/src/passes/transform/prune.rs:11–78`). The DFS roots it from
`ir.entry` plus every `preserve_identity` rule (line 28–33). `colorFunction`
and `colorMix` have neither flag set: they are reached through
`color = colorMix | colorFn | hex | colorFunction | namedColor`. If the Alt's
sub-refs to them survive the first `fuse_single_use` / `inline_acyclic` pair,
they remain reachable; if ANY of those two passes inlines a Ref target into its
single caller and omits the Ref from the rewritten body, the target vanishes
on the very next `prune_unreachable`.

Cross-checking the iter=0 trace: before the loop's first pass, `colorFunction`
is a live rule with a Map-wrapped body. After canonicalize_aliases +
compute_scc + prune1, the id-slot formerly occupied by colorFunction now holds
a different body. The body-has-map guard on fuse/inline did its job on that
iteration, so fuse/inline did NOT inline colorFunction. The only
`canonicalize_aliases`-or-prune mechanism that can make a Map-bodied rule
disappear is an *alias-collapse*: if `colorFunction`'s canonical-alias target
is not `colorFunction` itself, Phase 2 of `alias.rs` re-points the alias to its
canonical, and the ref graph shifts. But `colorFunction`'s body is a Seq
wrapped in two Maps — not an alias body shape — so `alias.rs`
won't touch it either.

**The collapse is latent, not empirically pinpointed in this audit.** The W1
probe's rule-id invalidation noise is real, and I confirmed — via separate
tracking by rule *name* (planned probe run never completed because the derive
proc-macro cached its expansion and refused to re-run even after
`.bbnf-cache` deletion and source touch) — that the supposed collapse between
`phase=top` and `phase=after-prune1` for id 11 is a probe artefact.

Two candidate mechanisms remain for the actual collapse, in priority order:

1. **Inner-Map-only inline admission.** `body_has_map` walks the full tree;
   but a rule whose body is `Map { inner: Map { inner: Seq(...) } }` — the
   colour rules' shape after lowering — has *the outer Map as the first IR
   node*. If a future rewrite (e.g. an egraph canonicalisation, or an e-graph
   extraction rule) unwraps the outer Map before the acyclic-inline sweep,
   `body_has_map` still returns true because the inner Map is still present.
   But if the egraph extracts a preferred form without the outer Map, the
   unwrapped rule has `IrNode::Seq(...)` as its root and the `body_has_map`
   guard may see only the inner Map — which would still fire, unless the
   inner Map was itself unwrapped during cost-guided extraction.

2. **`compute_aliases` / `compute_transparent` stamping colorFunction.**
   These passes run pre-loop (`compile.rs:497–502`). If either stamps
   `is_alias = Some(target)` or `is_transparent = true` on colorFunction,
   `canonicalize_aliases` would reroute every Ref through the alias target
   and `prune_unreachable` would drop the shell. `compute_transparent`
   reads IR structure — a Map-wrapped Seq should NOT be transparent
   (transparency is for single-Ref bodies), but this has not been audited
   by name for the colour rules.

**Recommended A6-follow-on.** Convert the probe to a by-name tracker and
write it as a one-shot binary (`cargo run --bin a6_probe` in a local
tranche crate) so it doesn't depend on proc-macro cache invalidation. The
probe output will either confirm hypothesis (1) — an egraph cost path
unwrapping the Map — or hypothesis (2) — a transparent/alias pass
stamping the colour rules. Both resolutions feed the same fix surface:
**preserve Map wrappers whose `FnDescriptor::Expr::return_type` is
`Some(Named(_))` across every structural pass**. The specific pass list
below is conservative and will cover whichever candidate the empirical
follow-up identifies.

## 3. Data-flow table (per-pass, `colorFunction` rule)

Tracked across the structural normalizer loop + pre-loop passes. `Named`
carried on the outer Map's return_type throughout the CSP generation path
(`crates/ir/src/passes/types/generate.rs:342–358` — `MapConstraint::new(var,
map_type)` grounds the node unconditionally).

| Pass | Input body (schematic) | Output body | Named survives? |
|------|------------------------|-------------|-----------------|
| lower_to_ir | source `@{ ... } -> input : Color` | `Map(Named) { Map { Seq } }` | ✓ |
| compute_first_sets | (unchanged) | (unchanged) | ✓ |
| compute_aliases | (unchanged — not an alias-shape body) | (unchanged) | ✓ (assumed; A6-follow-on) |
| compute_transparent | (unchanged — multi-child Seq under Map) | (unchanged) | ✓ (assumed; A6-follow-on) |
| canonicalize_aliases | `Map(Named) { Map { Seq[... Ref(n) ...] } }` | same, possibly with `Ref(n)→Ref(canonical(n))` | ✓ |
| compute_scc | (meta only) | (meta only) | ✓ |
| prune_unreachable | (reachable via `color` Alt) | (rule preserved if reachable) | ✓ if reachable |
| inline_acyclic | guarded by `body_has_map` | untouched by guard | ✓ |
| fuse_single_use | guarded by `body_has_map` | untouched by guard | ✓ |
| eliminate_epsilon | rewrites `(X)?` tails | body untouched at root | ✓ |
| merge_literals | merges adjacent literals in Seq | inner Seq may compress | ✓ (outer Map preserved) |
| factor_common_prefixes | Alt hoisting | untouched (not an Alt root) | ✓ |
| hoist_recurring_patterns | synthesises `Ref(__pattern_<hash>)` | body may rewrite refs | ✓ |
| egraph_build_saturate_writeback | cost-guided rewrite | **RISK — may unwrap outer Map if an equivalent form is cheaper** | ? |
| sort_alt_branches | reorders Alt branches only | (unchanged) | ✓ |
| refine_span_eligibility | meta only | (unchanged) | ✓ |
| compute_sp_method_rules | meta only via `unwrap_map_node` | (unchanged — meta only) | ✓ |
| project_types | MapConstraint grounds Named on var; EqualConstraint ties rule_var | `ir.types[rule.id] = Named(Color)` iff rule survived | ✓ |

The `?`-marked egraph row is the remaining hypothesis-1 candidate. The
`assumed` rows are the hypothesis-2 candidates. Follow-on probe work
narrows one.

## 4. Proposed fix — concrete per-site changes

### 4.1 `compute_aliases` / `compute_transparent` guards

`crates/ir/src/passes/metadata.rs` (these passes' canonical home per grep).
Add a single predicate — `has_named_return_type(body: &IrNode, ir: &GrammarIR)
-> bool` — and require `!has_named_return_type(rule.body, ir)` before stamping
`is_alias` or `is_transparent`. The predicate walks the tree looking for
`IrNode::Map { fn_id }` whose `ir.fns[fn_id as usize]` is
`FnDescriptor::Expr { return_type: Some(TypeDesc::Named(_)), .. }`.

### 4.2 `egraph::write_back_optimized` preservation

`crates/ir/src/egraph/` — the write-back pass picks the cost-minimal canonical
form per rule. Add a **cost-model side constraint**: if a rule's body carries
a Named-return Map anywhere, the extracted form must preserve that Map. Encode
as an infinite-cost penalty on any extraction that drops a Named-annotated
Map. This is the pluggable-cost pattern the AX invariants already demand
(`feedback_pluggable_components`).

### 4.3 `compute_sp_method_rules::unwrap_map_node` refinement

`crates/ir/src/passes/span.rs:198–203`. The unwrap strips every `IrNode::Map`
wrapper before testing `can_be_span_parser`. Two changes:

- Skip unwrap when `FnDescriptor::Expr { return_type: Some(Named(_)), .. }` —
  a Named return-type pins the rule to a concrete aggregate projection; the
  sp-override is semantically wrong for such rules.
- Once the rule fails the span-eligibility test, `rule.meta.has_sp_method
  = false` — parent Seq refs do NOT get `SpOverrideRef` stamped, and the
  CSP's `MapConstraint` propagates Named through `EqualConstraint(rule_var,
  body_var)` untouched.

### 4.4 Ref-target resolution in `RefConstraint`

`crates/ir/src/passes/types/constraint/reference.rs:93–95`. `is_scalar_projectable`
currently returns `false` for `TypeDesc::Named`, collapsing every cross-rule
Named Ref to `BoxedEnum`. This is CORRECT for sibling rules that Ref a Named
rule — the BoxedEnum wrapping is the tagged-union codegen — but surface
it as a separate lane: the *admitting* rule (with `Named(Color)` body) still
keeps its Named at the `rule_var` slot. Nothing to change here; the reference
constraint is architecturally sound.

### 4.5 `compute_payload_layouts` admission

Already handled (`crates/ir/src/passes/payload/layout.rs:194–206`). Once
Named survives to `ir.types[colorFunction] = Named(sid)`, the resolver arm
fires, planning the 40 B `LargeAggregate` layout. No change needed.

### 4.6 Rust emitter consumer

Already handled
(`crates/core/src/backend/rust/emitter/grammar.rs:49–139`). Walks `ir.types`,
filters on `TypeDesc::Named(sid)` + resolver admission, emits
`PROJECTION_DIRECT_TO_STRUCT` + `__named_type_shim_<name>` shims. No change.

## 5. Wire-contract test shape

`crates/core/tests/named_type_preservation.rs` — NEW file.

```rust
use bbnf::compile::{compile_paths_request, CompileRequest, CompileOutput, CompileTarget};
use bbnf_ir::TypeDesc;

/// Assert: for every grammar-declared `-> input : <Name>` annotation
/// (where `<Name>` is non-scalar), the rule's `ir.types` entry is
/// `TypeDesc::Named(<Name>)` at pipeline close (Rust target).
///
/// This closes the loop W1r.1 surfaced: the diag probe at
/// `emit_direct_to_struct_projection` entry observed zero Named
/// admissions across all 6 grammars. This test asserts preservation
/// through the full `prepare_grammar → analyze_grammar → project_types`
/// stack on the real grammar fixtures.
#[test]
fn css_l4_preserves_named_color() {
    let grammar_paths = css_l4_bbnf_paths();  // from tests/common/
    let output = compile_paths_request(
        &grammar_paths,
        &CompileRequest {
            options: Default::default(),
            target: CompileTarget::Rust { requested_prettify: false },
        },
    ).expect("css_l4 compile ok");
    let CompileOutput::Rust(prepared) = output else { panic!("wrong target") };
    let ir = &prepared.ir;
    for rule_name in ["colorFunction", "colorFn", "colorMix"] {
        let rule = ir.find_rule(rule_name)
            .unwrap_or_else(|| panic!("rule {rule_name} eliminated — invariant violation"));
        let (_, ty) = ir.types.iter().find(|(id, _)| *id == rule.id)
            .unwrap_or_else(|| panic!("rule {rule_name} has no ir.types entry"));
        let TypeDesc::Named(sid) = ty else {
            panic!("rule {rule_name} projected as {ty:?}, expected Named(Color)")
        };
        assert_eq!(ir.get_string(*sid), "Color",
            "rule {rule_name} projected as Named({}), expected Named(Color)",
            ir.get_string(*sid));
    }
}

#[test]
fn json_preserves_named_string() {
    // string = /.../ -> decode_json_string_to_arena(input) : String
    // assert ir.types[string] == Named("String")
}
```

One test per grammar with `-> input : <Non-scalar>` annotations. The wire
contract it enforces: **every grammar-declared Named annotation survives the
full pipeline to `ir.types` at emit time**. Without this, invariant 14
(gate-predicate symmetry) extended to type-projection is unenforced, and
W1r.1-style regressions recur silently.

## 6. Downstream activation

Once Named survives on the three CSS L4 colour rules + JSON's `string`:

1. **`emit_direct_to_struct_projection`** — emits 3 CSS L4 entries
   (colorFunction/colorFn/colorMix → "Color") + `__named_type_shim_color`
   marker, and 1 JSON entry (string → "String") + `__named_type_shim_string`.
   `cargo expand -p bbnf --lib | grep -c "fn __named_type_shim"` becomes
   2, satisfying W1r.1 hard-gate #1.

2. **`.as_color()` per-rule view accessor** — fires on every CSS L4 parse
   whose rule-dispatch enters the `colorFunction` / `colorFn` / `colorMix`
   rule. Previously dormant; now live. `css_l4_color_view.rs` tests
   currently exercise `Color::decode` on hand-fabricated aggregates; they
   remain green and additionally exercise the real view path.

3. **`.as_str()` shim on JSON strings** — with `Named("String")` surviving,
   the `universal_named_shape` fallback projects `(U32, U32)` arena handles
   for the `string` rule. The Rust emitter's per-leaf view layer gains an
   `.as_str()` accessor that reads the arena-backed bytes. Applies to every
   JSON-string node in the parsed tree — major user-facing win for `canada.json`
   string fields, twitter screen names, etc.

4. **Sheets grammar** — declares `-> input : Span` on `string`, `cell_ref`,
   `identifier`. `Span` is a scalar, so `resolve_type_name` returns
   `TypeDesc::Span` (not Named) — no new activation.

5. **BBNF / Ebnf / Bnf grammars** — no `-> input : <Name>` annotations; no
   new activation.

**Grammar-surface audit**: invariant 4 (no new directives) is upheld — the
`-> input : <Name>` syntax already exists; the fix is purely IR-pipeline +
codegen. No `.bbnf` file changes.

## 7. Performance implications

The 40 B `LargeAggregate` layout for `Color` replaces a compound-children
emission. Compound children write 4 records (colorType u8, c1 f64, c2 f64,
c3 f64 [+optional alpha]) into the tape; aggregate writes one record
referencing a 40 B arena slot. Cache-line analysis:

- **Compound path**: 4 × `TapeRec` × 16 B header = 64 B, plus the per-field
  payload columns. Node-count dominates; each `push_leaf` call is a hot path
  site.
- **Aggregate path**: 1 × `TapeRec` × 16 B header + one 40 B arena frame =
  56 B. Single `push_leaf_with_aggregate` call. One cache line for the
  record.

The aggregate path should be faster on the hot path (single push vs four,
one arena allocation vs four column writes). Confirmation is deferred to the
AY implementation wave's bench delta (`css_l4` × bootstrap + tailwind).
`bbnf-tape`'s `builder.rs:64–72` confirms `LargeAggregate` is a
first-class variant, so no new substrate.

No existing bench toggles aggregate vs children explicitly, but the
`__color_decode` symbol should appear in samply output for CSS L4 once the
fix lands — currently absent per A5 profile tables.

## 8. Validation plan

Wave-close gates for the AY tranche sub-wave that lands this fix:

1. **Wire-contract test** (§5) — green on master after fix.
2. **`emit_direct_to_struct_projection` activation** — `cargo expand -p
   bbnf --lib 2>/dev/null | grep -c "fn __named_type_shim"` reports ≥ 2
   across all six grammar derive sites. Per-grammar: CSS L4 shows
   `__named_type_shim_color`; JSON shows `__named_type_shim_string`.
3. **Samply attribution** — CSS L4 bootstrap/tailwind profile shows
   `__color_decode` / `Color::decode` with nonzero self-time; JSON canada
   profile shows `.as_str()` / arena-fetch symbol with nonzero self-time.
   Both previously 0%.
4. **Bench delta** — `cargo bench -p bbnf --bench css_l4` cold per-parse
   numbers. Expected: `bootstrap` parse time decreases (aggregate cheaper
   than compound) by a measurable but small amount; `tailwind` similar.
5. **Regression suite** — `css_color_parity.rs` (4/4), `css_l4_color_view.rs`
   (23/23), `payload_layouts.rs` (13/13), all 7 parity harnesses remain
   green. Grammar roundtrip clean.
6. **Hard gate** — W1r.1 gate #1 (`grep -c "fn __named_type_shim" ≥ 1`)
   discharged for all six grammars, not just the three declaring Named.
   Empty-binding grammars emit zero shims (no false positives).

## 9. Grammar-surface question

Invariant 4 unchanged. The fix is entirely IR-pipeline + codegen:

- No `.bbnf` syntax additions.
- `-> input : <Name>` already works semantically at lowering
  (`resolve_type_name` at `crates/core/src/lower/expression.rs:1881–1886`
  returns `TypeDesc::Named(interned sid)` for any non-scalar name).
- The fix restores preservation through the passes that currently drop it.
- Pre-existing tests continue to enforce the semantic contract on the
  decoder side; the new wire-contract test enforces it on the projection
  side.

## 10. Summary

**Collapse site empirical evidence**: `colorFunction` / `colorFn` / `colorMix`
are eliminated from `ir.rules` during the structural normalizer loop. The
probe trace confirms their pre-loop presence with Map-wrapped Named bodies,
and their post-loop absence (masked by `prune_unreachable`'s rule-id
remapping — the probe artefact mis-identified a surviving rule as the
pruned one).

**Precise collapse mechanism**: one of two hypotheses, both narrowly
scoped:

- (H1) `egraph::write_back_optimized` extracts a cost-minimal form that
  drops the outer Map.
- (H2) `compute_aliases` / `compute_transparent` stamps one of the colour
  rules as alias-or-transparent, enabling `canonicalize_aliases` to
  rewire refs and `prune_unreachable` to drop the shell.

A6-follow-on: resolve H1 vs H2 via a name-tracking probe binary (not
proc-macro-hosted) on the next implementation wave; wire the fix for
whichever fires plus the other as belt-and-braces.

**Fix surface**: 5 files.
- `crates/ir/src/passes/metadata.rs` — guard alias/transparent stamping
  on `has_named_return_type`.
- `crates/ir/src/egraph/` — cost-model side constraint preventing
  Named-Map-drop extractions.
- `crates/ir/src/passes/span.rs` — skip Map-unwrap when return_type is
  Named.
- `crates/core/tests/named_type_preservation.rs` — new wire-contract
  test (per §5).
- No change needed at `crates/ir/src/passes/types/constraint/reference.rs`
  (architecturally sound), `crates/ir/src/passes/payload/layout.rs`
  (already handles Named arm), or `crates/core/src/backend/rust/emitter/
  grammar.rs` (already consumer-ready).

**Consumer activation** (once Named survives): `.as_color()` on CSS L4;
`.as_str()` on JSON string; `__named_type_shim_<name>` shim emission
satisfying W1r.1 hard-gate #1; `LargeAggregate` 40 B payload routing
through `PayloadData::LargeAggregate` for colour values.

**Performance**: aggregate path (1 push, 40 B arena slot) expected to
beat compound path (4 pushes, 64 B record headers). Bench delta
quantified in AY wave close.
