# AZ-IV.W0 Regen Research — Root-Cause Audit

**Lane**: research (read-only)
**Date**: 2026-05-01
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-aziv-w0-regen-research`
**HARD CAP**: 20 min research

## §1 — R1 root cause

**Symptom**: regen-emitted `parse_hregex_BbnfBootstrap_int_lit` /
`parse_hregex_BbnfBootstrap_float_lit` push `push_leaf_with_str`
instead of `push_leaf_with_i64` / `push_leaf_with_f64`.

**Trace**: live `dump_ir grammar/bbnf/bbnf.bbnf int_lit --structural`
shows the IR for `int_lit` is a bare `Regex(...)` with NO `Map { fn_id, … }`
wrapper; types-table has `rule #0 (int_lit) -> Span` (NOT `I64`). Same
for `float_lit -> Span` (NOT `F64`). The `-> i64` / `-> f64` annotation
in `grammar/bbnf/expressions.bbnf` is silently dropped during lowering.

**Root cause**: `crates/core/src/lower/expression/wrap.rs:81`. The
mapping-node detection inside `lower_mapped_factor` matches a child
whose `c.span_text().trim().starts_with("->")`:

```rust
if trimmed.starts_with("->") || trimmed.starts_with("=>") {
    mapping_node = Some(c);
    continue;
}
```

The canonical generated `parse_flat_BbnfBootstrap_mapped_factor`
(`crates/core/src/grammar/generated/bbnf.rs:9208-9377` in HEAD) does
NOT wrap the optional `( "->" ?w , ( value_expr , type_annotation ? ) ) ?`
group in an anonymous compound. The `->` punctuator is consumed via a
direct `[45u8, 62u8]` byte check (no Span pushed); only the
`value_expr` and `type_annotation` compounds surface as direct children
of `mapped_factor`. So none of `body.children()` has a
`span_text().trim().starts_with("->")` — the mapping is silently
skipped, `mapping_node` stays `None`, and `lower_mapped_factor`
returns the bare factor result without wrapping in `IrNode::Map`.

**Pinned commit**: `954d166b feat(grammar/bbnf-self-host): replace
bootstrap_parser with canonical generated path`. The hand-written
`bootstrap_parser` previously synthesised an anonymous "mapping"
wrapper compound at the `->` byte (commit `a09173dc fix(bootstrap_parser):
wrap mapped_factor mapping in anonymous compound`) precisely so this
predicate would fire. Deleting `bootstrap_parser.rs` removed that
compensating wrap; the canonical generated parser was never extended
to push it.

The downstream emitter (`crates/core/src/backend/rust/emitter/shapes/hregex.rs:222-265`)
then sees `descriptor = None` (no `Map { fn_id, Regex }` to extract from)
and falls through to the `_ => quote! { push_leaf_with_str ... }`
catch-all arm. The descriptor `FnDescriptor::Expr { expr: MapExpr::Input,
return_type: Some(TypeDesc::I64) }` is never built because the IR Map
node never exists.

## §2 — R2 root cause

**Symptom**: `BbnfBootstrap::parse(grammar/bbnf/bbnf.bbnf)` errors at
`Syntax { offset: 36 }` after a fresh `cargo xtask regen`.

**Trace**: byte 36 is the start of the second comment line
(`// Self-hosted grammar definition.`). The first comment parses
through to byte 36 successfully. The error is reported by
`BbnfBootstrap::parse`'s tail check
(`crates/core/src/grammar/generated/bbnf.rs:17249`):

```rust
if pos != input.len() {
    return Err(crate::runtime::ParseErr::Syntax { offset: pos as u32, ... });
}
```

`parse_BbnfBootstrap_grammar` returns Ok after consuming only the
first grammar item, leaving 3412 bytes unparsed. Inspecting the
post-regen `parse_scalar_BbnfBootstrap_grammar` shows it's a
single-shot dispatcher (`parse_wrap_BbnfBootstrap_grammar_item`,
no loop), where the pre-regen tree had `parse_array_BbnfBootstrap_grammar`
with the canonical `loop { … }` over grammar_items.

**Root cause**: `crates/ir/src/passes/recognizers/shape_dispatch/array.rs:36`.
`detect_array` rejects `grammar` because the entry-rule body in IR is
just `Ref(#51 grammar_item)` (live dump: `=== rule #52 grammar
(entry=true) === Ref(#51 grammar_item)`). With no `Repeat` wrapper,
`mine_list_rules` (`crates/ir/src/passes/recognizers/list_rules.rs:107`)
returns empty for the entry rule and `detect_array` falls through.
Classification continues until `detect_scalar` admits the body as a
single `Ref(rule)` to a classified target
(`crates/ir/src/passes/recognizers/shape_dispatch/scalar.rs:43`).

The deeper cause is the SAME class of bug as R1: the canonical
parser-tree shape diverges from what the lowering layer expects, so
modifier information is silently lost. For
`grammar = ( grammar_item ?w ) *`:

- The outer factor's `*` modifier — emitted as a `Unit` leaf by
  `parse_keyword_BbnfBootstrap_modifier` in HEAD's `bbnf.rs`. The
  `recover_modifier` source-gap scan in
  `crates/core/src/lower/expression/repeat.rs:125-161` was added
  by `ee3e6c28` precisely to recover this. The IR dump shows the
  `*` was NOT recovered — the outer Repeat is gone.
- The inner `?w` modifier on `grammar_item ?w` — same shape; the
  `OptionalWhitespace` wrapper is also gone.

So `lower_factor` / `lower_mapped_factor` are dropping BOTH modifiers
on the grammar rule, not just one. The `recover_modifier` patch in
`ee3e6c28` only landed on the modifier slot; it does not cover the
deeper structural mismatch when the canonical parser inlines the
mapped_factor's optional mapping group without wrapping it.

**Pinned commit**: `954d166b feat(grammar/bbnf-self-host): replace
bootstrap_parser with canonical generated path` (same as R1). The
post-flip lowering chain has not absorbed the modifier-recovery
divergence at every layer. The `286425d5
fix(emitter/keyword-struct-direct): synthesize span leaf for
content-only keyword compounds` partial-fix is what then bridges the
modifier-text path post-regen, BUT (a) it only takes effect AFTER
regen rewrites the modifier rule to push Str — the parse used during
regen is HEAD's pre-regen Unit-pushing parser; (b) the broader IR
collapse that produces `Ref(grammar_item)` for `grammar` is
independent of this patch.

The 286425d5 commit message itself acknowledges:
> The remaining drift is dominated by independent canonical-regen
> divergences outside the keyword-shape carve: entry-rule shape
> classification (Array → Scalar misclassification in the recognizer
> passes), HRegex-shape payload semantics (i64 → str), and PHF-keyword
> table generation. These are W3a / W3c carve concerns.

The W3a.4 audit
(`docs/benchmarks/archive/AZ-III/W3a-4-regen-path-agnostic.txt:46-102`)
already named the same root cause and prescribed the same routes.
Neither route was fully landed; partial fixes (`ee3e6c28`,
`286425d5`) bridge the modifier-text path but miss the
mapped_factor mapping wrap and the broader Repeat preservation.

## §3 — Joint vs independent scope

**SHARED root cause**. R1 and R2 are two manifestations of the same
underlying defect: **the canonical generated parser's tree shape for
BBNF self-host diverges from what `lower/expression/*.rs` expects,
and the lowering chain silently drops information** (mapping arrows,
modifiers, repeats) instead of failing loudly.

The shared file/path: `crates/core/src/lower/expression/` (specifically
`wrap.rs` for the mapping wrap, `repeat.rs` for the modifier slot,
and possibly `alt.rs` for iteration-pair shape divergence).

The lossy mechanism is the same:
- predicate-driven detection (`span_text().starts_with("->")`,
  `kind() == BbnfKind::Unit`, `trimmed in {?, *, +, ?w}`)
- silent skip when the predicate fails
- no panic / no warning / no diagnostic — just lost IR information

R1 is the mapping-arrow case; R2 is the Repeat-modifier case (with
two losses on the grammar rule alone, plus the inner `?w`).

## §4 — Minimal change footprint

The W0.3 halt budget was "xtask + strategy registry + one
lowering/emitter surface". The actual surface to make regen green
(9/9 byte-identical) is **at least three** lowering surfaces plus the
existing two W0.3 scaffolds. Naming them:

1. **`crates/core/src/lower/expression/wrap.rs`**
   - `lower_mapped_factor`: replace span-prefix detection with
     structural detection (find the children whose role is
     value_expr / type_annotation by compound_kind / position rather
     than by span text). Reasonable elaboration: when no child has a
     `->`-prefixed span, scan the source gap between the factor child
     and the next substantive child for `->` to detect the mapping
     bytes; OR enumerate children positionally
     (`[factor, value_expr?, type_annotation?]`) and recover via
     find_descendant_by_kind.

2. **`crates/core/src/lower/expression/repeat.rs`**
   - `lower_factor`: investigate why `recover_modifier` does NOT recover
     the `*` on `grammar = ( grammar_item ?w ) *`. The `recover_modifier`
     source-gap scan from `ee3e6c28` should fire but the IR shows the
     Repeat is missing. Either the term's `byte_span()` returns wrong
     bounds for grouped (Paren) terms, or `has_unit_marker` is not
     being set for the outer factor. (Diagnostic note: the fix is
     in this file regardless — the mechanism for grouped-term modifier
     recovery needs to be made bullet-proof, not heuristic.)

3. **`crates/core/src/lower/expression/alt.rs`**
   - `lower_concatenation` / `lower_alternation`: per the W3a.4 audit,
     these may also drop iteration markers. The grammar rule's inner
     `?w` modifier on `grammar_item ?w` is also lost — that's an alt /
     concat / mapped_factor concern. Verify the `?w`-as-modifier path
     is preserved through every layer.

4. **`crates/core/src/grammar/generated/bbnf.rs`** (regenerated; not
   hand-edited but trees flip with the lowering fix above)
   - This is the OUTPUT of regen, not a hand-modified file. It will
     update mechanically once the lowering chain is fixed.

Plus (already W0.3):

5. **`crates/ir/src/registry/strategy.rs`** — manifest-driven resolver
   scaffold (landed in `138bd1ab`, no further change).
6. **`xtask/src/...`** — the regen entry point (no changes needed
   beyond what `89fbada8` shipped).

**Total**: 3 lowering surfaces (`wrap.rs`, `repeat.rs`, `alt.rs`) plus
the regen output flip in `bbnf.rs` (mechanical) and other generated
trees. The W0.3 halt budget of "one lowering / emitter surface" is
breached by 2x.

The `crates/core/src/backend/rust/emitter/shapes/hregex.rs` emitter
is NOT in the change set — its dispatch logic is correct given a
correct descriptor; the bug is upstream in lowering. Same for the
shape detector (`crates/ir/src/passes/recognizers/shape_dispatch/array.rs`)
— it correctly rejects `Ref(grammar_item)`; the IR is what's wrong.

## §5 — Plan-lane handoff

The plan lane MUST amend `docs/tranches/AZ-IV/waves/W0.md` to:

(a) widen the modify-carve from "xtask + strategy registry + one
lowering/emitter surface" to "xtask + strategy registry + the
`crates/core/src/lower/expression/{wrap,repeat,alt}.rs` triad plus
mechanical regen output". The single-surface budget cannot close
W0.3 because R1 and R2 are independent surface manifestations of a
lowering-chain divergence affecting at least three sibling files.

(b) name the SHARED defect explicitly in the wave doc: "the canonical
generated parser inlines mapped_factor's mapping group and emits the
modifier as a non-span leaf; lowering's predicate-based detection
silently drops the IR contributions". The fix is structural — emit
panics / obligations when the predicates fail to find a match, then
land matching detection (positional + structural by compound_kind +
source-gap fallback) so every annotation in the grammar source
reaches the IR. Per `feedback_typed-materialization-invariant`:
"every `->` in the grammar must reach the tape emitter; inference
composes types, never loses them; parity = full typed-AST equivalence".

(c) keep the W3a.4 audit
(`docs/benchmarks/archive/AZ-III/W3a-4-regen-path-agnostic.txt`) in
scope for the redress lane — it already prescribed the same routes
and the same dispatch verdict; the redress is to LAND those routes
fully, not redesign them. Routes A (lowering) and B (keyword emitter)
must both close, not just one — partial closes (ee3e6c28 +
286425d5) leave R1 and R2 unmasked.

## Evidence summary

- live IR dump: `=== rule #0 int_lit (entry=false) === Regex(...)`
  with NO Map wrapper; `rule #0 (int_lit) -> Span` (not I64).
- live IR dump: `=== rule #52 grammar (entry=true) === Ref(#51
  grammar_item)` with NO Repeat wrapper; `rule #52 (grammar) ->
  BoxedEnum`.
- live regen-then-parse: `cargo xtask regen --grammar bbnf` followed
  by `debug_parse grammar/bbnf/bbnf.bbnf` reproduces
  `BbnfBootstrap::parse Err: Syntax { offset: 36, rule: None }`
  exactly per the W0.3 halt.
- diff `/tmp/bbnf-pre-regen.rs` vs post-regen confirms: int_lit/float_lit
  push paths flip i64/f64 → str at lines 2291-2329; grammar entry
  rule flips Array → Scalar at lines 10126-10173 (deleted) /
  9626-9636 (added).

## Time accounting

- Read halt + W3a.4 audit + adjacent commits: ~6 min
- Build + run debug_parse / dump_ir + regen reproduction: ~7 min
- Trace lowering predicates + cross-check commit history: ~4 min
- Write artefact: ~3 min
- **Total**: ~20 min (at hard cap)
