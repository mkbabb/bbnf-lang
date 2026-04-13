# Self-Hosting Closure — Audit + AR Path

## Reproduction of regen failure

Procedure (worktree `agent-a62f8366`):

```bash
cp crates/core/src/grammar/generated.rs /tmp/generated_working.rs
rm -rf target/.bbnf-cache/
touch crates/derive/src/lib.rs
./scripts/bootstrap-bbnf.sh             # succeeds; regen is 78,307 lines
                                        # (working is 20,692 lines)
cargo test -p bbnf --test grammar_roundtrip -- --nocapture --test-threads=1
```

Six grammar_roundtrip tests panic. The exact panics:

| Test | Panic |
|------|-------|
| `bbnf_grammar_roundtrip` | `Rule value_expr not found in expressions.bbnf (imported from bbnf.bbnf)` plus 3 sibling import failures |
| `css_l4_grammar_roundtrip` | `assertion left == right failed: rule count drifted; left: 0, right: 184` |
| `css_pretty_grammar_roundtrip` | `@pretty targets undefined rule stylesheet` |
| `ebnf_grammar_roundtrip` | `@pretty targets undefined rule alternation` |
| `google_sheets_grammar_roundtrip` | `@pretty targets undefined rule func_args` |
| `json_grammar_roundtrip` | `@pretty targets undefined rule object` |

Common cause: every directive (`@import`, `@pretty`, `@recover`, `@token`,
`@debug`, `@ws`, `@host`) and every rule definition fails to extract from the
bootstrap parse tape under the regenerated `generated.rs`.

Both tries fail:

- Typed extraction (`cst_directives::try_as_pretty_directive(cursor, input)`)
  returns `None` because the `cursor.variant_idx()` it sees on the directive
  compound is `0..6` (the `__directive` Alt's branch index), not `45u8` (the
  schema-emitter's value for `pretty_directive`).
- Span-text fallback (`absorb_pretty_by_text`) is never reached because
  `host.rs::absorb_item` only invokes it from inside the `match
  item.rule_kind() { ... _ => {} }` arm AFTER the `match` returns; but
  `rule_kind()` interprets the same out-of-band variant_idx 0..6 as
  `int_lit` / `float_lit` / `bool_lit` / etc., still no match — and the
  outer iteration loop already stripped the directive's `grammar_item`
  wrapper, so the keyword-prefix probe runs against `item.span_text()`
  which is the full directive text. Inspection of host.rs shows the
  fallback IS reachable; the failing scenarios are tests run in cargo
  test which compile each grammar through the proc-macro derive, NOT
  through `parse(source)` — and that path constructs the `directive`
  compound correctly but fails on the deeper `__rule` extraction
  (rule count = 0 in css_l4) AND on `@pretty` rule-name lookup.

The systemic root cause is single: **`variant_idx` is overloaded**. For
non-Alt-bodied rules the codegen stamps `rule.id as u8` (so `rule_kind()`
maps it to the named rule). For Alt-bodied rules under Tranche AK.1 the
codegen stamps `__branch_idx` (so a downstream `rule_kind()` lookup
returns whatever rule happens to share that branch index globally —
typically a leaf terminal). Every consumer that walks by `rule_kind()`
or by `cursor.variant_idx() == known_rule_idx` mis-reads the regen.

Function-count diff (working vs regen):

```
working: 70 __* parser functions (one per surviving rule plus _prettify variants)
regen:   106 __* parser functions
```

The regen has standalone `__import_path`, `__import_items`,
`__recover_directive`, `__pretty_directive`, `__ws_directive`,
`__token_directive`, `__debug_directive`, `__host_directive`,
`__identifier`, `__literal`, `__regex`, `__lhs`, `__term`, `__factor`,
`__concatenation`, `__pretty_hint`, `__type_annotation`, `__type_name`
etc. — 36 sub-rule functions absent from the working version.

But these standalone functions are dead code: the parser path inlines
their bodies at every call site. `Self::__pretty_directive` is defined
but only `Self::__pretty_directive_prettify` is ever called. The Alt
inside `__directive` directly emits the `@pretty` keyword match plus the
identifier scan plus the hint-list `Repeat`, then stamps
`__branch_idx = 2u8` on the parent compound. So the regen ALSO inlines
sub-rules — it just gives the dead-callable body its own real
variant_idx (45u8) too, while the parent stamps the alt branch idx
unrelated to the schema's expectation.

## Why inlining happens

The CSP in `crates/core/src/backend/rust/analysis/inline.rs` decides each
rule's `CallMode ∈ {DirectCall, InlineBody}`. Priority order in
`analyze_parse_inline_plan` (lines 369-415):

1. `single_site_inline[idx]` — forced `InlineBody`. Only triggers for
   rules with `meta.is_cyclic && id != 0 && ref_counts == 1 && no_recover
   && no_pretty`. For `import_path` (acyclic, ref_count=1), this is
   `false`.
2. `operator_chain_rules.contains(rule.id)` — forced `DirectCall`.
3. `rule.meta.directives.token` — forced `InlineBody`.
4. `is_cyclic || recover.is_some() || pretty.is_some()` — forced
   `DirectCall`.
5. **`CostBudgetConstraint`** decides the rest. For a rule like
   `import_path = "\"" , /(\\.|[^"\\])*/ , "\""` (3-node Seq, ref_count=1):
   - `local_cost ≈ leaf_cost*3 + alt_branch_overhead = ~7-8`
   - `total_budget = local_cost * ref_count = ~7-8`
   - At default weights: `max_local_cost=80`, `max_total_budget=4096`,
     all shape guards pass with refs=0. `CostBudgetConstraint` falls
     through to the `InlineBody` arm.

`preserve_identity` is **not consulted anywhere** in
`analyze_parse_inline_plan` — neither in the priority guards nor inside
`CostBudgetConstraint`. The call mode is decided purely on cost and shape.

The orthogonal IR-level passes that DO consult `preserve_identity` are:

- `passes/transform/prune.rs:30` — preserve_identity rules act as DFS
  roots, never pruned.
- `passes/transform/inline.rs:31` — preserve_identity rules are NOT
  inlined into other rules' bodies.
- `passes/transform/fuse.rs:49` — preserve_identity rules are NOT fused
  into single-use callers.
- `passes/materialization/{pin_sweep,classify}.rs` — preserve_identity
  rules are pinned to `MustTape` materialization.
- `passes/csp_strategy/mod.rs:1037` — preserve_identity rules are pinned
  in the strategy solver.
- `crates/core/src/backend/rust/emitter/grammar.rs:114` — preserve_identity
  rules always materialize as `MustTape` (push_compound).
- `crates/core/src/backend/driver/mod.rs:293` — preserve_identity rules
  are excluded from inlining at the driver level.

So `preserve_identity` controls whether the rule SURVIVES the IR passes
and emits a callable function (and what materialization class it gets).
It does NOT control whether OTHER rules' bodies inline this rule's body
at the call site under `InlineBody` mode. The latter is the inline plan
CSP's job, and that CSP ignores `preserve_identity` entirely.

The AQ.6.A session attempted to gate the CSP on `preserve_identity` and
hit CSS L4 regressions; the change was reverted. The reversion is
recorded in `docs/benchmarks/post-AQ.json` as: *"generated.rs still
hand-maintained. Bootstrap regen path produces a divergent generated.rs
because the codegen inlines single-use sub-rules even under
preserve_identity."*

## Five architectural paths

### Path 1 — CSP gates on preserve_identity

Re-add the AQ.6.A change: add a sixth `ForceCallMode::DirectCall`
priority before the cost-budget constraint, gated on
`rule.meta.preserve_identity`.

- **Elegance**: medium. Conceptually clean — preserve_identity already
  pins materialization and survives all the cross-rule passes; extending
  the same gate to caller-side inlining is consistent.
- **Performance**: bad. Every directive sub-rule (`import_path`,
  `recover_directive`, etc.) becomes a function call instead of an inline
  body. CSS L4's `__compoundSelector` (40% self-time) and JSON's
  `__pair` (28% self-time) suffer if preserve_identity ever leaks onto
  hot rules — but in production builds preserve_identity is set ONLY in
  structural mode (`pipeline/compile.rs:488`), and structural mode is
  used ONLY for the bootstrap. So performance impact is bounded to the
  bootstrap regen itself.
- **Engineering cost**: ~10 lines. One new `ForceCallMode` constraint.
- **Risk**: low. The AQ.6.A regression on CSS L4 was likely caused by
  preserve_identity leaking outside structural mode, not by the gate
  itself. Worth re-attempting with a tighter gate (`structural &&
  preserve_identity`).

### Path 2 — Migrate consumers to typed view accessors

Eliminate every `rule_kind()`-driven branch in `host.rs` /
`lower/{expression, value_expr, mod, tape_walk}.rs` /
`graph/{deps, metadata}.rs` / `pipeline/compile.rs`. Replace with the
schema-emitted `try_as_<rule>_directive(cursor, input)` helpers.

- **Elegance**: high — but FATALLY broken by the same root cause. The
  typed accessors check `cursor.variant_idx() == rule_id_for(rule_name)`,
  and the regen stamps `__branch_idx` on Alt-bodied parent compounds.
  So `try_as_pretty_directive` returns `None` for the SAME reason the
  current host.rs typed dispatch fails.
- **Performance**: neutral.
- **Engineering cost**: ~2-3 days to migrate every consumer.
- **Risk**: doesn't actually solve the problem. Path 2 is INELIGIBLE on
  its own — it must be combined with a fix to the variant_idx-overloading
  issue (e.g., emit per-Alt-branch `try_as_*` helpers that consult both
  the parent `directive` rule_idx AND the branch idx).

### Path 3 — Tape recovery descriptors for inlined sub-rules

When the codegen inlines a sub-rule body, also emit a
`push_marker(rule_id)` instruction in the tape that downstream consumers
can scan past. Consumers walk the tape, recognize the marker, and treat
the next compound as the inlined sub-rule.

- **Elegance**: low. Adds a new tape opcode and complicates every walker.
- **Performance**: bad. Extra tape writes on every inlined body.
- **Engineering cost**: ~1 week (tape format change, all walkers
  updated, view layer extended).
- **Risk**: high. Tape format change ripples to bbnf-tape, every view
  emitter, every consumer.

### Path 4 — Eliminate ParsedGrammar; lower bootstrap CST directly to IR

Delete `crates/core/src/grammar/host.rs` (extract_grammar) AND
`crates/core/src/types.rs::ParsedGrammar`. The bbnf-bootstrap proc-macro
lowers directly into `bbnf_ir::GrammarIR`. Downstream consumers consume
IR, never the bootstrap CST.

`ParsedGrammar` carries:

| Field | IR equivalent |
|-------|---------------|
| `imports: Vec<ImportDirective>` | NEW — IR doesn't carry `@import`. Currently resolved at lowering time by walking `ParsedGrammar.imports` and recursively loading. |
| `recovers: Vec<RecoverDirective>` | `RuleMeta.directives.recover: Option<Recover>` — already in IR, but the `sync_expr` is a `BbnfBootstrapNodeView` reference, not an IR sub-tree. |
| `pretties: Vec<PrettyDirective>` | `RuleMeta.directives.pretty: Option<PrettyHints>` — already in IR. |
| `rules: AST<'a> = IndexMap<&str, RuleEntry>` | `IR.rules: Vec<IrRule>` — exists but the `RuleEntry.rhs` is a `BbnfBootstrapNodeView`, not IR yet. |
| `ws_pattern: Option<Cow<str>>` | `IR.ws_pattern: Option<StringId>` — already in IR. |
| `debug_rules: Vec<Cow<str>>` | `RuleMeta.directives.debug: bool` + `IR.debug_all: bool` — already in IR. |
| `token_rules: Vec<Cow<str>>` | `RuleMeta.directives.token: bool` — already in IR. |
| `host_fns: Vec<HostFnDecl>` | `IR.fn_table: FnTable` — already in IR (host functions stored as `FnDescriptor`s with names). |

What's blocking a direct `CST → IR` lowering:

1. **Imports** — must be resolved BEFORE rules are lowered (because rules
   reference imported nonterminals). The current pipeline does this via
   `crates/core/src/imports/loader.rs::load_module_graph`, which takes
   each `ParsedGrammar`'s imports and recursively loads dependent
   `ParsedGrammar`s. To direct-lower into IR, the loader has to operate
   on IR module fragments instead — feasible but requires a new
   `IrModule` type with `ir_module.imports: Vec<ImportSpec>`.
2. **Sync expressions for `@recover`** — the recover directive carries a
   `sync_expr: BbnfBootstrapNodeView` that's lowered LATER alongside its
   target rule's body. Direct lowering must produce an `IrNode` for the
   sync expression at the same time as everything else.
3. **Closures** — the closure rules are partitioned out of the AST in
   `pipeline/compile.rs::partition_closures` BEFORE lowering and held
   separately. Direct lowering must surface closures as a parallel
   `IrModule.closures: Vec<IrClosure>` field.
4. **All the structural CST helpers** in `tape_walk.rs`,
   `lower/expression.rs`, `lower/value_expr.rs` — they also walk by
   `rule_kind()` and `child(N)`. Same root-cause problem as
   `host.rs`. So Path 4 doesn't help unless the variant_idx overloading
   is also fixed.

- **Elegance**: high. Eliminates an entire intermediate format and
  ~600 lines of `host.rs` plus the `ParsedGrammar` type plus the
  `BbnfBootstrapNodeView`-borrowing AST IndexMap. Single source of
  truth.
- **Performance**: positive. Avoids the CST→AST→IR double-lowering on
  every grammar compile. Bootstrap regen succeeds because the
  bootstrap proc-macro doesn't NEED a tape walker — it operates on
  `IrNode`s.
- **Engineering cost**: large. ~2 weeks. Touches imports, lowering,
  closures, sub-variant collection, the entire `crates/core/src/lower/`
  tree, and the `bbnf-bootstrap` proc-macro itself.
- **Risk**: high during the migration window. Once landed, ALL tape-walk
  consumers go away with `host.rs` — fewer surfaces to break.

### Path 5 — Span-text parsing for ALL structural content

Extend the span-text fallback in `host.rs` to handle EVERY directive +
rule definition without typed dispatch. Re-parse each compound's source
slice with a tiny scanner.

- **Elegance**: low. Re-parsing the same bytes the bootstrap already
  parsed. Doubles the work.
- **Performance**: bad. ~2× the bootstrap parse time.
- **Engineering cost**: ~3 days. Move all `absorb_*_by_text` patterns
  into a complete fallback set; delete the typed accessor calls.
- **Risk**: low — the `absorb_*_by_text` functions exist and work; the
  question is whether they cover RULE bodies, not just directives.
  They do not — `absorb_item` falls through to `_ => {}` for non-rule,
  non-directive content. Extending the fallback to RULE bodies means
  re-implementing `lower/expression.rs` as a span-text parser, which
  is just rewriting the bootstrap parser by hand.

### Path 6 (synthesis) — fix variant_idx overloading + Path 1 gate

The actual root cause of ALL paths failing is that variant_idx is
overloaded. Fix this directly:

- For Alt-bodied parent rules, the codegen currently emits ONE
  `push_compound` with `__branch_idx`. Instead, emit TWO bytes worth of
  identification: the parent rule's id AND the alt branch idx.
  `TapeRec.flags` is currently a u8 `__branch_idx`; widen to u16 with
  `(rule_id_low_byte << 8) | branch_idx`. Or use a dedicated
  `meta_idx: u8` field alongside `variant_idx: u8`.
- `rule_kind()` then reads the rule_id half; `try_as_<sub>_directive`
  reads the branch idx half AFTER confirming the parent rule_id matches.
- Combined with Path 1's gate on `preserve_identity`, the bootstrap regen
  produces a tape that the typed extractors can decode AND the
  rule-by-rule dispatch in host.rs / lower works again.

- **Elegance**: highest. Solves the structural mismatch. Both the
  typed and dispatched paths have a unique unambiguous source of
  identification.
- **Performance**: minimal. One extra byte per Alt-bodied compound
  (cache-line-bounded — TapeRec is already padded).
- **Engineering cost**: medium. ~3-4 days. Touches `bbnf-tape`
  (TapeRec), the rust emitter (push_compound calls), the schema
  emitter (try_as accessors), and host.rs / lower walkers.
- **Risk**: medium. Tape format change is ripply but well-bounded.

## Root cause: why does host.rs exist at all

`host.rs::extract_grammar` exists because the bootstrap proc-macro
(`bbnf-bootstrap`) emits a TAPE-based parser. The bootstrap parses
`bbnf.bbnf` into a flat `Tape` of `TapeRec` entries, then a second pass
walks the tape and constructs `ParsedGrammar` by interpreting compound
records as rules / directives.

The two-stage design dates to AC.2 when `BbnfBootstrapEnum<'a>` was
deleted in favor of the tape format. Pre-AC.2 the bootstrap returned an
owned enum tree that downstream code pattern-matched directly. AC.2
replaced that with a tape + view layer to get tape-first parsing
performance — but the downstream code (host.rs, lower/) still walks the
view as if it were a typed enum.

The `ParsedGrammar` IndexMap of `&'a str → BbnfBootstrapNodeView<'a>`
is essentially a "lazy AST view" — every rule's RHS is a borrowed
view into the bootstrap tape. The full IR lowering in `lower/expression`
then consumes those views in `lower_to_ir`. So the data flow is:

```
bbnf.bbnf source
  → bootstrap-bbnf proc-macro
  → BbnfBootstrap::parse(source) → Parsed<BbnfBootstrap> (tape + input)
  → host::extract_grammar → ParsedGrammar (rules: name → tape view, directives)
  → pipeline/compile::compile_paths_request
    → calculate_ast_deps + tarjan_scc + topological_sort_scc (over tape views)
    → lower_to_ir(ast, scc, directives, closures) → GrammarIR
      → lower_rhs(view, ctx) walks each tape view producing IrNode
    → 16 IR passes
    → backend codegen
```

Every consumer between `BbnfBootstrap::parse` and `lower_to_ir` walks
the bootstrap tape via `rule_kind()` / `child(N)` / `children()`. The
tape's variant_idx overloading breaks all of them under the regen.

If the bootstrap proc-macro lowered directly into IR — either by
emitting IrNode-construction code instead of tape-construction code,
or by adding a `bootstrap_to_ir` helper that the proc-macro invokes —
ALL of host.rs / tape_walk.rs / the structural parts of
lower/expression.rs / graph/deps.rs go away. The bootstrap then has
one job (produce IR), and the bbnf-derive pipeline starts from IR
without an intermediate AST.

## Recommended path for AR

**Path 6 + Path 4**, in two milestones.

Path 6 first (fix variant_idx overloading) is a tight, focused change
that immediately closes the regen failure. Path 4 (eliminate
ParsedGrammar) is a larger structural payoff that becomes a clean
single-step refactor once Path 6 has decoupled the consumers from the
overloaded variant_idx.

Path 1 (CSP gate on preserve_identity) alone IS NOT recommended — it
would let the regen produce a structurally-valid tape but at the cost
of bootstrap-only function-call overhead per directive sub-rule, and
it doesn't address the underlying variant_idx mismatch that future
codegen optimizations could re-introduce.

Path 5 (extend span-text fallback) is REJECTED — re-parsing source
twice violates the "ONE codegen path" tenet and the `no-workarounds`
edict.

## Concrete milestones

### M1 — Resolve variant_idx overloading (Path 6)

Fix the structural mismatch. Two-byte rule identification on every
compound.

1. **Tape format change** (`crates/bbnf-tape/src/tape.rs`,
   `crates/bbnf-tape/src/builder.rs`): add `meta_idx: u8` to
   `TapeRec`. `variant_idx` continues to mean rule_id_low_byte;
   `meta_idx` carries the alt branch idx for Alt-bodied parents
   (zero for Seq-bodied or leaf rules).
2. **Codegen change** (`crates/core/src/backend/rust/emitter/grammar.rs`,
   `emitter/alt.rs`): `push_compound` for Alt-bodied rules takes
   `(rule.id as u8, __branch_idx)`. `push_compound` for Seq-bodied or
   leaf rules takes `(rule.id as u8, 0)`.
3. **Schema emitter** (`crates/core/src/grammar/schema/emit/rust/`):
   `try_as_<rule>_directive` checks `cursor.variant_idx() ==
   rule_id_for("directive") && cursor.meta_idx() == branch_idx_for(rule_name)`.
4. **View `rule_kind()`**: reads only `variant_idx`; correctly maps every
   rule (no false match for inlined sub-variants).
5. **Bootstrap regen + grammar_roundtrip**: regen + un-ignore +
   verify zero-diff except for the AC.2-era hand patches (which become
   obsolete).

### M2 — Add CSP `preserve_identity` gate (Path 1)

Even with M1, inline plan still inlines bodies of `preserve_identity`
sub-rules into their parents. This is fine for tape-shape correctness
under M1, but for tooling / DAP / introspection the standalone callable
should remain reachable. Add a sixth priority in
`analyze_parse_inline_plan`:

```rust
// Priority 0 (highest): preserve_identity → forced DirectCall.
if rule.meta.preserve_identity {
    csp.add_constraint(ForceCallMode::new(var, CallMode::DirectCall));
    continue;
}
```

Bench gate: no regression on JSON / CSS L4 (which never set
`preserve_identity` on hot rules — it's a structural-mode-only flag).

### M3 — Eliminate ParsedGrammar; direct CST → IR lowering (Path 4)

After M1+M2 land and stabilize:

1. Define `IrModule { imports: Vec<ImportSpec>, ir: GrammarIR,
   closures: Vec<IrClosure> }`.
2. Move `crates/core/src/lower/` from `BbnfBootstrapNodeView`-based
   inputs to direct `IrNode` construction inside the `bbnf-bootstrap`
   proc-macro. The proc-macro lowers each rule body during macro
   expansion using the bbnf grammar's static knowledge of its own
   structure (this is self-hosting in the strict sense).
3. Move imports loader (`crates/core/src/imports/`) to consume
   `IrModule` not `ParsedGrammar`.
4. Delete `crates/core/src/grammar/host.rs` (~600 LOC).
5. Delete `crates/core/src/types.rs::ParsedGrammar` and `RuleEntry` and
   the `BbnfBootstrapNodeView`-borrowing types.
6. Delete `crates/core/src/lower/tape_walk.rs` (~120 LOC).

Success criterion at M1+M2: `cargo test --test grammar_roundtrip`
passes without `#[ignore]`; `generated.rs` regenerates from
`./scripts/bootstrap-bbnf.sh` and produces a file the workspace builds
+ tests pass against, with zero hand patches.

Success criterion at M3: `crates/core/src/grammar/host.rs` and
`crates/core/src/types.rs::ParsedGrammar` no longer exist; bootstrap
regen still produces a working file; bbnf-derive consumes IR directly
from the proc-macro.
