# AZ-I.W2 — Emitter Rewire — File-Level Redress Plan

W2.A landed the StructBuilder substrate (`crates/core/src/runtime/builder.rs`,
`crates/core/src/runtime/json/`, `crates/core/tests/json_parity_struct.rs`)
and halted on scope-reveal: the per-shape emitter rewire crosses
~3,000 LOC across 6+ emitter files and ~200 emission sites, exceeding
a single 90-min implementation dispatch. This plan decomposes the
deferred rewire into five redress agents on disjoint file bounds.

## 1. Architectural decision — per-grammar codegen-time dispatch

The emitter selects the substrate per grammar identifier; the
generated parse fn takes a concrete `&mut JsonStructBuilder<'_>`
(struct-direct grammars) or `&mut Tape<()>` (tape grammars). No
generics on emitted code, no polymorphic trait objects, no fallback
branch within a grammar. The trait `StructBuilder` defined at
`crates/core/src/runtime/builder.rs` remains as an *implementation*
contract for concrete builders to share scalar / compound semantics;
it does not introduce runtime polymorphism on the emission path.

Justification anchors:

- `feedback_no-orthogonal-codepaths` + AZ-I.md §Invariant 2 demand
  ONE codegen path per grammar with no struct-or-tape conditional in
  the emitter. A trait-on-the-emission-path approach (Tape
  implementing StructBuilder, or a `BuilderTarget` super-trait
  generic) leaves runtime fallback machinery in place. Codegen-time
  dispatch collapses the conditional.
- `crates/tape/src/tape/push.rs:681` — `Tape::push_leaf_with_f64_direct`
  carries a `TapeKind` argument that no StructBuilder method models.
  Force-fitting Tape into the StructBuilder trait either drops
  `TapeKind` (BBNF and CSS L4 lose dispatch information) or extends
  the trait with tape-specific args (orthogonal-codepath violation).
- The `Wrap` open-frame variant in
  `crates/core/src/runtime/json/builder.rs:80` already encodes the
  Alt-over-Refs branch-tag-then-recurse semantics; the per-grammar
  emitter just routes to the existing JsonStructBuilder API.

Operative dispatch surface (introduced by Redress A):

```rust
pub enum EmitStrategy {
    /// Generated parse fn writes to a grammar-specific struct builder.
    /// `builder_path` is the fully-qualified type path the emitted
    /// code instantiates (e.g. "::bbnf::runtime::json::JsonStructBuilder").
    StructDirect { builder_path: &'static str },
    /// Generated parse fn writes to the tape substrate.
    TapeDirect,
}

impl EmitStrategy {
    pub fn for_grammar(ident: &str, registry: &StructRegistry) -> Self { ... }
}
```

`for_grammar` resolves `JsonGrammar` to `StructDirect` (W2 activation
target); every other grammar resolves to `TapeDirect`. W2.B extends
the resolver to map `GoogleSheetsGrammar` to `StructDirect`; W3
extends it for CSS L4. The discriminator is grammar-identity data,
not a hard-coded match-arm proliferation through every shape emitter.

Per-shape emitters take an `&EmitStrategy` parameter and emit one
body per strategy variant — no fallback, no merged "if struct then
else tape" arm inside a single emitted parse fn.

## 2. File-level decomposition

`crates/core/src/backend/rust/emitter/shapes/` LOC and emission-site
counts (in-emitter `tape.push_*` / `Tape::*` call sites; the lines
whose `quote! { ... tape.push_xxx(...) }` token streams are rewritten
to `quote! { ... builder.method(...) }` under struct-direct mode):

| File | LOC | Sites | Owner |
|---|---:|---:|---|
| `shapes/mod.rs` | 328 | 2 | A |
| `shapes/object.rs` | 647 | 32 | B |
| `shapes/array/mod.rs` | 117 | 1 | B |
| `shapes/array/element.rs` | 238 | 1 | B |
| `shapes/array/list.rs` | 359 | 8 | B |
| `shapes/array/visitor.rs` | 114 | 0 | B |
| `shapes/array/wrapped.rs` | 336 | 23 | B |
| `shapes/number.rs` | 545 | 2 | C |
| `shapes/string.rs` | 429 | 3 | C |
| `shapes/scalar.rs` | 236 | 1 | C |
| `shapes/keyword/mod.rs` | 500 | 7 | D |
| `shapes/keyword/payload.rs` | 159 | 2 | D |
| `shapes/keyword/visitor.rs` | 252 | 0 | D |
| `shapes/wrap/mod.rs` | 345 | 2 | D |
| `shapes/wrap/tape_dispatch.rs` | 439 | 13 | D |
| `shapes/wrap/visitor.rs` | 150 | 0 | D |
| `shapes/pratt/mod.rs` | 80 | 3 | E |
| `shapes/pratt/tape.rs` | 517 | 18 | E |
| `shapes/pratt/visitor.rs` | 271 | 0 | E |
| `shapes/flat/mod.rs` | 312 | 6 | E |
| `shapes/flat/tape.rs` | 507 | 19 | E |
| `shapes/flat/typed_payload.rs` | 294 | 7 | E |
| `shapes/flat/map_regex_host.rs` | 166 | 3 | E |
| `shapes/flat/visitor.rs` | 190 | 0 | E |
| `shapes/arglist.rs` | 751 | 15 | E |
| `shapes/unordered.rs` | 594 | 9 | E |
| `shapes/hregex.rs` | 726 | 7 | E |
| `shapes/inline/alt.rs` | 612 | 6 | E |
| `shapes/inline/{guard,regex,structural_branch,token_dispatch,branch_analysis,mod}.rs` | 1217 | 7 | E |
| `shapes/alt_dispatch/{branches,mod,payload,visitor}.rs` | 1111 | 8 | B |
| `shapes/dispatcher/{cross_shape,mod,ref_call,scan_policy,support,symbol_composition}.rs` | 2070 | 0 | A |
| `shapes/value_materialize.rs` | 431 | 1 | A |
| `shapes/registry_observer.rs` | 84 | 0 | A |
| `emitter/grammar.rs` (parse_body, lines 1060–1166) | 1293 | 0 | A |
| **Total** | ~15,127 | ~207 | |

W2 severs tape on JSON only (Sheets is W2.B; CSS L4 is W3). Redress
agents are scoped to JSON activation; per-shape emitters in B–E
switch on the strategy uniformly: any per-shape body marked
struct-direct emits builder calls; otherwise emits tape calls.
`for_grammar` is the single decision surface.

## 3. Parallel redress agent decomposition

Five redress agents on disjoint file bounds. Agent A is sequential
first stage; B / C / D / E run in 4-way parallel after A lands.

### Redress A — Strategy substrate + dispatch + parse_body

**Cap**: 60 min (SPEC §Caps redress default 30 min + 30 min substrate
justification: this agent threads a new emit strategy through
`mod.rs`, `dispatcher/`, `value_materialize.rs`, and
`grammar.rs:parse_body`, blocking B / C / D / E. Substrate requires
same-commit consumer wiring per AX invariant 13.)

**Allow-list:**

- `crates/core/src/backend/rust/emitter/shapes/mod.rs`
- `crates/core/src/backend/rust/emitter/shapes/dispatcher/cross_shape.rs`
- `crates/core/src/backend/rust/emitter/shapes/dispatcher/mod.rs`
- `crates/core/src/backend/rust/emitter/shapes/dispatcher/ref_call.rs`
- `crates/core/src/backend/rust/emitter/shapes/dispatcher/scan_policy.rs`
- `crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs`
- `crates/core/src/backend/rust/emitter/shapes/dispatcher/symbol_composition.rs`
- `crates/core/src/backend/rust/emitter/shapes/value_materialize.rs`
- `crates/core/src/backend/rust/emitter/shapes/registry_observer.rs`
- `crates/core/src/backend/rust/emitter/grammar.rs`
- `crates/core/src/backend/rust/emitter/strategy.rs` (new file; declares `EmitStrategy` enum + `for_grammar` resolver)
- `crates/core/src/backend/rust/emitter/mod.rs` (re-exports only)
- `crates/core/src/pipeline/compile.rs` (per-grammar dispatch hook; selects struct-direct path for JSON when the registry is populated for the grammar root)
- `crates/core/tests/emit_strategy.rs` (new; leaf test)
- `crates/core/tests/struct_direct_snapshots.rs` (new; per-shape `cargo expand`-style snapshot test driver — agents B / C / D contribute their per-shape `.snap` artefacts via the `insta` crate or equivalent file-based snapshot pattern; A owns the driver to keep file ownership disjoint)

**Forbidden:**

- Any per-shape file (B / C / D / E own those).
- Any `crates/ir/` file (registry is W1 substrate; A consumes only).
- `crates/tape/` (tape crate is unchanged in W2 per AZ-I.md §Invariant 5).
- `crates/core/src/runtime/json/` (W2.A substrate is closed).
- `crates/core/src/grammar/generated/**` (orchestrator-owned regen window).

**Hard gates:**

1. `EmitStrategy` enum exists with at minimum the `StructDirect { builder_path }` and `TapeDirect` variants; `for_grammar` resolves JSON to `StructDirect` and every other grammar to `TapeDirect`.
2. `emit_grammar_impl`'s `parse_body` arm has *two* disjoint code paths keyed on `EmitStrategy`: the existing tape body for `TapeDirect`, and a struct-direct body for `StructDirect` that allocates `JsonStructBuilder::new()`, threads it through the dispatcher, and finalises via `builder.finalise()` to a `JsonDocument`. No conditional within a single body.
3. `cargo check --profile ax-iter -p bbnf-core` passes on the worktree.
4. `cargo nextest run --profile ax-iter -p bbnf-ir` ≥ 375.
5. `cargo nextest run --profile ax-iter -p bbnf-core --test emit_strategy` ≥ 1 test passes; the strategy resolver rejects unpopulated grammar registries (`for_grammar("BbnfGrammar", &empty_registry) == TapeDirect`).

### Redress B — Object / Array / AltDispatch

**Cap**: 30 min.

**Allow-list:**

- `crates/core/src/backend/rust/emitter/shapes/object.rs`
- `crates/core/src/backend/rust/emitter/shapes/array/mod.rs`
- `crates/core/src/backend/rust/emitter/shapes/array/element.rs`
- `crates/core/src/backend/rust/emitter/shapes/array/list.rs`
- `crates/core/src/backend/rust/emitter/shapes/array/visitor.rs`
- `crates/core/src/backend/rust/emitter/shapes/array/wrapped.rs`
- `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs`
- `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/mod.rs`
- `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/payload.rs`
- `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/visitor.rs`
- `crates/core/tests/struct_direct_snapshots/object.snap` and `array.snap` (B contributes per-shape snapshot artefacts)

**Forbidden:** A's allow-list, C's, D's, E's. No edits to dispatcher, mod, grammar.rs.

**Hard gates:**

1. Every site that emits `tape.push_compound_pre_order` / `tape.end_compound` / `tape.push_branch_tag` (object body, array body, paired-Alt branch close) emits *both* a tape body (for `TapeDirect`) and a struct-direct body (for `StructDirect`) — keyed on the `EmitStrategy` threaded by A. The struct-direct body calls `builder.begin_compound(&__layout)` / `builder.end_compound(handle)` / `builder.push_branch_tag(idx)`.
2. The struct-direct body resolves `__layout` from the rule's `StructLayout` already bound by `mod.rs:196` (`layout = ir.struct_registry.layout(rule.id)`). Per `feedback_no-orthogonal-codepaths` no fallback when the layout is missing — the strategy must guarantee the layout exists for the StructDirect case (gated by `for_grammar`'s resolver, A's responsibility).
3. `cargo check --profile ax-iter -p bbnf-core` passes.
4. `cargo nextest run --profile ax-iter -p bbnf-core --test struct_direct_snapshots` confirms one Object-shape rule and one Array-shape rule emit `builder.begin_compound` and contain *zero* `tape.push` calls in their StructDirect snapshots.

### Redress C — Number / String / Scalar

**Cap**: 30 min.

**Allow-list:**

- `crates/core/src/backend/rust/emitter/shapes/number.rs`
- `crates/core/src/backend/rust/emitter/shapes/string.rs`
- `crates/core/src/backend/rust/emitter/shapes/scalar.rs`
- `crates/core/tests/struct_direct_snapshots/{number,string,scalar}.snap`

**Forbidden:** A / B / D / E allow-lists.

**Hard gates:**

1. Number-shape emitter, on `StructDirect`, emits `builder.push_leaf_with_f64(value)` instead of `tape.push_leaf_with_f64_direct(TapeKind::Span, ...)`. Strict number (twitter / canada / citm) routes through the existing Eisel-Lemire body unchanged; only the *consumer* of the parsed `f64` differs.
2. String-shape emitter, on `StructDirect`, emits `builder.push_leaf_with_str(slice)`. The slice is the arena-decoded `&'p str` produced by the existing `decode_string_to_arena` body, threaded through `JsonArena` (already on `JsonStructBuilder` per `crates/core/src/runtime/json/builder.rs:51`). On `TapeDirect` the existing `push_leaf_with_arena_payload` path is preserved.
3. Scalar-shape emitter (`scalar.rs:81` `TapeKind::Literal`), on `StructDirect`, emits `builder.push_leaf_with_unit()` for null markers and `builder.push_leaf_with_bool(value)` for `true` / `false` keywords (JSON's only literal-Scalar consumers).
4. `cargo check --profile ax-iter -p bbnf-core` passes.

### Redress D — Keyword / Wrap

**Cap**: 30 min.

**Allow-list:**

- `crates/core/src/backend/rust/emitter/shapes/keyword/mod.rs`
- `crates/core/src/backend/rust/emitter/shapes/keyword/payload.rs`
- `crates/core/src/backend/rust/emitter/shapes/keyword/visitor.rs`
- `crates/core/src/backend/rust/emitter/shapes/wrap/mod.rs`
- `crates/core/src/backend/rust/emitter/shapes/wrap/tape_dispatch.rs`
- `crates/core/src/backend/rust/emitter/shapes/wrap/visitor.rs`
- `crates/core/tests/struct_direct_snapshots/{keyword,wrap}.snap`

**Forbidden:** A / B / C / E.

**Hard gates:**

1. Keyword-shape emitter, on `StructDirect`, emits the keyword's payload-projection through `builder.push_leaf_with_*` (bool for JSON `true` / `false`; unit for JSON `null`).
2. Wrap-shape emitter (transparent `Alt(Ref, Ref, …)` — JSON `value`), on `StructDirect`, emits `builder.begin_compound(&__wrap_layout)` + `builder.push_branch_tag(idx)` + `builder.end_compound(handle)` matching `JsonStructBuilder::OpenFrame::Wrap` (`crates/core/src/runtime/json/builder.rs:80`).
3. `cargo check --profile ax-iter -p bbnf-core` passes.

### Redress E — Pratt / Flat / ArgList / Unordered / HRegex / Inline

**Cap**: 45 min (SPEC default 30 min + 15 min: largest emitter cluster — 5,438 LOC across 7 sub-modules — and the only cluster that handles paired-Alt branch payloads + post-order compound close.)

**Allow-list:**

- `crates/core/src/backend/rust/emitter/shapes/pratt/mod.rs`
- `crates/core/src/backend/rust/emitter/shapes/pratt/tape.rs`
- `crates/core/src/backend/rust/emitter/shapes/pratt/visitor.rs`
- `crates/core/src/backend/rust/emitter/shapes/flat/mod.rs`
- `crates/core/src/backend/rust/emitter/shapes/flat/tape.rs`
- `crates/core/src/backend/rust/emitter/shapes/flat/typed_payload.rs`
- `crates/core/src/backend/rust/emitter/shapes/flat/map_regex_host.rs`
- `crates/core/src/backend/rust/emitter/shapes/flat/visitor.rs`
- `crates/core/src/backend/rust/emitter/shapes/arglist.rs`
- `crates/core/src/backend/rust/emitter/shapes/unordered.rs`
- `crates/core/src/backend/rust/emitter/shapes/hregex.rs`
- `crates/core/src/backend/rust/emitter/shapes/inline/alt.rs`
- `crates/core/src/backend/rust/emitter/shapes/inline/branch_analysis.rs`
- `crates/core/src/backend/rust/emitter/shapes/inline/guard.rs`
- `crates/core/src/backend/rust/emitter/shapes/inline/mod.rs`
- `crates/core/src/backend/rust/emitter/shapes/inline/regex.rs`
- `crates/core/src/backend/rust/emitter/shapes/inline/structural_branch.rs`
- `crates/core/src/backend/rust/emitter/shapes/inline/token_dispatch.rs`

**Forbidden:** A / B / C / D.

**Hard gates:**

1. JSON does not exercise Pratt / Unordered / ArgList / Flat / HRegex shapes (per JSON's grammar surface); on `StructDirect` for JSON these emitters emit *only* the existing tape body when the strategy resolves to TapeDirect, and codegen-time `panic!()` if the strategy resolves to StructDirect for these shapes (firing only if A's `for_grammar` mistakenly routes a CSS / Sheets grammar to `StructDirect` in W2 — W3 lights them up). The panic is not a runtime fallback per `feedback_no-workarounds`; it is an unreachable assertion preventing silent codegen drift.
2. Inline-Alt emitter (`inline/alt.rs`) — paired-Alt branch payload emission already routes through the per-shape emitter for the matched branch, so on `StructDirect` it propagates the strategy unchanged to the per-branch call.
3. `cargo check --profile ax-iter -p bbnf-core` passes.

**Disjointness verification:** A's allow-list ∩ {B, C, D, E} = ∅; B ∩ C = ∅; B ∩ D = ∅; B ∩ E = ∅; C ∩ D = ∅; C ∩ E = ∅; D ∩ E = ∅. Confirmed by file-path enumeration in §2's owner column.

## 4. Per-agent dispatch hard gate summary

| Agent | Cap | Owns | Key gate |
|---|---:|---|---|
| A | 60 min | `mod.rs` + `dispatcher/` + `grammar.rs` + `strategy.rs` (new) + `pipeline/compile.rs` | `EmitStrategy::for_grammar` resolves; `parse_body` two-path; `cargo check` + ≥ 375 bbnf-ir tests |
| B | 30 min | `object` + `array/*` + `alt_dispatch/*` | Object / Array struct-direct snapshot; `cargo check` |
| C | 30 min | `number` + `string` + `scalar` | Leaf push routed to builder methods on StructDirect; `cargo check` |
| D | 30 min | `keyword/*` + `wrap/*` | Keyword payload + Wrap branch tag on StructDirect; `cargo check` |
| E | 45 min | `pratt/*` + `flat/*` + `arglist` + `unordered` + `hregex` + `inline/*` | TapeDirect-only on these shapes in W2; codegen-time assert on misrouted StructDirect; `cargo check` |

All five agents share a workspace gate post-merge:
`cargo nextest run --workspace --profile ax-iter` ≥ 1517 (W1-close
baseline per PROGRESS.md 2026-04-27 W1 close §Hard-gate ledger).

## 5. Integration sequencing

**Stage 1 (sequential):** Redress A runs solo. Master clean before
dispatch.

**Stage 2 (4-way parallel):** Redress B, C, D, E dispatched
simultaneously after A's commits cherry-pick to master. Each gets a
fresh worktree off master HEAD-post-A. Per-worktree
`CARGO_TARGET_DIR=$(pwd)/target.local` per
`feedback_single-cargo-per-target` (matching W1.B precedent in
PROGRESS.md).

**Stage 3 (orchestrator):**

1. Cherry-pick B / C / D / E commits onto master.
2. `cargo xtask regen --grammar json` to refresh
   `crates/core/src/grammar/generated/json.rs` against the new
   struct-direct emitter output.
3. `cargo nextest run --workspace --profile ax-iter` — verify ≥ 1517
   baseline preserved.
4. `cargo nextest run --profile ax-iter -p bbnf-core --test
   json_parity_struct` (W2.A wire-contract harness) — green.
5. JSON parity harness rewrite (sonic-rs / simdjson OnDemand /
   serde_json) — separate W2.A.parity sub-agent dispatch (out of
   W2-emitter-rewire scope; called out as carry).
6. Bench-gate verification (twitter ≥ 1967, canada ≥ 1231, citm
   ≥ 2438) — W2.A.bench sub-agent dispatch out of W2-emitter-rewire
   scope.
7. W2.B (Sheets) opens after W2-emitter-rewire close; reuses A's
   `EmitStrategy` substrate by adding `SheetsStructDirect { builder_path:
   "SheetsStructBuilder" }` once `SheetsStructBuilder` lands.

## 6. Risk register

**Risk 1 — Tape `TapeKind` argument has no StructBuilder analog.**
`tape.push_leaf_with_f64_direct(TapeKind::Span, ...)` carries
`TapeKind` for downstream cursor / view dispatch
(`crates/tape/src/tape/push.rs:681`). `StructBuilder::push_leaf_with_f64(value)`
does not. Rationale for omission: on the struct-direct path,
`TapeKind` is dead — the in-flight frame on
`JsonStructBuilder::stack` (`crates/core/src/runtime/json/builder.rs:60`)
already carries the typed shape (Array / Object / Pair / Wrap), and
the `JsonValue` enum's discriminant subsumes `TapeKind`'s
discriminative role. **Mitigation:** decision (c)'s per-grammar
codegen-time dispatch sidesteps this entirely — `TapeKind` emission
stays on the tape path; the struct-direct path simply does not need
it. No trait-surface mismatch.

**Risk 2 — Non-trivial layouts (Wrap-Alt over heterogeneous Refs)
require the per-shape emitter to know the *element* layout, not just
the rule's layout.** JSON `value` is an `Alt` over `null`, `bool`,
`number`, `string`, `array`, `object`. `JsonStructBuilder::OpenFrame::Wrap`
(`crates/core/src/runtime/json/builder.rs:80`) discards the branch
tag; the Alt-over-Refs body must call `push_branch_tag(idx)` then
enter the matched Ref's per-shape body, which calls `begin_compound`
on *its own* layout (the Object / Array / leaf layout).
**Mitigation:** Redress D's Wrap emitter resolves the wrap's own
layout from `ir.struct_registry.layout(rule.id)`; the dispatched
per-Ref call resolves the Ref-target's layout independently in its
own per-shape body. The two layout reads are the existing
`mod.rs:196` substrate; no new lookup mechanism is needed.

**Risk 3 — `cargo xtask regen --grammar json` may produce a generated
file that does not compile against the post-redress emitter (e.g.,
`JsonStructBuilder` import path drift, lifetime threading bug in the
parse_body return type).** **Mitigation:** A's `parse_body` arm is
the single source of truth for the struct-direct return type
(`Result<JsonDocument<'_>, ParseErr>` per W2.A re-shape in
PROGRESS.md). Same-commit a leaf compilation test
(`crates/core/tests/struct_direct_compile.rs`) that compiles a minimal
generated emitter snippet against the new strategy substrate. If
regen produces non-compiling output post-Stage-2-merge, the
orchestrator reverts the per-shape commits and re-dispatches with a
tightened strategy contract before re-attempting regen — this is a
wave-revert per AZ-I.md §Reversal rule 1, not a deferral.
