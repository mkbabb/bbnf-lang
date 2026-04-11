# Tranche AC — Full Tape Transposition

## Context

Tranche AB committed the tape substrate (AB.0 lattice, AB.1 CSP, AB.2a
driver/runtime/helpers) but stopped short of actually changing
generated code. Every production grammar still emits the eager-AST
shape: per-rule functions return `Option<<Grammar>Enum<'a>>`, Seq/Alt
compose `Option<Span<'a>>` or `Option<&'a Enum<'a>>` values, and the
slab allocator (`emit_alloc`) wraps compound results. The tape sits
idle.

Tranche AC is the full transposition. It replaces the entire Rust
emission pipeline atomically: one parser ABI, one control flow path,
one public API. No feature flag. No parallel path. No legacy code in
the final state.

The directive:

> "Architectural transpositions in the sake of elegance, simplicity,
> and performance above all are both necessary and desirable. NO
> legacy code."

AC treats the existing eager-AST emitter as the REFERENCE
implementation — it tells us what each rule must accept and produce
— and rewrites it from scratch against the tape ABI. The reference
is deleted in the same commit that lands the replacement.

AB.0, AB.1, and AB.2a are complete and live: `GrammarIR::materialization`
is populated, `DriverState::materialization_class` is threaded, and the
`tape_prelude.rs` helpers are in place. AC introduces no new ir-level
work; it consumes the AB substrate read-only.

## The single commitment

Every rule in every generated parser is emitted as:

```rust
fn __rule<'a>(
    state: &mut ::parse_that::ParserState<'a>,
    tape: &mut ::bbnf::runtime::tape::TapeBuilder,
) -> ::std::option::Option<::bbnf::runtime::tape::TapeOffset>
```

The `::bbnf::runtime::tape` path is a stable re-export of the
`bbnf-tape` leaf crate, so downstream consumers of
`#[derive(Parser)]` only need `bbnf` in their dependency list
(not a direct `bbnf-tape` dep).

The public API is a `Root` trait with a GAT view type, one
`Parsed<Grammar>` marker-type per grammar, and a `.view()` method that
lends a cursor-backed root view bound by `&self`:

```rust
// In bbnf::runtime:
pub trait Root {
    type View<'tape>;
    fn make_view(
        tape: &::bbnf::runtime::tape::Tape,
        root: ::bbnf::runtime::tape::TapeOffset,
    ) -> Self::View<'_>;
}

// In generated code:
impl Grammar {
    pub fn parse(
        input: &str,
    ) -> ::std::result::Result<::bbnf::runtime::Parsed<Self>, ParseErr> {
        /* ... */
    }
}

impl ::bbnf::runtime::Root for Grammar {
    type View<'tape> = GrammarRootView<'tape>;
    fn make_view(
        tape: &::bbnf::runtime::tape::Tape,
        root: ::bbnf::runtime::tape::TapeOffset,
    ) -> Self::View<'_> {
        GrammarRootView::new(tape, root)
    }
}
```

`Parsed<Grammar>` is marker-typed over the grammar struct itself —
never over the view type. The root view lifetime is bound by `&self`
on `Parsed`, via `Parsed<R: Root>::view(&self) -> R::View<'_>`. Callers
never name a `'tape` lifetime; they hold `Parsed<Grammar>` and call
`.view()` to obtain a cursor-backed root.

There is no `<Grammar>Enum<'a>` type. There is no `Parser<'a, T>`
return. There is no `emit_alloc`. There is no `BoxedEnum` variant.
There is no slab allocator. There is no `ValuePlacement::Alloc` arm.
There is no `Recovered` enum variant (`TapeKind::Recovered` sentinel
instead). There is no `&'a BbnfBootstrapEnum<'a>` pattern-matching
in `host.rs`.

## Inherited state from Tranche AB

- **`bbnf-tape` leaf crate** — `TapeRec` (16 bytes), `TapeKind`,
  `TapeBuilder::{mark_children, push_leaf, push_compound}`,
  `TapeCursor<'tape>`, `ChunkedArena<T>`.
- **`MaterializationClass` lattice** on `ir.materialization` — the
  per-`NodeId` classification (AB.0) + CSP refinement (AB.1) that
  decides per-rule emission shape.
- **`DriverState.materialization`** — populated from
  `ir.materialization` in `install_pattern_caches` and
  `generate/mod.rs`. Accessor:
  `DriverState::materialization_class(ir, node)`.
- **`GrammarIR::materialization`** — `HashMap<NodeId,
  MaterializationClass>` sidecar populated by
  `classify_materialization` (AB.0) and refined by
  `solve_strategy_and_materialization` (AB.1). AC consumes this
  sidecar read-only; no new ir-level passes are introduced.
- **`runtime::Parsed<View>`** — owning parse result type
  (`crates/core/src/runtime/parsed.rs`).
- **`tape_prelude.rs`** — emitter helpers
  (`emit_must_tape_prelude/epilogue`,
  `emit_tape_span_only_prelude/epilogue`, `emit_rule_signature`) at
  `crates/core/src/backend/rust/emitter/tape_prelude.rs`.

AC builds directly on top of that substrate. No new ir-level work;
no new CSP constraints; no new cost weights.

## The rewrite — file-by-file

### Emitter core (DELETE + REWRITE)

| File | Change |
|---|---|
| `crates/core/src/backend/rust/emitter/grammar.rs` | **Rewrite** `emit_rule_function_impl` to emit the tape-first signature. Dispatch prelude/epilogue shape on `MaterializationClass`. Delete `emit_grammar_impl`'s slab / recovered-static / public-`Parser<'a, T>` surface; replace with `impl Grammar { pub fn parse(...) -> Result<Parsed<RootView<'_>>, ParseErr> }`. |
| `crates/core/src/backend/rust/emitter/leaves.rs` | **Rewrite** `emit_literal_match_impl` / `emit_regex_match_impl` / `emit_epsilon_impl` / `emit_seq_all_span_impl` to produce `Option<TapeOffset>` via `tape.push_leaf(TapeKind::Span, ...)`. |
| `crates/core/src/backend/rust/emitter/seq.rs` | **Rewrite** Seq composition to match arms collecting `Option<TapeOffset>`, ending in `tape.push_compound(...)` at the rule epilogue (or consumed by the parent if nested). |
| `crates/core/src/backend/rust/emitter/alt.rs` | **Rewrite** Alt checkpoint / dispatch-table / all-literal / sub-variant paths to produce `Option<TapeOffset>`. Each branch pushes its own record; the Alt's compound header carries the chosen branch as `variant_idx`. |
| `crates/core/src/backend/rust/emitter/repeat.rs` | **Rewrite** many / optional / sep_by to accumulate child offsets under a Repeat compound. |
| `crates/core/src/backend/rust/emitter/binary.rs` | **Rewrite** Skip/Next/Minus/Negate to produce `Option<TapeOffset>`. |
| `crates/core/src/backend/rust/emitter/map_value.rs` | **Rewrite** Map / FnDescriptor paths. `EnumWrap` / `BoxWrap` become tape pushes with the rule's variant index. `NumberConvert` / `HexConvert` / `SpanCapture` / `Expr` wrap the inner tape offset under a compound with the mapper's metadata. |
| `crates/core/src/backend/rust/emitter/dispatch.rs` | **Rewrite** dispatch-table / token-dispatch paths to produce `Option<TapeOffset>`. |
| `crates/core/src/backend/rust/emitter/operator_chain.rs` | **Rewrite** chained binary flattening. |
| `crates/core/src/backend/rust/emitter/reference.rs` | **Rewrite** `compile_ref` so every child call is `__rule(state, tape)` returning `Option<TapeOffset>`. When `materialization[ref_node] == TransparentElide`, inline the referenced rule's body at the call site. |
| `crates/core/src/backend/rust/trace.rs` | **Rewrite** `emit_trace_entry` / `emit_trace_exit` to operate on `Option<TapeOffset>` results instead of `Option<Enum>`. Update the `grammar.rs` trace wrap site accordingly — otherwise `@debug` grammars silently break under `--features parser-trace`. |
| `crates/core/src/backend/rust/emitter/mod.rs` | Update `impl Emitter for RustEmitter` to match new signatures. |

### Tape substrate (NEW variant)

| File | Change |
|---|---|
| `crates/bbnf-tape/src/kind.rs` | **Add** `TapeKind::Recovered` variant. Pushed by `@recover` arms when recovery fires; views expose `.is_recovered()`. Replaces the old `Recovered` enum variant emitted by `ir_enums.rs:82-94`. |

### Enum & type layer (Rust backend only)

`ValuePlacement::Alloc` and `TypeDesc::BoxedEnum` are **NOT**
deleted in AC.2 — they are shared-driver and TS/WASM backend
concerns. The Rust backend stops consuming them (collapses
`Alloc`/`Inline` to one tape push; panics on `BoxedEnum` in
`type_desc_to_syn_raw` as a driver-bug canary), but the types
and variants remain. Full deletion happens if/when TS + WASM
migrate to the tape API in a post-AC tranche.

| File | Change |
|---|---|
| `crates/core/src/backend/rust/ir_enums.rs` | **Delete** `<Grammar>Enum<'a>` generation (`generate_enum`). **Keep** `generate_grammar_arr` — it emits the grammar string constant, still needed. The per-rule `<Rule>View<'tape>` types come from `backend/rust/view/generate_views` instead. |
| `crates/core/src/backend/rust/ir_types.rs` | **Delete** `emit_alloc` + `emit_alloc_let` family. **Replace** `type_desc_to_syn_raw`'s `BoxedEnum` arm with a panic (Rust backend under tape-first should never see it — the panic is a driver-bug canary). `IrCodegenCtx` itself remains load-bearing. |
| `crates/core/src/backend/types/decisions.rs` | **Keep** `ValuePlacement` — the shared driver + TS/WASM backends still call `child_alloc`. The Rust emitter reads the value but treats `Alloc` and `Inline` identically (both map to one tape push). |
| `crates/ir/src/types/type_desc.rs` | **Keep** `TypeDesc::BoxedEnum` — TS backend's `type_desc_to_ts` (`crates/core/src/backend/ts/helpers.rs`) still emits `BoxedEnum` arms. Deletion here is blocked on cross-backend migration. |

### View type codegen (NEW)

| File | Role |
|---|---|
| `crates/core/src/backend/rust/view/mod.rs` | Public entry: `generate_views(ir, ir_ctx) -> TokenStream`. Emits per-rule `<Rule>View<'tape>` structs + the top-level `<Grammar>View<'tape>` discriminator enum. |
| `crates/core/src/backend/rust/view/leaves.rs` | `TapeSpanOnly` rule views — single `.span()` accessor. |
| `crates/core/src/backend/rust/view/seq.rs` | `MustTape` Seq rule views — child accessors via `TapeCursor::children()`. |
| `crates/core/src/backend/rust/view/alt.rs` | `MustTape` Alt rule views — discriminator on `variant_idx()`. |
| `crates/core/src/backend/rust/view/repeat.rs` | `MustTape` Repeat rule views — iterator accessor. |
| `crates/core/src/backend/rust/view/grammar.rs` | Top-level `<Grammar>View<'tape>` enum emission. |

### Public API (REWRITE)

| File | Change |
|---|---|
| `crates/derive/src/lib.rs` | **Rewrite** `#[derive(Parser)]` entry point. Instead of delegating to `bbnf::pipeline::compile_paths_request` and emitting `Parser<'a, T>` wrappers, emit the new tape-first API: `impl #ident { pub fn parse(input: &str) -> Result<::bbnf::runtime::Parsed<Self>, ParseErr> { ... } }` plus the `impl ::bbnf::runtime::Root for #ident` GAT binding. |
| `crates/core/src/runtime/parsed.rs` | **Extend** the AB.2a-shipped `Parsed<View>` type. Add the `Root` trait with GAT `type View<'tape>` and the `make_view` constructor; change the phantom marker to `PhantomData<R>`; add `impl<R: Root> Parsed<R> { pub fn view(&self) -> R::View<'_> }`. AB.2a shipped storage-only; AC.2 lands the view constructor. |
| `crates/core/src/runtime/error.rs` | `ParseErr` enum: `Syntax { offset, rule }`, `Tape(TapeBuildError)`. **Already shipped in AC.1.** |

### Bootstrap regeneration

| File | Change |
|---|---|
| `crates/bootstrap/Cargo.toml` | Add `bbnf-tape` dep — the new derived code references `::bbnf_tape::TapeBuilder`. |
| `crates/core/src/grammar/generated.rs` | **Regenerate** from `scripts/bootstrap-bbnf.sh` after the emitter rewrite lands. 13 000 lines — new shape throughout. |
| `crates/core/src/grammar/host.rs` | **Rewrite** `extract_grammar` to walk `BbnfBootstrapView<'tape>` accessors instead of pattern-matching on `BbnfBootstrapEnum<'a>` variants. The CST helpers (`span_text`, `identifier_text`, `as_*_directive`) are already schema-emitted — the grammar-specific walk becomes a shallower one-pass over the view. |

### Consumer migration

Every non-prettify `#[derive(Parser)]` consumer updates from
`Grammar::rule()` → `Grammar::parse(input).view()` with cursor
accessors replacing enum pattern-matching. Prettify consumers bind
against the untouched `emitter/prettify/` tree and need no change.
The full audited scope lives under **AC.3** below.

### Auxiliary pieces (DELETE)

| File | Change |
|---|---|
| `crates/core/src/backend/rust/alloc_emit.rs` | **Delete** — slab scratch emission is dead code post-rewrite. |
| `crates/core/src/backend/rust/ir_enums.rs` | Rewritten, not deleted (now emits views). |
| `crates/ir/src/types/grammar.rs` (`collapse_simple_spans` field) | **Delete in AC.5** — subsumed by `MaterializationClass::TapeSpanOnly`. |
| `crates/ir/src/passes/span/` (`compute_sp_method_rules`) | **Audit for deletion in AC.5** — the `has_sp_method` pass overlaps `TapeSpanOnly` classification. If `TapeSpanOnly` subsumes it, delete. |

## Phase plan

Each phase is a self-contained commit. Master stays buildable
between commits except for the two explicitly marked (AC.2 and AC.4)
where the atomic nature of the transposition requires changes to
multiple subsystems in one commit.

### AC.0 — Plan doc

This file. One commit.

### AC.1 — Prep: `bbnf-tape` dep + `runtime::error` module

AC.1 is a commit-only step. The three changes are already staged in
the working tree:

- `crates/bootstrap/Cargo.toml` — `bbnf-tape` + `bbnf = { path =
  "../core" }` already added.
- `crates/core/src/runtime/error.rs` — already created with the full
  `ParseErr` enum (`Syntax { offset, rule }` + `Tape(TapeBuildError)`),
  `Display`, `Error`, and `From<TapeBuildError>` impls.
- `crates/core/src/runtime/mod.rs` — already declares `pub mod error`
  and re-exports `ParseErr` alongside `Parsed`.

AC.1 verifies `cargo build --workspace` is clean with those changes,
then commits them as a single prep commit. Master stays green; no
behavioral change.

### AC.2 — Atomic emitter transposition (the big commit)

#### Additive preps (shipped before the atomic commit)

Several pieces of AC.2 landed as small additive commits that leave
master green — each one is either new code no consumer calls yet,
or a purely additive change (new trait, new enum variant). The
atomic commit's remaining scope is narrower as a result:

| Commit | What it ships | Why additive |
|---|---|---|
| `TapeKind::Recovered` | New variant in `bbnf-tape::kind` + `is_recovered()` predicate. | Additive enum variant; no existing matches elsewhere. |
| `Root` trait + `Parsed::view` | GAT trait in `bbnf::runtime` + `impl<R: Root> Parsed<R> { view(&self) -> R::View<'_> }`. | New trait; no existing consumers. |
| `backend/rust/view/` module | Per-rule `<Rule>View<'tape>` generator (`generate_views`). Returns valid TokenStream emitting view structs + the `Root` binding. | Not yet called from `emit_type_definitions_impl`; pure dead code until the atomic commit wires it in. |
| `runtime_root` integration test | 5 tests exercising `Parsed<R> + Root` with a hand-written `Root` impl. | Independent of generated code; validates the API surface. |

#### Remaining atomic work

One large commit that rewrites the entire emission pipeline and
regenerates the bootstrap in the same atomic change:

1. Delete every legacy piece listed above (`emit_alloc`,
   `BoxedEnum`, `ValuePlacement::Alloc`, old `ir_enums` generation,
   `Parser<'a, T>` public surface, `Recovered` enum variant,
   `alloc_emit.rs`).
2. Rewrite every emitter kind (`leaves`, `seq`, `alt`, `repeat`,
   `binary`, `map_value`, `dispatch`, `operator_chain`) to produce
   `Option<TapeOffset>` via the `tape_prelude.rs` helpers.
3. Rewrite `emit_rule_function_impl` to emit the tape-first rule
   signature with prelude/epilogue dispatched on
   `MaterializationClass`.
4. Rewrite `emit_grammar_impl` to emit the `parse` entry point
   returning `Parsed<RootView<'_>>`.
5. Write `backend/rust/view/` module generating per-rule
   `<Rule>View<'tape>` types + the `<Grammar>View<'tape>` enum.
6. Rewrite `#[derive(Parser)]` in `crates/derive/src/lib.rs` to
   match the new ABI.
7. Regenerate `crates/core/src/grammar/generated.rs` via
   `scripts/bootstrap-bbnf.sh`.
8. Rewrite `crates/core/src/grammar/host.rs` to walk view types.
9. Delete `TypeDesc::BoxedEnum` variant if grep is clean.

After this commit: `cargo build --workspace` passes. Every grammar
compiles. Consumer migration (AC.3) follows.

#### Load-bearing coupling surprise — lowering reads the bootstrap CST

The initial audit focused on the Rust emitter and bootstrap
`host.rs`. A deeper audit (worktree-agent-a985875e, 2026-04-10)
found the real scope: **216 `BbnfBootstrapEnum` references across
12 files**, not the 8 the early audit suggested. Measured by
`grep -c BbnfBootstrapEnum`:

| File | LOC | Refs |
|---|---|---|
| `crates/core/src/lower/value_expr.rs` | 457 | 67 |
| `crates/core/src/lower/expression.rs` | 483 | 60 |
| `crates/core/src/grammar/host.rs` | 187 | 23 |
| `crates/core/src/pipeline/compile.rs` | 601 | 20 |
| `crates/core/src/lower/mod.rs` | 263 | 17 |
| `crates/core/src/graph/metadata.rs` | 64 | 12 |
| `crates/core/src/graph/deps.rs` | 75 | 7 |
| `crates/core/src/types.rs` | 108 | 3 |
| `crates/core/src/grammar/mod.rs` | ~50 | 3 |
| `crates/core/src/pipeline/directives.rs` | 183 | 2 |
| `crates/core/src/grammar/schema/build.rs` | ? | 1 |
| `crates/core/src/grammar/schema/model.rs` | ? | 1 |

Each pattern-matches on `BbnfBootstrapEnum` variants and calls
`.span_text()` / `.identifier_text()` / other schema helpers to
walk the CST. Under the atomic AC.2 rewrite, `BbnfBootstrapEnum`
is deleted and replaced with `BbnfBootstrapView<'tape>` — every one
of these call sites migrates to cursor-walking the view in the
same commit, plus the schema helper emit paths
(`grammar/schema/emit/rust/`) must switch from `impl <Enum> { fn
span_text() }` to `impl <View> { fn span_text() }`.

The bootstrap consumer is not just `host.rs`; it is the entire
lowering pipeline. ~2500 LOC of pattern-matching code migrates
alongside the emitter rewrite.

#### Staged workflow within AC.2

The atomic commit has a chicken-and-egg:
`crates/core/src/grammar/generated.rs` is a source file of `bbnf`,
and `bbnf-bootstrap` (whose `cargo expand` output produces
`generated.rs`) depends on `bbnf`. Regenerating `generated.rs`
requires `bbnf` to compile, which requires a valid `generated.rs`.
Resolution — staged workflow within the single commit:

1. Temporarily comment out `pub mod generated;` and `pub mod host;`
   in `crates/core/src/grammar/mod.rs`, and delete both files. `bbnf`
   now has no grammar module.
2. Add `TapeKind::Recovered` to `crates/bbnf-tape/src/kind.rs`.
3. Rewrite every emitter file (`leaves`, `seq`, `alt`, `repeat`,
   `binary`, `map_value`, `dispatch`, `operator_chain`, `reference`,
   `grammar`, `trace`, `ir_enums`, `ir_types`) to produce
   `Option<TapeOffset>` via the `tape_prelude` helpers.
4. Generate the `backend/rust/view/` module and extend
   `runtime/parsed.rs` with the `Root` trait + `.view()` method.
5. Rewrite `#[derive(Parser)]` in `crates/derive/src/lib.rs`.
6. Delete legacy: `alloc_emit.rs`, `ValuePlacement::Alloc`,
   `TypeDesc::BoxedEnum` (if grep-clean), old `Recovered` enum
   emission from `ir_enums.rs`.
7. Verify `cargo build -p bbnf` is clean without the grammar module.
8. Run `scripts/bootstrap-bbnf.sh` — regenerates
   `crates/core/src/grammar/generated.rs` via
   `cargo expand -p bbnf-bootstrap`. **The script itself needs
   updating**: its Python post-processor (lines 23-180) strips
   `pub enum BbnfBootstrapEnum` and related eager-AST shapes that
   the tape-first emitter no longer produces, re-adds
   `#[derive(Debug)]` on the enum, and strips auto-generated
   `impl Debug for BbnfBootstrapEnum<'a>` blocks. Under AC.2,
   the post-processor must be rewritten to strip the new
   view-type shapes (`pub struct BbnfBootstrapRuleView<'tape>`,
   `impl<'tape> BbnfBootstrapRuleView<'tape>`, etc.) and
   preserve the `impl Root for BbnfBootstrap` binding and the
   `impl Grammar { fn parse }` entry point. Budget ~100 LOC of
   Python rewrite.
9. Write the new `crates/core/src/grammar/host.rs` walking
   `BbnfBootstrapView<'tape>` accessors instead of pattern-matching
   `BbnfBootstrapEnum<'a>`.
10. Migrate every `BbnfBootstrapEnum` consumer to the view
    accessor surface (216 refs across 12 files per the audit):
    `crates/core/src/types.rs`,
    `lower/{mod,expression,value_expr}.rs`,
    `graph/{metadata,deps}.rs`,
    `pipeline/{compile,directives}.rs`,
    `grammar/{mod,schema/build,schema/model}.rs`. Plus update the
    schema helper emit paths (`grammar/schema/emit/rust/`) so
    `.span_text()` / `.identifier_text()` / `.as_*_directive()`
    are emitted as view impls rather than enum impls.
11. Re-enable `pub mod generated;` and `pub mod host;` in
    `grammar/mod.rs`.
12. Create `crates/core/tests/tape_parity.rs` with golden snapshots
    for JSON, CSS L4, BBNF, Sheets, EBNF under
    `tests/fixtures/tape_golden/`.
13. Verification: `cargo build --workspace` clean;
    `cargo test -p bbnf-ir` green (AB.0/AB.1 tests);
    `cargo expand -p bbnf --bench json_monolithic | grep
    push_compound` finds ≥1 call; `cargo test --test tape_parity`
    green.
14. Atomic commit.

### AC.3 — Consumer migration

One or more commits — batched by domain. The audited scope is:

| Area | Files | Migration needed? |
|---|---|---|
| Gorgeous prettify modules | `bbnf.rs`, `css.rs`, `json.rs`, `bnf.rs`, `ebnf.rs`, `google_sheets.rs` | **No** — prettify emission tree under `emitter/prettify/` is unchanged. `RuleName_prettify()` calls still bind against the same surface. |
| Gorgeous jit | `jit.rs` | **No** — calls `bbnf::grammar::parse` (library fn), not a derive-generated method. AC.2's host.rs rewrite updates the library implementation transparently; `jit.rs` still gets back a `ParsedGrammar` with a `.rules` field. |
| Core slab tests | `json_slab`, `google_sheets_slab`, `ebnf_prettify`, `css_pretty`, `css_l4`, `serialize_roundtrip`, `bench_grammar_parse` | Yes. |
| Core examples | `mono_test.rs`, `json_check.rs`, `test_pretty.rs`, `test_l4.rs` | Yes. |
| Core benches | `json/{monolithic,stress,vm,wasm,ts,parse_that,competitors}.rs`, `css/{monolithic,l4,stress,vm,wasm,ts,competitors}.rs`, `google_sheets/{monolithic,vm}.rs` | Yes. |
| Crates with zero parser consumers | `crates/ser/`, `crates/analysis/`, `crates/lsp/` | N/A. |

Each commit updates a related group of consumers from the old
`Grammar::rule()` + `__XxxEnumCtx` construction pattern to
`Grammar::parse(input)?.view()` + cursor accessors. The migration
is mechanical once the view type surface is settled.

**Before (slab-mode bench/test/example):**
```rust
let ctx = __JsonParserEnumCtx::with_capacity(input.len() / 32);
let parser = JsonParser::value();
let (result, state) = parser.parse_return_state_with_context(&input, &ctx);
assert!(result.is_some());
assert!(state.offset >= input.trim_end().len());
```

**After (tape-first):**
```rust
let parsed = JsonParser::parse(&input).expect("parse failed");
let _root = parsed.view();            // or: let tape = parsed.tape();
// The tape-first API already enforces full input consumption
// (the parser rejects trailing garbage) so the separate
// "completeness" assertion collapses into the parse success.
```

The `__XxxEnumCtx` slab context vanishes entirely — the tape is
owned by `Parsed<Grammar>` and allocated inside the library.

After AC.3 closes: `cargo test --workspace` green. Every production
grammar parses correctly end-to-end on the tape.

### AC.4 — Tape elision enablement

Enable `TapeSpanOnly` and `TransparentElide` emission paths (deferred
from AB.4). The emitter scaffolding from AC.2 already supports the
class dispatch; AC.4 turns on the optimizations and verifies parity.

### AC.5 — Post-AC baseline + cleanup

- Delete any `cfg!(debug_assertions)` debug asserts left from AC.2.
- `cargo clippy --all-targets -- -D warnings` clean.
- `grep -rn "BoxedEnum\|emit_alloc\|BbnfBootstrapEnum\|Recovered"
  crates/core/src crates/ir/src` returns zero.
- Delete `GrammarIR::collapse_simple_spans` flag + its consumers —
  subsumed by `MaterializationClass::TapeSpanOnly`.
- Audit `compute_sp_method_rules` for deletion — overlaps
  `TapeSpanOnly` classification.
- Delete `crates/core/src/backend/rust/alloc_emit.rs` (if any
  residue survived AC.2).
- Fresh samply profiles for every production bench:
  `json_canada`, `json_twitter`, `json_citm`, `json_data_xl`,
  `css_tailwind`, `css_bootstrap`, `css_normalize`, `compile_bbnf`,
  `compile_css_l4`.
- `docs/benchmarks/post-AC.json` with every delta cited from a
  samply symbol + self-time diff.

**Floor gates** (tape-only commitment — projection is deferred to a
follow-up tranche):
- `json_canada` parse ≥ 1.5 GB/s
- `json_twitter` parse ≥ 1.7 GB/s
- `json_citm` parse ≥ 2.0 GB/s
- `css_tailwind` parse ≥ 0.32 GB/s
- `css_bootstrap` parse ≥ 0.30 GB/s
- `compile_bbnf` ≤ 1.5× pre-AC
- `compile_css_l4` ≤ 2.0× pre-AC

## Architectural commitments

1. **One parser ABI.** `fn __rule(state, tape) -> Option<TapeOffset>`,
   no exceptions. The classifier refines prelude/epilogue shape;
   the signature is universal.
2. **One control-flow path.** Per-rule function bodies are
   single-path labeled blocks. No conditional branching on a
   runtime mode bit.
3. **Owning parse result.** `Parsed<View>` owns the tape inline and
   lends views by borrow. No `(View, Tape)` tuple surface.
4. **No legacy code in the final state.** `BoxedEnum`, `emit_alloc`,
   `ValuePlacement::Alloc`, `Recovered` enum, `Parser<'a, T>` public
   surface, `<Grammar>Enum<'a>` — all deleted in AC.2.
5. **Prettify is preserved.** The prettify emitter walks parser
   state directly and does not touch the typed AST. AC.2 verifies
   every `@pretty` grammar still produces byte-identical formatter
   output.
6. **`@recover` via `TapeKind::Recovered` sentinel.** The old
   `Recovered` enum variant becomes a special-cased leaf record
   pushed when a recovery arm fires. View accessors expose
   `.is_recovered()`.
7. **Debug instrumentation unchanged.** `@debug` rules emit trace
   calls inside the rule body. The trace macros already take
   `&ParserState` only.
8. **Bootstrap regeneration is part of AC.2.** The atomic
   transposition includes rerunning the bootstrap script, so
   `generated.rs` and the emitter are always consistent.
9. **One parse emission path.** `#[parser(slab)]` and
   `#[parser(structural)]` mode attributes become vestigial as
   parse-path selectors under the one-tape-path commitment. `slab`
   is dropped; `structural` becomes the only parse path (aliased to
   default). `#[parser(prettify)]` remains orthogonal — it
   additively emits the prettify emission tree
   (`emitter/prettify/`, unchanged) alongside the tape-first parse
   path. Gorgeous prettify consumers are therefore unaffected by
   AC.2.

## Verification

**AC.1** — `cargo build --workspace` clean. `bbnf::runtime::ParseErr`
in scope.

**AC.2** — `cargo build --workspace` clean. `cargo test -p bbnf-ir`
green (AB.0 + AB.1 tests still pass). `cargo expand -p bbnf
--bench json_monolithic 2>&1 | grep -c 'fn __' > 0` — generated
code has rule functions. `cargo expand -p bbnf --bench
json_monolithic 2>&1 | grep 'TapeBuilder::push_compound'` — at
least one `push_compound` call. `grep -rn BoxedEnum crates/core/src
crates/ir/src` returns zero. `grep -rn emit_alloc
crates/core/src` returns zero.
`crates/core/tests/tape_parity.rs` created and green on JSON, CSS
L4, BBNF, Sheets, EBNF. Parity gate shape: for each grammar, parse
≥20 sample inputs from the benchmark corpus, walk view accessors
(`.kind()`, `.span()`, `.variant_idx()`, `.children()`), and
compare against a golden structural snapshot under
`tests/fixtures/tape_golden/`.
`grep -rn Recovered crates/core/src/backend/rust/ir_enums.rs`
returns zero (old enum variant is gone). `cargo expand -p bbnf
--bench bbnf_grammar_parse | grep 'TapeKind::Recovered'` appears
on grammars with `@recover` directives. `@debug` grammars compile
and trace under `cargo build --features parser-trace` — trace.rs
adaptation verified.

**AC.3** — `cargo test --workspace` green. Every gorgeous module
tests pass. Every core integration test passes.

**AC.4** — Tape record counts drop on fixture grammars with
transparent wrappers and punctuation tokens. Parity gate still
green.

**AC.5** — Clippy clean. Bench floor gates met. Samply profiles
committed.

End-to-end smoke test:
```bash
cargo build --workspace
cargo test --workspace
BBNF_EGRAPH_REPORT=1 BBNF_PIPELINE_REPORT=1 BBNF_CSP_REPORT=1 \
    cargo bench -p bbnf --bench compile_pipeline 2>&1 | head -200
cargo bench -p bbnf --bench json_monolithic
cargo bench -p bbnf --bench css_l4
```

## Why AC.2 is one commit

The two alternatives both have fatal flaws:

1. **Decompose into per-file commits** — every per-kind emitter is
   tightly coupled to every other via the `Option<Enum>` return
   contract. Migrating `leaves.rs` to return `Option<TapeOffset>`
   while `seq.rs` still expects `Option<Span<'a>>` produces code
   that doesn't compile. Interim scaffolding (bridging the two
   return types temporarily) IS the legacy code the directive
   forbids.

2. **Feature-flagged parallel path** — adding `emit_tape_*`
   variants alongside the existing emitters, gated by a config
   flag, is the definition of legacy code carried alongside the new
   path. Explicitly rejected by the directive.

3. **Regenerate `generated.rs` in a follow-up commit** — keeping
   `generated.rs` stale for one commit means `cargo build -p bbnf`
   is broken on master during that window. The workspace build is
   a hard-blocked prerequisite for every downstream task (tests,
   benches, LSP, playground). Breaking master, even briefly, is
   explicitly rejected by the directive.

The only idiomatic option is the atomic rewrite: delete everything
that's about to be replaced, emit the replacement in the same
commit, regenerate the bootstrap, migrate every `BbnfBootstrapEnum`
consumer (`host.rs` plus the lowering pipeline). Scope per the
audit:

- **~2900 LOC** Rust emitter rewrite across 13 files (leaves,
  seq, alt, dispatch, repeat, binary, map_value, operator_chain,
  grammar, trace, ir_enums, ir_types, alloc_emit).
- **~2500 LOC** CST consumer migration across 12 files
  (216 `BbnfBootstrapEnum` references to rewrite as view walks).
- **13 000 LOC** regenerated `generated.rs` via the bootstrap
  `cargo expand` pipeline.
- New `crates/core/tests/tape_parity.rs` + golden snapshots
  (~500 LOC + fixtures) under `tests/fixtures/tape_golden/`.

The change is mechanical once the emitter helpers
(`tape_prelude.rs`) are settled and the `generate_views`
generator is wired in, but the atomic sequencing requirement
means every file flips together — no incremental per-file
testing is possible because the driver/emitter contract binds
all consumers to one return-type shape.

## What this tranche does NOT include

- **Typed view accessors.** AC.2 generates views with universal
  cursor accessors (`.kind()`, `.span()`, `.variant_idx()`,
  `.children()`). Rule-specific typed accessors (e.g.,
  `PairView::key() -> StringView`) are a post-AC tranche over the
  stable view substrate.
- **Projection into scalar view accessors.** `FnDescriptor` paths
  (`NumberConvert`, `HexConvert`, `SpanCapture`, `Expr`,
  `Constant`) emit span leaves (or compound wrappers over sub-tape
  offsets) in AC.2; scalar accessor specialization happens in a
  post-AC tranche over the stable view substrate. The view layer
  is responsible for lazy `f64`/`u8`/struct extraction.
- **Direct projection.** Parser rules never return scalar or
  aggregate types. That's a follow-up tranche over the stable
  tape.
- **Bulk-encoded runs (`TapeCompact`).** Deferred.
- **TS / WASM backend migration.** The `Emitter` trait gains no new
  required methods; TS and WASM keep their existing eager AST
  surface. The AC commit scope is Rust-backend-only.
- **Profile-guided cost calibration.** Follow-up.

## The key insight

The existing eager-AST emitter is the REFERENCE implementation. It
tells us what each rule accepts and produces. AC rewrites it against
the tape ABI from scratch, deleting the reference in the same
commit that lands the replacement.

One ABI. One path. One commit for the pipeline flip, followed by
consumer migration commits. No feature flag. No transitional shim.
No legacy code in the final state.
