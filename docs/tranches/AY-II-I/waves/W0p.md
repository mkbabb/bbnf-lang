# AY-II.W0' — FusedBuilder Collapse + Consumer Closure + Legacy Cruft Deletion

**Opens after**: W0 (partial) — supersedes the remainder of W0's close ceremony
**Agents**: 3 parallel (W0'.a / W0'.b / W0'.c on disjoint file bounds)
**Hard gate**: `FusedBuilder<R>` replaces the `TapeBuilder<R>` + `ValueBuilder<R>` split at the type level inside the tape crate; every shape's existing `begin_compound` / `end_compound` / `push_leaf_*` write BOTH tape and value columns atomically with zero signature churn; `project_value_output` per-admission arms route through the 69 emitted `materialize_projection_*` fns (currently with zero call sites); `STRUCTURAL_SCAN_POLICY` splices at emission time inside `__path_walk` and object-key-seek hot paths; every W0-era `#[allow(dead_code)]` retires as the annotated surface becomes live OR the surface deletes; `TapeBuilder::push_compound` + `mark_children` public APIs retire; `<Grammar>Value::Unknown` fallback retires where totality holds; `Parsed::to_value()` no longer panics and delivers the fused semantic surface; double-regen cycle-1 = cycle-2 byte-identical; full fat-LTO 5-bench matrix clean at close.
**Status**: complete — source/runtime landings are in; B1 closed 2026-04-24 (`docs/tranches/B1/FINAL.md`); B3 + B4.W0 + B2 closed 2026-04-25 (`docs/tranches/{B3,B4,B2}/FINAL.md`); the W0' close ceremony folded into B4.W1 close 2026-04-25 — the unified `builder.rollback_to(...)` path (atomic tape + value substrate unwind keyed on `ValueCheckpoint::tape_idx`) lands the contract the W0'.a substrate shipped without; the alias surface (`TapeBuilder`, `ValueBuilderOutput`, `value_builder` shim, `_ValueBuilderShim` ZST, 4-arg `new_fused` bridge) retires entirely; the 327-failure runtime-parser regression resolves; `cargo xtask regen --check` is clean across 9 grammars; `cargo nextest run --workspace --profile ax-iter` reaches 1480/1490 pass with the remaining 10 failures in unrelated cursor-shape-projection / ir_enums populator scope. See `docs/tranches/B4/audit/W1-close.md`.

## Rationale — audit triumvirate synthesis

The four pre-close audits (`audit/AY-II-AUDIT-{A,B,C,D}-*.md`, commits
`baeed709` / `a809d12f` / `ad70effd` / `319c432a`) converge: W0
landed the tape-substrate retirement, the emitter shape migration,
and the runtime scaffolding but **did not land the fused semantic
pipeline it promised**. Three distinct substrate-without-consumer
seams opened:

1. `ValueBuilder<R>` allocated at parse entry, never threaded into
   shape-fn signatures → slab empty → `Parsed::to_value()` panics on
   any non-empty parse (`value_materialize.rs:281-285` +
   `generated.rs:25660-25664`).
2. 69 emitted `materialize_projection_<rule>_<Grammar>` fns carry
   zero production call sites; `project_value_output` bypasses them
   entirely (AUDIT-C Q3).
3. `STRUCTURAL_SCAN_POLICY` const table emitted per grammar with
   zero call sites, zero activation bits, zero `__path_walk`
   consumer reference (AUDIT-B §5, triple-dead).

AUDIT-C's Path B — **FusedBuilder** — closes all three seams with
one architectural transposition rather than three separate
threadings. `FusedBuilder<R>` absorbs `TapeBuilder<R>` +
`ValueBuilder<R>` at the type level; every shape's existing
`builder: &mut TapeBuilder<R>` becomes `builder: &mut FusedBuilder<R>`;
`begin_compound` / `end_compound` / `push_leaf_*` stamp both column
families atomically; projection consumers read the value slab that's
already a first-class sibling to the tape.

This wave is a **plan pivot within AY-II** per SPEC §Mid-tranche
plan pivots (Absorb mode). The AY-II thesis is unchanged; the
mechanism refines. W1-W5 wave specs hold without rewrite; only W0
re-sequences.

## Architectural thesis

1. **ONE builder.** `FusedBuilder<R>` at `crates/tape/src/builder.rs`
   replaces the `TapeBuilder` + `ValueBuilder` split. `Columns`
   gains per-record parallel value columns (value-frame slots,
   payload tags, payload values). `rollback_to` truncates tape +
   value slots atomically. `finish(root_off)` yields both the
   finalised `Tape` AND the `ValueBuilderOutput`-equivalent slab in
   one call. No standalone `ValueBuilder` type survives.
2. **ONE projection path.** `project_value_<grammar_name>` routes
   every admission arm through its emitted `materialize_projection_
   <rule>_<Grammar>(output, input)` helper. The 69 pre-emitted
   materializers become authoritative; the `<Grammar>Value::<rule>`
   variant wraps the projection struct. Totality tightens from
   structural-count to runtime-call-count truth.
3. **ONE navigation path.** `STRUCTURAL_SCAN_POLICY` splices at
   emission time into `view/value.rs::emit_path_walk` — rules whose
   policy admits `OBJECT_KEY_SEEK` / `BOUNDED_LOOKAHEAD` /
   `SCAN_STRUCTURAL_BOUNDED` inline the matching cursor primitive at
   codegen; no runtime lookup, no dispatch flag. `__path_walk`
   bodies become per-rule-specialised.
4. **ZERO legacy cruft.** Every symbol whose W0 role ended retires.
   `TapeBuilder::push_compound` + `mark_children` (public API, 0
   callers) delete. `ValueBuilder` as a standalone type deletes.
   `navigate_tape` comments in `value_api_apples_to_apples.rs`
   trim to the new contract. Every `#[allow(dead_code)]` added
   during W0 retires as its surface becomes live OR the surface
   deletes. `<Grammar>Value::Unknown` fallback deletes where
   totality holds.
5. **ZERO dead IIFEs.** Shape emitters that landed retry-IIFE
   bodies whose sole purpose was rollback semantics re-evaluate
   post-W0' — if a site's IIFE only exists to scope `rollback_to`
   calls that now inline cleanly, the IIFE deletes. No
   side-effect-free wrapper survives.
6. **ZERO workarounds.** The wave ships complete within its scope.
   Post-wave "future consumer" hooks violate substrate-with-consumer.

## Invariants (augment the AY-II parent invariants)

14. `FusedBuilder<R>` is the sole parser-facing builder type;
    `ValueBuilder<R>` and `ValueBuilderOutput<R>` as standalone types
    retire. Evidence:
    `rg 'pub struct ValueBuilder|pub struct ValueBuilderOutput' crates/core/src/runtime/`
    returns 0.
15. `TapeBuilder::push_compound` + `TapeBuilder::mark_children`
    public API retires. Evidence:
    `rg 'pub fn push_compound|pub fn mark_children' crates/tape/src/builder.rs`
    returns 0.
16. Every `materialize_projection_<rule>_<Grammar>` fn has at least
    one production call site post-W0'. Evidence:
    `cargo expand -p bbnf --bench json_monolithic` shows
    `materialize_projection_*_JsonParser` invocations inside
    `project_value_JsonParser`; same for every grammar.
17. `STRUCTURAL_SCAN_POLICY` is referenced by at least one emission
    site per grammar. Evidence:
    `rg 'STRUCTURAL_SCAN_POLICY\[' crates/core/src/backend/rust/` ≥ 1.
18. Zero `#[allow(dead_code)]` introduced during W0 survives W0'
    close. Evidence: diff of `rg '#\[allow(dead_code)\]' crates/`
    pre-W0 vs post-W0'.
19. `Parsed::to_value()` exits successfully on every fixture of
    every grammar's parity corpus. Evidence:
    `cargo test --test value_api_apples_to_apples --release` green
    with a per-grammar smoke suite asserting non-panic + shape
    equality against the parity oracle.

## Scope

### W0'.a — FusedBuilder collapse + tape legacy deletion

Owner: `crates/tape/` substrate + runtime builder allocator.

Mechanism:

1. Extend `Columns` with per-record value columns (frame slot,
   payload tag, payload value). Parallel storage discipline mirrors
   `sib_skip`. Capacity reserved alongside the existing per-record
   columns.
2. Rename `TapeBuilder<R>` to `FusedBuilder<R>`; absorb `ValueBuilder`'s
   frame arena, checkpoint type, and payload columns into the builder.
   The public surface (`begin_compound` / `end_compound` /
   `end_compound_post_order` / `push_leaf_*` / `rollback_to` /
   `finish`) retains its signatures — shape emitters compile without
   change at the call-site granularity (the type behind
   `&mut builder` shifts; `cargo expand` verifies).
3. Atomic stamping: every `begin_compound` + `end_compound` now
   writes BOTH column families in one call; `rollback_to(offset)`
   truncates BOTH atomically. `push_leaf_*` writes both.
4. `FusedBuilder::finish(root_off: u32)` returns the finalised
   tuple `(Tape, ValueFramesOutput)` (or the single `FusedOutput`
   holding both). `Parsed::new_fused` consumes that output directly.
5. Delete `TapeBuilder::push_compound` + `TapeBuilder::mark_children`
   public entries. Any remaining caller in tests retires; tests
   migrate to the unified surface.
6. Delete `crates/core/src/runtime/value_builder.rs`. Retire the
   module declaration in `runtime/mod.rs`. Migrate callers of
   `ValueBuilder::new` / `ValueBuilderOutput::frame_at` to the
   equivalent `FusedBuilder` / `FusedOutput` accessors.
7. Audit `crates/core/src/backend/rust/emitter/grammar.rs` parse
   entry: allocate ONE `FusedBuilder<R>`; delete the dual-allocator
   sequence landed at `1f97a8cc`.
8. Audit the parse-entry retry sites for any dead IIFE wrappers
   that existed solely to scope tape/value dual rollback; collapse
   to single-builder inline rollback.
9. `cargo expand -p bbnf --bench json_monolithic` post-W0'.a shows
   zero `ValueBuilder` references.

Sub-gate:
- `cargo check --profile ax-iter -p tape` exits 0.
- `cargo check --profile ax-iter -p bbnf --lib` exits 0 with the
  fresh-regen `generated.rs` against the renamed type (W0' close
  ceremony regens; agent can pre-regen in its own worktree to
  validate).
- `rg 'pub struct ValueBuilder|pub struct ValueBuilderOutput|pub fn push_compound|pub fn mark_children' crates/` → 0.
- `wc -l crates/tape/src/builder.rs` approximately equal to pre-W0
  + absorbed ValueBuilder LOC; the LOC-neutral measurement is
  `wc -l crates/tape/src/builder.rs + wc -l crates/tape/src/columns.rs`
  vs pre-W0' total minus deleted `value_builder.rs`.

### W0'.b — Projection-consumer wiring + materializer slab migration

Owner: `crates/core/src/backend/rust/{view,emitter/shapes/value_materialize}.rs`.

Mechanism:

1. Rewrite `emit_project_value_<grammar>` (`view/value.rs`) so each
   admission arm calls the corresponding
   `materialize_projection_<rule>_<Grammar>(output, input)` fn and
   wraps the returned projection struct in the matching
   `<Grammar>Value::<rule>` variant. The `Unknown` fallback deletes
   except for genuinely un-admitted rule kinds (empty when totality
   holds).
2. Migrate every `materialize_projection_*` body (source:
   `shapes/value_materialize.rs`; target: post-W0.d emitted) from
   tape-backed reads (`view.cursor().tape().payload_bytes(...)`) to
   fused-slab reads (`output.frame_at(...)`, `output.payload_for(...)`).
   The projection struct fields populate from the value columns the
   fused builder already stamped.
3. Assert wire-contract closure: every
   `PROJECTION_DIRECT_TO_STRUCT` entry has a matching materializer
   (pre-W0.d invariant) AND at least one call site in
   `project_value_<grammar>` (W0' invariant §16). `projection_totality.rs`
   test tightens from structural-count to runtime-call-count.
4. Retire `<Grammar>Value::Unknown` fallback where totality holds
   for that grammar; retain only where the grammar carries rules
   not yet admitted (record that subset in the W0'.b return
   report).
5. `cargo test --test value_api_apples_to_apples --release` green
   per grammar (non-panic + shape equality).

Sub-gate:
- `cargo test -p bbnf --test projection_totality --release` green
  with the runtime-call-count assertion.
- `cargo test -p bbnf --test value_api_apples_to_apples --release`
  green on all five JSON fixtures + one-per-grammar smoke.
- `cargo expand -p bbnf --bench json_monolithic` shows
  `materialize_projection_*_JsonParser` callable from
  `project_value_JsonParser`; same for every grammar.
- `grep 'view.cursor().tape().payload_bytes' target/expand/ay-ii-*.rs`
  → 0 (materializers no longer read from tape).

### W0'.c — Structural-scan policy splice + emitter cleanup

Owner: `crates/core/src/backend/rust/{emitter/shapes/dispatcher,view/value}.rs`.

Mechanism:

1. `view/value.rs::emit_path_walk` reads `STRUCTURAL_SCAN_POLICY`
   per-rule at emission time. Rules whose policy admits
   `OBJECT_KEY_SEEK` emit an inline `cursor.object_key_seek(...)`
   call at the matching `PathSegment::Field` handler; rules admitting
   `BOUNDED_LOOKAHEAD` emit `cursor.bounded_lookahead(end_span)`; rules
   admitting `SCAN_STRUCTURAL_BOUNDED` emit
   `cursor.scan_structural_bounded(end_span)`. Rules with
   `ScanAlphabetClass::Empty` emit the generic linear walker (current
   default).
2. Retire the `#[allow(dead_code)]` on the
   `STRUCTURAL_SCAN_POLICY` emission
   (`shapes/dispatcher.rs:1883`) — the emitted const now has
   per-rule consumers.
3. Audit `shapes/dispatcher.rs` for any remaining dead dispatch
   entries; retire.
4. Audit `alt_dispatch.rs` and `string.rs` `#[allow(dead_code)]`
   markers (4 total per pre-W0' inventory) — retire as the
   annotated item becomes live OR the item deletes.
5. `rg '#\[allow(dead_code)\]' crates/core/src/backend/rust/emitter/`
   — output delta vs pre-W0' ≥ 6 removals (every W0-era addition
   retires).

Sub-gate:
- `cargo expand -p bbnf --bench json_monolithic` shows at least one
  `cursor.object_key_seek` or `cursor.bounded_lookahead` invocation
  inside `__path_walk`.
- `rg '#\[allow(dead_code)\]' crates/core/src/backend/rust/emitter/`
  count strictly less than pre-W0'.
- `cargo iter-check-full` exits 0.

## Orchestrator-owned close ceremony (compressed-honest, post-B2 substrate)

The pre-B2 ceremony — `bash scripts/bootstrap-bbnf.sh` cycles +
`.bbnf-cache` clears + a synchronous fat-LTO bench matrix + samply
per primary grammar + `nm` of bench binaries — presupposed an 80-min
proc-macro IR-pipeline wall and a content-keyed cache that no longer
exist. AUDIT-B isolated the load-bearing core from the theatrical
remainder; B2's close removed the theatrical preconditions.
Post-B2 the ceremony is ~15 min of formalisation:

1. **Cycle-1 regen**: `cargo xtask regen` (full sweep) — wall ~5 min
   dominated by xtask incremental compile; the IR pipeline itself
   runs in milliseconds per grammar; output writes to
   `crates/core/src/grammar/generated/<ident>.rs`. The pre-B2
   `bash scripts/bootstrap-bbnf.sh` script retired at B2.W3; the
   xtask is the canonical regen entrypoint.
2. **Invariant verification**: run the W0' invariant grep suite —
   `rg 'pub struct ValueBuilder|pub struct ValueBuilderOutput'
   crates/` → 0; `rg 'pub fn push_compound|pub fn mark_children'
   crates/tape/src/builder/` → 0; `STRUCTURAL_SCAN_POLICY` reference
   count ≥ 1 per grammar; `#[allow(dead_code)]` count strictly less
   than pre-W0'.
3. **Projection-totality test**: `cargo test -p bbnf --test
   projection_totality --release` runtime-call-count assertion green.
4. **`<Grammar>Value::Unknown` retirement audit**: per-grammar
   exception ledger captured in the W0' close entry.
5. **Close-status formalisation**: PROGRESS.md W0' close entry +
   `waves/W0p.md` status flip to closed, with commit hashes.

Cycle-2 idempotency, fresh expands, the fat-LTO 5-bench matrix,
samply per primary grammar, and `nm` on bench binaries route to
wave-specific close gates where peer-parity context is meaningful
(AY-II.W1.c JSON, W2 CSS, W3 Sheets, W4.e BBNF). Running them at
W0' close would double-pay walls for numbers the wave-specific
gates republish anyway.

## Hard gate (wave close, compressed-honest form)

1. `rg 'pub struct ValueBuilder|pub struct ValueBuilderOutput' crates/`
   → 0 matches.
2. `rg 'pub fn push_compound|pub fn mark_children' crates/tape/src/builder/`
   → 0 matches.
3. `cargo test -p bbnf --test projection_totality --release` green
   with runtime-call-count assertion.
4. `cargo test -p bbnf --test value_api_apples_to_apples --release`
   green on all 5 JSON fixtures + one smoke per grammar; no panic
   in `Parsed::to_value()`.
5. `cargo xtask regen` (full sweep) exits 0; idempotent re-run
   produces zero-line diff against the checked-in tree. (Replaces
   the pre-B2 cycle-1 = cycle-2 byte-identical gate; the xtask is
   the fixed point against the source tree itself.)
6. `rg '#\[allow(dead_code)\]' crates/core/src/backend/rust/emitter/
   crates/core/src/runtime/ crates/tape/src/` count strictly less
   than pre-W0' (every W0-era addition retires).
7. Post-W0' projection totality holds per AY-II.md invariant §7:
   `PROJECTION_DIRECT_TO_STRUCT.len() == materializer count ==
   production call-site count` per grammar.

## Verification artefacts

- W0' close entry in `PROGRESS.md` with commit SHAs + artefact paths.
- `crates/core/src/grammar/generated/<ident>.rs` per-grammar source
  (refreshed at cycle-1 regen).
- Per-sub-agent commit SHAs.
- Wave-specific close gates carry the deferred artefacts (fresh
  expands, fat-LTO bench matrix, samply, nm).

## Dependencies

- **Depends on**: W0 (partial) at master `a809d12f`.
- **Blocks**: W1.

## Archaeology

- W0 (AY-II) landed the emitter + tape + runtime scaffolding at
  commits `a13840a0…58271da1` with the W0-fix correction
  `f8ac2cd7` + `c9142405`. It did not close on its stated thesis —
  three substrate-without-consumer seams (value-builder write-side,
  projection-consumer wiring, scan-policy splice) were documented
  in the pre-close pause at `b5bbda6c` and confirmed by the 4-agent
  audit triumvirate at `baeed709` / `ad70effd` / `319c432a` /
  `a809d12f`.
- W0' is the idiomatic transposition AUDIT-C Q1 prescribed: Path B
  over Path A (thread explicit param) — Path A pays a ~500-LOC
  signature churn for no observable benefit. Path C (tape-walk
  projection) contradicts AY-II invariant §1.
- `f372e7ef` compose-bridge stays in history as transient per
  AUDIT-D Q7 (stub only lives at that commit; master HEAD regen
  overwrites); AUDIT-C's Q4 reasoning carries — history rewriting
  breaks predecessor audit SHA citations.

## Non-negotiables (sub-agent inherited)

- No stubs, no fallbacks, no feature flags, no `#[ignore]`, no
  `#[allow(dead_code)]` introduced to hide incomplete work.
- No un-wired legacy cruft. Every symbol whose role ended retires
  in the same commit that supersedes it.
- No dead IIFEs. Every retry-IIFE wrapper whose body simplifies
  under FusedBuilder collapse simplifies.
- No workarounds. Architectural transpositions for elegance +
  simplicity + performance are mandatory.
- One codegen path. One builder type. One projection path. One
  navigation path.

## d-lineage amendment (2026-04-22 retro-doc)

W0'.a / W0'.b / W0'.c (the original three-agent decomposition
above) returned at PAUSE SNAPSHOT. Six follow-on d-sub-phases
landed between 2026-04-21 and 2026-04-22 while W0' was
open. They are recorded here so the wave spec matches the
PROGRESS log.

| Sub-phase | Commit | Scope | Invariant discharged |
|---|---|---|---|
| d1 | `60f92743` | Test migration from `push_compound`/`mark_children` to FusedBuilder (tape tests, tape_walker_allocs, json-prototype visitor) | §15 public-API retirement evidence |
| d2 | (skipped; `4f4c9ec9` draft fix subsumed by d3) | — | — |
| d3 | `f768f50d` | O(1) `direct_child_count` in `value_end_compound`; replaces Θ(N²) recursive `subtree_size` path | W0p regen close precondition |
| d4 | `5c737bd1` | Gate gorgeous `#[derive(Parser)]` sites behind per-grammar cargo features | dev-loop infra; routes to B1 |
| d5 | `f5cdcd52` | Drop gorgeous as mandatory `bbnf` dev-dep | dev-loop infra; routes to B1 |
| d6 | `2e5e3ff5` | Narrow `crates/derive/build.rs` fingerprint scan to codegen-relevant subtrees | dev-loop infra; routes to B1 |
| d7 | `700501f5` | `.cargo/config.toml` `iter-check` alias excludes gorgeous + bbnf-bootstrap + bbnf-analysis + bbnf-lsp; `iter-check-full` retains `--workspace` for CI | dev-loop infra; routes to B1 |

d4-d7 touch `.cargo/config.toml` + `Cargo.toml` files — the
W0 `Do NOT touch` list in `waves/W0.md:75-81` declares those
out-of-bounds. The pivot is recorded here; the formal
re-audit of d4-d7 is B1.W0's scope
(`docs/tranches/B1/waves/W0.md`). W0' does not close on the
correctness of d4-d7; B1 does.
