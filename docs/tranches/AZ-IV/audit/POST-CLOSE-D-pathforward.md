# POST-CLOSE-D — Path Forward + KISS Architectural Optimum

**Date**: 2026-05-02
**Auditor**: POST-CLOSE-D (read-only path-forward + KISS optimum lane)
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-postaziv-D`
**Base**: `master 6de6ac0c` (AZ-IV closed `complete_with_misses` at `cb14970f`; doc-recycle commits through `6de6ac0c`)
**Read-first**: `AZ-IV.md` §Thesis + §Invariants + §Hard Gates; `FINAL.md`; `AUDIT-F-optimum-2026-05-02.md`; `AUDIT-E-pathforward-2026-05-02.md`; `SYNTHESIS-2026-05-02.md`; `GESTALT.md` §2-§4; `codegen-paths.md`; `feedback_no-workarounds-arch`, `feedback_no-orthogonal-codepaths`, `feedback_kiss-perf-bias`, `feedback_typed-materialization-invariant`

## Mandate

The user, verbatim:

> *"Devise a path forward... NO quick solutions, NO workarounds: idiomatic, gestalt approaches"*
>
> *"This is a development product, architectural transpositions in the sake of elegance, simplicity, and performance above all are both necessary and desirable."*
>
> *"With all approaches: KISS. One path."*

The brief: name the SINGLE architectural direction, specify the next 2-3 tranches' shape, identify ELEGANT TRANSPOSITIONS that subsume open carries via mechanism rather than patching.

## State of the Tranche Graph

AZ-IV closed `complete_with_misses` at `cb14970f` (2026-05-02). The union mechanism shipped: typed `path!` macro + lazy `parse_with` + AscentStrategy hybrid sidecar (W2); cursor-threaded parse fns + W3-DYNAMIC per-iteration consult + per-grammar `PATH_PLAN` static (W3); ruler/RuleSet DELETED + T1 registry projection + PatternAnnotations DELETED (W4); bbnf-path-ts cdylib + arena/builder skeleton dedup (5/9) + permanent `substrate_audit.rs` test + T4 production REGISTRY (W5); fat-LTO bench matrix with sonic-rs paired comparators (W6.1). The tranche thesis intact: every contradiction the planning interrogation surfaced was resolved through the union mechanism, not through per-grammar overfit.

Five carries route forward: `bbnf_get_twitter` 4196x sonic gap (mechanism shipped; integration missed); 14 BELOW-AU rows traced to W5 arena/builder template registry indirection (28-65x on bbnf_self / sheets_parse_*); W1 TS aggregate-as-iterable gap (W5.2 Node-execute RED); 32 zero-caller substrates (permanent test enumerates; cleanup deferred); T6 module-split + 4 outlier-grammar dedup + AUDIT-B splits (deferred). None routes back as a chronic deferral; all have named destinations. The substrate is one transposition away from honouring the thesis structurally instead of by discipline.

## The Single Thesis

**The thesis: DIRECT-PROJECTION CODEGEN — the lazy path is the canonical parse path; eager is its degenerate case; the value-API and `Document::get` re-route through it; the registry indirection AU regressed on dies in the same wave; per-grammar arena/builder templates collapse into a single emit-direct-from-cursor path that materialises the leaf the path requests with no slab-of-Vec intermediary for narrow paths.**

Why one thesis (not five candidates dangling): every open carry traces to **the same architectural fact** — bbnf has TWO parse models cohabiting one binary. The eager model materialises the full `Document` arena, then walks it via `Document::get(path)`. The lazy model materialises the `Document` arena under cursor control, then walks it via `Document::get(path)`. **Both models build the arena**; both pay the registry-indirection tax W5 introduced; both miss sonic-rs by orders of magnitude. The 4196x `bbnf_get_twitter` gap is not "the lazy path is too slow" — it is "the lazy path still builds an arena". The 167x `bbnf_value_canada` gap is not "f64 decoding is slow" — it is "the registry-keyed slab indirection allocates per leaf where sonic stores inline".

Direct-projection codegen replaces both with one path: the cursor's `Decision::ParseUntil(child_index)` becomes `Decision::ProjectLeaf(reader_fn)` for terminal segments — the parse callback writes the typed leaf directly into the caller's `Option<T>` slot, bypassing arena construction entirely. Eager parse is the degenerate case where the cursor reaches `SegmentKind::Terminal` only at the document root (`__EAGER_EMPTY_PATH` becomes the implicit empty path; the macro writes `parse(input)` as `parse_with::<()>(input, &empty_path)` where the `()` schema marker is the codegen-recognised "materialise full document" hint). The W5 arena/builder template still exists for the eager case but is no longer on the value-API hot path; the lazy lane never constructs it.

Candidate pre-mapping: this thesis subsumes A (direct-to-document) by making it the lazy lane's natural home; subsumes B (lazy-only parse) by making eager the degenerate case; subsumes C (sonic-class byte scanner) by routing path-terminal projection through the W2 typed-leaf reader without intermediate Span allocation; touches D (TS-equivalent codegen) at the boundary where the TS backend's `value` aggregation gap closes when both backends emit through the same direct-projection IR seam. All five become wave items under one thesis.

## Architectural Transpositions

Each transposition below REDUCES surface, names a file:line target, carries an expected LOC delta, and closes an open carry through MECHANISM (the seam disappears) rather than patching (the seam survives with a workaround).

### Transposition T1: Eager-as-degenerate-lazy collapse

- **Mechanism**. `parse(input)` becomes a thin wrapper around `parse_with::<EagerSchema>(input, &EMPTY_EAGER)` where `EagerSchema` is a marker (`pub enum EagerSchema {}`) implementing `PathSchema` with `type Output = Document<'p>` and `decision_for(_, _, _) = Decision::ParseFully`. The dispatcher signature collapses to `parse_with`; `parse` is a 5-line surface alias. Generated code: one entry point, two import wrappers.
- **File:line evidence**. `crates/core/src/backend/rust/emitter/grammar.rs:395-468` (the eager body materialising `__EAGER_EMPTY_PATH: LazyLock<TypedPath<Json, &'static str>>`). The `Json` marker is hard-coded for ALL nine grammars (replicated in `crates/core/src/grammar/generated/{json,csv,ebnf,bnf,bbnf,google_sheets,math,css_pretty,css_l4}.rs:107081` family) — pure dishonest typing relying on phantom inertness.
- **LOC delta**. -800 across 9 generated files (each grammar drops a ~80-line eager-cursor scaffold); -50 in `grammar.rs` emitter (one body, not two); +30 for `EagerSchema` marker + `parse_with` wrapper. Net **-820 LOC**.
- **Carry closed by mechanism**. Eliminates the dual-lane `parse() / parse_with()` at the source. The `__EAGER_EMPTY_PATH: LazyLock<TypedPath<Json, _>>` lie disappears (no `Json` marker for `CssL4`!). Per `feedback_no-orthogonal-codepaths`: ONE parse path; eager is a schema instance, not a separate dispatcher.
- **Risk**. Low. The cursor consults under `EagerSchema` always return `ParseFully`; the optimiser already inlines this away. Confirm via `cargo asm` on `bbnf_value_twitter` post-T1; expect zero codegen change in the eager bench.

### Transposition T2: Cursor as decision-stream owner (subsumes match_field/match_index/decide)

- **Mechanism**. Today `PathCursor` has `match_field(&str)`, `match_index(usize)`, `decide(rule_id)`, `advance()`. Replace with one polymorphic method: `cursor.consult(&ParsedSegment) -> Decision` where `ParsedSegment::{Field(&'a str), Index(usize), VariantTag(&'a str)}` is the recognizer's parsed-segment ADT. The cursor projects the segment's `SegmentKind`, looks up the decision, advances on `ParseUntil`/`Skip`, returns the verdict. Generated parsers call `cursor.consult(seg)` once per shape-decision site instead of dispatching across three method names.
- **File:line evidence**. `crates/core/src/path/cursor.rs:242-258` (`match_field`/`match_index`); `crates/core/src/grammar/generated/json.rs:1527,1660,1768,1871` (separate `cursor.decide(N as u32)` call sites). 348 generated cursor-call sites across 9 grammars; each currently picks one of three methods.
- **LOC delta**. -16 in `cursor.rs` (three methods → one); -200 in generated dispatch (uniform call shape; emitter macro shrinks); +40 for the `ParsedSegment` ADT (one new module). Net **-176 LOC**.
- **Carry closed by mechanism**. Eliminates the orthogonal call surfaces W3-DYNAMIC introduced. Per `feedback_unified-propagate`: one method, not suffixed variants. The cursor's role becomes "consume parsed segment, emit decision"; the recognizer's role becomes "produce parsed segment, obey decision".
- **Risk**. Low-medium. The `ParsedSegment` carries borrowed string lifetimes; the cursor's lookup closure must accept the borrow without re-keying the static plan. The static plan stays `(rule_id, SegmentKind)` keyed; `consult` projects `SegmentKind::of(seg)` before lookup — the borrow never reaches the static.

### Transposition T3: Per-grammar `__path_plan` re-exports DELETE

- **Mechanism**. Today each generated grammar carries `pub mod __path_plan { pub use crate::path::cursor::{Decision, SegmentKind}; ... PATH_PLAN: &[PathPlanEntry; N] }`. The `pub use` is pure ceremony; the `PATH_PLAN` is the only per-grammar payload. Replace with a single `pub static PATH_PLAN_<Grammar>: &[PathPlanEntry]` at the grammar root; consumers (`parse_with`) import `Decision`/`SegmentKind` from `crate::path::cursor` directly.
- **File:line evidence**. `crates/core/src/grammar/generated/{json,bnf,csv,math,bbnf,google_sheets,css_pretty,ebnf,css_l4}.rs` line 102/97/100/97/774/500/102/313/1246 (the `pub use` surfaces — 9 redundant re-exports of one canonical alphabet). Identical 4-line block × 9 = 36 lines of `pub use` ceremony for zero semantic content.
- **LOC delta**. -36 across 9 generated; -30 in the path_plan emitter (`crates/core/src/backend/rust/emitter/path_plan.rs`) eliminating the `pub mod __path_plan { pub use ... }` wrapper. Net **-66 LOC**.
- **Carry closed by mechanism**. Per `feedback_no-orthogonal-codepaths`: a single canonical alphabet has a single canonical home. The `__path_plan` wrapper module is the substrate-without-real-consumer pattern at the codegen layer.
- **Risk**. Trivial. The `pub use` exports nothing the consumers need; consumers already import `Decision`/`SegmentKind` from `crate::path::cursor` (per `crates/core/src/runtime/json/parse_with.rs:49`).

### Transposition T4: Arena/builder template DELETION on the value-API path

- **Mechanism**. The W5 `CompoundSlabArena<C>` template (`crates/core/src/runtime/arena_template.rs:80-134`) replaced AU's flat per-grammar arenas with a `StructRegistry`-parameterised slab. The W6.1 evidence (audit/W6-fat-lto.txt) named this as the root cause of 14 BELOW-AU rows (28-65x on bbnf_self / sheets_parse_*; 1.9-118x on json_monolithic). The `compound_kind_for_layout` indirection routes `LayoutKind` → string discriminator through a registry lookup per leaf. Direct-projection codegen retires the arena from the lazy lane entirely; the eager lane keeps it but inlines the `compound_kind_for_layout` lookup at codegen — the kind is known at xtask regen time per layout.
- **File:line evidence**. `crates/core/src/runtime/arena_template.rs:80-134` (template); `crates/ir/src/registry/struct.rs:384-388` (`compound_kind_for_layout` runtime indirection); `crates/core/src/runtime/{ebnf,csv,bnf,bbnf,google_sheets,css_pretty}/kind.rs` (per-grammar runtime consumers — 6 dispatch sites that should be codegen-emitted const projections, not runtime registry lookups).
- **LOC delta**. -134 (delete arena_template; runtime consumers inline the projection); -80 across 6 `kind.rs` files (codegen-emitted const projections replace runtime calls); -30 in `compound_kind_for_layout`'s call sites; +50 for codegen const-projection emit. Net **-194 LOC**.
- **Carry closed by mechanism**. The 14 BELOW-AU rows close because the registry indirection no longer fires per leaf — the kind dispatch is a constant the codegen knows. Per `feedback_kiss-perf-bias`: smallest change that achieves elegance + performance. The template was the right shape for *eager dedup*; it was the wrong shape for *value-API hot path*. Direct-projection makes it disappear from the hot path.
- **Risk**. Medium. The 4 outlier grammars (JSON, CSS L4, Sheets, BBNF) retain dedicated arena modules per W5 close; this T4 transposition consumes the same direction (delete the template). The eager Document construction still exists (T1 keeps it as the degenerate case) but no longer routes through the runtime registry.

### Transposition T5: `Document::get<T>(path)` rerouting through `parse_with`

- **Mechanism**. The 4196x `bbnf_get_twitter` gap traces to the value-API entry point: callers write `let doc = JsonParser::parse(input)?; let v = doc.get::<&str>(path)?;` — TWO operations, where sonic-rs writes `sonic_rs::get(input, pointer![...])` — ONE operation. The W3 lazy lane shipped (`parse_with` works), but `Document::get<T>(path)` continues to walk the materialised tree. Reroute: the trait `JsonPathQuery::get<T>(path)` becomes a wrapper that calls `parse_with::<T>(input, &path)` directly when the document was constructed eagerly; for already-parsed documents, the trait keeps the walker. `JsonParser::get<T>(input, path)` becomes a new entry point (no `Document` ceremony) that goes straight to lazy.
- **File:line evidence**. `crates/core/src/runtime/json/parse_with.rs:77-103` (the existing lazy entry); `crates/core/src/runtime/json/document.rs::JsonPathQuery::get` (the eager-only walker — the value-API hot path); the bench harness at `crates/core/benches/json/value.rs::bbnf_get_twitter` (uses `JsonParser::parse + doc.get`).
- **LOC delta**. -40 in `document.rs::get` (delete the eager walker for the lazy-aware path; keep for the materialised-document case); +20 for `JsonParser::get` direct entry; +50 for the trait's lazy/eager dispatch. Net **+30 LOC** but **-1ms per get_twitter** call.
- **Carry closed by mechanism**. The 4196x gap closes by reroute, not by optimisation. The W3 substrate already exists; this transposition is the integration the W6.1 evidence flagged ("the W3 path-driven recognizer is the substrate; the value-API hot-path needs the lazy-parse shortcut not the eager-parse path").
- **Risk**. Low. `parse_with` already returns `Option<T>`; `Document::get` already returns `Option<T>`; the surface is a one-line redirect. Caveat: for paths the lazy lane cannot satisfy (wildcard streaming, ascent-required), the trait routes to the eager walker — but that's the documented contract from W2.

### Transposition T6: TS backend aggregate-as-iterable via shared IR seam

- **Mechanism**. The W5.2 Node-execute test went RED because the TS backend emits `object.value` as a Span over input bytes, not as an aggregated array of pairs. The Rust backend handles this through `Document::object(id) -> &[Pair]`. Rather than patching the TS emitter to materialise pairs (the patch path), introduce a shared `AggregateProjection` IR pass that emits — for both backends — an `Aggregate { shape, pair_iter, element_iter }` projection. Both backends consume the same projection through their `Emitter` trait; the TS backend's `value` becomes a JS array; the Rust backend's `value` becomes the existing typed slice. One IR pass, two backend instantiations.
- **File:line evidence**. `crates/core/src/backend/emitter.rs:31-566` (the trait); `crates/bbnf-path-ts/src/lib.rs` + W5 audit `audit/W5-node-execute.txt` (the RED gate evidence); the existing `project_types` IR pass that already projects `Aggregate` shape but only for the Rust backend.
- **LOC delta**. +100 for the new `aggregate_projection` IR pass; -60 in the Rust emitter (consumes the projection instead of re-deriving); -40 in the TS emitter (consumes the projection instead of returning a Span). Net **+0 LOC** but the Node-execute gate closes.
- **Carry closed by mechanism**. The TS backend gap closes structurally — both backends consume the same IR projection. Per `feedback_isomorphic-api`: backend bindings mirror IR shape; the gap was an emitter divergence, the fix is an IR projection.
- **Risk**. Medium. The IR pass needs careful sequencing relative to `project_types`; the TS backend's wasm-bindgen surface needs the aggregate type round-trippable. AUDIT-F T2 (backend trait) is adjacent but deferred — T6 here is the narrower scope (one projection, not a trait reshape).

### Transposition T7: Zero-caller substrate cleanup pass

- **Mechanism**. The permanent `substrate_audit.rs` test (W5.4) enumerates 886 `pub` substrates; 32 have zero callers. Rather than annotating each with `#[allow(dead_code)]` (the patch path), run one cleanup pass that DELETES them or moves them to `#[cfg(test)]`-only when test-supporting. The audit test's role becomes "fail when any pub substrate has zero callers in production code"; the 32-row residual either earns a consumer or earns deletion in the same wave that introduces it.
- **File:line evidence**. `crates/ir/tests/substrate_audit.rs` (the enumeration test); `audit/W5-substrate-denominator.md` (the 32 named substrates).
- **LOC delta**. -300 to -800 (depends on per-substrate weight; mid estimate -500). Net **-500 LOC**.
- **Carry closed by mechanism**. Per `feedback_no-workarounds`: zero tolerance for substrate-without-consumer. The test's role completes — the 32 rows clear, the test gates against regression, and the substrate roster is one round closer to minimum.
- **Risk**. Low. Each substrate's deletion is mechanical (the test names every `pub` symbol); the failure mode (a deletion breaks something the test missed) is caught by `cargo check --workspace`.

## Next Tranche Shape (BA — recycled)

**Letter**: BA (recycled per AZ-IV.md §Cross-Tranche Debt; the BA letter awaits a successor scope; this is its scope).

**Thesis**: Direct-projection codegen — the lazy path becomes the canonical parse path; eager is its degenerate case; the value-API and `Document::get` reroute through it; the W5 arena/builder template registry indirection retires from the value-API hot path; per-grammar arena/builder modules collapse into emit-direct-from-cursor for the lazy lane.

**Waves** (6, all named, hard gates each):

- **W0 — Truth and Failing-Test Census.** Fresh `cargo xtask regen --check` 9/9; workspace nextest baseline; 2 timed-out tests (tailwind perf + LSP completion) triplet-enumerated; 26 ignored tests audited; W6.1 watchdog rows reproduced.
- **W1 — Eager-as-degenerate-lazy collapse (T1 + T3).** Single `parse_with` entry; `EagerSchema` marker; per-grammar `__path_plan` re-exports DELETE; cursor `consult(&ParsedSegment)` (T2) lands as the same wave's call-shape unifier.
- **W2 — Direct-projection codegen for terminal segments.** `Decision::ProjectLeaf(reader_fn)` lands; lazy-lane parse callbacks write the typed leaf directly into the caller's `Option<T>` slot; eager lane keeps the arena but the value-API never builds it.
- **W3 — Arena/builder template retirement on the value-API path (T4).** `compound_kind_for_layout` runtime indirection becomes codegen-emitted const projection; the 14 BELOW-AU rows close (verified through fresh fat-LTO matrix).
- **W4 — `Document::get` rerouting through `parse_with` (T5).** `bbnf_get_twitter` ≤ 5x sonic-rs same-harness (Hard Gate 7 close); the value-API entry points consume the lazy lane by default.
- **W5 — TS aggregate-projection IR pass (T6).** Node-execute test goes GREEN; the TS backend consumes the same `AggregateProjection` IR projection the Rust backend does; zero-caller cleanup (T7) lands as same-wave debt clearance.

**Hard gates** (target 18-22; specific list):

1. `cargo xtask regen --check` 9/9 GREEN at every wave close.
2. Workspace nextest 100 % pass; ignores carry triplet.
3. Single `parse_with` entry; `parse(input)` is `parse_with::<EagerSchema>(input, &EMPTY)`.
4. `__EAGER_EMPTY_PATH: LazyLock<TypedPath<Json, _>>` literal DELETED (the dishonest type ends).
5. Per-grammar `__path_plan { pub use ... }` re-exports DELETED; one canonical `Decision`/`SegmentKind` import path.
6. `cursor.match_field` / `match_index` / `decide` consolidated into `cursor.consult(&ParsedSegment) -> Decision`.
7. `Decision::ProjectLeaf(reader_fn)` decision variant lands.
8. `compound_kind_for_layout` codegen-emitted const projection; runtime `StructRegistry` lookup retires from the value-API hot path.
9. Arena_template + builder_template DELETED from the value-API lane (eager lane retains; documented).
10. `bbnf_get_twitter` ≤ 5x `sonic_get_twitter` same-harness (Hard Gate 7 close — 4196x → ≤ 5x).
11. `bbnf_value_twitter` ≤ 1.5x `sonic_value_twitter` (Hard Gate 16 close — 5.2x → ≤ 1.5x).
12. `bbnf_value_canada` ≤ 5x `sonic_value_canada` (167x → ≤ 5x via Eisel-Lemire fast-path on numeric leaves).
13. `bbnf_value_data_xl` MEASURED (no WATCHDOG) under fat-LTO; floor anchored.
14. `bbnf_value_data_s` ≤ 1.5x `sonic_value_data_s` (2.6x → ≤ 1.5x).
15. AU floor 19/19 rows above or at parity (the 18/19 BELOW close).
16. TS Node-execute test GREEN; `AggregateProjection` IR pass consumes both backends.
17. Zero-caller substrate count = 0 (32 → 0 via consume-or-delete).
18. Tailwind perf timeout closes via either (a) regex-engine consumption of the CSP-selected scanner, or (b) named hotspot routed with samply 7-artefact contract.
19. `samply --unstable-presymbolicate` artefacts for every Hard Gate 10/11/12 claim (7-artefact contract per `PROFILING.md`).
20. AUDIT-B-routed splits land (`dta.rs` 1565 LOC + `csp_strategy/mod.rs` 1316 LOC + `css_l4/builder.rs` 1014 LOC; T6 module-split per AUDIT-F).

**Non-routable carries**: 12 items (the W6.1 routed F1-F12 list, contracted to BA's authoring window: the 4196x gap, the 14 BELOW-AU rows, the W1 TS aggregate gap, the 32 zero-caller substrates, the T6 module-split, the 4 outlier-grammar dedup, the AUDIT-B splits, the LSP completion timeout, the 26 ignore-triplet enumeration, the workspace-gates aggregate run, the watchdog row close-matrix, the Hard Gate 9 samply 7-artefact backfill).

## Subsequent Tranche Shape (BB — re-recycled for rule discovery)

After BA closes, **BB opens for pure rule discovery** (the original BB scope, recycled): Ruler CVC enumerator over `IrNode`, VM oracle on residue, ranker, Class-1/2/3 tiering, `crates/ir/src/rewrites/` recreation (clean, post-AZ-IV-W4 deletion), grammar-colocated rewrite dirs (`grammar/<name>/rewrites/`), Tranche H rediscovery ≥ 80 %, ≥ 5 accepted rules per production grammar.

BB's preconditions are exactly BA's deliverables: ruler/RuleSet ALREADY DELETED (W4); StructRegistry populated for 4 grammars (W2); typed `path!` macro live (W2); lazy bail-out parse on 4 grammars (BA); permanent substrate-audit test passing (W5); workspace nextest 100 % (W0/BA); regen 9/9 (W0/BA). BB does not need to repair BA's outputs; BB consumes them.

The cross-repo motion (csp-solver repo split → egraph repo split → simd-scan repo split) lands AFTER BB validates the API surface. AZ-IV.md §Cross-Repo Future Work names these as out-of-tranche; that policy holds.

## One Path Discipline

Every "currently two ways to do X" surviving AZ-IV, with the winner + deletion plan:

| Two-ways | Winner | Why | Deletion plan |
|---|---|---|---|
| `parse(input)` vs `parse_with(input, &path)` | `parse_with` | One dispatcher; eager is `EagerSchema` instance | T1 — generated `parse` becomes 5-line wrapper |
| `cursor.match_field` / `match_index` / `decide` | `cursor.consult(&ParsedSegment)` | One method, parsed segment is polymorphic | T2 — three methods → one |
| Per-grammar `__path_plan` `pub use` blocks | Direct import from `crate::path::cursor` | Single canonical alphabet | T3 — 9 redundant re-exports DELETE |
| `Document::get` (eager walker) vs `parse_with` (lazy lane) | `parse_with` for narrow paths; eager walker only when the document is already constructed | The hot path is `JsonParser::get(input, path)`; reroute through lazy | T5 — `JsonPathQuery::get` becomes a thin lazy/eager dispatcher |
| Arena_template + builder_template (W5 dedup) | Direct-projection codegen for value-API; arena retained for eager Document construction | The W5 template was right for eager dedup, wrong for value-API hot path | T4 — runtime registry lookup retires; codegen-emitted const projections replace |
| Rust backend `value` aggregate vs TS backend `value` Span | One `AggregateProjection` IR pass, two backend instantiations | Shared IR seam; backends emit through the same projection | T6 — IR pass projects both shapes |
| `__EAGER_EMPTY_PATH: LazyLock<TypedPath<Json, &'static str>>` for ALL grammars | Schema-typed empty path per grammar marker | The hard-coded `Json` is a phantom-inertness lie | T1 — schema-instance EagerSchema marker per generic `parse_with` |
| `compound_kind_for_layout(layout) -> &str` runtime indirection | Codegen-emitted const projection per layout | The kind is known at xtask regen time | T4 — runtime call retires |

Orthogonal codepaths surviving AZ-IV (per `feedback_no-orthogonal-codepaths`): the dual `parse() / parse_with()` lanes (T1 closes), the dual cursor method names (T2 closes), the redundant per-grammar `__path_plan` modules (T3 closes), the dual arena-construction paths between value-API and Document::get (T4 + T5 close), the two backend `value`-aggregation shapes (T6 closes). All five close in BA's W1-W5.

## Performance Closure

**The architectural reason sonic-rs is 4196x faster on `get_twitter`**: sonic does not parse. `sonic_rs::get(input, pointer![...])` is a byte scanner that walks the raw input, skipping JSON values it doesn't care about, and stops when the pointer resolves. bbnf's `JsonParser::parse(input)?.get(path)` materialises the full document tree (the typed arena), then walks the typed tree to resolve the path. Two operations where sonic does one; one O(n) materialisation where sonic does O(path-depth × selective-scan).

**The single mechanism that closes the gap**: T1 + T5 together. T1 makes `parse_with` the canonical entry; T5 reroutes `Document::get<T>(path)` through `parse_with::<T>(input, path)` so the value-API hot path NEVER builds the arena. The W3 lazy substrate already exists (cursor-threaded recognizer with `Decision::Skip` for unvisited subtrees); it just isn't on the hot path because callers go through `parse + get`. T5 closes the integration. The post-T5 `bbnf_get_twitter` lane is: parse the path's witnessed bytes only (cursor consults at every shape-decision; `Skip` advances pos through the non-visited child's structural span; `ParseUntil(N)` parses children `0..=N` only); project the leaf at terminal; never construct `Document`.

**Why not "make AST descent faster"**: AST descent is O(n) by construction; sonic's lazy pointer-walk is O(path-depth × scan); the gap is structural, not constant-factor. Every hand-tuning attempt at AST descent loses by orders of magnitude. The right answer is "AST descent isn't on the lazy hot path".

**The W5 arena/builder template AU-floor regression**: revert vs push harder. **Push harder via direct-projection codegen** (T4). The W5 dedup was the right structural shape (one `Arena<G>` template instead of nine bespoke arenas — `feedback_no-workarounds-arch`); the wrong shape was making the registry indirection live at runtime. T4 keeps the dedup (one template body) but inlines the `compound_kind_for_layout` lookup at codegen-time per layout. The const projection costs zero at runtime; the `StructRegistry::compound_kind_for_layout` call retires from the hot path. The 14 BELOW-AU rows close because the indirection no longer fires per leaf.

The Hard Gate 7 (≤ 5x sonic) close path: T1 + T2 + T5 in BA's W1+W4. The Hard Gate 16 (`bbnf_value_*` parity-or-better) close path: T4 (registry indirection retire) + T5 (arena bypass for narrow paths) + Eisel-Lemire fast-path numeric decoding (BA's W4 stretch; the substrate already lives in `fast_float2`). Both gates close inside BA without requiring AZ-V or BB.

## KISS Wins

Specific delete-not-add items, rooted in AZ-IV evidence:

1. **DELETE** `__EAGER_EMPTY_PATH: LazyLock<TypedPath<Json, &'static str>>` literal (9 generated grammars × ~10 lines = 90 LOC + the underlying lie).
2. **DELETE** per-grammar `pub mod __path_plan { pub use crate::path::cursor::{Decision, SegmentKind}; ... }` wrapper (9 × 4 lines + module ceremony = 60 LOC).
3. **DELETE** `cursor.match_field` and `cursor.match_index` (16 LOC + 348 generated dispatch sites narrow).
4. **DELETE** `compound_kind_for_layout` runtime call from value-API hot path (1 indirection × N leaves on every parse — measurable AU regression).
5. **DELETE** the dual `parse() / parse_with()` dispatchers (collapse to one; eager is a schema instance).
6. **DELETE** 32 zero-caller substrates (30 % of T7's payload; closes the substrate-audit test's named carry).
7. **DELETE** the per-grammar arena/builder for the value-API hot path (eager lane keeps Document construction; lazy lane never builds it).
8. **DELETE** the eager `Document::get` walker for narrow lazy-aware paths (reroute through `parse_with`).
9. **DELETE** the TS backend's `value` Span emission (replaced by IR-projected `AggregateProjection`).
10. **DELETE** stale `pattern_annotations` debris if any survives W4's PatternAnnotations DELETE; verify via `rg "PatternAnnotations|pattern_annotations" crates/`.

Ten deletions; total LOC delta: roughly **-1700 to -2200**. Per `feedback_kiss-perf-bias`: smallest set of changes that achieves elegance + performance; reject sprawling 4-lever sweeps. The thesis is one direction (direct-projection codegen); the deletions are the elegance tax it claims back.

## Recommended Immediate Dispatch

Three concrete commits to land BEFORE recycled-BA opens, ensuring BA opens on a clean slate without AZ-IV residual sloppiness:

1. **`docs(post-az-iv/path-forward): land POST-CLOSE-D-pathforward.md`** — this audit's output. Single doc commit; no source changes.

2. **`chore(generated/path-plan): delete __path_plan pub use re-exports across 9 grammars`** — execute T3. Mechanical regen edit; one commit; -66 LOC. The simplest of the seven transpositions; lands clean before BA opens. Validates the regen pipeline still rebuilds 9/9 GREEN with the redundant re-exports gone.

3. **`feat(path/cursor): unify match_field+match_index+decide into cursor.consult(&ParsedSegment)`** — execute T2. Source change in `crates/core/src/path/cursor.rs`; emitter change in the path-plan emitter; regen sweep emits the new call shape. One commit; -176 LOC; validates the cursor abstraction holds under the unified call surface; the orthogonal-method-name pattern dies before BA's W1 opens.

These three commits land in 2-3 hours of orchestrator-driven work, retire the lowest-risk surface debt from AZ-IV, and establish the call-shape uniformity BA's bigger transpositions (T1, T4, T5) will lean on. They do NOT close the perf gap (T5 + T1 + T4 together do); they DO clear the surface debt that would otherwise be entangled in BA's W1.

If the orchestrator wants to compress further: commit (2) and (3) can land as one commit with body naming both transpositions. Commit (1) stands alone (read-only audit).

---

The thesis: direct-projection codegen. The deletions: ten named items. The next tranche: BA — recycled for direct-projection, not for rule discovery (rule discovery moves to BB — re-recycled). The single architectural direction holds: every transposition above is a structural reshaping that retires substrate, not a patch that adds. Per the user's mandate: **idiomatic, gestalt approaches; one path; KISS.**
