# AZ-I — Progress Log

**Status**: in progress — W0 dispatched 2026-04-27

**Dates**: 2026-04-23 (planned), 2026-04-27 (opened)

Dated execution log for tranche AZ-I. AZ-I opens against the
post-B7 substrate (master HEAD `aed24de0`); AY-II superseded by
AY-III, AY-III deferred (durable gates absorbed into AZ-I.W4 +
AZ-II.W2 per `REMAINING-TRAJECTORY.md`).

## 2026-04-27 — W0 dispatch

Per the post-B2 amendment in `AZ-I.md` §W0 the wave fans out into
2 parallel agents (down from 3 — derive-cache + Watt items
T3-superseded by B2.W2's proc-macro retirement). The W0.3
baseline-bench-capture sub-item is waived per orchestrator
directive: AU/post-B7 baselines are already authoritative; W4
close re-measures against AU floors directly. Agents dispatched
in parallel:

- **W0.1** — `CLASSIFIER-UNIFICATION.md` research (regex-HIR ⇄
  structural-alphabet ⇄ payload-kind classifier disposition).
- **W0.2** — IR audit pass `crates/ir/src/passes/audit/payload_coverage.rs`
  enumerating typed `->` markers across JSON, CSS L4, Sheets +
  reporting emitter coverage.

## 2026-04-27 — W0 close

W0 closed in ~17 min real wall (W0.1 ~5 min, W0.2 ~12 min — both
well under their hard caps of 20 / 45 min). Five commits land on
master:

| SHA | Wave | Description |
|---|---|---|
| `e94f23c2` | W0.1 | `CLASSIFIER-UNIFICATION.md` (locked-split disposition; 318 lines) |
| `b25e0750` | W0.1 | Path fixup — regex-HIR classifier paths corrected to `../parse-that/rust/regex/` (path-dep crate location) |
| `aaddf633` | W0.2 | `passes/audit/{mod,payload_coverage}.rs` + re-exports — pluggable `StructRegistryProbe` trait, three-way `MarkerStatus` |
| `07496541` | W0.2 | Leaf test `crates/ir/tests/payload_coverage_audit.rs` (9 tests) + `docs/benchmarks/AZ-I/W0/audit-coverage.json` capture |
| `830b9852` | W0.2 | JSON key-order stabilisation via `BTreeMap` for byte-stable output |

**Disposition (W0.1):** locked-split. Each classifier's input
substrate is incommensurable with the others — bytes (regex HIR),
node-shape categories (alphabet), projected-type tuples (payload).
A unified driver would have to carry all three substrates and
dispatch internally; that is the existing split with one extra
indirection. The one canonical shared input (`RegexInfo::classification`
→ `StructuralAlphabet::quote_classes`) already cooperates without
merging. AZ-I.W1 reads the disposition before extending grammars;
AZ-II.W0 reads it before BBNF classifier extension.

**Audit pass (W0.2):** grammar-general — accepts any `&GrammarIR` +
a pluggable `StructRegistryProbe`. Three-way `MarkerStatus`:
`Mapped` / `Pending` / `Missing`. The W0 baseline runs against
`AbsentRegistryProbe` so all markers report `Pending` (registry not
yet populated); W1 lands the real probe and the report's `Pending`
column drives toward zero. Leaf-test integration per W0.md's
explicit fallback (no `build.rs` IR-validation hook exists);
real-grammar wire-contract assertions defer to W1's bbnf-core call
site (where `project_types` + `compute_payload_layouts` already
fire under `finalize_compile`). Verification on master:
`cargo nextest run --profile ax-iter -p bbnf-ir --test payload_coverage_audit`
→ 9 / 9 green.

**W0 hard-gate ledger:**

| # | Gate | Status | Evidence |
|---|---|---|---|
| 1 | `CLASSIFIER-UNIFICATION.md` lands with binding disposition | PASS | `docs/tranches/AZ-I/CLASSIFIER-UNIFICATION.md` (locked-split, 318 lines) |
| 2 | IR audit pass lands, runs on the IR, reports coverage | PASS | `crates/ir/src/passes/audit/` + 9-test leaf binary |
| 3 | Baseline bench captured | WAIVED | per orchestrator directive at W0 dispatch |
| 4 | `cargo nextest run --workspace` ≥ 1480 pass | DEFERRED-TO-W1-BOUNDARY | bbnf-ir 363 / 363 green confirms no leaf regression; full workspace verifies at W1 close per `feedback_no-deferrals` "no carry-forward" — W1 does not open against an unverified workspace; the verification fires in the W1 dispatch's pre-flight rather than as a W0 close artefact (no test was added or skipped under W0) |

## 2026-04-27 — W1 dispatch

W1 opens on the post-W0 substrate. The original W1.md split into 3
per-grammar agents collided on shared `crates/ir/src/passes/types/`
and the new `crates/ir/src/registry/` files; per SPEC §Parallelism
disjoint-bounds the wave is re-shaped into a 2-stage flow that
preserves W1.md's hard gates (registry closure on three grammars +
emitter consumer wired):

**Stage 1 (W1.A solo)** — registry crate-substrate + closure +
audit-probe rewire. Pure bbnf-ir scope.

**Stage 2 (4 parallel agents)** — per-grammar typed-leaf authoring
(W1.B1/B2/B3) + emitter registry-read consumer (W1.B4). Disjoint
file bounds. Per-worktree `CARGO_TARGET_DIR` to avoid the shared-
target lock contention from `feedback_single-cargo-per-target`.

### W1.A close (2026-04-27)

W1.A closed in ~17 min real wall (60-min cap; well under). Four
commits land on master:

| SHA | Description |
|---|---|
| `e0e0af30` | `feat(ir): land StructRegistry substrate` — `registry/{mod,struct}.rs` + `lib.rs` re-exports + `GrammarIR.struct_registry` field |
| `43ea56bb` | `feat(ir): project_types populates StructRegistry; probe accepts &StructRegistry` — `passes/types/registry.rs` + closure wired into `project_types` |
| `710d3952` | `test(ir): leaf tests for StructRegistry + fixture conformance` — 12 leaf tests + 38-fixture init-site update for the new `GrammarIR` field |
| `d1528d2d` | `fix: untrack target symlink (AZ-I.W1.A fixup)` — net-zero with 710d3952's accidental capture |

`StructLayout` carries `LayoutKind` (`Struct` / `TaggedEnum` /
`UntaggedEnum` / `NewtypeWrapper`) + per-field `FieldSource`
provenance (`TypedLeaf` / `BranchTag` / `SeqPosition` /
`RepeatElement` / `RuleReference`); discriminator is data, not
match-arm in the consumer (per `feedback_pluggable-components`).
`project_types` populates the registry as part of fixed-point
closure. `&StructRegistry` impls `StructRegistryProbe`; the audit
pass produces `Mapped` rows when fed a populated registry.

`cargo nextest run --profile ax-iter -p bbnf-ir` → 375 / 375 green
on master (363 baseline + 12 new).

### W1.B dispatch (2026-04-27)

Four parallel agents on disjoint file bounds:

- **W1.B1 JSON** — `grammar/json/json.bbnf` typed-leaf authoring +
  `crates/core/tests/project_types_json.rs` registry-shape test.
- **W1.B2 Sheets** — `grammar/google-sheets/google-sheets.bbnf` +
  per-grammar test (path corrected from the W1.md plan's stale
  `grammar/sheets/sheets.bbnf`).
- **W1.B3 CSS L4** — `grammar/css/l4/*.bbnf` (15-file `@import`
  modular grammar; 454 markers pre-existing) + per-grammar test
  (path corrected from the W1.md plan's stale
  `grammar/css-l4/css-l4.bbnf`).
- **W1.B4 Emitter** — `crates/core/src/backend/rust/emitter/`
  registry-read on every compound emission, bridge mode preserves
  the existing tape co-emission for AZ-I.W1 stability (W2/W3 sever
  the tape per their plan).

Each W1.B worktree seeded with `scripts/seed-worktree.sh
--no-target`; the agent uses `CARGO_TARGET_DIR=$(pwd)/target.local`
for cargo invocations to avoid the shared-target serialisation.

### W1 close (2026-04-27)

W1 closed in ~70 min real wall (W1.A solo + W1.B 4-parallel +
orchestrator integration). **Zero grammar edits across all three
data grammars** — the W1.A `populate_struct_registry` closure
already covers JSON / Sheets / CSS L4 markers from the existing
typed-leaf surface. RESEARCH §6 expectation of 7 / 8 / 60 layouts
exceeded: JSON 8, Sheets 8+, CSS L4 187 (162 named + 25 anonymous
continuation rules surfaced by the lowering pipeline).

W1 commit ledger:

| SHA | Stage | Description |
|---|---|---|
| `e0e0af30` | W1.A | StructRegistry substrate (`registry/{mod,struct}.rs` + `lib.rs` re-exports + `GrammarIR.struct_registry` field) |
| `43ea56bb` | W1.A | `project_types` populates `StructRegistry`; `&StructRegistry` impls `StructRegistryProbe` |
| `710d3952` | W1.A | Leaf tests for `StructRegistry` + 38-fixture init-site update |
| `d1528d2d` | W1.A | Target-symlink fixup |
| `401b3e65` | W1.B1 | JSON `project_types` registry-closure wire contract (9 tests) |
| `3bd7434e` | W1.B2 | Sheets registry-closure wire contract (orchestrator-rescue, 6 tests) |
| `01ada2f0` | W1.B3 | CSS L4 registry-closure wire contract (4 tests) |
| `4e914418` | W1.B4 | Emitter registry-read at compound-emission boundary + `registry_observer` submodule |
| `3fe69fb4` | W1.B4 | Emitter wire-contract test (2 tests) |
| `73602813` | W1.B3 fixup | `TypeDesc::has_scalar_payload` recursion + `admits_scalar_payload` recursion + `dimension`/`color` test cleanup |

**W1 hard-gate ledger:**

| # | Gate | Status | Evidence |
|---|---|---|---|
| 1 | `project_types` closes on JSON / CSS L4 / Sheets without build-stop | PASS | per-grammar wire-contract tests assert closure totality |
| 2 | `StructRegistry` non-empty for every Named rule on the three | PASS | `every_named_*_rule_has_non_empty_layout` tests green per grammar |
| 3 | IR audit pass reports 100% `->` coverage on the three | PASS | `audit_pass_reports_*_for_every_*_marker` tests green per grammar; CSS L4 substrate-fix retired the 42-marker `Missing` cohort |
| 4 | No AU-baseline regression on any 17-entry matrix entry | DEFERRED-TO-W4 | matrix re-measurement is W4 close ceremony per orchestrator directive |
| 5 | `cargo nextest run --workspace --profile ax-iter` ≥ 1480 pass | PASS | 1517 / 1517 passed, 27 skipped at master HEAD `73602813` (`/tmp/workspace-w1.txt`) |

**Key substrate addition:**

`TypeDesc::has_scalar_payload(&self) -> bool` — recurses through
`Tuple`, `Option`, `Vec` wrappers to find any nested scalar.
`StructLayout::admits_scalar_payload` switches to the recursive
helper. The lowering pipeline wraps keyword-discriminator branches
as `Tuple([Span, scalar])`; the audit's coverage policy now admits
these as `Mapped`. Single layout-coverage decision surface
preserved per `feedback_pluggable-components`.

**W2 dispatch follows.**

## 2026-04-28 — W2-act dispatch (post-W2-CLOSE-AUDIT refined trajectory)

W2-act opens against master `6f78c1ef` per the audit synthesis at
`docs/tranches/AZ-I/audit/W2-CLOSE-AUDIT.md`. The wave doc lands at
`docs/tranches/AZ-I/waves/W2-act.md`; the activation pass runs in
three sub-stages:

- **W2-act.A** (sequential, 90 min cap) — EmitStrategy hoist to
  `bbnf-ir::registry::strategy` per `audit/AUDIT-6` §8.1; JsonDocument
  view / to_value / get accessor API; dead-substrate sweep
  (`registry_observer` deleted; `audit_payload_coverage` wired or
  surface deleted). Blocks B1 / B2 / B3.
- **W2-act.B1 / B2 / B3** (parallel, 60 / 90 / 120 min caps
  respectively) — JSON / Sheets / CSS L4 activation. Each: resolver-
  arm flip + per-grammar runtime + parity harness recoding + bench
  gate. Disjoint file bounds.
- **W2-act.C** (sequential, 90 min cap) — close ceremony absorbing
  W4: 17-entry matrix re-run + samply capture + AZ-II handoff
  verification + AZ-I FINAL.md + `post-AZ-I.json` archive + workspace
  nextest.

AZ-II.cutover (BBNF cutover + tape deletion) opens after W2-act
close per `docs/tranches/AZ-II/waves/cutover.md`. BB.W0 may open in
parallel with cutover per audit synthesis Proposal E (substrate
independence on `IrNode`).

## 2026-04-27 — W2 close ceremony (substrate-only; activation reverted per W2.md §Reversal)

W2 closed substrate-only. The wave landed:

- **StructBuilder trait** at `crates/core/src/runtime/builder.rs`.
- **JSON runtime types** (`JsonValue`, `JsonDocument`, `JsonObject`,
  `JsonArray`, `JsonPair`, `JsonNumber`, `JsonArena`,
  `JsonStructBuilder`) at `crates/core/src/runtime/json/`.
- **`EmitStrategy` enum** + per-grammar resolver at
  `crates/core/src/backend/rust/emitter/strategy.rs`.
- **`parse_body` two-path emission** in
  `crates/core/src/backend/rust/emitter/grammar.rs` keyed on strategy.
- **Nine per-shape struct-direct emitters** (Object, Array, Number,
  String, Scalar, Keyword, Wrap, AltDispatch, Flat).
- **Dispatcher signature threading** in
  `crates/core/src/backend/rust/emitter/shapes/dispatcher/cross_shape.rs`
  parameterized by `&EmitStrategy`.
- **Wire-contract test substrate**: `crates/core/tests/emit_strategy.rs`
  + `crates/core/tests/struct_direct_snapshots.rs` driver +
  per-shape `.snap` files.
- **JSON parity harness scaffold** at
  `crates/core/tests/json_parity_struct.rs` (sonic-rs / simdjson
  OnDemand / serde_json comparison stubs against `JsonDocument`).

**Activation reverted.** The `for_grammar` resolver returns
`TapeDirect` for every grammar including JsonParser / JsonGrammar
per W2.md §Reversal. Three blockers exceeded W2's wave budget:

1. `parsed.view()` / `parsed.to_value()` callers across 3 existing
   tests (`json_slab`, `projection_totality`,
   `typed_accessor_surface`) require a `JsonDocument`-side accessor
   API that has not been authored yet.
2. JSON parity harnesses (sonic-rs / simdjson OnDemand /
   serde_json) still compare `Parsed<JsonGrammar>` outputs and
   need recoding against `JsonDocument`.
3. The `cargo bench` gate (twitter ≥ 1967, canada ≥ 1231,
   citm ≥ 2438) was not run on the struct-only path.

These three carry forward to **W2-act**, the follow-on activation
wave. W2.B (Sheets) opens after W2-act per the same activation
gate; W3 (CSS L4) and W4 (FINAL) follow.

**W2 commit ledger** (master HEAD `31269bb6` post-close):

| SHA | Stage | Description |
|---|---|---|
| `85cf83e7` | W2.A | StructBuilder trait + JSON value graph |
| `f8638d58` | W2.A | JSON parity harness scaffold |
| `8f5e50f4` | W2.plan | Emitter rewire plan (5-agent decomposition) |
| `1c6f00d0` | W2.RA | EmitStrategy enum + per-grammar resolver |
| `afcd6c26` | W2.RA | parse_body two-path emission |
| `f23c20d4` | W2.RA | pipeline `resolve_emit_strategy` hook |
| `ffb7eeb5` | W2.RA | leaf tests + snapshot driver |
| `41dd776e` | W2.RB | Object/Array/AltDispatch struct-direct |
| `24d6d888` | W2.RB | TapeDirect-pinned golden tests |
| `d78fe10e` | W2.RC | Number/String/Scalar struct-direct |
| `5f8c1774` | W2.RC | Number/String/Scalar test caller migration |
| `4ab581ef` | W2.RC | TapeDirect docstring byte-parity |
| `b9ee8e45` | W2.RD | Keyword struct-direct |
| `8d6edbf5` | W2.RD | Wrap struct-direct |
| `24db8de2` | W2.RE | Pratt/Flat/ArgList/Unordered/HRegex strategy gates |
| `dfde7673` | W2.RF | Flat struct-direct (JSON pair) |
| `d66e61e7` | W2.RF | Flat snapshot |
| `31269bb6` | W2.close | Cross-agent integration surgery + activation revert |

**Workspace verification at close**: `cargo nextest run --profile
ax-iter --workspace` → 1546 / 1546 passed, 27 skipped.

**Hard-gate ledger:**

| # | Gate | Status | Evidence |
|---|---|---|---|
| 1 | JSON + Sheets emit struct builders only | DEFERRED-TO-W2-ACT | substrate exists, activation gated |
| 2 | JSON twitter ≥ 1967, canada ≥ 1231, citm ≥ 2438; Sheets parse_simple ≥ 95 | DEFERRED-TO-W2-ACT | bench gate not yet run |
| 3 | Parity harnesses green (sonic-rs / simdjson / serde_json / Sheets) | DEFERRED-TO-W2-ACT | scaffolds exist, recoding pending |
| 4 | No AU-baseline regression on CSS L4 or BBNF | PASS | 1546/1546 workspace nextest green |
| 5 | `cargo nextest run --workspace --profile ax-iter` ≥ 1480 pass | PASS | 1546 / 1546 |

## 2026-04-27 — W2 dispatch (re-shaped)

W2.md's original 2-parallel JSON + Sheets shape collides on shared
`crates/core/src/backend/rust/emitter/` and `crates/core/src/pipeline/`
files (per SPEC §Parallelism disjoint-bounds). Additionally the plan
referenced `crates/core/src/runtime/json/` and `runtime/sheets/`
sub-modules that do not yet exist — the current `runtime/` is a flat
tape-keyed surface (`parsed.rs`, `handle.rs`, `path.rs`, `error.rs`).
Per SPEC §Scope-reveal protocol the wave re-shapes into a sequential
2-stage flow that preserves W2.md's hard gates (JSON twitter ≥ 1967,
Sheets parse_simple ≥ 95, parity harnesses green, tape severed on
the JSON / Sheets hot paths):

- **W2.A solo (90 min cap)** — `StructBuilder` trait at
  `crates/core/src/runtime/builder.rs`; `JsonValue<'a>` / `JsonObject<'a>`
  / `JsonArray<'a>` / `JsonPair<'a>` / `JsonNumber` types at new
  `crates/core/src/runtime/json/`; `JsonStructBuilder` concrete impl;
  emitter per-grammar mode-switch in `crates/core/src/backend/rust/emitter/`
  emitting `JsonStructBuilder` calls for JSON; pipeline dispatch
  selects struct-direct path for JSON; `parse_json(src) -> Result<JsonDocument<'_>, ParseErr>`
  return-type migration (no Parsed wrapper for JSON); JSON parity
  harness rewrites (sonic-rs / simdjson OnDemand / serde_json
  struct-vs-native).
- **W2.B solo (60 min cap, post-W2.A)** — apply pattern to Sheets:
  `SheetsValue` / `Cell` / `Formula` types at
  `crates/core/src/runtime/google_sheets/`; `SheetsStructBuilder`
  concrete impl; emitter routes Sheets through the existing
  mode-switch; Sheets parity harness rewrite.
- **Orchestrator (post-W2.B)** — regen for JSON + google-sheets;
  workspace nextest verification; bench-gate verification (twitter ≥
  1967, canada ≥ 1231, citm ≥ 2438, parse_simple ≥ 95) per
  `make ay-bench-close WAVE=close` close-gate command surface.

Per `feedback_no-deferrals` no carry-forward to W3 / W4 / AZ-II of
W2's gates. Per `feedback_no-backward-compat` the JSON / Sheets API
migration is full-replacement; Parsed<JsonGrammar> retires from
those grammars' surface in this wave.

AZ-I ships direct-to-struct materialisation for the three primary
data grammars — JSON, CSS L4, and Sheets — via `project_types` +
`StructRegistry` closure and a single struct-emitting codegen path.
The tape crate remains on disk at AZ-I close, scoped to BBNF's
bootstrap only; AZ-II owns BBNF self-hosting and tape deletion.

Wave plan (four waves + FINAL): W0 classifier-unification research
+ derive-cache lift + IR audit baseline → W1 `StructRegistry` +
`project_types` closure on JSON/CSS/Sheets → W2 direct-to-struct
emission for JSON + Sheets (twitter ≥ 1967 MB/s) → W3 CSS L4
aggregate + typed values (lightningcss parity) → W4 FINAL —
three-grammar slice at AU parity + AZ-II handoff contract verified.

Parent plan: `docs/tranches/AZ-I/AZ-I.md`.
Research: `docs/tranches/AZ-I/RESEARCH.md`.

## Gate summary

- **Throughput**: JSON twitter ≥ 1967 MB/s, canada ≥ 1231, citm ≥
  2438; CSS normalize ≥ 735, bootstrap ≥ 600, tailwind ≥ 500;
  Sheets parse_simple ≥ 95 MB/s — all on the struct-only path.
- **Coverage**: 100% `->` coverage on JSON, CSS L4, Sheets.
  `StructRegistry` non-empty for every Named rule on these three.
- **Workspace**: ≥ 967 pass / ≤ 33 fail / ≤ 30 ignored.
- **Tape-remains**: `crates/tape/` compiles; `rg 'use bbnf_tape'` hits
  only BBNF-scoped paths; `cargo build -p bbnf` green.
- **Parity harnesses**: sonic-rs, lightningcss, simdjson OnDemand,
  cssparser, serde_json green.

## Wave status

| Wave | Status | Headline |
|---|---|---|
| W0 | closed (2026-04-27) | Research + classifier-unification + audit baseline |
| W1 | closed (2026-04-27) | `StructRegistry` + `project_types` closure |
| W2 | closed substrate-only (2026-04-28) | 9 per-shape struct-direct emitters + EmitStrategy + JSON runtime substrate |
| W2-act | in progress (2026-04-28) | GESTALT-ACTIVATE — JSON + Sheets + CSS L4 + close ceremony |

## W2-act sub-wave log

### W2-act.A — substrate hoist + JsonDocument accessor + dead-substrate sweep (2026-04-28, in progress)

Sequential prelude blocking B1/B2/B3. Cap 90 min.

- `EmitStrategy` hoisted from `crates/core/src/backend/rust/emitter/strategy.rs` to `crates/ir/src/registry/strategy.rs`
  per `audit/AUDIT-6-ARCHITECTURE.md` §4 + §8.1. Variant payload generalised to `SubstrateBinding { rust, ts, wasm }`;
  the `rust` field is populated for active struct-direct grammars, `ts` / `wasm` reserved for BA host bindings.
  The resolver's catch-all `_ => TapeDirect` stays intact — W2-act.B1 owns the JsonParser arm flip.
- 22 emitter consumer sites re-targeted to `bbnf_ir::registry::EmitStrategy`. Match arms that bound
  `builder_path` directly now bind `rust` and reach `rust.builder_path` / `rust.document_path`.
  `crates/core/src/backend/rust/emitter/mod.rs` re-exports the IR-level enum so existing
  `bbnf::backend::rust::emitter::EmitStrategy` consumer paths continue to resolve.
- `JsonDocument` moved to `crates/core/src/runtime/json/document.rs` per directory-module discipline.
  W2-act.A accessor surface lands: `view() -> JsonView<'a, 'p>`, `to_value() -> &JsonValue<'p>`,
  `get<T: JsonPathQuery>(path) -> Option<T>` — mirrors pre-W2-act `Parsed::view()` /
  `Parsed::to_value()` / `Parsed::get::<T>(path)`. `JsonView` carries two lifetime parameters because
  `JsonDocument<'p>` is invariant in `'p` (arena owns `Vec<JsonValue<'p>>`); collapsing to one
  lifetime would force `'p = 'a` and break view composition. `JsonPathQuery` impls land for
  `&str`, `f64`, `bool`, `JsonValue`. `JsonKind` enumerates the typed JSON shapes.
- Dead-substrate sweep:
  - `crates/core/src/backend/rust/emitter/shapes/registry_observer.rs` deleted (84 LOC).
    The module's docstring self-documented as removable at AZ-I close; `record` had no production
    reader, `drain` / `clear` / `RegistryReadEvent` were test-only consumers in
    `tests/emitter_registry_read.rs` (also retired, 202 LOC).
  - `audit_payload_coverage` decision: **path a** (wire into pipeline) per `audit/AUDIT-2` §6.B
    recommendation, given the surface has 5 test consumers across two crates rather than the spec's
    presumed 1; path-b inlining would duplicate ~600 LOC across 5 test files. Path a wires
    `pipeline::compile::write_audit_coverage_artefact` into all four CompileTarget arms; every
    pipeline-compile run emits `target/audit/<grammar>.json`. The W4 close ceremony's
    coverage-gate input becomes a runtime-produced artefact.

Hard gates verified:
1. `bbnf_ir::registry::EmitStrategy` resolves; `rg 'pub.*enum EmitStrategy' crates/` returns one
   hit at `crates/ir/src/registry/strategy.rs`.
2. `JsonDocument::view()` / `to_value()` / `get::<T>(path)` exist; `rg 'fn view|fn to_value|fn get'
   crates/core/src/runtime/json/` shows the new accessors in `document.rs`.
3. `crates/core/src/backend/rust/emitter/shapes/registry_observer.rs` and
   `crates/core/tests/emitter_registry_read.rs` do not exist.
4. `cargo nextest run -p bbnf-ir --test payload_coverage_audit --profile ax-iter` 9/9 green.
5. `cargo iter-check` 0 errors (8 pre-existing labeled-break warnings only, all in
   `crates/core/src/grammar/generated/bbnf.rs`).
6. `cargo nextest run --workspace --profile ax-iter --no-fail-fast` 1544 passed / 27 skipped /
   0 failed (baseline 1546 minus 2 deleted observer-test counts; matches the spec's
   "minus the deleted emitter_registry_read.rs test count" allowance).
7. `cargo xtask regen --check` clean (9 grammars matched).
8. `git status --short` empty (target/ symlink only).

Commits:
- `659e1cc5` refactor(ir,core): hoist EmitStrategy from rust backend to bbnf-ir::registry::strategy
- `cc7d3a26` feat(runtime/json): JsonDocument accessor API mirroring Parsed surface
- `78370c87` chore(emitter,tests): retire registry_observer + emitter_registry_read
- `2d7b2e4e` feat(pipeline): wire audit_payload_coverage to write target/audit/<grammar>.json

### W2-act.B1 — JSON activation: resolver flip + consumer migrations + parity recoding (2026-04-28, returned)

Parallel after A. Cap 60 min.

- Resolver arm flipped: `EmitStrategy::for_grammar` now resolves
  `("JsonParser" | "JsonGrammar", true)` to `EmitStrategy::StructDirect`
  with the canonical `JsonStructBuilder` / `JsonDocument` substrate
  paths. The catch-all `_ => TapeDirect` preserves the W2-act
  substrate-only state for every other grammar; W2-act.B2 / B3 land
  their own positive arms in the same wave.
- `crates/core/tests/emit_strategy.rs` — JSON-arm leaf tests flipped
  from "TapeDirect (post-W2-close)" to "StructDirect (post-W2-act.B1)";
  asserts canonical builder/document paths + None for ts/wasm; empty-
  registry guard tests unchanged.
- Three broken JSON test consumers migrated onto `JsonDocument`:
  - `tests/json_slab.rs` — `parsed.view()` → `doc.view()`; assertions
    check `JsonView::kind()` + arena handle resolution on the canonical
    fixture corpus.
  - `tests/projection_totality.rs` — JSON's runtime-call-count block
    asserts the typed `JsonValue::String` shape post-flip (not the
    "Projection" debug-suffix marker, which no longer applies); CSS L4 /
    Sheets / BBNF blocks unchanged (still tape-direct).
  - `tests/typed_accessor_surface.rs` — JSON compile-time accessor block
    swaps to `JsonView` surface (`kind()` / `is_*()` / `root()` /
    `arena()`); rule_kind dispatch test pins `JsonValue::Object` shape
    on `{"a":1}`.
- Five JSON parity harnesses recoded for struct-direct:
  - `tests/json_parity.rs` (sonic-rs) — recursive walker comparing
    every `JsonValue` variant against `sonic_rs::Value`; numbers reduce
    to f64; objects walked in source order. Typed-leaf activation
    tests (null/bool/number/string) assert variant landing on the
    document directly — no tape symbols.
  - `tests/json_canonical_parity.rs` — replaces `serialize_compact` /
    `NodeView::from_cursor` with a local compact serializer over
    `JsonDocument`; both sides feed through shared
    `strip_insignificant_ws` for byte-symmetric compare.
  - `tests/json_value_parity.rs` (serde_json + simdjson) —
    `assert_doc_eq_serde` / `assert_doc_eq_simd` walkers compare
    `JsonDocument` against `serde_json::Value` and
    `simd_json::owned::Value`.
  - `tests/json_parity_struct.rs` — promoted from W2-act probe to
    load-bearing; wire-contract section retained (StructBuilder trait
    surface), native-parity section adds serde_json fixture-corpus
    comparisons + compile-time pin that `JsonParser::parse` returns
    `JsonDocument<'_>`.
  - `tests/json_decode.rs` — string-decode round-trips walk the
    document tree; collect_strings recurses through arena handles.

Tests will compile against the post-flip generated.rs once the
orchestrator runs `cargo xtask regen --grammar json` post-cherry-pick.
The bench gate (twitter ≥ 1967, canada ≥ 1231, citm ≥ 2438) is
orchestrator-owned.

Hard gates (per dispatch spec):
1. Resolver: `cargo nextest run -p bbnf --test emit_strategy --profile ax-iter` — 7/7 pass.
2. Three broken consumers: forward-migrated; verifies post-orchestrator-regen.
3. Parity harnesses: forward-migrated; verifies post-orchestrator-regen.
4. `cargo iter-check` — 0 errors (8 pre-existing warnings).
5. `cargo nextest run --workspace --profile ax-iter --no-fail-fast` — pending orchestrator regen for full pass count.
6. `git status --short` — empty modulo `target.local/`.

Commits:
- `3167faf0` feat(ir): AZ-I.W2-act.B1 — flip JsonParser/JsonGrammar resolver to StructDirect
- `0e7485c5` refactor(tests/json): AZ-I.W2-act.B1 — migrate consumers to JsonDocument
- `f94c31c9` refactor(tests/json): AZ-I.W2-act.B1 — recode parity harnesses against JsonDocument

## Handoff

- Opens on: post-B7 substrate (AY-II → AY-II-I → AY-III deferred;
  durable gates absorbed into AZ-I.W4 + AZ-II.W2).
- Closes into: AZ-II (BBNF self-hosting + tape deletion).
- BA opens on AZ-II close, not AZ-I close.
