# AY-II.W0' — Infra fix plan (post-research phase II)

## Intended outcome

Collapse the proc-macro wall that `gorgeous` erects in front of every
`cargo check -p bbnf --tests` by (a) severing `gorgeous` from `bbnf`'s
`[dev-dependencies]` — where it is already dead-gated — and (b) gating
each of gorgeous's six `#[derive(Parser)]` sites behind a per-grammar
cargo feature so gorgeous itself builds incrementally against the
grammars it actually needs. The secondary lever narrows
`crates/derive/build.rs`'s fingerprint scan to the three subtrees that
actually influence codegen output, eliminating the cross-tree cascade
that forces full derive re-expansion on every unrelated source edit.

## Evidence baseline for change sets

Attribution source: `docs/tranches/AY-II/audit/W0p-infra-root-cause.md`
lines 5, 7, 16, 40, 56, 58 (master `ba52a322`).

- **Six** `#[derive(Parser)]` sites in gorgeous, not five as the
  root-cause doc stated; re-verified by
  `rg '#\[derive\(.*Parser.*\)\]' crates/gorgeous/src/` →
  `bbnf.rs:5`, `bnf.rs:5`, `ebnf.rs:5`, `css.rs:5`, `json.rs:5`,
  `google_sheets.rs:5`. Per-site cost therefore averages ~90 s cold
  rather than ~110 s; the aggregate 556 s wall reading stands.
- Gorgeous is declared at `crates/core/Cargo.toml:41` under
  `[dev-dependencies]` as
  `gorgeous = { version = "0.1", features = ["vm"] }`. Inside `bbnf`,
  the only source references are eight lines in
  `crates/core/tests/pipeline.rs:379-619` — **all eight are wrapped in
  `#[cfg(feature = "gorgeous")]`**, and `crates/core/Cargo.toml:30-31`
  declares only a `dhat-heap` feature; there is no `gorgeous` feature
  anywhere in `bbnf`'s `[features]` block. Those references are
  permanently dead-gated. Every other hit under `crates/core/tests/`
  is a grep-false-positive (doc-comment, path-resolution string,
  ignore-message) — verified below.
- `crates/derive/build.rs:17-25` currently tracks the entire
  `../core/src` tree (6 non-codegen subdirs including `runtime/`,
  `lsp/`-style reverse-dep signalling via `types.rs`, and the
  backend/wasm sub-tree). Any edit anywhere under that roots forces
  `bbnf_derive`'s fingerprint bump and cascades re-expansion across
  every derive site.

## Change set (ordered, file-level)

### Change 1 — gorgeous: gate every `#[derive(Parser)]` site behind a named cargo feature

- File: `crates/gorgeous/Cargo.toml`
- Before (lines 17-20, current content):

```
[features]
default = []
vm = ["dep:bbnf-ir"]
```

- After:

```
[features]
# Default builds ONLY the feature-independent surface (PrinterConfig,
# the builtin/jit glue, and the vm module when `vm` is enabled). Every
# grammar derive site is opt-in so a downstream consumer pays only for
# the grammars it imports.
default = ["bbnf-grammar", "json-grammar", "css-grammar", "ebnf-grammar", "bnf-grammar", "sheets-grammar"]
# Per-grammar gates — each turns on exactly ONE `#[derive(Parser)]`
# invocation inside gorgeous's lib (see Change 2 for the module
# `#[cfg(feature = …)]` splice). `bin-full` is the aggregate feature
# the `gorg` binary needs; it re-enables every grammar.
bbnf-grammar = []
json-grammar = []
css-grammar = []
ebnf-grammar = []
bnf-grammar = []
sheets-grammar = []
bin-full = ["bbnf-grammar", "json-grammar", "css-grammar", "ebnf-grammar", "bnf-grammar", "sheets-grammar"]
vm = ["dep:bbnf-ir"]
```

- Rationale: transforms gorgeous from a monolithic compile wall into a
  feature-selectable surface. Downstream consumers (bbnf, gorg binary,
  wasm) opt into the exact per-grammar derive sites they need. The
  default set keeps `cargo check -p gorgeous` green without flags;
  downstream tests opt out by pinning `default-features = false` plus
  the per-grammar gate that test actually exercises.

### Change 2 — gorgeous: conditionally compile each grammar module

- File: `crates/gorgeous/src/lib.rs`
- Before (lines 5-13, current content):

```
pub mod json;
pub mod bbnf;
pub mod ebnf;
pub mod bnf;
pub mod css;
pub mod google_sheets;

#[cfg(feature = "vm")]
pub mod vm;
```

- After:

```
#[cfg(feature = "json-grammar")]
pub mod json;
#[cfg(feature = "bbnf-grammar")]
pub mod bbnf;
#[cfg(feature = "ebnf-grammar")]
pub mod ebnf;
#[cfg(feature = "bnf-grammar")]
pub mod bnf;
#[cfg(feature = "css-grammar")]
pub mod css;
#[cfg(feature = "sheets-grammar")]
pub mod google_sheets;

#[cfg(feature = "vm")]
pub mod vm;
```

- Interaction with Change 1: a consumer that depends on
  `gorgeous = { default-features = false, features = ["json-grammar"] }`
  compiles exactly ONE `#[derive(Parser)]` site (≈ 90 s cold) instead
  of six (≈ 556 s cold). `gorg` binary and gorgeous's own tests keep
  the default set and see no change. The `builtin.rs` + `jit.rs` +
  `main.rs` modules are feature-independent and stay unconditionally
  compiled; `PrinterConfig` in `lib.rs` stays a top-level item.

### Change 3 — bbnf: drop `gorgeous` from `[dev-dependencies]`

- File: `crates/core/Cargo.toml`
- Before (line 41):

```
gorgeous = { version = "0.1", features = ["vm"] }
```

- After: line deleted outright. `crates/core/Cargo.toml:33-45`
  `[dev-dependencies]` block shrinks by one line.
- Rationale: the eight call sites in `crates/core/tests/pipeline.rs:
  379-619` all live under `#[cfg(feature = "gorgeous")]`. bbnf's
  `[features]` block at `crates/core/Cargo.toml:30-31` defines only
  `dhat-heap`; no cargo invocation of `cargo test -p bbnf`,
  `cargo check -p bbnf --tests`, or `cargo check --workspace` can
  reach the gated code through the public feature graph. Removing the
  dev-dep therefore loses no existing test coverage — it only drops
  the serial 6-derive expansion wall from every bbnf compile-gate
  path.
- Side effects on existing bbnf tests that actually import gorgeous:
  **none**. Verified by
  `rg '^use gorgeous|^extern crate gorgeous' crates/core/tests/` → 0
  matches. Every `gorgeous::` token under `crates/core/tests/` is
  either inside the dead `#[cfg(feature = "gorgeous")]` gate
  (`pipeline.rs`), a doc-comment (`pratt_const_fold.rs:349`,
  `typed_accessor_surface.rs:5`, `typed_accessor_surface.rs:119`), an
  ignore-message string (`ebnf_prettify.rs:43`), or a filesystem path
  under `crates/gorgeous/grammar` walked at test runtime
  (`regex_fast_path_consistency.rs:149,179`). None of these
  references pull a Rust crate dependency.

### Change 4 — migrate dependent bbnf tests

There are **no** bbnf tests with a live dependency on the gorgeous
Rust crate. Exhaustive evidence (every file that `grep -l gorgeous`
matched):

| File | Line(s) | Kind | Action |
|---|---|---|---|
| `crates/core/tests/pipeline.rs` | 379, 612 | `#[cfg(feature = "gorgeous")]` gate with no matching feature in `bbnf`'s `[features]` block → dead code | **delete the gated blocks.** Gate 1 spans lines 379-477 (closes with a `}` before the next `#[test] fn pipeline_google_sheets_multiline_let`); gate 2 spans lines 611-633 (closes with a `}` before the `// Same formula without leading =` comment at line 635). Both re-verified in the plan worktree. The functional coverage is zero today; preserving them as dead gates perpetuates the confusion that originally triggered this plan. If an author wants this back, it lands via a dedicated `crates/prettify-smoke/` crate (see Out of scope). |
| `crates/core/tests/pipeline.rs` | 382-476, 614-632 | call sites inside those gates | removed with parent block |
| `crates/core/tests/ebnf_prettify.rs` | 43 | `#[ignore = "… pre-existing in gorgeous"]` string | **keep** — narrative only, zero crate contact |
| `crates/core/tests/regex_fast_path_consistency.rs` | 149, 179 | path traversal of `crates/gorgeous/grammar/` via `env!("CARGO_MANIFEST_DIR")` + `std::fs` | **keep** — filesystem-only walk, zero crate contact |
| `crates/core/tests/pratt_const_fold.rs` | 349, 357, 359 | doc-comment referencing the gorgeous test file | **keep** |
| `crates/core/tests/typed_accessor_surface.rs` | 5, 119 | doc-comment | **keep** |

Sub-gate for Change 4: after Change 3 lands,
`cargo check --profile ax-iter -p bbnf --tests` must typecheck
`pipeline.rs` without the `gorgeous` name resolving. With the
`#[cfg(feature = "gorgeous")]` blocks deleted, the
`use gorgeous::…` paths vanish and there is no lingering reference.

### Change 5 — narrow build.rs fingerprint

- File: `crates/derive/build.rs`
- Before (lines 16-25):

```
    let manifest_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let tracked_roots = [
        manifest_dir.join("Cargo.toml"),
        manifest_dir.join("build.rs"),
        manifest_dir.join("src"),
        manifest_dir.join("../core/Cargo.toml"),
        manifest_dir.join("../core/src"),
        manifest_dir.join("../ir/Cargo.toml"),
        manifest_dir.join("../ir/src"),
    ];
```

- After:

```
    let manifest_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    // Only the subtrees that actually influence `#[derive(Parser)]`
    // emission output are tracked here. The proc-macro cache in
    // `src/lib.rs` is content-keyed on the grammar bytes + attrs +
    // schema version, so unrelated bbnf source edits under `runtime/`,
    // `backend/wasm/`, `backend/ts/`, `imports/`, `graph/`, or at
    // `lib.rs`/`types.rs` do NOT change derive output — tracking them
    // here only fires a spurious fingerprint cascade that re-rehydrates
    // 60+ cross-crate derive TokenStreams for zero output delta.
    let tracked_roots = [
        manifest_dir.join("Cargo.toml"),
        manifest_dir.join("build.rs"),
        manifest_dir.join("src"),
        manifest_dir.join("../core/Cargo.toml"),
        manifest_dir.join("../core/src/backend/rust"),
        manifest_dir.join("../core/src/generate"),
        manifest_dir.join("../core/src/grammar"),
        manifest_dir.join("../core/src/pipeline"),
        manifest_dir.join("../core/src/pipeline.rs"),
        manifest_dir.join("../core/src/lower"),
        manifest_dir.join("../ir/Cargo.toml"),
        manifest_dir.join("../ir/src"),
    ];
```

- Which subdirs stay tracked: `crates/core/src/backend/rust`
  (emission + view codegen), `crates/core/src/generate` (root
  generator), `crates/core/src/grammar` (grammar loader + host fns +
  generated.rs), `crates/core/src/pipeline` + `pipeline.rs` (the IR
  pipeline the derive walks), `crates/core/src/lower` (AST → IR
  lowering the derive depends on), plus every top-level `Cargo.toml`
  and `bbnf_derive`'s own `src/`.
- Which subdirs drop: `crates/core/src/backend/ts`,
  `crates/core/src/backend/wasm`, `crates/core/src/backend/emitter.rs`
  (cross-backend facade, not consumed by the Rust derive),
  `crates/core/src/runtime` (parser-runtime surface, not codegen
  source), `crates/core/src/graph`, `crates/core/src/imports`,
  `crates/core/src/lib.rs`, `crates/core/src/types.rs`. These are
  NEVER read by `bbnf_derive::bbnf_derive` at
  `crates/derive/src/lib.rs:281` — the entry point calls
  `compile_paths_request` → `bbnf::pipeline::compile_grammar` and
  `bbnf::generate::generate_all`, whose module closure sits entirely
  inside the tracked roots. An edit to `runtime/parsed.rs` changes
  parser behaviour but not the TokenStream the derive emits; the
  fingerprint cascade it currently triggers is pure waste.
- Rationale: even with gorgeous excised from bbnf's dev-deps, every
  bbnf test file carries its own `#[derive(Parser)]` (28 files × 53
  sites per the root-cause doc). Today, any edit to
  `crates/core/src/runtime/*.rs` — the exact directory AY-II hot-loop
  work touches most — bumps `bbnf_derive`'s fingerprint, re-rehydrates
  every one of those 53 TokenStreams inside the test binaries, and
  wastes the `.bbnf-cache` hit the doc says should be fast. Narrowing
  the roots means AY-II's typical hot-loop edit under
  `runtime/builder_view.rs` or `runtime/parsed.rs` stops cascading.

### Change 6 — documentation (feature matrix note)

- File: `crates/gorgeous/Cargo.toml`
- Add a short comment block above the `[features]` section (already
  drafted inline above in Change 1) naming the per-grammar gates and
  documenting the `default` / `bin-full` aggregate. No separate
  README change required — the comments in the toml are the contract.

## Validation gates

Each gate is a cold measurement from a clean `target/`. `time` is the
wall clock floor; crossings triggers redress rollback.

1. **Cold bbnf lib (regression sentinel)**
   `rm -rf target && time cargo check --profile ax-iter -p bbnf --lib`
   ≤ **15 s** (root-cause doc probe 1b: 8.92 s; 50 % headroom).
2. **Cold bbnf tests (primary gate)**
   `rm -rf target && time cargo check --profile ax-iter -p bbnf --tests`
   ≤ **120 s**. Pre-plan observation: ≥ 15 min. Predicted
   post-Change-3 alone: ≤ 180 s (gorgeous excised; residual cost is
   28 bbnf test-binary derive expansions parallelised by cargo's job
   server). Predicted post-Change-5 compound: ≤ 120 s.
3. **Cold single-test rebuild**
   `rm -rf target && time cargo test --profile ax-iter -p bbnf --test json_slab --no-run`
   ≤ **60 s**. `json_slab` is the representative single-derive test;
   any test listed in `ls crates/core/tests/*.rs` works, gate is the
   median.
4. **Warm incremental after runtime edit (Change 5 discriminator)**
   After gate 2 passes once:
   `touch crates/core/src/runtime/parsed.rs && time cargo check --profile ax-iter -p bbnf --tests`
   ≤ **30 s**. Pre-Change-5 prediction: ≥ 180 s (full derive
   re-expansion cascade). Post-Change-5 prediction: ≤ 30 s (cargo
   sees no bbnf_derive fingerprint bump, reuses the proc-macro
   output).
5. **Warm incremental after emitter edit (Change 5 sanity)**
   `touch crates/core/src/backend/rust/emitter/shapes/dispatcher.rs && time cargo check --profile ax-iter -p bbnf --tests`
   ≤ **180 s**. `backend/rust/` STAYS tracked after Change 5, so an
   emitter edit correctly invalidates and cascades. This gate asserts
   Change 5 did NOT over-prune — the proc-macro output still reflects
   emitter source edits.
6. **Gorgeous still compiles with default feature set**
   `rm -rf target && time cargo check --profile ax-iter -p gorgeous --lib`
   ≤ **600 s** (same as current cold baseline; default-features
   preserves the six derive sites for the `gorg` binary).
7. **Gorgeous compiles with a single grammar gate**
   `rm -rf target && time cargo check --profile ax-iter -p gorgeous --lib --no-default-features --features json-grammar`
   ≤ **150 s**. Single derive site; directly demonstrates Change 1 +
   Change 2 composition.
8. **Gorgeous's own test suite still passes**
   `cargo test -p gorgeous --tests` green on default features.
   Assertion: Change 1 + Change 2 do not change gorgeous's
   self-test matrix (same six derive sites, same test files
   in `crates/gorgeous/tests/`).
9. **Dependent workspace stays green**
   `cargo test -p tape --tests`,
   `cargo test -p bbnf-ir --tests`,
   `cargo test -p csp-solver --tests` all green. These three are the
   crates that do NOT touch gorgeous today; no regression possible
   from this plan, asserted for the record.
10. **Existing bbnf tests post-migration**
    `cargo test --profile ax-iter -p bbnf --tests --no-run` compiles.
    Pre-plan verification: zero live call sites to gorgeous from
    bbnf tests (Change 4 table). This gate asserts the compile still
    succeeds after the dead `#[cfg(feature = "gorgeous")]` blocks
    in `pipeline.rs` are removed.
11. **bootstrap script untouched**
    `bash scripts/bootstrap-bbnf.sh` runs unchanged.
    `diff crates/core/src/grammar/generated.rs` pre-plan vs post-plan
    → empty. This plan touches zero codegen-affecting source.

## Invariant preservation ledger

W0p.md invariants 14-19 are preserved one-for-one; each is
orthogonal to the dev-dep + fingerprint reshape this plan performs.

- **§14 FusedBuilder sole builder** — untouched; plan modifies only
  `crates/gorgeous/{Cargo.toml,src/lib.rs}`, `crates/core/Cargo.toml`,
  `crates/derive/build.rs`, and the dead
  `#[cfg(feature = "gorgeous")]` blocks in `crates/core/tests/pipeline.rs`.
  The tape builder surface is untouched.
- **§15 `push_compound` / `mark_children` absent** — untouched; no
  `crates/tape/` edit.
- **§16 materializer call-count truth** — untouched; no
  `backend/rust/view/` or `backend/rust/emitter/shapes/` edit.
- **§17 `STRUCTURAL_SCAN_POLICY` splice** — untouched; no dispatcher
  edit.
- **§18 zero W0-era `#[allow(dead_code)]`** — untouched; no
  `#[allow(dead_code)]` added or removed.
- **§19 `Parsed::to_value()` non-panic** — untouched; no runtime edit.

## Redress-agent checklist

Each numbered step is a single command, single edit, or single
verification. Execute in order under a 30-min cap.

1. `cd` into the plan worktree (agent dispatches per standard
   triumvirate; no path hardcoded here).
2. `rg '#\[derive\(.*Parser.*\)\]' crates/gorgeous/src/` and confirm
   output lists exactly the six files (`bbnf.rs`, `bnf.rs`,
   `css.rs`, `ebnf.rs`, `google_sheets.rs`, `json.rs`, each at
   line 5). Abort if count ≠ 6.
3. Edit `crates/gorgeous/Cargo.toml`: replace the current `[features]`
   block (lines 17-20) with the post-block from Change 1 verbatim.
4. Edit `crates/gorgeous/src/lib.rs`: replace lines 5-10 with the
   six `#[cfg(feature = "<grammar>-grammar")] pub mod <grammar>;`
   lines from Change 2, preserving the existing `#[cfg(feature =
   "vm")] pub mod vm;` at lines 12-13.
5. Run `cargo check -p gorgeous --lib` (default features); must exit 0.
6. Run `cargo check -p gorgeous --lib --no-default-features --features json-grammar`;
   must exit 0 and compile visibly fewer modules. Grep stderr for
   `Compiling gorgeous` to confirm the build runs.
7. Run `cargo test -p gorgeous --tests`; must exit green. This
   validates gate 8.
8. Commit: `git add crates/gorgeous/Cargo.toml crates/gorgeous/src/lib.rs`
   then
   `git commit -m "refactor(gorgeous): gate derive sites behind cargo features (AY-II.W0'.d4)"`.
9. Edit `crates/core/Cargo.toml`: delete line 41 (`gorgeous = { version = "0.1", features = ["vm"] }`).
10. Edit `crates/core/tests/pipeline.rs`: delete the two
    `#[cfg(feature = "gorgeous")]` blocks. First block: lines 379-477
    inclusive, where line 379 is `    #[cfg(feature = "gorgeous")]`
    and line 477 is the closing `}` of the block (line 478 is blank,
    line 479 begins `#[test] fn pipeline_google_sheets_multiline_let`).
    Second block: lines 611-633 inclusive, where line 611 is
    `    // Format via VM (requires gorgeous dev-dependency)`, line
    612 is `    #[cfg(feature = "gorgeous")]`, and line 633 is the
    closing `}` (line 634 is blank, line 635 begins
    `    // Same formula without leading =`). Re-verify both spans
    before edit via `sed -n '377,480p' crates/core/tests/pipeline.rs`
    and `sed -n '609,636p' crates/core/tests/pipeline.rs`. Adjacent
    non-gated code retains its existing control flow — the blocks
    are leaf assertions, not wrappers.
11. Run `cargo check --profile ax-iter -p bbnf --tests`; must exit 0.
12. Commit: `git add crates/core/Cargo.toml crates/core/tests/pipeline.rs`
    then
    `git commit -m "refactor(bbnf): drop gorgeous as mandatory dev-dep (AY-II.W0'.d5)"`.
13. Edit `crates/derive/build.rs`: replace lines 16-25 with the
    post-block from Change 5 verbatim.
14. Run `cargo check --profile ax-iter -p bbnf --tests`; must exit 0.
15. `touch crates/core/src/runtime/parsed.rs && time cargo check --profile ax-iter -p bbnf --tests`
    must finish ≤ 30 s.
16. `touch crates/core/src/backend/rust/emitter/shapes/dispatcher.rs && time cargo check --profile ax-iter -p bbnf --tests`
    must finish ≤ 180 s and MUST rebuild derive-dependent crates
    (visible as "Compiling bbnf_derive" in stderr) — this asserts
    Change 5 does not over-prune.
17. Commit: `git add crates/derive/build.rs`
    then
    `git commit -m "refactor(derive): narrow build.rs fingerprint to codegen-relevant paths (AY-II.W0'.d6)"`.
18. Execute cold gate 2: `rm -rf target && time cargo check --profile ax-iter -p bbnf --tests`;
    must finish ≤ 120 s. Log wall time to
    `docs/tranches/AY-II/audit/W0p-infra-fix-results.md`.
19. Execute cold gate 3: `rm -rf target && time cargo test --profile ax-iter -p bbnf --test json_slab --no-run`;
    must finish ≤ 60 s. Log wall time.
20. Execute gate 11: `bash scripts/bootstrap-bbnf.sh` and
    `git diff --stat crates/core/src/grammar/generated.rs` must print
    nothing.

## Commit message templates

Three commits, one per logical change cluster. Templates below — the
redress agent executes them verbatim in steps 8, 12, 17 of the
checklist.

```
refactor(gorgeous): gate derive sites behind cargo features (AY-II.W0'.d4)

Gorgeous's six #[derive(Parser)] invocations now sit behind per-grammar
feature gates (bbnf-grammar / json-grammar / css-grammar / ebnf-grammar /
bnf-grammar / sheets-grammar), all enabled by default so the gorg
binary and gorgeous's own tests keep working unchanged. Downstream
consumers can now opt into a subset, collapsing the serial 6-derive
compile wall into whatever cross-section they actually use.

Part of AY-II.W0p-infra-fix-plan.md.
```

```
refactor(bbnf): drop gorgeous as mandatory dev-dep (AY-II.W0'.d5)

bbnf's [dev-dependencies] carried `gorgeous = { features = ["vm"] }`
purely to satisfy eight call sites under
`#[cfg(feature = "gorgeous")]` inside tests/pipeline.rs — a feature
bbnf's [features] block never declared, making those blocks dead
forever. Remove the dev-dep and the dead-gated blocks so
`cargo check -p bbnf --tests` cold drops from 15 min to ≤ 2 min.
If the prettify smoke those tests encoded needs to come back, it
lands in a dedicated crates/prettify-smoke/ workspace member.

Part of AY-II.W0p-infra-fix-plan.md.
```

```
refactor(derive): narrow build.rs fingerprint to codegen-relevant paths (AY-II.W0'.d6)

bbnf_derive's build.rs was tracking the entire crates/core/src tree,
fingerprinting every runtime/ edit into a full proc-macro rerun across
every downstream consumer's ~60 derive sites. Narrow to the five
subtrees (backend/rust/, generate/, grammar/, pipeline, lower/)
plus pipeline.rs that actually influence derive output, so AY-II's
typical runtime hot-loop edit no longer cascades.

Part of AY-II.W0p-infra-fix-plan.md.
```

## Risk / rollback

Primary risk: Change 5's fingerprint narrowing drops a subtree that
someone later adds to the codegen path silently (e.g. a future refactor
that makes `crates/core/src/runtime/parsed.rs` participate in derive
emission). Mitigation: gate 5 (`touch backend/rust/dispatcher.rs →
≤ 180 s AND bbnf_derive recompiled`) catches the positive case; the
negative case (an edit that SHOULD cascade but does not) would show
up as a test producing stale behaviour, at which point the tracked
roots list is the single file to adjust. Rollback is one-commit
revert per change cluster (`git revert <sha>` of the d6 / d5 / d4
commits in reverse order) — each commit is independently reversible
because the plan orders them so later changes only compose atop
earlier ones, never depend on them.

Secondary risk: deleting the dead `#[cfg(feature = "gorgeous")]`
blocks in `pipeline.rs` also deletes the `assert!(formatted.contains("LET"))`
and neighbouring formatting assertions. These have been dead for the
entire gorgeous-as-dev-dep era; they assert nothing that another test
covers today. If a future agent wants this coverage, the plan
prescribes the path: a `crates/prettify-smoke/` workspace member that
opts into exactly `gorgeous/sheets-grammar`.

## Out of scope

- Any edit to `scripts/bootstrap-bbnf.sh`. Its proc-macro cache path
  at `target/.bbnf-cache/` is orthogonal to the gates above; the
  root-cause doc's fix-sketch item (c) is a follow-on.
- Any edit to `crates/core/src/grammar/generated.rs` or the bootstrap
  crate. The double-regen invariant is tested by gate 11 but no
  plan-driven source change touches those artefacts.
- Any edit to `crates/tape/`, `crates/core/src/backend/rust/view/`,
  `crates/core/src/backend/rust/emitter/shapes/`, or
  `crates/core/src/runtime/`. Those are W0' substrate commits; this
  plan preserves their current state byte-for-byte.
- Introducing a new `crates/prettify-smoke/` workspace member. The
  root-cause doc's fix-sketch item (b) lists this as an alternative
  to Change 3; the current plan elects Change 3 (outright removal)
  because bbnf's tests have zero live gorgeous references today.
  Creating a new crate is reserved for a future wave where someone
  deliberately wants to add prettify smoke coverage back.
- Any CI config change. The gates above are measured locally; CI
  absorbs them as implicit steady-state floors.
