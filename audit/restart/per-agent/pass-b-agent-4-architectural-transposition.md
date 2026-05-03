# Pass B — Agent B.4 — Architectural Transposition

Date: 2026-05-03
Lens: Macro-level restructuring proposals for codegen / runtime / optimiser shape.
Source: `docs/restart/PASS-B-CODEGEN-MID.md` §Methodology — Agent B.4.

The directive poses six concrete questions; each is treated below
with the rubric **current state (path:line) | proposed shape | locks
honoured | migration cost | carry to synthesizer**.

---

## Q1 — Should `codegen/` become a `crates/bbnf-codegen/` crate? Should `runtime/` become `crates/bbnf-runtime/`?

### Current state

`crates/core/src/backend/` — 119 files, ~22,161 LOC (per §B.1 inventory).
`crates/core/src/runtime/` — 75 files, ~12,007 LOC.
`crates/core/src/pipeline/` + `pipeline.rs` — 9 files, ~1,526 LOC.

All three live inside `crates/core/`. The `crates/core/` crate carries:
- `lib.rs` (38 LOC) re-exports
- `types.rs`, `css_types.rs` (top-level)
- `grammar/` (incl. `generated/` 168K LOC + `host.rs` + `schema/`)
- `imports/` (Pass A — source acquisition)
- `lower/` (Pass A — BbnfView → IR)
- `path/` (path types — under Lock 7 consolidation)
- `pipeline/` + `pipeline.rs`
- `backend/`
- `runtime/`
- `graph/` (Pass A — dep graph)

`crates/core/` is the workspace's god crate.

### Proposed shape

Two new crates plus a path consolidation:

| New crate | Membership | Source moves from |
|---|---|---|
| `crates/bbnf-codegen/` | the `Emitter` trait + driver + per-shape strategy + per-backend impls (Rust / TS / WASM) | `crates/core/src/backend/` entire subtree (119 files) |
| `crates/bbnf-runtime/` | grammar-agnostic runtime — path, error, view trait, runtime template | `crates/core/src/runtime/` mechanism files: `builder.rs`, `builder_template.rs`, `arena_template.rs`, `error.rs`, `handle.rs`, `path.rs`, `view.rs`, `mod.rs` |
| `crates/<grammar>/` | per-grammar declaration crates (one per grammar) | `crates/core/src/runtime/<g>/` per-grammar specialised content; per-grammar host fns (e.g. `parse_hex_color`); per-grammar `<g>Value` / `<g>Document` if they cannot template |
| `crates/path/` | consolidated path crate per Lock 7 | `crates/core/src/path/` + `crates/core/src/runtime/path.rs` + `crates/bbnf-path/` proc-macro shell + `crates/bbnf-path-ts/` cdylib shell |
| `crates/path-core/` | non-proc-macro crate for shared path lex/lower/validate logic per CENSUS §4.1 | factored out of `bbnf-path` + `bbnf-path-ts` mirrors |

`crates/core/` shrinks dramatically — the main shifts are codegen
(out), runtime (out), per-grammar (out), path (out). What remains:
`grammar/` source-side + `pipeline/` + `lower/` + `imports/` + `graph/`
+ top-level `types.rs`. CENSUS §2.1 also moves `css_types.rs` to a
per-grammar declaration crate.

### Locks honoured

- **Lock 13** — splitting `crates/core/src/runtime/` and `backend/` into separate crates eliminates the god directory archetype
- **Lock 14** — per-grammar declaration crates carry per-grammar specialised content; `bbnf-runtime` carries ZERO grammar-named modules
- **Lock 11** — `bbnf-codegen` + `bbnf-runtime` join the path-dep family; promote to registry once API stabilises
- **Lock 5** — `bbnf-codegen` is the `Emitter` trait + driver crate; per-backend lowerers are organised within (Rust / TS / WASM as sub-modules or feature-gated)
- **Lock 7** — `crates/path/` consolidates per Lock 7

### Migration cost

- `bbnf-codegen` extraction — moderate. The Rust shape-dispatcher's
  per-shape modules carry intricate interdependencies via `RustEmitCtx`
  (see `backend/rust/emitter_types.rs`). Extracting requires moving
  `RustEmitter` + `RustEmitCtx` + `BackendType` + every `shapes/` sub-module
  (~9000 LOC) plus the driver's per-IrNode dispatch (~1500 LOC) plus
  the strategy/types/kernels/prettify substrate (~2500 LOC) plus
  TS+WASM (~3100 LOC).
- `bbnf-runtime` extraction — easier. The mechanism files are 8 in
  count, total ~1077 LOC. The trait surface is settled (CENSUS §4.2
  KEEP). The per-grammar dirs migrate to `crates/<grammar>/`.
- `crates/<grammar>/` declaration crates — large. 9 grammars × ~1500
  LOC (averaged) = ~13K LOC of per-grammar specialised content moves.
  Trivial cohort retires (template-emitted) so net is 4 specialised
  crates × ~3000 LOC = ~12K LOC.
- `crates/path/` consolidation — moderate. `bbnf-path` proc-macro
  + `bbnf-path-ts` cdylib + `crates/core/src/path/` runtime types +
  `runtime/path.rs` legacy alphabet → one consolidated `crates/path/`
  with `path-core` sibling for shared logic.

### Carry to synthesizer

Aggregate macro-pivot: split `crates/core/` into ~6 sub-crates by
concern. The synthesizer must adjudicate the per-grammar declaration
crate granularity (one crate per grammar? one crate per cohort?).

---

## Q2 — Per-grammar runtime modules retire and emit from a single `bbnf-runtime-template`. Where does the template live? How is it consumed?

### Current state

`crates/core/src/runtime/builder_template.rs` (286 LOC) factors the
`StructBuilder` impl shared by 5 trivial cohort grammars. The trivial
cohort's `builder.rs` is a 54-LOC instantiation. `arena_template.rs`
(134 LOC) factors the arena.

But: `document.rs` (171-237 LOC), `view.rs` (64-80 LOC), `kind.rs`
(46-67 LOC), `value.rs` (23-57 LOC), `mod.rs` (18-49 LOC) are NOT
templated; each trivial-cohort grammar has 7 hand-written files
totalling ~440 LOC.

The four specialised grammars (BBNF, CSS L4, Sheets, JSON) each have
~1500-3100 LOC of hand-written specialised content.

### Proposed shape

A `crates/bbnf-runtime-template/` proc-macro crate, OR a `bbnf-runtime`
proc-macro module alongside the runtime crate, that consumes:
- grammar source `<g>.bbnf` (or its post-pass IR projection)
- workspace metadata declaring strategy
- per-grammar registry sidecar (`<g>.registry.json`)

and emits, at xtask-regen time, the per-grammar runtime modules:
- `<g>Value` enum (typed-projection of grammar's typed leaves)
- `<g>Document` struct with `Path<'p>::get<T>` impl
- `<g>View` cursor
- `<g>Kind` enum (compound kinds)
- `<g>Arena` slab arena
- `<g>Builder` (concrete `StructBuilder` impl)
- `<g>::mod` re-export hub

Per Lock 14 verbatim: "*Per-grammar runtime modules (value, document,
view, kind) are emitted from a single grammar-agnostic generator
template that consumes (grammar source + workspace metadata) and
produces typed Rust; hand-written per-grammar runtime files are
forbidden.*"

### Where the template lives

Two places it could live:

**(a) Inside xtask** — `xtask/src/regen/runtime_template.rs` consumes
grammar IR + registry → emits typed Rust per-grammar. The output
lands at `crates/bbnf-runtime/src/generated/<g>.rs` or per-grammar
declaration crate `crates/<grammar>/src/runtime.rs`. Per Lock 6, the
output is committed.

**(b) Separate `bbnf-runtime-template` crate** — a generator library
xtask path-deps. Cleaner separation; allows third-party grammars to
share the template. Same emission surface.

The synthesis adjudicates between (a) and (b); `feedback_one-codegen-path`
favours (a) for fewer crates.

### Per-grammar specialisation

The four specialised cohorts (BBNF, CSS L4, Sheets, JSON) carry hand-written
specialised content. Per Lock 14, this content lives in:

- per-grammar declaration crate `crates/<grammar>/`
- the grammar source `.bbnf` declares `@runtime_extension <ident>` for
  the specialised type
- the runtime template emits the canonical per-grammar machinery; the
  declaration crate's extension extends it (via Rust traits or an
  emit hook the template calls into)

Open question for the synthesis: is the *14-variant CSS L4 OpenFrame*
template-emittable (each variant describable from grammar shape +
host-fn metadata) or genuinely hand-written? If the former, no special
case. If the latter, the synthesis must accept per-grammar declaration
crates carrying ~1000 LOC of specialised runtime code — but then the
generic template emits the CANONICAL surface and the declaration
crate is *additive*, not replacing.

### Locks honoured

- **Lock 14** — zero hand-written per-grammar runtime files in the
  generic substrate
- **Lock 6** — template output committed under per-grammar declaration
  crates' `src/`
- **Lock 13** — `crates/bbnf-runtime/src/` no longer carries 9
  per-grammar dirs

### Migration cost

- Template development — 200-400 LOC of generator code (proc-macro2 + quote)
- Extension hooks for specialised grammars — 50-100 LOC per hook
- Per-grammar declaration crate scaffold — 9 new crates × ~50 LOC = ~450 LOC
- Validate behavioural parity via existing runtime tests; the 9 grammars'
  test surface persists

### Carry to synthesizer

This is the **single largest Pass-B architectural pivot**. Lock 14
demands it. The 9-per-grammar runtime dirs × 7 hand-written files =
63 files; emission template retires ~50 of them, leaves 13 specialised
files in declaration crates.

---

## Q3 — Optimiser sister crates: workspace-internal vs crates.io-ready

### Current state

- `crates/egraph/` — workspace member; no `[publish]` set; description
  "General-purpose e-graph: equality saturation, rewrite rules,
  cost-model extraction"; documented as "deliberately domain-agnostic"
- `crates/egraph-derive/` — workspace member; `#[derive(Language)]`
  proc-macro
- `crates/csp-solver/` — workspace member; isomorphic Rust+Python per
  `feedback_csp-solver-crate`; PyO3 bindings under `py.rs` feature flag
- `crates/simd-scan/` — workspace member; per `feedback_general-infra-crates`
  in own crate

### Proposed shape

| Crate | Disposition | Rationale |
|---|---|---|
| `egraph` | promote to crates.io | already domain-agnostic; sample analyses + tests substantial; `csp_scheduler` a contribution to e-graph literature |
| `egraph-derive` | promote alongside egraph | proc-macro derive; one publishes alongside the parent crate |
| `csp-solver` | promote to crates.io | sister-repo's csc411 carries Python+Rust isomorphism; per `feedback_csp-always-optimize` foundational library |
| `simd-scan` | per Lock 11 stays workspace-internal | "simd-scan + bootstrap + analysis + lsp stay workspace-internal" |

Per Lock 11 explicit: "*promote to registry once stable*". For each
of egraph + egraph-derive + csp-solver, the API-freeze checklist:

- public API audit (no `pub(crate)` types in trait surfaces; no leaked
  internal types)
- doc-comments on every public item (`#![cfg_attr(not(test), warn(missing_docs))]`
  already set on egraph)
- semver discipline (0.1.x window for incubation; 1.0 once consumed
  externally)
- README + examples (egraph already has tests demonstrating; csp-solver
  has puzzles; simd-scan has SOTA-cited correctness oracles)
- LICENSE — egraph has `license = "MIT"`; verify on csp-solver

### Locks honoured

- **Lock 11** — promotion respects the path-dep-then-registry pattern
- **Lock 4** — orthogonal optimisation by output-piping persists

### Migration cost

- API freeze audit — moderate (per-crate ~1 day)
- Registry release scaffolding (Cargo.toml metadata, CI publish gate)
- workspace `Cargo.toml` swap path-dep → version-dep

### Carry to synthesizer

Promote 3 crates (egraph, egraph-derive, csp-solver) to crates.io
once API freeze passes. Keep simd-scan workspace-internal per Lock 11.

---

## Q4 — simd-scan promotion question

Per Lock 11 explicit: "*simd-scan + bootstrap + analysis + lsp stay
workspace-internal.*" Lock-direct: stays internal.

But simd-scan's mechanism (`StructuralIndex` + per-arch kernels) is
a SOTA-cited general-purpose construct (`feedback_general-infra-crates`,
`feedback_kiss-perf-bias`). The crate has ~3400 LOC src, 4 per-arch
kernels (NEON, AVX2, AVX-512, WASM SIMD), scalar fallback as
correctness oracle, fuzz tests.

Alternative: keep workspace-internal but namespace cleanly. Per Lock 11,
no promotion. The synthesis adjudicates.

### Carry to synthesizer

Default disposition per Lock 11 verbatim: workspace-internal. The
synthesiser may revisit if the alphabet/kernel surface becomes
externally consumed.

---

## Q5 — xtask split or stay monolithic

### Current state

`xtask/` carries one subcommand (Regen) with sub-flags (`--grammar`,
`--check`, `--staged`, `--output`). 5 source files, ~1047 LOC. Per
§B.5 inventory.

xtask is *not* monolithic per the directive's framing; it has one
subcommand. The directive asks "*currently bundles regen + bench +
check + test*" — but bench / check / test live elsewhere (bench in
`crates/lsp/benches/`, etc.).

### Proposed shape

Keep monolithic. The directive's framing is incorrect: xtask carries
ONE subcommand. There is no `xtask bench` / `xtask check` / `xtask test`.

If the synthesis adds new subcommands (e.g. `xtask runtime-template`
for the Lock-14 template-emit flow per Q2), grow them as additional
subcommands of one binary. Per `feedback_no-god-modules` the `regen.rs`
file at 849 LOC IS a god module — split into:

- `xtask/src/regen/manifest.rs` — read `[workspace.metadata.bbnf.grammars]`
- `xtask/src/regen/pipeline.rs` — invoke IR pipeline
- `xtask/src/regen/emit.rs` — proc-macro2 + quote + prettyplease
- `xtask/src/regen/check.rs` — diff against tempdir
- `xtask/src/regen/staged.rs` — git-aware fast path
- `xtask/src/regen/mod.rs` — orchestration

### Locks honoured

- **Lock 6** — xtask's emit + commit chain unchanged
- **Lock 13** — split god module per `feedback_no-god-modules`

### Migration cost

- Light. ~849 LOC split into 5-6 files; behaviour identical.

### Carry to synthesizer

Keep one binary; split `regen.rs` god module.

---

## Q6 — Generated-output tree relocation

### Current state

`crates/core/src/grammar/generated/` — 10 files, 168,785 LOC committed
in `crates/core/`. The generated files are `include!`d via `crates/core/src/grammar/generated/mod.rs`
into the bbnf crate's namespace.

Each generated file contains the per-grammar parser implementation:
parse fns, scan helpers, and the runtime-trait method calls into
`crate::runtime::<g>::*`.

### Proposed shape

Two viable relocation targets:

**(a) `crates/bbnf-parse/src/parse/generated/`** — per Phase-4 surgery 22
- the generated parsers live in the parse crate's `parse/generated/`
- consumers (per-grammar declaration crates) `pub use bbnf_parse::generated::<g>::*`
- one consolidated parse-output location

**(b) `crates/<grammar>/generated/`** — per-grammar
- each grammar's generated parser lives in its own declaration crate
- `crates/json/src/generated/json.rs` (3500 LOC)
- `crates/css-l4/src/generated/css_l4.rs` (107K LOC)
- the `bbnf-parse` crate carries only the generic substrate (Cursor,
  ScanState, dispatch helpers); per-grammar code lives per-grammar

(b) honours Lock 13 + Lock 14 more cleanly: the 168K LOC distributes
across declaration crates rather than concentrating in one.

(a) is closer to the Phase-4 surgery's intent; the synthesis decides.

### Locks honoured

- **Lock 13** — neither central god-directory; (b) more decentralised
- **Lock 14** — (b) avoids any per-grammar code in `bbnf-parse`
- **Lock 6** — output committed; xtask emits unchanged

### Migration cost

- Relocate 168K LOC of generated files — moderate; xtask's output-path
  flag already supports per-grammar dirs (`--output` per main.rs:48-55)
- Update consumers (`bbnf-bootstrap`, `bbnf` re-exports, runtime
  per-grammar dirs)

### Carry to synthesizer

Synthesis chooses (a) vs (b). (b) is more aligned with Lock-14;
(a) is simpler to implement.

---

## Q7 — `Emitter` trait reshape

### Current state

`crates/core/src/backend/emitter.rs:31` — `pub trait Emitter` with ~30
methods covering Leaves / Sequences / Alternations / Repeats /
References / Binary / Value-manipulation / Ws-trim / Token-dispatch.
`Self::Output: Default`. Default impls return `Self::Output::default()`.

The Rust backend overrides 3 methods (`emit_grammar`, `emit_type_definitions`,
`emit_rule_function_impl`); the rest land via default. The Rust path
walks via shape-dispatcher (`backend/rust/emitter/shapes/`). TS + WASM
override every method; they walk per-IrNode.

### Proposed shape

Two paths walked:
- per-IrNode (TS + WASM)
- per-shape (Rust)

Synthesis question: collapse to one walking pattern, OR document the
two-pattern surface explicitly.

**Option (a)** — Rust adopts per-IrNode walk. The shape-dispatcher
retires; each shape's emit logic moves into the corresponding `emit_*`
method. The Emitter trait becomes the single walking surface.

Migration cost: high. ~9000 LOC of Rust shape-dispatcher logic
re-distributes across 30 trait methods. Loss of cohesion: a single
shape's emit currently lives in one file; under per-IrNode walk, it
distributes across multiple methods.

**Option (b)** — TS + WASM adopt per-shape walk. `backend/ts/shapes/`
+ `backend/wasm/shapes/` mirror Rust's shape-dispatcher. The Emitter
trait coarsens to ~10 methods (one per shape).

Migration cost: lower. TS + WASM re-organise their emitters; the
Emitter trait surface shrinks. Per `feedback_isomorphic-api`, the
backends become more parallel.

**Option (c)** — Document the two-pattern surface. Accept that Rust
walks per-shape (rich, cohesive) and TS / WASM walk per-IrNode (flat,
simple). The trait carries both surfaces.

(c) is `feedback_no-orthogonal-codepaths`-fault. The synthesis chooses
between (a) and (b).

### Locks honoured

- **Lock 5** — IR + per-backend lower; one walking pattern more strictly
- **Lock-system-cohesion** — backends share one walking shape

### Migration cost

(a): high (9000 LOC redistribute)
(b): moderate (TS + WASM re-organise, ~3000 LOC)

### Carry to synthesizer

The synthesis picks (a) or (b). (b) is lower cost; (a) is more
elegant. Per `feedback_no-workarounds-arch` "Architectural
transpositions for elegance/simplicity/performance are mandatory",
(a) wins on elegance.

---

## Q8 (extra) — Sub-crate granularity for codegen

The `bbnf-codegen` crate at ~22K LOC is itself a candidate for
sub-splitting. Per `feedback_directory-modules`:

- `bbnf-codegen-driver` — driver subtree (1500 LOC)
- `bbnf-codegen-strategy` — strategy subtree (~400 LOC)
- `bbnf-codegen-rust` — Rust backend (~13K LOC)
- `bbnf-codegen-ts` — TS backend (~1700 LOC)
- `bbnf-codegen-wasm` — WASM backend (~1400 LOC)
- `bbnf-codegen-prettify` — prettify channel (~700 LOC)
- `bbnf-codegen-types` + `bbnf-codegen-kernels` — shared types + kernels (~1500 LOC)

This is a 6-way crate split. Per `feedback_kiss-perf-bias`, the
synthesis should propose the smallest set of changes; 6-way split is
sprawling. Default: keep as `bbnf-codegen` with internal sub-modules.

Re-visit only if external consumers want per-backend imports without
the whole codegen surface (unlikely; xtask is the sole consumer).

---

## Q9 (extra) — Pipeline crate

`crates/core/src/pipeline/` + `pipeline.rs` (1526 LOC) + the `pipeline.rs`
file-form / `pipeline/` directory collision. Per CENSUS §4.3:
RESTRUCTURE — convert `pipeline.rs` into `pipeline/mod.rs`.

Beyond the file-form fix, the pipeline crate should remain in
`crates/core/` until `bbnf-codegen` extraction settles; the pipeline
is the orchestration glue between IR (Pass A) + codegen (Pass B) +
xtask. Once `bbnf-codegen` lives separately, the pipeline either
follows or stays in `crates/core/` as the integration layer.

---

## Aggregate carry to synthesizer

Pass-B macro-pivots the synthesis must adjudicate:

| # | Pivot | Locks | Cost |
|---|---|---|---|
| 1 | Split `crates/core/` into `bbnf-codegen` + `bbnf-runtime` + `crates/<grammar>/` | 5, 11, 13, 14 | high |
| 2 | `bbnf-runtime-template` for per-grammar runtime emission (xtask vs proc-macro vs build script) | 14 | moderate |
| 3 | Promote egraph + egraph-derive + csp-solver to crates.io | 11 | moderate |
| 4 | Keep simd-scan workspace-internal | 11 | none |
| 5 | xtask: split `regen.rs` god module; keep monolithic binary | 13 | low |
| 6 | Relocate `generated/` to per-grammar declaration crates OR `bbnf-parse/src/parse/generated/` | 13, 14 | moderate |
| 7 | Reshape `Emitter` trait to one walking pattern (per-IrNode wins on elegance; per-shape wins on cost) | 5 | high (per-IrNode) / moderate (per-shape) |
| 8 | Convert `pipeline.rs` to `pipeline/mod.rs` | directory-module-structure | low |
| 9 | Consolidate path machinery per Lock 7 (4 locations → `crates/path/` + `path-core`) | 7 | moderate |

The two highest-impact pivots are #1 (the crate split) and #2 (the
runtime template). #2 *requires* #1; #1 enables #2 at no marginal
cost. Together they retire Lock-13 + Lock-14 violations across Pass B
in one pass.
