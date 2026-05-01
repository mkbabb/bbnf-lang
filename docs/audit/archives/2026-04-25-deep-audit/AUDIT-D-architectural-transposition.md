# AUDIT-D — Architectural transposition: proc-macro IR pipeline retirement

**Authored**: 2026-04-25 (audit-delta worktree).
**Posture**: read-only, gestalt audit. Companion to γ (substrate
relocation), β (consumer policy), α (graph topology).
**Context premise**: γ has determined that AY-I.W0's derive-cache +
Watt precompilation does not reduce the cold-miss expansion wall —
relocation lowers the macro-crate compile cost, but the macro itself
still runs the 17-pass IR pipeline + emits ~30 k-line `TokenStream`
inside rustc's expand phase on every cold consumer.

## 1. Gestalt thesis

**Code generation is build-time work that produces source on disk; the
proc-macro contract is for small, local transformations of user code,
not workspace-wide IR pipeline execution.** A `#[derive(Parser)]`
annotation at every consumer site re-runs a 17-pass intermediate
representation pipeline + a 30 k-line `TokenStream` emission inside
rustc's expand phase, and rustc has no recourse but to re-tokenise,
re-parse, re-borrow-check, and re-metadata-emit that surface for every
consuming crate. The transposition is structural: the IR pipeline
moves out of `rustc`'s expand phase into a workspace-level build step
that emits source files once; the proc-macro retires; the
already-existing `crates/core/src/grammar/generated.rs` becomes
the singular product, generalised from "BBNF self-hosting only"
to "every grammar consumed in the workspace."

## 2. Current contract dissection — exact mechanism + cost attribution

### 2.1 Mechanism (cited)

`bbnf_derive::bbnf_derive` at `crates/derive/src/lib.rs:280-361` is the
proc-macro entry. On cache miss (lines 300-303), it:

1. Parses `#[parser(...)]` attrs (`crates/derive/src/lib.rs:226-278`).
2. Calls `compile_paths_request` (`crates/derive/src/lib.rs:317`),
   defined at `crates/core/src/pipeline/compile.rs:113-120`. That
   function loads + merges grammar files from disk
   (`load_merged_paths`) and routes to `compile_ast_request_internal`
   (`crates/core/src/pipeline/compile.rs:150-158`).
3. `compile_ast_request_internal` calls `compile_ast_common`
   (`crates/core/src/pipeline/compile.rs:418`) which runs the
   17-pass pipeline. The call sites at lines 467-755 cover:
   `compute_first_sets`, `eliminate_indirect_lr`, `eliminate_direct_lr`,
   `compute_aliases`, `compute_transparent`, `canonicalize_aliases`,
   `compute_scc`, `prune_unreachable`, `inline_acyclic`, `fuse_single_use`,
   `eliminate_epsilon`, `merge_literals`, `factor_common_prefixes`,
   `hoist_recurring_patterns`, `sort_alt_branches`,
   `refine_span_eligibility`, `compute_follow_sets`,
   `factor_regex_with_lookahead`, `fuse_token_dispatch`,
   `compute_regex_info`, `compute_structural_alphabet`,
   `mine_recognizers`, CSP solver passes, `generate_dispatch_tables`,
   `classify_materialization`, `solve_grammar_components`. The 17-pass
   description is approximate; the actual count exceeds 25 if every
   `bbnf_ir::passes::*` invocation is counted.
4. `finalize_compile` at `crates/core/src/pipeline/compile.rs:160-168`
   calls `prepare_grammar` (`crates/core/src/backend/driver/analysis.rs:79`)
   which runs additional analysis (alt strategies, delim-scan configs,
   key-dispatch configs, materialization map).
5. `bbnf::generate::generate_all` at `crates/core/src/generate/mod.rs:36-110`
   runs Track 1 (CST schema emission, `generate/mod.rs:99-104`) and
   Track 2 (`backend::driver::compile_grammar`, `generate/mod.rs:86-93`)
   which together emit the parser functions, type definitions, dispatch
   tables, view structs, materializers, scan policies, prettify methods.
6. The result is wrapped in a uniquely-named submodule
   (`crates/derive/src/lib.rs:336-353`) and returned as `TokenStream`.
7. The cache-write at `crates/derive/src/lib.rs:356-358` persists the
   stringified `TokenStream` to `target/.bbnf-cache/<key>.rs`.

The cached entry is read back at `crates/derive/src/lib.rs:206-210` and
parsed back into a `TokenStream` via `cached.parse::<TokenStream>()`.

### 2.2 Cost attribution

The W0p infra-root-cause audit (`docs/tranches/AY-II/audit/W0p-infra-root-cause.md`)
measured one `rustc` invocation on `gorgeous` at **9 min 16 s** wall-clock
across 5 derive sites — **~110 s/site** averaged. The 80-min
bbnf-bootstrap wall is roughly 1 derive site × the maximally-large
grammar (BBNF self-hosting) under `cargo expand` overhead.

Cost decomposes structurally as:

| Phase | Approximate share (cold) | Skipped on cache hit? |
|---|---:|---|
| File I/O + grammar parse | < 1 % | Yes |
| 17-pass IR pipeline (`compile_ast_common`) | ~30 % | Yes |
| `prepare_grammar` analysis (CSP, alt strategies, materialization) | ~15 % | Yes |
| `generate_all` (`backend::driver::compile_grammar` + schema emit) | ~25 % | Yes |
| `quote!` + `TokenStream` build | ~5 % | Yes |
| **rustc tokenisation + parse + borrow-check of returned TokenStream** | **~25 %** | **NO** |
| TokenStream parse on cache-hit (`cached.parse::<TokenStream>()`) | ~3 % | N/A (cache-hit cost) |

The 25 % rustc-side share on a 30 k-line `TokenStream` is the
**load-bearing irreducible cost** the derive-cache cannot touch, because
the cache stores a stringified `TokenStream` and rustc must still
ingest it. On a content-keyed cache hit, that ~25 % becomes the entire
remaining cost (bbnf-bootstrap warm-cache observed ~40+ s pre-kill).

The 80-min bootstrap wall is the cold pathology: the full pipeline
runs because there is no on-disk product the rustc invocation can
shortcut to.

## 3. First-principles critique — why this contract is the wrong primitive

**Proc-macros are designed for small, local transformations of user
code.** `serde::Serialize`, `Debug`, `clap::Parser`, `thiserror::Error`
all expand to ≪1 k LOC per site. They are sub-second per invocation.
The `bbnf_derive` contract emits 30 k LOC per site — three orders of
magnitude beyond the proc-macro design centre.

**rustc's expand phase has no internal parallelism across sibling
macro invocations.** The W0p audit at line 35 cites: "the rustc
front-end expands all 5 proc-macro invocations **serially** inside that
single process — because a single rustc invocation has no internal
parallelism across macro expansions in sibling modules." Five derive
sites in `gorgeous` lib serialise to one rustc process; the only
parallelism comes from cargo splitting test files into separate rustc
invocations (and even then each test file's expansions serialise).

**IR pipeline output is a function of grammar source, not of the
consuming crate.** The cache key at `crates/derive/src/lib.rs:128-175`
already encodes this: grammar contents + attrs + ident → unique key.
But the cache still pays rustc's TokenStream-rehydration cost per
consumer because the cached artefact is a stringified TokenStream, not
a Rust source file. The cache is the right idea applied to the wrong
artefact.

**The on-disk artefact already exists for one grammar.**
`crates/core/src/grammar/generated.rs` (33 293 lines, 1.55 MB) is the
bootstrap-script-produced source-file form. `scripts/bootstrap-bbnf.sh`
runs `cargo expand -p bbnf-bootstrap`, post-processes the output, and
writes the result. That product is already what the proc-macro is
recreating at expand-time on every consumer — but only for BBNF.

**The derive site is a documentation marker, not a transformation.** A
`#[derive(Parser)]` site declares "this struct is the entry symbol for
parser code generated from grammar X with attrs Y." The annotation
itself transforms nothing the user wrote — it generates fresh code
adjacent to the marker struct. That intent is faithfully captured by a
build.rs / xtask that emits a source file named after the struct, not
by an expand-time emission cycle that re-runs on every consumer.

## 4. Transposition options

### T1 — build.rs codegen (idiomatic Rust)

`crates/core/build.rs` runs the IR pipeline once per workspace build,
emits `OUT_DIR/grammar_<name>.rs` for every grammar declared in a
manifest manifest `[package.metadata.bbnf]` table. The `bbnf` lib
`include!`s the products. The proc-macro retires; `#[derive(Parser)]`
becomes a marker trait impl `parser::Parser for X` that the included
file already provides.

- **Survives**: `crates/core/src/generate/`, `crates/ir/`,
  `crates/core/src/pipeline/`. Most of `bbnf_derive` deletes;
  attribute parsing moves into a build-script helper.
- **Deletes**: 95 % of `crates/derive/src/lib.rs`; the
  `target/.bbnf-cache/` content-keyed cache (cargo's own
  `OUT_DIR` dependency tracking replaces it); the cargo-expand step
  in `scripts/bootstrap-bbnf.sh` (the post-processing script becomes a
  no-op once the build.rs path is canonical for every grammar).
- **Workspace integration**: each consuming crate declares a
  `[package.metadata.bbnf]` table listing grammars + attrs;
  `crates/core/build.rs` reads sibling manifests via
  `cargo_metadata` and emits one source file per declaration into
  `OUT_DIR`; the consumer adds `include!(concat!(env!("OUT_DIR"),
  "/grammar_<ident>.rs"));` once.
- **Compatibility**: AY-II invariants 1-7 unchanged (the source-level
  contract is unchanged; only the production path moves). AZ-I/II
  StructRegistry emission goes through the same build.rs;
  `project_types` runs once and writes results into the emitted file.
  BA/BB cache invalidation is cargo's `OUT_DIR` cache, simpler than
  `.bbnf-cache`.

### T2 — pre-serialised IR + thin proc-macro

Bootstrap step writes `GrammarIR` MessagePack bytes to disk
(`crates/ir/src/types/grammar.rs:508-516` already provides
`to_msgpack` / `from_msgpack`). The proc-macro at expand-time
deserialises the blob and emits a small wrapper referencing
pre-generated function bodies that live in `generated.rs`.

- **Survives**: serialisation surface; `bbnf_derive` reduced from 361
  to ~80 lines; on-disk `generated.rs` per grammar.
- **Deletes**: in-macro 17-pass pipeline; in-macro `generate_all`.
- **Cost**: bootstrap step becomes more complex (must produce blob +
  source); proc-macro retains a non-trivial expand-time cost (deserialise
  + tokenise wrapper).
- **Failure mode**: still re-emits TokenStream per consumer; rustc
  still pays the rehydration cost on the wrapper, though the wrapper
  is much smaller (a few hundred lines vs 30 k).

### T3 — xtask + checked-in generation

`cargo xtask regen` is the canonical regen entry. It runs
`compile_paths_request` + `generate_all` + post-processing, writing
`crates/core/src/grammar/generated/<grammar>.rs` for every workspace
grammar. The proc-macro retires entirely; `#[derive(Parser)]` becomes
a documentation marker that does not expand to any code; the
already-emitted `generated/<grammar>.rs` provides the surface.

- **Survives**: `crates/core/src/generate/`, `crates/ir/`,
  `crates/core/src/pipeline/`. The bootstrap script generalises to
  any grammar.
- **Deletes**: `crates/derive/` entirely; `target/.bbnf-cache/`;
  the `cargo expand` post-process tail of `scripts/bootstrap-bbnf.sh`
  (the regen now goes directly through the codegen API, not through
  rustc's expand phase).
- **Workspace integration**: workspace-level `xtask` crate; CI gate
  to assert checked-in `generated/*.rs` matches xtask output;
  `cargo build` does not run codegen — it consumes the checked-in
  source.
- **Compatibility**: cleanest separation. `cargo build` is pure
  compile; xtask is the codegen tool; CI checks freshness. AY-II
  invariants unchanged. AZ-II's "byte-equal stage A vs stage B"
  becomes a `cargo xtask regen && git diff --exit-code` check.

### T4 — pre-computed IR cached as static data

Hybrid of T2 and T3: bootstrap binary writes IR blob; proc-macro reads
blob + emits parser bodies. Strictly worse than T1/T3 because the
proc-macro is retained; strictly better than T2 because the cache is
content-addressed by a checked-in artefact rather than a derived
target/ entry.

## 5. Recommendation + sketch — T3 (xtask + checked-in generation)

T3 is the elected path. Rationale:

1. **Eliminates the entire 80-min wall.** The IR pipeline never runs
   inside rustc. `cargo build` consumes pre-emitted source.
2. **Idiomatic Rust.** xtask is the workspace-canonical pattern for
   codegen tools. `cargo xtask regen` is composable with CI, with
   pre-commit hooks, with developer workflow.
3. **Single source of truth.** The `generated/<grammar>.rs` files are
   the canonical product. No dual surface (proc-macro AND on-disk).
4. **Existing artefact generalises.** `crates/core/src/grammar/generated.rs`
   already exists for BBNF. T3 generalises the production of that file
   to every grammar in the workspace. The shape is already proven.
5. **Cache discipline collapses.** `target/.bbnf-cache/` retires;
   `BBNF_SCHEMA_VERSION` retires; the W0p audit's Rank 3 candidate
   (build.rs fingerprint cascade) retires. CI freshness check replaces
   them all.
6. **Bootstrap simplification.** AZ-II's stage A / stage B byte-equal
   gate becomes `cargo xtask regen` running twice in succession with
   `git diff --exit-code` between runs — far simpler than the current
   `cargo expand` + post-process pipeline.

### Wave breakdown

**W0 — substrate**: extract the regen entry from `bbnf_derive` /
`scripts/bootstrap-bbnf.sh` into `xtask/src/regen.rs`. The xtask reads
a workspace-root manifest enumerating grammar+attrs+ident triples; for
each, runs `compile_paths_request` + `generate_all`; writes
`crates/core/src/grammar/generated/<ident>.rs` with the standard
header (matching the post-processing emitted today by the bootstrap
script). LOC delta: +400 (xtask), no source deletions yet.

**W1 — consumers cut over**: every `#[derive(Parser)]` site in the
workspace (5 in `gorgeous/`, 1 in `bbnf-bootstrap/`, ~50 in
`crates/core/tests/`) replaces the derive with `include!(concat!(
"path/to/generated/", ident, ".rs"))` + the marker struct.
`#[parser(path = ...)]` attrs migrate to entries in the workspace-root
manifest. LOC delta: ~−200 net (derive sites shrink; manifest grows).

**W2 — proc-macro retirement**: `crates/derive/` deletes entirely.
`crates/core/Cargo.toml` removes the `bbnf-derive` dep. The
W0p-cited Rank 3 (build.rs fingerprint cascade) retires with it. LOC
delta: −500.

**W3 — script simplification**: `scripts/bootstrap-bbnf.sh` retires.
The cargo-expand + post-process body is unnecessary because the
xtask path emits clean source directly. The header-emission Python
fragment becomes a small Rust function inside `xtask/src/regen.rs`.
LOC delta: −350.

**W4 — CI + pre-commit gate**: CI runs `cargo xtask regen --check`
which regenerates into a tempdir and `git diff`s against the checked-in
`generated/`. Pre-commit hook offers `cargo xtask regen` as a
quick-fix. Replaces `BBNF_SCHEMA_VERSION` discipline (manual
schema-version bumps retire). LOC delta: +50 CI workflow, +30
xtask check mode.

**W5 — AY-II close ceremony adaptation** (trivial under new contract):
the W0p invariant restoration (`§14-§19`) is checked by re-running
`cargo xtask regen` and asserting the tape-side runtime tests. The
fused-builder + scan-policy + materializer-call-count invariants
remain enforced because the codegen output is unchanged shape — only
the production path moves.

Estimated total delta: **−1 000 LOC** (proc-macro deletion + script
simplification dominate the net), substantial cold-build wall
collapse (80 min → 0 min IR-pipeline wall; only rustc compile time
on the same source the consumer would compile under any path).

## 6. Compatibility with AY-II / AZ-I / AZ-II / BA / BB

- **AY-II.W0' close**: the FusedBuilder, projection totality, scan
  policy emission, materializer call-count — all are properties of the
  emitted source. T3 changes how the source is produced, not what it
  contains. Invariants `§14-§19` survive verbatim.
- **AY-II W1-W5**: the same emission code paths run; the surface is
  unchanged. Audit-pass enforcement (typed materialisation totality,
  consumer totality) runs against the checked-in `generated/<grammar>.rs`.
- **AZ-I**: direct-to-struct emission for JSON / CSS L4 / Sheets goes
  through the same xtask path. `StructRegistry` closure runs at xtask
  time; the emitted source contains the struct definitions + builder
  calls. AZ-I's "no tape on data grammars" invariant is unaffected.
- **AZ-II**: BBNF self-hosting cutover becomes "stage A xtask emits
  generated.rs from the tape-based parser; stage B xtask emits
  generated.rs from the struct-based parser; `git diff --exit-code`
  between stages is the close gate." Substantially simpler than the
  current `cargo expand` two-cycle dance. `crates/tape/` deletion
  unchanged.
- **BA**: pointer-path codegen extends `generated/<grammar>.rs` with a
  `Path<Grammar, Target>` type. The xtask is the natural extension
  point; the `path!` macro can either retire (paths are typed values
  emitted into the generated source) or remain as a small surface
  macro that constructs typed values referencing pre-emitted
  accessors.
- **BB**: e-graph rewrite rules become inputs to the xtask (RON files
  under `grammar/<name>/rewrites/*.ron`). Discovery runs as a separate
  xtask subcommand (`cargo xtask discover-rules`) and writes RON files
  consumed on the next `cargo xtask regen`. The current `bbnf_derive`
  build-step that BB.md references for RON compilation retires;
  xtask absorbs that role.

## 7. Risk + mitigation register

| Risk | Likelihood | Impact | Mitigation |
|---|---|---|---|
| `include!`-emitted source files break IDE rust-analyzer ergonomics. | Medium | Medium | rust-analyzer supports `OUT_DIR`-style includes via `cargo metadata`; checked-in `generated/<grammar>.rs` is a real file rust-analyzer indexes natively. T3's checked-in form is strictly better than T1's `OUT_DIR` form for IDE support. |
| `cargo xtask regen` drift between developer machines (line endings, formatting). | Medium | Medium | xtask runs `rustfmt` on output deterministically; CI gate uses byte-exact `git diff` against a frozen rustfmt config. |
| AZ-II byte-equal gate breaks if xtask non-determinism leaks. | Low | High | Existing `BBNF_SCHEMA_VERSION` discipline already enforced determinism in cache key derivation; the same determinism applies to xtask output (sort iteration orders, stable hashing in dispatch table emit). |
| Test-file derive sites are numerous (~50); cutover is mechanical but tedious. | Certain | Low | `cargo xtask migrate-derives` subcommand performs the AST rewrite once across the workspace. |
| Workspace-root grammar manifest format becomes a new design surface. | Medium | Low | Use `cargo metadata` `[package.metadata.bbnf]` per-crate tables instead of a workspace-root TOML; existing pattern in cargo-* tools. |
| Loss of grammar-edit-detect via `bbnf_derive`'s `build.rs`. | Low | Low | `cargo xtask regen --check` in CI catches forgotten regens; pre-commit hook offers the same locally. |
| Audit-γ recommends a different transposition. | Medium | Medium | γ studies whether AZ-I.W0's derive-cache + Watt is sufficient; this audit's premise (γ found it isn't) is the gating condition. The recommendations should converge on T3; if γ recommends T1 (build.rs), the difference is `OUT_DIR` vs checked-in — T3's checked-in form is strictly better for AZ-II's byte-equal gate and for IDE ergonomics. |
| BB rule discovery integrates poorly with xtask topology. | Low | Medium | xtask is the natural home for codegen tooling; BB's `cargo xtask discover-rules` is the obvious sibling subcommand. The RON-files-as-inputs contract decouples discovery from regen. |

## Closing note

The 80-min wall is the symptom; the proc-macro-as-IR-pipeline contract
is the cause. Every other audit-γ remediation (Watt precompilation,
cache locality, derive-cache TTL) operates downstream of the wrong
primitive. T3 retires the primitive itself: code generation moves to
build-time, the proc-macro retires, the on-disk source becomes the
singular product, and the consumer pays only the rustc compile cost
its source files inherently demand. The transposition is one of
contract, not of optimisation; under-the-hood tweaks compound a
mistake that this audit names structurally.
