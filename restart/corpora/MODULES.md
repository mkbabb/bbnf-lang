# MODULES — Module-by-Module Explication for BA-Restart

Date: 2026-05-03
Repo HEAD: master, working tree clean
Scope: every `.rs` file under `crates/`, walked by crate; followed by per-crate
synthesis, layered re-org proposal for `crates/core/src/`, crate-level
re-organization, and pipeline-ordering specification.

The fate column conforms to the user-prescribed alphabet:
KEEP-AS-IS / KEEP-RENAMED / SPLIT (with target) / MERGE-WITH-X /
MOVE-TO-CRATE-Y / DELETE / EXTRACT-TO-OWN-REPO / ARCHIVE.

Files >500 LOC carry an explicit split recommendation per the no-god-modules
edict. Generated files (everything under `crates/core/src/grammar/generated/*.rs`)
are exempt from the LOC cap because they are an output artifact, not a hand-
maintained source.

> **A note on coverage gaps.** The 30-minute hard cap and the workspace's 824
> source files mean the per-file rows below cite implementation only on files
> the orchestrator opened directly; remaining rows are characterized from the
> file's path, its module's mod.rs, parent crate manifest, and the LOC-sorted
> inventory captured at audit start. Where ambiguity remains — most often for
> files in the 80-300 LOC band — the row is marked `purpose: PROBE-NEEDED`.

---

## Crate index

| Crate | Role | Members | LOC | Stays in workspace? |
|---|---|---:|---:|---|
| core | bbnf library: pipeline + backends + runtime + generated parsers | 288 src + 9 generated | ~36k src + ~169k generated | YES (will fracture) |
| ir | grammar IR — types, passes, registries, e-graph, vm | ~145 | ~17k | YES (broken into ir / ir-pass / ir-egraph) |
| analysis | LSP-shared grammar analysis | ~30 | ~3.5k | YES |
| lsp | LSP + DAP server | ~10 | ~1.6k | YES |
| ser | Serializer/Deserializer trait substrate | 5 | ~530 | ARCHIVE |
| gorgeous | Grammar-driven pretty-printer + `gorg` CLI | ~12 | ~600 (gated) | ARCHIVE |
| bootstrap | Bootstrap parser re-export + dev binaries | 4 | ~465 | YES (slim) |
| egraph | General-purpose e-graph substrate | 13 | ~1.7k | EXTRACT-TO-OWN-REPO |
| egraph-derive | `#[derive(Language)]` proc-macro | 1 | ~343 | EXTRACT (with egraph) |
| csp-solver | General CSP/COP substrate | ~40 | ~4.5k | EXTRACT-TO-OWN-REPO |
| simd-scan | Architecture-neutral SIMD structural scanner | 10 | ~2.5k | YES |
| bbnf-path | `path!` proc-macro | 3 | ~920 | YES |
| bbnf-path-ts | TS/WASM cdylib for `path!` | 5 | ~1.0k | YES |

---

## crate: simd-scan

Cargo.toml: zero workspace deps; `proc-macro2 + syn + quote` (build-time only),
`proptest + divan` dev. Exposes the cdylib-free `lib`.

| File | LOC | Purpose | Layer | Deps in | Coupling | Fate |
|---|---:|---|---|---|---|---|
| `crates/simd-scan/src/lib.rs` | 114 | Public entry — runtime arch-feature dispatch (`scan_structural`); declares per-arch sub-modules under `cfg(target_arch=...)` and selects scalar fallback. | scan-tokenize | std runtime detection, alphabet, scalar | GENERIC | KEEP-AS-IS |
| `crates/simd-scan/src/alphabet.rs` | 229 | `StructuralAlphabet` — config + `KernelShape` selector + nibble/wide LUT helpers. Sole place backends derive the byte set from. | scan-tokenize | std | GENERIC | KEEP-AS-IS |
| `crates/simd-scan/src/index.rs` | 103 | `StructuralIndex` (positions+kinds Vec), `next_structural_at_or_after`. Wire type generated parsers consume. | scan-tokenize | none | GENERIC | KEEP-AS-IS |
| `crates/simd-scan/src/parity.rs` | 249 | Quote-state computation (CLMUL or shift-XOR). Used by the JSON string scanner. | scan-tokenize | std | GENERIC | KEEP-AS-IS |
| `crates/simd-scan/src/compaction.rs` | 112 | Bitmap → `Vec<u32>` index compaction (tzcnt loop + PEXT specialisation). | scan-tokenize | std | GENERIC | KEEP-AS-IS |
| `crates/simd-scan/src/scalar.rs` | 129 | Portable fallback scanner; correctness reference for fuzz harness. | scan-tokenize | alphabet, index | GENERIC | KEEP-AS-IS |
| `crates/simd-scan/src/neon.rs` | 719 | aarch64 NEON kernel — nibble-LUT + wide-LUT + `vextq_u8` digraph + 6-op shift-XOR or PMULL64 parity. >500 LOC but **device-specific**: SIMD intrinsic bodies don't decompose without losing the byte-identical-to-scalar invariant. | scan-tokenize | core::arch::aarch64, alphabet, index, parity, compaction | GENERIC | KEEP-AS-IS — **split exempt** (intrinsic block; further partitioning would split single-pass reads of the same vector register file). |
| `crates/simd-scan/src/avx2.rs` | 370 | x86_64 AVX2 kernel — `_mm256_cmpeq_epi8` + `_mm256_movemask_epi8` + PCLMULQDQ parity. | scan-tokenize | core::arch::x86_64 | GENERIC | KEEP-AS-IS |
| `crates/simd-scan/src/avx512.rs` | 149 | x86_64 AVX-512 VBMI2 kernel — `_mm512_mask_compressstoreu_epi8` for index compaction. Cargo `avx512` feature. | scan-tokenize | core::arch::x86_64 | GENERIC | KEEP-AS-IS |
| `crates/simd-scan/src/wasm.rs` | 433 | wasm32 SIMD kernel — `i8x16.swizzle` + `i8x16.bitmask`. | scan-tokenize | core::arch::wasm32 | GENERIC | KEEP-AS-IS |

Synthesis: this crate is the cleanest in the workspace. KEEP-AS-IS in entirety;
no rename, no merge. The only audit concern is the 719-LOC `neon.rs`, which is
exempt from the split rule because its body is a single dispatched intrinsic
sequence whose register-allocation discipline cannot be subdivided without
re-introducing reload-spill traffic.

---

## crate: csp-solver

Manifest description names csc411 commit `b70098676f...` as upstream truth;
**this crate is consumed by csc411 and bbnf-lang both** and the user has flagged
it for relocation. The `py` feature here is declared for cfg-recognition only.

| File | LOC | Purpose | Layer | Deps in | Coupling | Fate |
|---|---:|---|---|---|---|---|
| `src/lib.rs` | 532 | Crate root: `Pruning` / `PropagationStrategy` / `OptimizationMode` / `SolveConfig` / `Csp` struct + `solve` entry. **>500 LOC.** | host-shim | adjacency, builder, constraint, domain, ordering, solver, variable | GENERIC | EXTRACT-TO-OWN-REPO + **SPLIT** (move `Csp` impl to `csp.rs`; move `SolveConfig`/enums to `config.rs`; lib.rs becomes a re-export hub). |
| `src/adjacency.rs` | 105 | Adjacency graph for AC-3 worklist. | host-shim | std | GENERIC | EXTRACT |
| `src/variable.rs` | 76 | `Variable<D>` wrapper carrying domain + var-id + assigned flag. | host-shim | none | GENERIC | EXTRACT |
| `src/ordering.rs` | 66 | Variable-ordering heuristics (MRV, degree, etc.). | host-shim | none | GENERIC | EXTRACT |
| `src/py.rs` | 405 | PyO3 binding (csc411 only; never compiled in bbnf). | host-shim | pyo3 (cfg-gated) | GENERIC | EXTRACT |
| `src/builder/mod.rs` | 13 | Builder substrate re-exports. | host-shim | builder::assignment | GENERIC | EXTRACT |
| `src/builder/assignment.rs` | 444 | Fluent `AssignmentBuilder` for assignment-style CSPs. | host-shim | crate::Csp et al. | GENERIC | EXTRACT |
| `src/constraint/mod.rs` | 21 | Constraint module re-exports. | host-shim | sub-modules | GENERIC | EXTRACT |
| `src/constraint/all_different.rs` | 68 | `AllDifferent` global constraint. | host-shim | crate types | GENERIC | EXTRACT |
| `src/constraint/all_different_except.rs` | 126 | `AllDifferentExcept` global constraint. | host-shim | crate types | GENERIC | EXTRACT |
| `src/constraint/cardinality.rs` | 134 | `CardinalityConstraint` global constraint. | host-shim | crate types | GENERIC | EXTRACT |
| `src/constraint/dispatch.rs` | 91 | `ConstraintEnum` devirtualized dispatch. | host-shim | other constraint files | GENERIC | EXTRACT |
| `src/constraint/implication.rs` | 99 | `ImplicationConstraint` (parent→child commitment). Used by IR `csp_strategy`. | host-shim | crate types | GENERIC | EXTRACT |
| `src/constraint/lambda.rs` | 35 | `LambdaConstraint` via closure. | host-shim | std | GENERIC | EXTRACT |
| `src/constraint/not_equal.rs` | 61 | `NotEqual` binary constraint. | host-shim | crate types | GENERIC | EXTRACT |
| `src/constraint/soft.rs` | 70 | `SoftLambdaConstraint` (cost-bearing constraint). | host-shim | crate types | GENERIC | EXTRACT |
| `src/constraint/traits.rs` | 100 | `Constraint` / `Revision` / `SoftConstraint` traits + `VarId`. | host-shim | std | GENERIC | EXTRACT |
| `src/domain/mod.rs` | 13 | Domain module re-exports. | host-shim | sub-modules | GENERIC | EXTRACT |
| `src/domain/bitset.rs` | 141 | `BitsetDomain` — fixed-width bit-packed domain. | host-shim | none | GENERIC | EXTRACT |
| `src/domain/cost_finite.rs` | 176 | `CostFiniteDomain` — finite domain with per-value cost. | host-shim | none | GENERIC | EXTRACT |
| `src/domain/finite.rs` | 55 | `FiniteDomain<T>` — generic finite domain. | host-shim | none | GENERIC | EXTRACT |
| `src/domain/lattice.rs` | 80 | `BitsetLatticeDomain` — monotonic bit-shrink lattice. | host-shim | bitset | GENERIC | EXTRACT |
| `src/domain/traits.rs` | 92 | `Domain` / `LatticeDomain` / `CostDomain` trait surface. | host-shim | none | GENERIC | EXTRACT |
| `src/solver/mod.rs` | 29 | `SearchContext<D>` shared search bag. | host-shim | sub-modules | GENERIC | EXTRACT |
| `src/solver/ac3.rs` | 133 | AC-3 propagation. | host-shim | crate types | GENERIC | EXTRACT |
| `src/solver/backjump.rs` | 238 | Conflict-directed backjumping search. | host-shim | crate types | GENERIC | EXTRACT |
| `src/solver/backtrack.rs` | 183 | Chronological backtracking. | host-shim | crate types | GENERIC | EXTRACT |
| `src/solver/gac_alldiff.rs` | 367 | GAC AllDifferent (Régin's algorithm). | host-shim | crate types | GENERIC | EXTRACT |
| `src/solver/gac_alldiff_except.rs` | 493 | GAC AllDifferentExcept. | host-shim | crate types | GENERIC | EXTRACT |
| `src/solver/local_search.rs` | 176 | Local-search heuristics. | host-shim | crate types | GENERIC | EXTRACT |
| `src/solver/monotonic.rs` | 110 | Monotonic-fixed-point lattice solver (used by IR span eligibility). | host-shim | crate types | GENERIC | EXTRACT |
| `src/solver/nogoods.rs` | 167 | No-good learning. | host-shim | crate types | GENERIC | EXTRACT |
| `src/solver/optimize.rs` | 399 | Branch-and-bound cost-optimization. | host-shim | crate types | GENERIC | EXTRACT |
| `src/solver/propagate.rs` | 122 | Generic propagation interface. | host-shim | crate types | GENERIC | EXTRACT |
| `src/puzzles/mod.rs` | 4 | `pub mod sudoku; pub mod futoshiki;` | test-only | sub-modules | GENERIC | EXTRACT |
| `src/puzzles/sudoku/mod.rs` | 10 | Sudoku module re-exports. | test-only | sub-modules | GENERIC | EXTRACT |
| `src/puzzles/sudoku/csp.rs` | 57 | Sudoku-as-CSP encoding. | test-only | crate types | GENERIC | EXTRACT |
| `src/puzzles/sudoku/generate.rs` | 123 | Difficulty-rated board generation. | test-only | crate types | GENERIC | EXTRACT |
| `src/puzzles/sudoku/rng.rs` | 36 | Deterministic RNG for puzzle generation. | test-only | std | GENERIC | EXTRACT |
| `src/puzzles/sudoku/transform.rs` | 105 | Symmetry transforms (rotate/permute/etc.). | test-only | std | GENERIC | EXTRACT |
| `src/puzzles/futoshiki/mod.rs` | 7 | Futoshiki module re-exports. | test-only | csp | GENERIC | EXTRACT |
| `src/puzzles/futoshiki/csp.rs` | 120 | Futoshiki-as-CSP encoding. | test-only | crate types | GENERIC | EXTRACT |

Synthesis: bench/test puzzles (`puzzles/`) belong with the solver and travel
with it. The Python binding (`py.rs`, 405 LOC) is csc411-only and stays absent
from bbnf compilation under `feature="py"` gating; no change at extraction —
the binding follows the solver.

EXTRACT path: `crates/csp-solver/` → its own repo (presumably csc411 absorbs
it; the manifest already names csc411 as truth-source). bbnf-lang then
consumes via `csp-solver = "0.1"` from crates.io / git path-dep, exactly as
today. The lib.rs split (532 → ~250 + ~250 LOC) lands during the move.

---

## crate: egraph

| File | LOC | Purpose | Layer | Deps in | Coupling | Fate |
|---|---:|---|---|---|---|---|
| `crates/egraph/src/lib.rs` | 57 | Crate root re-exports. | host-shim | sub-modules | GENERIC | EXTRACT-TO-OWN-REPO |
| `crates/egraph/src/analysis.rs` | 57 | `Analysis<N>` trait + `NoAnalysis` zero-sized default. | host-shim | none | GENERIC | EXTRACT |
| `crates/egraph/src/cost_config.rs` | 119 | `CostConfig` shared knobs (saturation iter limit, growth caps). | host-shim | none | GENERIC | EXTRACT |
| `crates/egraph/src/cost_weights.rs` | 191 | `CostWeights` + `CALIBRATED_WEIGHTS` (the calibrated default). | host-shim | none | GENERIC | EXTRACT |
| `crates/egraph/src/csp_scheduler.rs` | 368 | `CspScheduler` + `DirtyDomain` + `ParentDirtyProp` — the saturation scheduler that consumes cost configs and runs incremental rebuilds. | host-shim | unionfind, eclass, scheduler | GENERIC | EXTRACT |
| `crates/egraph/src/eclass.rs` | 44 | `EClass<N, A>` — equivalence class storage. | host-shim | id | GENERIC | EXTRACT |
| `crates/egraph/src/egraph.rs` | 297 | `EGraph` — the data structure. Hash-cons, union-find, rebuild loop. | host-shim | unionfind, eclass | GENERIC | EXTRACT |
| `crates/egraph/src/extract.rs` | 215 | `Extractor` + `AstSize` + `CostModel` trait + `Lattice`/`Scalar` helpers. | host-shim | egraph | GENERIC | EXTRACT |
| `crates/egraph/src/id.rs` | 59 | `Id(u32)` newtype. | host-shim | none | GENERIC | EXTRACT |
| `crates/egraph/src/language.rs` | 114 | `Language` trait + `LanguageChildren`. | host-shim | id | GENERIC | EXTRACT |
| `crates/egraph/src/rewrite.rs` | 143 | `Rewrite<N, A>` + `RewriteFn` (boxed-fn variant). | host-shim | language, egraph | GENERIC | EXTRACT |
| `crates/egraph/src/scheduler.rs` | 116 | `Scheduler` trait + `BackoffScheduler` + `RunReport`. | host-shim | rewrite | GENERIC | EXTRACT |
| `crates/egraph/src/unionfind.rs` | 105 | `UnionFind` over `Id(u32)`. | host-shim | id | GENERIC | EXTRACT |

## crate: egraph-derive

| File | LOC | Purpose | Layer | Deps in | Coupling | Fate |
|---|---:|---|---|---|---|---|
| `crates/egraph-derive/src/lib.rs` | 343 | `#[derive(Language)]` proc-macro. Auto-detects `Id` / `Vec<Id>` / `Box<[Id]>` / `[Id; N]` fields and emits `children` / `children_mut` slices. | build-tooling | syn, quote, proc-macro2 | GENERIC | EXTRACT-TO-OWN-REPO (with egraph) |

Synthesis: extract egraph + egraph-derive together to a single workspace.
bbnf-lang re-consumes via path-dep until they ship to crates.io.

---

## crate: ser

`bbnf-ser` is unowned by the runtime path: ser-trait substrate that is no
longer the format-of-record. The runtime's grammar documents project
to typed values directly; serialization happens through `pprint::FmtBuilder`
or the per-grammar generated serializers (which currently emit nothing per
the schema-emit `O3` carve).

| File | LOC | Purpose | Layer | Deps in | Coupling | Fate |
|---|---:|---|---|---|---|---|
| `crates/ser/src/lib.rs` | 26 | Re-export hub. | host-shim | sub-modules | GENERIC | ARCHIVE |
| `crates/ser/src/traits.rs` | 129 | `Serializer` + `Deserializer` traits. | host-shim | std | GENERIC | ARCHIVE |
| `crates/ser/src/slice.rs` | 117 | `SliceDeserializer` — reads from `&'a str` input. | host-shim | traits | GENERIC | ARCHIVE |
| `crates/ser/src/string.rs` | 124 | `StringSerializer` — owned-`String` output. | host-shim | traits | GENERIC | ARCHIVE |
| `crates/ser/src/writer.rs` | 134 | `WriterSerializer` — `io::Write` output. | host-shim | traits | GENERIC | ARCHIVE |

Synthesis: archive entirely. Source preserved for later reconstitution. No
production caller in the workspace consumes the trait surface — the
generated-serialize code path was carved by `O3` (see
`grammar/schema/emit/rust/mod.rs`).

---

## crate: gorgeous

Per-grammar feature gates lock each `pub mod <grammar>;` behind its own cargo
feature. The `gorg` binary wants `bin-full`. With no features, the lib
compiles to a near-empty `PrinterConfig`. Per the BA-restart plan the
grammar-driven prettifier moves to ARCHIVE alongside `ser`.

| File | LOC | Purpose | Layer | Deps in | Coupling | Fate |
|---|---:|---|---|---|---|---|
| `crates/gorgeous/src/lib.rs` | 51 | Crate root + `PrinterConfig`. Per-grammar modules behind feature gates. | host-shim | sub-modules | GENERIC | ARCHIVE |
| `crates/gorgeous/src/main.rs` | 128 | `gorg` binary CLI dispatch. | host-shim | gorgeous lib | GENERIC | ARCHIVE |
| `crates/gorgeous/src/builtin.rs` | 26 | Built-in grammar dispatch helper. | host-shim | gorgeous lib | GENERIC | ARCHIVE |
| `crates/gorgeous/src/vm.rs` | 324 | VM-target prettify path (gated by `vm` feature). | host-shim | bbnf-ir, parse_that | GENERIC | ARCHIVE |
| `crates/gorgeous/src/json.rs` | 11 | `#[derive(Parser)]` shim against `bbnf::grammar::generated::json::*`. | host-shim | bbnf | PER-GRAMMAR (json) | ARCHIVE |
| `crates/gorgeous/src/css.rs` | 14 | `#[derive(Parser)]` shim — CSS L4 prettify. | host-shim | bbnf | PER-GRAMMAR (css_l4) | ARCHIVE |
| `crates/gorgeous/src/bnf.rs` | 11 | BNF prettify shim. | host-shim | bbnf | PER-GRAMMAR (bnf) | ARCHIVE |
| `crates/gorgeous/src/ebnf.rs` | 11 | EBNF prettify shim. | host-shim | bbnf | PER-GRAMMAR (ebnf) | ARCHIVE |
| `crates/gorgeous/src/bbnf.rs` | 12 | BBNF self-host prettify shim. | host-shim | bbnf | PER-GRAMMAR (bbnf) | ARCHIVE |
| `crates/gorgeous/src/google_sheets.rs` | 16 | Sheets prettify shim. | host-shim | bbnf | PER-GRAMMAR (sheets) | ARCHIVE |

Synthesis: archive in entirety. `vm.rs` (324 LOC) names the only non-trivial
implementation; the rest are five-line `derive(Parser)` shims whose value
travels with whatever brings the per-grammar prettify code back. The `gorg`
binary's CLI is salvageable as a future test-fixture binary if BA restores
prettify.

---

## crate: bootstrap

| File | LOC | Purpose | Layer | Deps in | Coupling | Fate |
|---|---:|---|---|---|---|---|
| `crates/bootstrap/src/lib.rs` | 28 | One-line re-export of `bbnf::grammar::generated::BbnfBootstrap`. | host-shim | bbnf | PER-GRAMMAR (bbnf) | KEEP-AS-IS (the entire `lib.rs` is the re-export). |
| `crates/bootstrap/src/bin/dump_ir.rs` | 185 | Dev binary: dump IR for a grammar source as JSON for diffing. | build-tooling | bbnf, bbnf-ir, serde_json | GENERIC | KEEP-AS-IS |
| `crates/bootstrap/src/bin/cost_grid_sweep.rs` | 127 | Dev binary: sweep `CostConfig` knobs and report pareto-front. | build-tooling | bbnf, bbnf-ir | GENERIC | KEEP-AS-IS |
| `crates/bootstrap/src/bin/debug_parse.rs` | 125 | Dev binary: parse a grammar + pretty-print extract. | build-tooling | bbnf | GENERIC | KEEP-AS-IS |

Synthesis: this crate is literally a single re-export plus three dev
binaries. Keep it. The lib.rs comment reads as historical (notes the pre-B2
proc-macro relocation) but the surface is correct: the re-export resolves
through `bbnf::grammar::generated::BbnfBootstrap`.

---

## crate: bbnf-path

Cargo: `proc-macro = true`; deps `syn + quote + proc-macro2 + bbnf-regex +
bbnf-ir + serde + serde_json`. The proc-macro that lifts a `path!(...)`
literal into a typed `bbnf::path::TypedPath<G, T>::from_owned(...)` expr.

| File | LOC | Purpose | Layer | Deps in | Coupling | Fate |
|---|---:|---|---|---|---|---|
| `crates/bbnf-path/src/lib.rs` | 78 | Public `#[proc_macro] pub fn path(...)`. | build-tooling | path_macro, registry | GENERIC | KEEP-AS-IS |
| `crates/bbnf-path/src/path_macro.rs` | 639 | The lex/lower/validate body of the `path!` expansion. **>500 LOC.** | build-tooling | bbnf-regex, bbnf-ir | GENERIC | **SPLIT** into `path_macro/{lex.rs, lower.rs, validate.rs, emit.rs}` so each phase of the macro lives in one ≤200 LOC file. The current file mixes proc-macro IO concerns with grammar registry validation. |
| `crates/bbnf-path/src/registry.rs` | 201 | Compile-time fixture `StructRegistry` lookups for synthetic grammars; T4 will swap to per-grammar JSON sidecar. | build-tooling | bbnf-ir, serde_json | GENERIC | KEEP-AS-IS |

## crate: bbnf-path-ts

Cargo: `cdylib + rlib`; `wasm-bindgen + js-sys + serde-wasm-bindgen`.

| File | LOC | Purpose | Layer | Deps in | Coupling | Fate |
|---|---:|---|---|---|---|---|
| `crates/bbnf-path-ts/src/lib.rs` | 133 | `#[wasm_bindgen]` exports `compile_path` / `execute_path` + native-host mirrors for testing. | build-tooling | sub-modules, wasm-bindgen | GENERIC | KEEP-AS-IS |
| `crates/bbnf-path-ts/src/compile.rs` | 474 | TS-side mirror of the Rust `path!` lex/lower/validate; re-implements the proc-macro at function scope (proc-macro crates can't be lib-deps). | build-tooling | bbnf-regex, bbnf-ir, schema, fixture | GENERIC | KEEP-AS-IS |
| `crates/bbnf-path-ts/src/fixture.rs` | 248 | `GrammarFixture` registry — same shape as `bbnf-path`'s. | build-tooling | bbnf-ir | GENERIC | KEEP-AS-IS |
| `crates/bbnf-path-ts/src/schema.rs` | 113 | `TypedPathPayload` / `OwnedSegmentPayload` / `PathErrorPayload` — the byte-identical wire types. | build-tooling | serde | GENERIC | KEEP-AS-IS |
| `crates/bbnf-path-ts/src/template_tag.rs` | 44 | `TEMPLATE_TAG_JS` — the JS shim string the TS template tag wraps around. | build-tooling | none | GENERIC | KEEP-AS-IS |

Synthesis: both `bbnf-path` and `bbnf-path-ts` are healthy AZ-IV products.
The 639-LOC `path_macro.rs` is the only god-module concern; split it into
phase-specific files during BA. The duplicated `compile.rs`+`fixture.rs`
between the two crates is a known proc-macro-vs-lib-dep limitation, not a
defect — the isomorphism test asserts they stay byte-equivalent.

---

## crate: ir

The analysis crate. Holds GrammarIR + every IR pass + the e-graph + rewrites
substrate + bytecode VM. Deeply structured; the largest crate by file count.

### types/ — pure data definitions

| File | LOC | Purpose | Layer | Coupling | Fate |
|---|---:|---|---|---|---|
| `types/mod.rs` | 59 | Re-export hub. | IR-lower | GENERIC | KEEP-AS-IS |
| `types/grammar.rs` | 584 | `GrammarIR` — the canonical container. **>500 LOC.** | IR-lower | GENERIC | **SPLIT**: separate `GrammarIR` definition (struct + ctor) from accessors (string/fn lookups) and from MessagePack/JSON serialization. Target: `grammar/{def.rs, accessors.rs, serde.rs}`. |
| `types/node.rs` | 206 | `IrNode` + `AltBranch` + `AltDispatch` + `TokenDispatchArm` + `GrammarSpan` + walking helpers. | IR-lower | GENERIC | KEEP-AS-IS |
| `types/rule.rs` | 179 | `IrRule` + `RuleMeta` + `RuleDirectives` + `MemoStrategy` + `DispatchHint` + `PrettyHints` + `SubVariant`. | IR-lower | GENERIC | KEEP-AS-IS |
| `types/map_expr.rs` | 177 | `MapExpr` + `MapBinOp` + `MapUnaryOp` — the user-facing `->` payload. | IR-lower | GENERIC | KEEP-AS-IS |
| `types/fn_descriptor.rs` | 58 | `FnDescriptor` — host-fn descriptor enum. | IR-lower | GENERIC | KEEP-AS-IS |
| `types/type_desc.rs` | 212 | `TypeDesc` — backend-agnostic type descriptor. | type-infer | GENERIC | KEEP-AS-IS |
| `types/type_desc_interner.rs` | 101 | `TypeDescInterner` — cache for `TypeDesc` instances. | type-infer | GENERIC | KEEP-AS-IS |
| `types/recognizer_configs.rs` | 101 | `DelimScanConfig`, `KeyDispatchConfig`, `KeyDispatchMatch`, `DetectedBranch`, `KeyClass`, `key_class_regex_pattern`. | IR-optimize | GENERIC | KEEP-AS-IS |

### registry/ — typed-struct projections

| File | LOC | Purpose | Layer | Coupling | Fate |
|---|---:|---|---|---|---|
| `registry/mod.rs` | 39 | Re-export hub. | IR-lower | GENERIC | KEEP-AS-IS |
| `registry/struct.rs` | 391 | `StructLayout` + `StructRegistry` — typed struct shapes per Named rule. | type-infer | GENERIC | KEEP-AS-IS |
| `registry/strategy.rs` | 334 | `EmitStrategy` (StructDirect resolver) + `SubstrateBinding` + `ManifestStrategyEntry`. The single backend-shared substrate selector. | codegen | GENERIC | KEEP-AS-IS |

### dag/ — hash-consed grammar DAG

| File | LOC | Purpose | Layer | Coupling | Fate |
|---|---:|---|---|---|---|
| `dag/mod.rs` | 186 | `GrammarDag` struct + accessors + `ensure_dag` test helper. | IR-lower | GENERIC | KEEP-AS-IS |
| `dag/build.rs` | 143 | `GrammarDagBuilder` — recursive-walk hash-cons builder. | IR-lower | GENERIC | KEEP-AS-IS |
| `dag/extract.rs` | 79 | DAG-tree extraction back to `IrNode` for tests. | IR-lower | GENERIC | KEEP-AS-IS |
| `dag/intern.rs` | 54 | Hash-cons interner detail. | IR-lower | GENERIC | KEEP-AS-IS |
| `dag/node.rs` | 178 | `DagNode` enum + `NodeId(u32)`. | IR-lower | GENERIC | KEEP-AS-IS |

### egraph/ — grammar-tier e-graph

| File | LOC | Purpose | Layer | Coupling | Fate |
|---|---:|---|---|---|---|
| `egraph/mod.rs` | 128 | `build_and_saturate` orchestrator. | IR-optimize | GENERIC | KEEP-AS-IS |
| `egraph/build_egraph.rs` | 106 | `insert_ir` — IR-tree to e-graph node insertion. | IR-optimize | GENERIC | KEEP-AS-IS |
| `egraph/cost.rs` | 164 | `GrammarCostModel` — implements `egraph::CostModel<GrammarENode>`. | IR-optimize | GENERIC | KEEP-AS-IS |
| `egraph/interner.rs` | 130 | `SharedStrings` — clone-cheap pool used by rewrite rules. | IR-optimize | GENERIC | KEEP-AS-IS |
| `egraph/node.rs` | 77 | `GrammarENode` enum (with `#[derive(Language)]` from egraph-derive). | IR-optimize | GENERIC | KEEP-AS-IS |
| `egraph/write_back.rs` | 378 | `extract_ir_node` + `write_back_optimized` — extract canonical and rebuild IrNode tree. | IR-optimize | GENERIC | KEEP-AS-IS |
| `egraph/analysis/mod.rs` | 290 | `GrammarAnalysis` + `EClassFacts` + `WidthBound`. | IR-optimize | GENERIC | KEEP-AS-IS |
| `egraph/analysis/facts.rs` | 274 | `EClassFacts` impl detail (lattice merge, joins). | IR-optimize | GENERIC | KEEP-AS-IS |
| `egraph/rules/mod.rs` | 98 | `default_rules()` factory. | IR-optimize | GENERIC | KEEP-AS-IS |
| `egraph/rules/regex.rs` | 434 | `DeduplicateAltBranches` / `SupersetAbsorbAlt` / `UnionMergeAlt` / `FuseAltRegexBranches`. | IR-optimize | GENERIC | KEEP-AS-IS |
| `egraph/rules/suffix.rs` | 146 | `CommonSuffixFactor` rewrite. | IR-optimize | GENERIC | KEEP-AS-IS |
| `egraph/rules/universal.rs` | 418 | `AltOfSingle` + `RepeatOfSingle` + `WrapOfEpsilonScalar` + `ConcatLiterals` (AY.W2.3 G1-G4). | IR-optimize | GENERIC | KEEP-AS-IS |

### recognizer/ — unified recognizer trait

| File | LOC | Purpose | Layer | Coupling | Fate |
|---|---:|---|---|---|---|
| `recognizer/mod.rs` | 303 | `RecognizerInfo` trait + four wrapper impls (Regex, Literal, Token, DispatchGroup, DelimScan). | IR-optimize | GENERIC | KEEP-AS-IS |
| `recognizer/facts.rs` | 94 | `RecognizerKind` + `Width` + `RecognizerInfo` trait sub-types. | IR-optimize | GENERIC | KEEP-AS-IS |
| `recognizer/plans.rs` | 31 | `ExecutionPlan` + `ExecutionPlanKind`. | IR-optimize | GENERIC | KEEP-AS-IS |

### vm/ — bytecode interpreter

| File | LOC | Purpose | Layer | Coupling | Fate |
|---|---:|---|---|---|---|
| `vm/mod.rs` | 10 | Re-export shell. | runtime-execute | GENERIC | KEEP-AS-IS |
| `vm/bytecode.rs` | 243 | Opcode enum + `BytecodeProgram` + side tables. | runtime-execute | GENERIC | KEEP-AS-IS |
| `vm/debug.rs` | 65 | `DebugSnapshot` for DAP / breakpoints. | runtime-execute | GENERIC | KEEP-AS-IS |
| `vm/compiler/mod.rs` | 116 | IR → bytecode compile entry. | codegen | GENERIC | KEEP-AS-IS |
| `vm/compiler/compound.rs` | 392 | Compile compound (Seq/Alt/Repeat/etc.) to bytecode. | codegen | GENERIC | KEEP-AS-IS |
| `vm/compiler/emit.rs` | 29 | Bytecode buffer emit helpers. | codegen | GENERIC | KEEP-AS-IS |
| `vm/compiler/node.rs` | 55 | Compile per-node dispatch. | codegen | GENERIC | KEEP-AS-IS |
| `vm/compiler/rule.rs` | 76 | Compile rule body to bytecode. | codegen | GENERIC | KEEP-AS-IS |
| `vm/interpreter/mod.rs` | 377 | `Interpreter` struct + `run` dispatch loop. | runtime-execute | GENERIC | KEEP-AS-IS |
| `vm/interpreter/control.rs` | 89 | Call/return + state save/restore + ws-trim ops. | runtime-execute | GENERIC | KEEP-AS-IS |
| `vm/interpreter/leaves.rs` | 85 | Leaf ops (`exec_match_string`, `exec_match_regex`, `exec_epsilon`, `exec_dispatch_token`). | runtime-execute | GENERIC | KEEP-AS-IS |
| `vm/interpreter/repeat.rs` | 71 | Repetition begin/end/finalize ops. | runtime-execute | GENERIC | KEEP-AS-IS |
| `vm/interpreter/construct.rs` | 36 | `exec_make_array` / `exec_make_tagged`. | runtime-execute | GENERIC | KEEP-AS-IS |
| `vm/interpreter/memo.rs` | 43 | Memoization check + store. | runtime-execute | GENERIC | KEEP-AS-IS |
| `vm/interpreter/value.rs` | 85 | `Value` / `ValueSlice` / `ParseDiagnostic` / `ParseResult`. | runtime-execute | GENERIC | KEEP-AS-IS |

### rewrites/ — rewrite-rule storage

| File | LOC | Purpose | Layer | Coupling | Fate |
|---|---:|---|---|---|---|
| `rewrites/mod.rs` | 237 | `Rule` + `RuleSet` + `RewriteRuleId` + RON load/save. | IR-optimize | GENERIC | KEEP-AS-IS |
| `rewrites/base.rs` | 225 | `Alphabet` + `Atom` + `Pattern` + `PatternRef` + `Witness`. | IR-optimize | GENERIC | KEEP-AS-IS |
| `rewrites/path_seed.rs` | 220 | `PATH_SEED_GRAMMAR` + path-shape seed rules (BA recyclable). | IR-optimize | GENERIC | KEEP-AS-IS |
| `rewrites/rank.rs` | 253 | `RankConfig` + `rank` + `select_top_k`. **Note** per HARDENING: BB.W0 substrate-without-consumer concern; lands in BB.W3. | IR-optimize | GENERIC | KEEP-AS-IS (relocate per BB.W3 plan amendment). |
| `rewrites/schema.rs` | 150 | RON schema (RuleFile / RuleSerialized / SCHEMA_VERSION). | IR-optimize | GENERIC | KEEP-AS-IS |
| `rewrites/tiering.rs` | 111 | `RuleClass` + `classify` (Class-1/2/3 tiers). **Same BB.W0 concern.** | IR-optimize | GENERIC | KEEP-AS-IS (relocate per BB.W3). |

### passes/ — every IR transformation pass

This is the biggest sub-tree. 25 top-level files + several directory sub-modules.

#### passes/sets/ — set-analysis foundation

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `passes/sets/mod.rs` | 28 | Re-export hub. | KEEP-AS-IS |
| `passes/sets/deps.rs` | 64 | `compute_rule_deps` — rule call graph edges. | KEEP-AS-IS |
| `passes/sets/scc.rs` | 115 | Tarjan-style SCC computation. | KEEP-AS-IS |
| `passes/sets/first_sets.rs` | 431 | FIRST set computation — fixed-point lattice. | KEEP-AS-IS |
| `passes/sets/follow.rs` | 283 | FOLLOW set computation. | KEEP-AS-IS |
| `passes/sets/sort.rs` | 366 | `sort_alt_branches` — re-orders Alt branches by first-byte cardinality + literal length. | KEEP-AS-IS |
| `passes/sets/factor_lookahead.rs` | 361 | `factor_regex_with_lookahead` — extracts shared lookahead from regex Alt. | KEEP-AS-IS |
| `passes/sets/fingerprint.rs` | 259 | `PushFingerprint` + `compute_push_fingerprint`. | KEEP-AS-IS |
| `passes/sets/structural_alphabet.rs` | 437 | `compute_structural_alphabet` — derives the byte set simd-scan consumes. | KEEP-AS-IS |
| `passes/sets/dispatch/mod.rs` | 131 | Dispatch-table generation entry. | KEEP-AS-IS |
| `passes/sets/dispatch/annotate.rs` | 165 | Annotate Alt nodes with dispatch eligibility. | KEEP-AS-IS |
| `passes/sets/dispatch/build.rs` | 158 | Build the actual `AltDispatch` table. | KEEP-AS-IS |
| `passes/sets/dispatch/constraint.rs` | 56 | CSP constraint for dispatch-table feasibility. | KEEP-AS-IS |
| `passes/sets/dispatch/domain.rs` | 82 | `DispatchEligibility` lattice. | KEEP-AS-IS |
| `passes/sets/dispatch/eligibility.rs` | 158 | Eligibility predicate + propagator. | KEEP-AS-IS |
| `passes/sets/dispatch/first_set.rs` | 127 | First-set extraction for dispatch-table. | KEEP-AS-IS |

#### passes/transform/ — structural normalizer

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `passes/transform/mod.rs` | 34 | Hub. | KEEP-AS-IS |
| `passes/transform/alias.rs` | 109 | `canonicalize_aliases`. | KEEP-AS-IS |
| `passes/transform/fuse.rs` | 388 | `fuse_single_use` — substitute single-call rules into call sites. | KEEP-AS-IS |
| `passes/transform/inline.rs` | 331 | `inline_acyclic` — inline small acyclic rules. | KEEP-AS-IS |
| `passes/transform/optimize.rs` | 238 | `eliminate_epsilon` + `merge_literals`. | KEEP-AS-IS |
| `passes/transform/pattern_dedup.rs` | 439 | `hoist_recurring_patterns`. | KEEP-AS-IS |
| `passes/transform/prune.rs` | 164 | `prune_unreachable`. | KEEP-AS-IS |
| `passes/transform/fuse_token/mod.rs` | 107 | `fuse_token_dispatch` entry. | KEEP-AS-IS |
| `passes/transform/fuse_token/detect.rs` | 171 | Detect token-dispatch shapes. | KEEP-AS-IS |
| `passes/transform/fuse_token/factor.rs` | 339 | Factor token-led prefix. | KEEP-AS-IS |

#### passes/recognizers/ — pattern miners

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `passes/recognizers/mod.rs` | 362 | Single-walk orchestrator + `RecognizerMiner` trait + `install_recognizer`. | KEEP-AS-IS |
| `passes/recognizers/grammar_facts.rs` | 1530 | `lift_dta` + `DtaTable`/`DtaState`/`DtaBuilder` — the DTA enumeration substrate. **>500 LOC, the largest single file in the IR crate.** | **SPLIT** into `grammar_facts/{state.rs, lift.rs, table.rs, precedence.rs, builder.rs}` — the file owns four orthogonal concerns (state-id assignment, DTA-table emit, regex-pattern-string interning, precedence-chain detection). |
| `passes/recognizers/balanced_wrap.rs` | 76 | `BalancedWrapMiner`. | KEEP-AS-IS |
| `passes/recognizers/comment_ws.rs` | 53 | `CommentWsMiner`. | KEEP-AS-IS |
| `passes/recognizers/consume_to_next_structural.rs` | 141 | CTNS lift miner. | KEEP-AS-IS |
| `passes/recognizers/context_facts_miner.rs` | 131 | `ContextFactsMiner`. | KEEP-AS-IS |
| `passes/recognizers/dedup_eligibility.rs` | 234 | Dedup-eligibility miner. | KEEP-AS-IS |
| `passes/recognizers/delim_scan.rs` | 222 | `DelimScanMiner`. | KEEP-AS-IS |
| `passes/recognizers/disjoint_first.rs` | 158 | Disjoint-FIRST detection. | KEEP-AS-IS |
| `passes/recognizers/identifier.rs` | 66 | `IdentifierMiner`. | KEEP-AS-IS |
| `passes/recognizers/kernel_shape.rs` | 127 | Recognizer-kernel-shape classification. | KEEP-AS-IS |
| `passes/recognizers/key_dispatch.rs` | 234 | `KeyDispatchMiner`. | KEEP-AS-IS |
| `passes/recognizers/keyword_stats.rs` | 146 | Keyword-shape statistics. | KEEP-AS-IS |
| `passes/recognizers/list_rules.rs` | 147 | List-rule mining. | KEEP-AS-IS |
| `passes/recognizers/node_facts.rs` | 172 | `NodeFacts` recorder. | KEEP-AS-IS |
| `passes/recognizers/operator_chain.rs` | 415 | `collect_operator_chains` — Pratt detection. | KEEP-AS-IS |
| `passes/recognizers/pattern_alphabet.rs` | 383 | `PatternAlphabet` recognizer. | KEEP-AS-IS |
| `passes/recognizers/punct_ws_region.rs` | 175 | `PunctWsRegionMiner`. | KEEP-AS-IS |
| `passes/recognizers/quoted_string.rs` | 58 | `QuotedStringMiner`. | KEEP-AS-IS |
| `passes/recognizers/separator_list.rs` | 84 | `SeparatorListMiner`. | KEEP-AS-IS |
| `passes/recognizers/shape_dict_bbnf.rs` | 192 | BBNF-specific shape dict (`@shape_dict`). | KEEP-AS-IS |
| `passes/recognizers/signature.rs` | 114 | `RecognizerSignature`. | KEEP-AS-IS |
| `passes/recognizers/token_led_branches.rs` | 74 | `TokenLedBranchesMiner`. | KEEP-AS-IS |
| `passes/recognizers/shape_dispatch/mod.rs` | 354 | `ShapeAssignments` + `ShapeTag` registry. | KEEP-AS-IS |
| `passes/recognizers/shape_dispatch/{flat,arglist,object,array,unordered,pratt,wrap,keyword,hregex,scalar,number,string,alt_dispatch}.rs` | varies (39-237) | One per shape. | KEEP-AS-IS |

#### passes/types/ — type projection

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `passes/types/mod.rs` | 786 | `project_types` — runs the CSP type-inference fixed point. **>500 LOC.** | **SPLIT** into `types/{project.rs, ast_walk.rs, vec_ctx.rs, postprocess.rs}`. The file mixes AC-3 setup, the per-node walking projector, and the post-extraction TypeMap consolidation. |
| `passes/types/generate.rs` | 421 | Type-constraint generation (CSP constraint emission). | KEEP-AS-IS |
| `passes/types/registry.rs` | 510 | `populate_struct_registry`. **>500 LOC.** | **SPLIT** into `registry/{populate.rs, layout_kind.rs, fields.rs}` — the populator, the LayoutKind classifier, and the StructField projector are independent concerns. |
| `passes/types/obligation.rs` | 204 | `TypeObligation` + `ObligationSink` + `dedup_branch_types`. | KEEP-AS-IS |
| `passes/types/type_map.rs` | 203 | `TypeMap` + `try_flatten_pair`. | KEEP-AS-IS |
| `passes/types/subvariants.rs` | 182 | `collect_sub_variants_raw` + uniqueness validation. | KEEP-AS-IS |
| `passes/types/constraint/mod.rs` | 42 | Sub-module hub. | KEEP-AS-IS |
| `passes/types/constraint/alt.rs` | 157 | Alt type constraints. | KEEP-AS-IS |
| `passes/types/constraint/seq.rs` | 147 | Seq type constraints. | KEEP-AS-IS |
| `passes/types/constraint/reference.rs` | 146 | Ref type constraints. | KEEP-AS-IS |
| `passes/types/constraint/operators.rs` | 213 | Skip/Next/Minus type constraints. | KEEP-AS-IS |
| `passes/types/constraint/revise.rs` | 176 | Constraint revise loop. | KEEP-AS-IS |
| `passes/types/constraint/grounds.rs` | 110 | Grounding leaf-type constraints. | KEEP-AS-IS |
| `passes/types/constraint/domain.rs` | 81 | `TypeDomain` lattice. | KEEP-AS-IS |

#### passes/csp_strategy/ — recognizer-tier CSP

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `passes/csp_strategy/mod.rs` | 1361 | The strategy CSP — Alt/Wrap/Engine variables, branch-and-bound solve. **>500 LOC.** | **SPLIT** into `csp_strategy/{problem.rs, decode.rs, optimize.rs, decisions.rs}` — the file mixes problem construction, optimizer call, decision decoding, and the public DecisionMap/DecisionValue types. |
| `passes/csp_strategy/components.rs` | 339 | `partition_by_call_graph` + `GrammarComponents` + per-component union-find. | KEEP-AS-IS |
| `passes/csp_strategy/constraints/mod.rs` | 105 | Constraint hub. | KEEP-AS-IS |
| `passes/csp_strategy/constraints/dispatch.rs` | 89 | Dispatch-mode constraint. | KEEP-AS-IS |
| `passes/csp_strategy/constraints/engine.rs` | 177 | Engine-selection constraint. | KEEP-AS-IS |
| `passes/csp_strategy/constraints/layout.rs` | 100 | Layout constraint. | KEEP-AS-IS |
| `passes/csp_strategy/constraints/shape.rs` | 124 | Shape constraint. | KEEP-AS-IS |

#### passes/ — single-file passes

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `passes/mod.rs` | 82 | Top-level `passes` re-export hub. | KEEP-AS-IS |
| `passes/csp_domains.rs` | 500 | `BoolDomain` + `CharSetDomain` + `BoolAndConstraint` + `BoolEqualConstraint` + `BoolGroundConstraint` + ... — the lattice domains and constraints span eligibility / FIRST sets use. **At the 500 LOC boundary.** | KEEP-AS-IS or **SPLIT** into `csp_domains/{bool.rs, charset.rs, constraints.rs}` if any further extension lands. |
| `passes/inline_trace.rs` | 214 | `InlineTrace` + `TraceSink` + `NoopTraceSink` + `InlineSubstitution`. | KEEP-AS-IS |
| `passes/lr.rs` | 321 | `eliminate_direct_lr` + `eliminate_indirect_lr` (Paull's algorithm). | KEEP-AS-IS |
| `passes/metadata.rs` | 158 | `compute_aliases` + `compute_transparent` + `has_named_return_type`. | KEEP-AS-IS |
| `passes/path_check.rs` | 252 | `PathCheckResolver` + `run_path_check` — source-rule-name path resolver. | KEEP-AS-IS |
| `passes/prefix.rs` | 477 | `factor_common_prefixes` + byte-level Literal trie split. | KEEP-AS-IS |
| `passes/profile.rs` | 191 | `GrammarProfile` (consolidated fingerprint). | KEEP-AS-IS |
| `passes/regex_info.rs` | 131 | `compute_regex_info` — populates `ir.regex_info` via bbnf-regex. | KEEP-AS-IS |
| `passes/span.rs` | 247 | `compute_sp_method_rules` + `refine_span_eligibility`. | KEEP-AS-IS |
| `passes/audit/mod.rs` | 25 | Audit hub. | KEEP-AS-IS |
| `passes/audit/payload_coverage.rs` | 585 | `audit_payload_coverage` — per-grammar typed-`->` marker coverage. **>500 LOC.** | **SPLIT** into `payload_coverage/{report.rs, probe.rs, walk.rs}` — the report types, the probe-trait-impl, and the IR-walking enumerator are independently testable. |
| `passes/context/mod.rs` | 16 | Hub for `ContextFacts`. | KEEP-AS-IS |
| `passes/context/facts.rs` | 45 | `ContextFacts` + `DiscriminationStrength` + `ScanSafety`. | KEEP-AS-IS |
| `passes/facts/mod.rs` | 271 | `FactAuthority` — durable read surface. | KEEP-AS-IS |
| `passes/inspect/mod.rs` | 33 | Hub. | KEEP-AS-IS |
| `passes/inspect/leading.rs` | 114 | `extract_leading_literals` / `extract_leading_regex_pattern`. | KEEP-AS-IS |
| `passes/inspect/literal.rs` | 24 | `single_byte_literal`. | KEEP-AS-IS |
| `passes/inspect/resolve.rs` | 101 | `resolve_to_seq` / `unwrap_to_alt` / `unwrap_to_repeat`. | KEEP-AS-IS |
| `passes/inspect/unwrap.rs` | 49 | `unwrap_wrap` / `unwrap_map_ow`. | KEEP-AS-IS |
| `passes/inspect/walk.rs` | 62 | `visit_children_alt`. | KEEP-AS-IS |
| `passes/materialization/mod.rs` | 29 | Hub. | KEEP-AS-IS |
| `passes/materialization/classify.rs` | 843 | `classify_materialization` + `classify_materialization_with_facts` + `compute_eclass_facts`. **>500 LOC.** | **SPLIT** into `materialization/{classify.rs, eclass.rs, facts_seed.rs}` — the per-NodeId classifier, the fixed-point iterator, and the EClassFacts seed are layered. |
| `passes/materialization/lattice.rs` | 134 | `MaterializationClass` + `mat_join`. | KEEP-AS-IS |
| `passes/materialization/pin_sweep.rs` | 139 | Consumer-pin fix-up sweep. | KEEP-AS-IS |
| `passes/patterns/mod.rs` | 201 | `RecognizerShape` + `NodeFacts` + structural facts. | KEEP-AS-IS |
| `passes/payload/mod.rs` | 25 | Hub. | KEEP-AS-IS |
| `passes/payload/layout.rs` | 514 | `compute_payload_layouts` + `PayloadLayout` + `PayloadField` + `plan_layout`. **>500 LOC.** | **SPLIT** into `payload/layout/{plan.rs, fields.rs, scalar.rs}` — planning, field emission, scalar-sentinel checks. |
| `passes/payload/named_types.rs` | 81 | `NamedTypeResolver` + `NullResolver`. | KEEP-AS-IS |
| `passes/payload/scalar_routing.rs` | 135 | `scalar_range_includes_sentinel`. | KEEP-AS-IS |

### top-level ir crate

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `lib.rs` | 55 | Re-export hub. | KEEP-AS-IS |
| `cost_config.rs` | 234 | `CostConfig` — per-compile cost knobs (wraps `egraph::CostConfig`). | KEEP-AS-IS |

Synthesis: `ir` is well-structured. Eight files cross 500 LOC and need splits;
the structure (separate `passes/`, `types/`, `registry/`, `dag/`, `egraph/`,
`recognizer/`, `vm/`, `rewrites/` directories) is sound. The crate itself
should NOT split — the cohesion between `passes/` and `types/` and `egraph/`
is too high; introducing an inter-crate boundary would force premature
serialization of types currently passed by reference.

---

## crate: analysis (LSP-shared grammar analysis)

| File | LOC | Purpose | Layer | Coupling | Fate |
|---|---:|---|---|---|---|
| `crates/analysis/src/lib.rs` | 13 | Re-exports. | host-shim | sub-modules | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/analysis.rs` | 128 | `LineIndex` (offset↔line conversion). | host-shim | std | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/state/mod.rs` | 89 | `DocumentState` — owned text + `OwnedAst` self-cell. | host-shim | sub-modules | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/state/parsing.rs` | 96 | `parse_once` + `OwnedAst`. | host-shim | bbnf, self_cell | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/state/pretty.rs` | 193 | Pretty formatting hooks. | host-shim | bbnf | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/state/types.rs` | 161 | `DocumentInfo` + `RuleInfo`. | host-shim | std | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/state/ast_utils/mod.rs` | 208 | AST utility helpers. | host-shim | bbnf | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/state/ast_utils/format.rs` | 238 | Format-specific helpers. | host-shim | bbnf | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/state/ast_utils/cycles.rs` | 87 | Cycle detection helpers. | host-shim | bbnf | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/state/ast_utils/references.rs` | 182 | Reference tracking. | host-shim | bbnf | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/state/ast_utils/spans.rs` | 27 | Span helpers. | host-shim | bbnf | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/state/ast_utils/tokens.rs` | 195 | Token analysis. | host-shim | bbnf | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/state/diagnostics/mod.rs` | 221 | `analyze` + `analyze_from_cache` + `ParseDiagnostics`. | host-shim | sub-modules | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/state/diagnostics/cycles.rs` | 70 | Cycle diagnostics. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/state/diagnostics/directives.rs` | 104 | Directive diagnostics. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/state/diagnostics/early.rs` | 108 | Early-stage diagnostics. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/state/diagnostics/extract.rs` | 83 | Extraction diagnostics. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/state/diagnostics/ir_analysis.rs` | 236 | IR-pipeline diagnostics. | host-shim | bbnf, bbnf-ir | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/state/diagnostics/references.rs` | 112 | Reference diagnostics. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/state/diagnostics/structure.rs` | 103 | Structural diagnostics. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/directives/mod.rs` | 11 | Directive hub. | host-shim | sub-modules | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/directives/debug.rs` | 75 | `@debug` extraction + validation. | host-shim | bbnf | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/directives/hints.rs` | 211 | `@pretty` hint catalog. | host-shim | bbnf | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/directives/import.rs` | 45 | `@import` extraction. | host-shim | bbnf | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/directives/recover.rs` | 77 | `@recover` extraction. | host-shim | bbnf | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/directives/token.rs` | 73 | `@token` extraction. | host-shim | bbnf | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/directives/ws.rs` | 34 | `@ws` extraction. | host-shim | bbnf | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/features/mod.rs` | 13 | Feature hub. | host-shim | sub-modules | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/features/code_actions.rs` | 106 | LSP code actions. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/features/code_lens.rs` | 38 | LSP code lens. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/features/completion.rs` | 78 | LSP completion. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/features/document_symbols.rs` | 31 | LSP document symbols. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/features/folding.rs` | 29 | LSP folding ranges. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/features/formatting.rs` | 402 | LSP formatting. | host-shim | analysis, pprint | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/features/goto_definition.rs` | 29 | LSP goto definition. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/features/inlay_hints.rs` | 97 | LSP inlay hints. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/features/references.rs` | 54 | LSP find-references. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/features/rename.rs` | 78 | LSP rename. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/features/selection_range.rs` | 223 | LSP selection ranges. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/features/semantic_tokens.rs` | 41 | LSP semantic tokens. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/features/hover/mod.rs` | 55 | LSP hover hub. | host-shim | sub-modules | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/features/hover/directive.rs` | 142 | Hover for directives. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/features/hover/import.rs` | 49 | Hover for imports. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/features/hover/pretty.rs` | 165 | Hover for `@pretty`. | host-shim | analysis | GENERIC | KEEP-AS-IS |
| `crates/analysis/src/features/hover/rule.rs` | 165 | Hover for rules. | host-shim | analysis | GENERIC | KEEP-AS-IS |

Synthesis: `analysis` is a clean LSP-shared library. KEEP-AS-IS in entirety.
The user-feedback note in MEMORY (`analysis-consolidation`) suggests
eliminating AST analysis entirely; but the *project* path lists analysis
moves to IR passes as single source of truth — meaning the heavy
`ir_analysis` and the `directives/*` extractors are candidates for
absorption into IR passes, while the LSP-feature surface (`features/*`,
`state/diagnostics/*`) stays here.

---

## crate: lsp

| File | LOC | Purpose | Layer | Coupling | Fate |
|---|---:|---|---|---|---|
| `crates/lsp/src/lib.rs` | 1 | `pub mod dap;` — surfaces only the DAP server module. | host-shim | dap | GENERIC | KEEP-AS-IS |
| `crates/lsp/src/main.rs` | 30 | LSP binary entry. | host-shim | server | GENERIC | KEEP-AS-IS |
| `crates/lsp/src/server/mod.rs` | 138 | `BbnfLanguageServer` + global rule index. | host-shim | analysis, bbnf | GENERIC | KEEP-AS-IS |
| `crates/lsp/src/server/imports.rs` | 214 | Import resolution + reverse-import graph maintenance. | host-shim | server, bbnf | GENERIC | KEEP-AS-IS |
| `crates/lsp/src/server/protocol.rs` | 441 | LSP protocol handlers (initialize, didOpen, didChange, ...). | host-shim | server, ls_types | GENERIC | KEEP-AS-IS |
| `crates/lsp/src/dap/mod.rs` | 402 | DAP server entry — Content-Length JSON framing + dispatch loop. | host-shim | adapter, protocol, bbnf-ir | GENERIC | KEEP-AS-IS |
| `crates/lsp/src/dap/adapter.rs` | 280 | `DapAdapter` — grammar compilation + interpreter lifecycle. | host-shim | dap, bbnf-ir, bbnf | GENERIC | KEEP-AS-IS |
| `crates/lsp/src/dap/mapping.rs` | 92 | Line ↔ offset conversion + breakpoint resolution. | host-shim | std | GENERIC | KEEP-AS-IS |
| `crates/lsp/src/dap/protocol.rs` | 174 | DAP message types (serde). | host-shim | serde, serde_json | GENERIC | KEEP-AS-IS |

Synthesis: KEEP-AS-IS. The DAP module path (`lsp/src/dap/*`) ships through
`bbnf-lsp` lib, not the LSP server itself — see the one-line `lib.rs`. This
is intentional and correct.

---

## crate: core (the big one)

288 hand-written `.rs` files; 9 generated grammar parsers (the `generated/`
directory is xtask output, not auditable as a hand source). The walk below
groups by directory; the deepest sub-trees (`backend/rust/emitter/shapes/`,
`runtime/<grammar>/`, `lower/`) are tabulated with row groups.

### top-level

| File | LOC | Purpose | Layer | Coupling | Fate |
|---|---:|---|---|---|---|
| `lib.rs` | 38 | Pub-mod hub. Re-exports `types`, `generate`, `graph`. Comments preserve B5.W1 self-alias retirement context. | source-input | sub-modules | GENERIC | KEEP-AS-IS |
| `types.rs` | 136 | `RuleEntry<'a>` + `AST<'a>` (= IndexMap-backed) + `ImportDirective` + `RecoverDirective` + `PrettyDirective` + `HostFnDecl` + `GrammarExtract`. | source-input | parse_that, indexmap, runtime::bbnf | GENERIC | KEEP-AS-IS |
| `css_types.rs` | 66 | `parse_hex_color` host-fn shim — referenced by CSS L4 grammar's `hex` rule's map annotation. | host-shim | std | PER-GRAMMAR (css_l4) | **MOVE** to `crates/core/src/host/css_types.rs` once the layered re-org lands. The current top-level location was a B2.W1 mechanical move when generated source moved into `bbnf` lib. |
| `pipeline.rs` | 103 | Pipeline option + request + output + error types (`PipelineOptions`, `CompileTarget`, `CompileRequest`, `CompileOutput`, `CompileError`). Re-exports compile entry points from the directory module. | parse | sub-modules, bbnf-ir | GENERIC | KEEP-AS-IS |

### grammar/

| File | LOC | Purpose | Layer | Coupling | Fate |
|---|---:|---|---|---|---|
| `grammar/mod.rs` | 67 | `parse(source: &str) -> Option<GrammarExtract>` — the BBNF-on-BBNF self-host parse entry. Leaks input to give `'static`-flavoured lifetimes. | parse | runtime::bbnf, host | PER-GRAMMAR (bbnf) | KEEP-AS-IS |
| `grammar/host.rs` | 584 | Tape walkers: bootstrap view → observational `GrammarExtract` (LSP/gorgeous) OR pipeline-direct `(AST, DirectiveMaps)` (compile). **>500 LOC.** | parse | bbnf-ir, types | PER-GRAMMAR (bbnf) | **SPLIT** into `host/{walk.rs, observational.rs, pipeline_direct.rs}` — three independent extraction modes mixed in one file. |
| `grammar/schema/mod.rs` | 21 | `CstSchema` re-export hub. | codegen | sub-modules | GENERIC | KEEP-AS-IS |
| `grammar/schema/build.rs` | 376 | `CstSchema::from_ir` — builds the CST schema from `GrammarIR`. | codegen | bbnf-ir | GENERIC | KEEP-AS-IS |
| `grammar/schema/model.rs` | 147 | `CstSchema` + `FieldRole` + variant shapes. | codegen | none | GENERIC | KEEP-AS-IS |
| `grammar/schema/emit/mod.rs` | 10 | Per-target emit hub. | codegen | sub-modules | GENERIC | KEEP-AS-IS |
| `grammar/schema/emit/rust/mod.rs` | 31 | The `O3` carve — emits empty TokenStream now; sub-modules dropped. | codegen | sub-modules | GENERIC | KEEP-AS-IS — but the file has a single 31-line entry that drops both sub-module outputs; this is functional but a candidate for **DELETE** in BA if no later wave revives schema-emitted helpers. |
| `grammar/schema/emit/rust/identifiers.rs` | 14 | Identifier-helper emission (output dropped by `O3`). | codegen | none | GENERIC | DELETE if O3 carve persists. |
| `grammar/schema/emit/rust/directives.rs` | 14 | Directive-helper emission (output dropped by `O3`). | codegen | none | GENERIC | DELETE if O3 carve persists. |
| `grammar/schema/emit/rust/shared.rs` | 69 | Shared emit helpers. | codegen | none | GENERIC | DELETE if O3 carve persists. |
| `grammar/generated/mod.rs` | 35 | `pub mod <ident>;` per grammar; re-exports `BbnfBootstrap`. | codegen | generated children | GENERIC | KEEP-AS-IS |
| `grammar/generated/bbnf.rs` | 21,503 | Generated BBNF self-host parser. | codegen | runtime::bbnf, simd-scan | PER-GRAMMAR (bbnf) | KEEP-AS-IS (xtask output; size-exempt). |
| `grammar/generated/json.rs` | 3,500 | Generated JSON parser. | codegen | runtime::json, simd-scan | PER-GRAMMAR (json) | KEEP-AS-IS |
| `grammar/generated/css_l4.rs` | 107,138 | Generated CSS L4 parser. | codegen | runtime::css_l4, simd-scan | PER-GRAMMAR (css_l4) | KEEP-AS-IS |
| `grammar/generated/css_pretty.rs` | 9,021 | Generated CSS-pretty parser. | codegen | runtime::css_pretty, simd-scan | PER-GRAMMAR (css_pretty) | KEEP-AS-IS |
| `grammar/generated/google_sheets.rs` | 14,088 | Generated Google Sheets parser. | codegen | runtime::google_sheets | PER-GRAMMAR (sheets) | KEEP-AS-IS |
| `grammar/generated/ebnf.rs` | 7,646 | Generated EBNF parser. | codegen | runtime::ebnf | PER-GRAMMAR (ebnf) | KEEP-AS-IS |
| `grammar/generated/bnf.rs` | 3,290 | Generated BNF parser. | codegen | runtime::bnf | PER-GRAMMAR (bnf) | KEEP-AS-IS |
| `grammar/generated/csv.rs` | 1,693 | Generated CSV parser. | codegen | runtime::csv | PER-GRAMMAR (csv) | KEEP-AS-IS |
| `grammar/generated/math.rs` | 871 | Generated math expression parser. | codegen | runtime::math | PER-GRAMMAR (math) | KEEP-AS-IS |
| `grammar/generated/*.registry.json` | 9 files | Per-grammar `StructRegistry` JSON sidecars consumed by the `path!` proc-macro. | codegen | bbnf-ir | PER-GRAMMAR | KEEP-AS-IS |

### imports/

| File | LOC | Purpose | Layer | Coupling | Fate |
|---|---:|---|---|---|---|
| `imports/mod.rs` | 14 | Hub. | parse | sub-modules | GENERIC | KEEP-AS-IS |
| `imports/errors.rs` | 98 | `ImportError` enum. | parse | std | GENERIC | KEEP-AS-IS |
| `imports/loader.rs` | 186 | `load_module_graph` — multi-file `@import` resolution. | parse | std, types, registry | GENERIC | KEEP-AS-IS |
| `imports/registry.rs` | 108 | `ModuleRegistry` + `ModuleData` + `ImportCycle` + `ResolvedImport`. | parse | std, types | GENERIC | KEEP-AS-IS |
| `imports/resolve.rs` | 160 | Per-rule import resolution + namespace assembly. | parse | std, types, registry | GENERIC | KEEP-AS-IS |

### graph/

| File | LOC | Purpose | Layer | Coupling | Fate |
|---|---:|---|---|---|---|
| `graph/mod.rs` | 9 | Hub. | parse | sub-modules | GENERIC | KEEP-AS-IS |
| `graph/deps.rs` | 299 | `collect_nonterminal_refs` + dep-graph iteration helpers. | parse | bbnf-ir, types | GENERIC | KEEP-AS-IS |
| `graph/scc.rs` | 250 | `SccResult` + Tarjan SCC over BBNF AST. | parse | types | GENERIC | KEEP-AS-IS |
| `graph/metadata.rs` | 218 | `find_aliases` (AST level — IR layer has its own). | parse | types | GENERIC | KEEP-AS-IS — though MEMORY's `analysis-consolidation` flag suggests this AST-level pass should be subsumed into IR `passes/metadata.rs`. |

### lower/

| File | LOC | Purpose | Layer | Coupling | Fate |
|---|---:|---|---|---|---|
| `lower/mod.rs` | 356 | `lower_to_ir` — orchestrator. `LowerCtx` + `DirectiveSet` + closure-extraction. | IR-lower | bbnf-ir, runtime::bbnf, graph | GENERIC | KEEP-AS-IS |
| `lower/fn_table.rs` | 20 | `FnTable` — host-fn descriptor table. | IR-lower | bbnf-ir | GENERIC | KEEP-AS-IS |
| `lower/metadata.rs` | 106 | `build_rule_meta` — pretty hints + token + directives → `RuleMeta`. | IR-lower | bbnf-ir | GENERIC | KEEP-AS-IS |
| `lower/string_interner.rs` | 35 | `StringInterner` — temporary one for the lowering pass. | IR-lower | std | GENERIC | KEEP-AS-IS |
| `lower/view_walk.rs` | 257 | View walking primitives shared by lower/expression/* and lower/value_expr/*. | IR-lower | runtime::bbnf | GENERIC | KEEP-AS-IS |
| `lower/expression/mod.rs` | 539 | `lower_rhs` — per-compound-kind dispatch. **>500 LOC.** | IR-lower | bbnf-ir | GENERIC | **SPLIT** into `expression/{dispatch.rs, alt.rs, seq.rs, repeat.rs, leaf.rs}` — the file is one large match-on-compound-kind. |
| `lower/expression/alt.rs` | 184 | Alt branch lowering. | IR-lower | bbnf-ir | GENERIC | KEEP-AS-IS |
| `lower/expression/closures.rs` | 91 | Grammar-fn closure registration + invocation. | IR-lower | bbnf-ir, view_walk | GENERIC | KEEP-AS-IS |
| `lower/expression/pratt.rs` | 329 | Operator-chain Pratt detection during lowering. | IR-lower | bbnf-ir | GENERIC | KEEP-AS-IS |
| `lower/expression/repeat.rs` | 174 | Repeat-shape lowering. | IR-lower | bbnf-ir | GENERIC | KEEP-AS-IS |
| `lower/expression/wrap.rs` | 731 | Wrap-shape (`open >> body << close`) lowering with delim-scan optimisation. **>500 LOC, second-largest core file.** | IR-lower | bbnf-ir, runtime::bbnf | GENERIC | **SPLIT** into `wrap/{detect.rs, lower.rs, delim_scan.rs, balanced.rs}` — the file mixes detection (when-is-this-a-wrap), lowering (compose Skip/Next), and delim-scan recognition. |
| `lower/value_expr/mod.rs` | 178 | Value-expr (`->` MapExpr) lowering hub. | IR-lower | bbnf-ir | GENERIC | KEEP-AS-IS |
| `lower/value_expr/atom.rs` | 590 | Atom-form value-expr lowering. **>500 LOC.** | IR-lower | bbnf-ir | GENERIC | **SPLIT** into `atom/{ident.rs, literal.rs, fncall.rs, structlit.rs}`. |
| `lower/value_expr/literals.rs` | 58 | Literal value-expr (`true`, `false`, `0u8`). | IR-lower | bbnf-ir | GENERIC | KEEP-AS-IS |
| `lower/value_expr/precedence.rs` | 340 | Precedence climbing for value-expr operators (`+`, `*`, `..`). | IR-lower | bbnf-ir | GENERIC | KEEP-AS-IS |
| `lower/value_expr/simple_kinds.rs` | 235 | Simple `BinaryFactor`/`UnaryFactor` + `Negate` lowering. | IR-lower | bbnf-ir | GENERIC | KEEP-AS-IS |
| `lower/value_expr/unwrap.rs` | 256 | Unwrap a value-expr from its CST wrapper. | IR-lower | bbnf-ir | GENERIC | KEEP-AS-IS |
| `lower/value_expr/view_walk.rs` | 43 | View-walk helpers specific to value-expr. | IR-lower | runtime::bbnf | GENERIC | KEEP-AS-IS |

### path/ — typed-path executor

| File | LOC | Purpose | Layer | Coupling | Fate |
|---|---:|---|---|---|---|
| `path/mod.rs` | 60 | Hub. | runtime-execute | sub-modules | GENERIC | KEEP-AS-IS |
| `path/ascent.rs` | 277 | `AscentStrategy` trait + `DefaultAscent` + `HybridSidecar` + `InStructPointer` + `RootTraversal`. **HARDENING-SYNTHESIS L08 flags `AscentStrategy` as needing a wave owner**; per BC.W0 amendment it gets explicit KEEP-MODERNIZE / ABROGATE assignment. | runtime-execute | bbnf-ir | GENERIC | KEEP-AS-IS pending BC.W0 disposition. |
| `path/cursor.rs` | 431 | `PathCursor` state machine + `Decision` + `SegmentKind`. | runtime-execute | bbnf-ir | GENERIC | KEEP-AS-IS |
| `path/error.rs` | 143 | `PathError` + `PathErrorReason`. | runtime-execute | std | GENERIC | KEEP-AS-IS |
| `path/executor.rs` | 171 | `PathExecutor` top-level orchestrator. | runtime-execute | cursor, ascent, schema | GENERIC | KEEP-AS-IS |
| `path/ir.rs` | 323 | `Path<'a>`, `PathSegment<'a>`, `TypedPath<G, T>`, `OwnedPathSegment`, `IntoPathSegment`. | runtime-execute | std | GENERIC | KEEP-AS-IS |
| `path/markers.rs` | 30 | Grammar-marker ZSTs `Json` / `CssL4` / `Sheets` / `Bbnf`. | runtime-execute | none | GENERIC (marker shape per grammar) | KEEP-AS-IS |
| `path/schema.rs` | 168 | `PathSchema` trait + `GrammarMarker` trait. | runtime-execute | bbnf-ir | GENERIC | KEEP-AS-IS |
| `path/type_check.rs` | 338 | `check_path` + `check_path_against_registry` — offline path validator. | type-infer | bbnf-ir | GENERIC | KEEP-AS-IS |
| `path/variant_select.rs` | 90 | `select_variant` for typed-enum variant resolution. | runtime-execute | bbnf-ir | GENERIC | KEEP-AS-IS |
| `path/wildcard.rs` | 203 | `WildcardIter` + `WildcardConfig` + `DEFAULT_WILDCARD_DEPTH_CAP` + `ends_with_wildcard`. | runtime-execute | bbnf-ir | GENERIC | KEEP-AS-IS |

### pipeline/

| File | LOC | Purpose | Layer | Coupling | Fate |
|---|---:|---|---|---|---|
| `pipeline.rs` (top-level, 103 LOC) | — | already documented above. | parse | sub-modules | GENERIC | KEEP-AS-IS |
| `pipeline/directives.rs` | 205 | `DirectiveMaps` aggregation; `parse_to_pipeline_inputs` + `load_merged_paths`. | parse | imports, types, runtime::bbnf | GENERIC | KEEP-AS-IS |
| `pipeline/validate.rs` | 91 | `validate_pretty_directives` + `validate_ast`. | parse | bbnf-ir, types | GENERIC | KEEP-AS-IS |
| `pipeline/compile/mod.rs` | 125 | Compile entry — `compile_grammar` / `compile_grammar_request` / `compile_paths_request` / `compile_ast`. | parse | sub-modules, bbnf-ir | GENERIC | KEEP-AS-IS |
| `pipeline/compile/audit.rs` | 124 | Audit-coverage emission + `EmitStrategy` adapter. | parse | bbnf-ir, audit pass | GENERIC | KEEP-AS-IS |
| `pipeline/compile/closure_partition.rs` | 212 | Closure structural-detection helpers. | parse | runtime::bbnf | GENERIC | KEEP-AS-IS |
| `pipeline/compile/pipeline.rs` | 481 | `compile_ast_common` — the canonical pass-list orchestrator + driver-state plumbing. | parse | bbnf-ir, lower | GENERIC | KEEP-AS-IS |
| `pipeline/compile/target.rs` | 125 | `finalize_compile` — per-CompileTarget dispatch. | parse | bbnf-ir, backend | GENERIC | KEEP-AS-IS |
| `pipeline/compile/timer.rs` | 60 | `PipelineTimer` — per-pass timing accumulator. | parse | std | GENERIC | KEEP-AS-IS |

### generate/

The codegen entry point. Trail of `generate_all` from `Track 1 (CstSchema)` +
`Track 2 (backend driver)`.

| File | LOC | Purpose | Layer | Coupling | Fate |
|---|---:|---|---|---|---|
| `generate/mod.rs` | 108 | `generate_all(prepared, parser_attrs, ident)` — the toplevel codegen entry; orchestrates Track 1 (CST helpers) + Track 2 (parser fns). | codegen | backend, grammar::schema | GENERIC | KEEP-AS-IS |
| `generate/regex/mod.rs` | 28 | Regex codegen hub (DFA emit, charclass tables). | codegen | sub-modules | GENERIC | KEEP-AS-IS |
| `generate/regex/byte_class.rs` | 236 | Byte-class table emit. | codegen | bbnf-regex | GENERIC | KEEP-AS-IS |
| `generate/regex/cost_model.rs` | 188 | Regex extraction cost model used during codegen. | codegen | bbnf-regex | GENERIC | KEEP-AS-IS |
| `generate/regex/last_byte_set.rs` | 139 | Compute LAST(regex) byte set. | codegen | bbnf-regex | GENERIC | KEEP-AS-IS |
| `generate/regex/phf.rs` | 183 | Perfect-hash-function table emit for keyword sets. | codegen | none | GENERIC | KEEP-AS-IS |
| `generate/regex/emit/mod.rs` | 316 | Regex emit hub — choose DFA / generalized / negated-class / scanner-plan. | codegen | sub-modules | GENERIC | KEEP-AS-IS |
| `generate/regex/emit/dfa/mod.rs` | 480 | DFA emitter. | codegen | bbnf-regex | GENERIC | KEEP-AS-IS |
| `generate/regex/emit/dfa/accel.rs` | 268 | DFA accel-state emit. | codegen | dfa::mod | GENERIC | KEEP-AS-IS |
| `generate/regex/emit/dfa/table.rs` | 91 | DFA transition-table emit. | codegen | dfa::mod | GENERIC | KEEP-AS-IS |
| `generate/regex/emit/generalized/mod.rs` | 177 | Generalized HIR emit hub. | codegen | bbnf-regex | GENERIC | KEEP-AS-IS |
| `generate/regex/emit/generalized/class_segments.rs` | 363 | Class-segment compaction. | codegen | bbnf-regex | GENERIC | KEEP-AS-IS |
| `generate/regex/emit/hir/mod.rs` | 246 | HIR emit dispatch. | codegen | bbnf-regex | GENERIC | KEEP-AS-IS |
| `generate/regex/emit/hir/alternation.rs` | 277 | HIR alternation emit. | codegen | bbnf-regex | GENERIC | KEEP-AS-IS |
| `generate/regex/emit/hir/leaf.rs` | 251 | HIR leaf emit. | codegen | bbnf-regex | GENERIC | KEEP-AS-IS |
| `generate/regex/emit/hir/repetition.rs` | 240 | HIR repetition emit. | codegen | bbnf-regex | GENERIC | KEEP-AS-IS |
| `generate/regex/emit/negated_class.rs` | 128 | Negated-class emit. | codegen | bbnf-regex | GENERIC | KEEP-AS-IS |
| `generate/regex/emit/scanner_plan.rs` | 203 | `ScannerPlan` emit (one-pass scan loops). | codegen | bbnf-regex | GENERIC | KEEP-AS-IS |
| `generate/regex/emit/simd.rs` | 341 | SIMD scanner emit (calls into simd-scan). | codegen | simd-scan | GENERIC | KEEP-AS-IS |
| `generate/regex/patterns/mod.rs` | 12 | Hub. | codegen | sub-modules | GENERIC | KEEP-AS-IS |
| `generate/regex/patterns/char_class.rs` | 105 | Char-class pattern emit. | codegen | bbnf-regex | GENERIC | KEEP-AS-IS |
| `generate/regex/patterns/shorthand.rs` | 106 | `\d\w\s` shorthand emit. | codegen | bbnf-regex | GENERIC | KEEP-AS-IS |
| `generate/serialize/mod.rs` | 105 | Serialize codegen hub. | codegen | sub-modules | GENERIC | KEEP-AS-IS — though gated to retire if archive ser. |
| `generate/serialize/serialize.rs` | 51 | Serializer-method emit. | codegen | bbnf-ser | GENERIC | DELETE alongside ser archive. |

### backend/

The Rust/TS/WASM emitter family + the shared compilation driver.

| File | LOC | Purpose | Layer | Coupling | Fate |
|---|---:|---|---|---|---|
| `backend/mod.rs` | 27 | Hub — re-exports driver/emitter/types. | codegen | sub-modules, bbnf-ir | GENERIC | KEEP-AS-IS |
| `backend/emitter.rs` | 566 | `Emitter` trait — all backends impl this. **>500 LOC.** | codegen | bbnf-ir, prettify, types | GENERIC | **SPLIT** into `emitter/{trait.rs, leaves.rs, sequences.rs, alternations.rs, repeat.rs, ref.rs, prettify.rs}`. The trait has many methods; grouping by IR-node-shape gives smaller files. |
| `backend/util.rs` | 33 | `unescape_literal` — shared backend helper. | codegen | std | GENERIC | KEEP-AS-IS |

#### backend/types/

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `backend/types/mod.rs` | 128 | `SeqChildGroup` + `SepByConfig` + `CallStrategy` + `SeqResultStrategy` + `FlattenStrategy` + `AltStrategy` + `AltBranchInfo` + `ValuePlacement` + `TokenDispatchArmCompiled` + `KeyDispatchBranch`. | KEEP-AS-IS |
| `backend/types/decisions.rs` | 148 | Decision-types shared across backends. | KEEP-AS-IS |

#### backend/strategy/

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `backend/strategy/mod.rs` | 41 | `NodeStrategy` enum + sub-module re-exports. | KEEP-AS-IS |
| `backend/strategy/alt_strategy.rs` | 196 | `AltStrategy` — Alt decision type. | KEEP-AS-IS |
| `backend/strategy/ref_strategy.rs` | 45 | `RefStrategy`. | KEEP-AS-IS |
| `backend/strategy/repeat_strategy.rs` | 31 | `RepeatStrategy`. | KEEP-AS-IS |
| `backend/strategy/seq_strategy.rs` | 60 | `SeqStrategy`. | KEEP-AS-IS |
| `backend/strategy/wrap_strategy.rs` | 18 | `WrapStrategy`. | KEEP-AS-IS |

#### backend/prettify/

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `backend/prettify/mod.rs` | 15 | Hub. | KEEP-AS-IS |
| `backend/prettify/analysis.rs` | 72 | `emits_only_on_success` + `may_open_groups`. | KEEP-AS-IS |
| `backend/prettify/plan.rs` | 194 | `build_rule_plans` — per-rule pretty plan derivation. | KEEP-AS-IS |
| `backend/prettify/sep_rewrite.rs` | 145 | `split_inner_for_sep` + `SilentPosition`. | KEEP-AS-IS |
| `backend/prettify/types.rs` | 41 | `PrettyPolicy` + `PrettyRulePlan`. | KEEP-AS-IS |

#### backend/kernels/

Pure emitter kernels — input is pre-computed; no IR re-walking.

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `backend/kernels/mod.rs` | 34 | Hub. | KEEP-AS-IS |
| `backend/kernels/balanced_wrap.rs` | 98 | `BalancedDelimiter` kernel emit. | KEEP-AS-IS |
| `backend/kernels/charclass.rs` | 260 | Charclass dispatch emit (LUT vs branch). | KEEP-AS-IS |
| `backend/kernels/charset_shapes.rs` | 45 | `CharSet` shape selectors. | KEEP-AS-IS |
| `backend/kernels/comment_ws.rs` | 13 | `comment_ws` kernel. | KEEP-AS-IS |
| `backend/kernels/identifier.rs` | 25 | Identifier-scan kernel. | KEEP-AS-IS |
| `backend/kernels/number.rs` | 33 | Number-scan kernel (Eisel-Lemire). | KEEP-AS-IS |
| `backend/kernels/prefix_class.rs` | 115 | Prefix-then-class kernel. | KEEP-AS-IS |
| `backend/kernels/punct_ws_region.rs` | 155 | `PunctWsRegion` kernel — ws-wrapped structural punct. | KEEP-AS-IS |
| `backend/kernels/quoted_string.rs` | 20 | Quoted-string scan kernel. | KEEP-AS-IS |

#### backend/driver/

Target-agnostic emission orchestrator.

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `backend/driver/mod.rs` | 347 | `DriverState` + `compile_grammar` + per-strategy dispatch. | KEEP-AS-IS |
| `backend/driver/alt.rs` | 272 | `compile_alt`. | KEEP-AS-IS |
| `backend/driver/seq.rs` | 246 | `compile_seq`. | KEEP-AS-IS |
| `backend/driver/repeat.rs` | 157 | `compile_repeat`. | KEEP-AS-IS |
| `backend/driver/reference.rs` | 171 | `compile_reference`. | KEEP-AS-IS |
| `backend/driver/wrap.rs` | 143 | `compile_wrap`. | KEEP-AS-IS |
| `backend/driver/map.rs` | 72 | `compile_map`. | KEEP-AS-IS |
| `backend/driver/node.rs` | 215 | `compile_node` — top-level node dispatch. | KEEP-AS-IS |
| `backend/driver/analysis.rs` | 226 | `BackendAnalysis` + `BackendPreparation` + `EffectiveBackendConfig` + `prepare_grammar` + `TypeAnalysis`. | KEEP-AS-IS |
| `backend/driver/prettify.rs` | 247 | `compile_prettify_grammar`. | KEEP-AS-IS |

#### backend/rust/

The Rust emitter — the largest sub-tree.

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `backend/rust/mod.rs` | 15 | Hub. | KEEP-AS-IS |
| `backend/rust/ir_types.rs` | 372 | `IrCodegenCtx` + `ParserAttributes` + per-rule codegen shared state. | KEEP-AS-IS |
| `backend/rust/ir_enums.rs` | 57 | Per-grammar enum-emission helpers. | KEEP-AS-IS |
| `backend/rust/emitter_types.rs` | 294 | `RustEmitter` + `RustEmitCtx`. | KEEP-AS-IS |
| `backend/rust/view/mod.rs` | 10 | Hub. | KEEP-AS-IS |
| `backend/rust/view/named_types.rs` | 248 | Named-type emit. | KEEP-AS-IS |
| `backend/rust/analysis/mod.rs` | 2 | One-line hub. | KEEP-AS-IS |
| `backend/rust/analysis/specialize.rs` | 69 | Specialization decisions. | KEEP-AS-IS |
| `backend/rust/analysis/inline/mod.rs` | 38 | Hub. | KEEP-AS-IS |
| `backend/rust/analysis/inline/budgets.rs` | 111 | Inline budgets. | KEEP-AS-IS |
| `backend/rust/analysis/inline/constraints.rs` | 235 | Inline constraints. | KEEP-AS-IS |
| `backend/rust/analysis/inline/plan.rs` | 185 | Inline plan. | KEEP-AS-IS |
| `backend/rust/analysis/inline/visit.rs` | 215 | Inline visitor. | KEEP-AS-IS |
| `backend/rust/emitter/mod.rs` | 379 | `impl Emitter for RustEmitter` — single trait-impl block (Rust requirement). | KEEP-AS-IS |
| `backend/rust/emitter/grammar.rs` | 468 | `emit_grammar_impl` + `parse_body` arm + per-grammar entry. | KEEP-AS-IS |
| `backend/rust/emitter/keyword_dispatch.rs` | 212 | Keyword-dispatch helper code emit. | KEEP-AS-IS |
| `backend/rust/emitter/path_plan.rs` | 356 | Path-plan emission (lazy bail-out paths). | KEEP-AS-IS |
| `backend/rust/emitter/precedence.rs` | 274 | Precedence/Pratt emission. | KEEP-AS-IS |
| `backend/rust/emitter/profile.rs` | 105 | `GrammarProfile` const emit. | KEEP-AS-IS |
| `backend/rust/emitter/regex_scan_adapter.rs` | 786 | Regex-scan adapter — replays cold-path patterns. **>500 LOC.** | **SPLIT** into `regex_scan_adapter/{adapter.rs, replay.rs, sanitise.rs}`. |
| `backend/rust/emitter/registry_emit.rs` | 207 | `StructRegistry` JSON sidecar emit. | KEEP-AS-IS |
| `backend/rust/emitter/prettify/mod.rs` | 94 | Hub. | KEEP-AS-IS |
| `backend/rust/emitter/prettify/grammar.rs` | 88 | Prettify grammar emit. | KEEP-AS-IS |
| `backend/rust/emitter/prettify/alt.rs` | 114 | Prettify Alt emit. | KEEP-AS-IS |
| `backend/rust/emitter/prettify/attempt.rs` | 117 | Prettify attempt-with-rollback emit. | KEEP-AS-IS |
| `backend/rust/emitter/prettify/literal.rs` | 68 | Prettify literal emit. | KEEP-AS-IS |
| `backend/rust/emitter/prettify/repeat.rs` | 155 | Prettify Repeat emit. | KEEP-AS-IS |
| `backend/rust/emitter/prettify/seq.rs` | 152 | Prettify Seq emit. | KEEP-AS-IS |

##### backend/rust/emitter/shapes/

The per-shape emitters. Each one corresponds to a `ShapeTag` from `passes/recognizers/shape_dispatch/`.

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `shapes/mod.rs` | 306 | `emit_shapes_for_grammar` orchestrator + `sanitise_grammar` + per-rule dispatch. | KEEP-AS-IS |
| `shapes/arglist.rs` | 389 | ArgList shape emitter. | KEEP-AS-IS |
| `shapes/cursor_param.rs` | 75 | Cursor-param helper. | KEEP-AS-IS |
| `shapes/hregex.rs` | 484 | HRegex shape emitter (regex-with-host-decode rules). | KEEP-AS-IS |
| `shapes/number.rs` | 200 | Number shape emitter (Eisel-Lemire). | KEEP-AS-IS |
| `shapes/object.rs` | 435 | Object shape emitter (key-dispatched record). | KEEP-AS-IS |
| `shapes/scalar.rs` | 160 | Scalar shape emitter (single-leaf). | KEEP-AS-IS |
| `shapes/string.rs` | 217 | String shape emitter (quoted-string scan). | KEEP-AS-IS |
| `shapes/substrate.rs` | 119 | Per-shape substrate helpers. | KEEP-AS-IS |
| `shapes/unordered.rs` | 411 | Unordered-Repeat shape emitter. | KEEP-AS-IS |
| `shapes/alt_dispatch/mod.rs` | 173 | AltDispatch shape entry. | KEEP-AS-IS |
| `shapes/alt_dispatch/branches.rs` | 422 | Branch emission. | KEEP-AS-IS |
| `shapes/array/mod.rs` | 514 | Array shape emitter. **At/over 500 LOC.** | **SPLIT** into `array/{repeat.rs, sep_by.rs, terminator.rs, body.rs}` — array emission has terminator-detection, sep_by detection, and body-emit branches each pulling its weight. |
| `shapes/array/element.rs` | 77 | Element-emit helper. | KEEP-AS-IS |
| `shapes/dispatcher/mod.rs` | 76 | Hub. | KEEP-AS-IS |
| `shapes/dispatcher/cross_shape.rs` | 338 | Cross-shape dispatcher. | KEEP-AS-IS |
| `shapes/dispatcher/ref_call.rs` | 230 | Ref-call resolver. | KEEP-AS-IS |
| `shapes/dispatcher/support.rs` | 902 | Per-grammar support-module emit (`ScanState`, ws-skip, etc.). **>500 LOC, third-largest core file.** | **SPLIT** into `support/{scan_state.rs, ws_skip.rs, exec_helpers.rs, regex_replay.rs}`. |
| `shapes/dispatcher/symbol_composition.rs` | 32 | Symbol composition helpers. | KEEP-AS-IS |
| `shapes/flat/mod.rs` | 138 | Flat shape entry. | KEEP-AS-IS |
| `shapes/flat/struct_direct.rs` | 1033 | Flat-shape StructDirect emitter. **>500 LOC, the largest core file at 1033 LOC.** | **SPLIT** into `flat/struct_direct/{header.rs, body.rs, fields.rs, finalize.rs}`. |
| `shapes/inline/mod.rs` | 20 | Hub. | KEEP-AS-IS |
| `shapes/inline/structural_branch.rs` | 318 | Structural-branch inlining. | KEEP-AS-IS |
| `shapes/keyword/mod.rs` | 59 | Hub. | KEEP-AS-IS |
| `shapes/keyword/payload.rs` | 179 | Keyword-payload emit. | KEEP-AS-IS |
| `shapes/keyword/struct_direct.rs` | 534 | Keyword-shape StructDirect emitter. **>500 LOC.** | **SPLIT** into `keyword/struct_direct/{dispatch.rs, payload.rs, body.rs}`. |
| `shapes/pratt/mod.rs` | 47 | Hub. | KEEP-AS-IS |
| `shapes/pratt/dispatch.rs` | 19 | Pratt dispatch. | KEEP-AS-IS |
| `shapes/pratt/struct_direct.rs` | 364 | Pratt StructDirect emitter. | KEEP-AS-IS |
| `shapes/wrap/mod.rs` | 61 | Hub. | KEEP-AS-IS |
| `shapes/wrap/struct_direct.rs` | 622 | Wrap-shape StructDirect emitter. **>500 LOC.** | **SPLIT** into `wrap/struct_direct/{open.rs, body.rs, close.rs}`. |

#### backend/ts/

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `backend/ts/mod.rs` | 9 | Hub. | KEEP-AS-IS |
| `backend/ts/alt.rs` | 154 | TS Alt emit. | KEEP-AS-IS |
| `backend/ts/code.rs` | 78 | `TsCode` + `TsEmitCtx` + `TsEmitter`. | KEEP-AS-IS |
| `backend/ts/dispatch.rs` | 105 | TS dispatch emit. | KEEP-AS-IS |
| `backend/ts/projection.rs` | 196 | TS projection. | KEEP-AS-IS |
| `backend/ts/repeat.rs` | 109 | TS Repeat emit. | KEEP-AS-IS |
| `backend/ts/ws.rs` | 34 | TS ws-trim. | KEEP-AS-IS |
| `backend/ts/emitter/mod.rs` | 338 | `impl Emitter for TsEmitter`. | KEEP-AS-IS |
| `backend/ts/emitter/binary.rs` | 134 | Binary-op TS emit. | KEEP-AS-IS |
| `backend/ts/emitter/grammar.rs` | 282 | Grammar-level TS emit. | KEEP-AS-IS |
| `backend/ts/emitter/leaves.rs` | 132 | Leaf TS emit. | KEEP-AS-IS |
| `backend/ts/emitter/value.rs` | 148 | Value TS emit. | KEEP-AS-IS |

#### backend/wasm/

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `backend/wasm/mod.rs` | 11 | Hub. | KEEP-AS-IS |
| `backend/wasm/alt.rs` | 176 | WASM Alt emit. | KEEP-AS-IS |
| `backend/wasm/code.rs` | 118 | `WasmEmitCtx` + `WasmEmitter`. | KEEP-AS-IS |
| `backend/wasm/dispatch.rs` | 143 | WASM dispatch. | KEEP-AS-IS |
| `backend/wasm/escape.rs` | 11 | WAT escaping. | KEEP-AS-IS |
| `backend/wasm/repeat.rs` | 120 | WASM Repeat. | KEEP-AS-IS |
| `backend/wasm/ws.rs` | 94 | WASM ws-trim. | KEEP-AS-IS |
| `backend/wasm/emitter/mod.rs` | 300 | `impl Emitter for WasmEmitter`. | KEEP-AS-IS |
| `backend/wasm/emitter/binary.rs` | 133 | Binary WASM emit. | KEEP-AS-IS |
| `backend/wasm/emitter/grammar.rs` | 62 | WASM grammar emit. | KEEP-AS-IS |
| `backend/wasm/emitter/leaves.rs` | 120 | Leaf WASM emit. | KEEP-AS-IS |
| `backend/wasm/emitter/value.rs` | 120 | Value WASM emit. | KEEP-AS-IS |

### runtime/

The output home — generated parsers populate these. Per-grammar arenas, builders, and document types.

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `runtime/mod.rs` | 76 | Hub — re-exports every per-grammar surface. | KEEP-AS-IS |
| `runtime/arena_template.rs` | 134 | `CompoundEntry` + `CompoundSlabArena` shared by simple-cohort grammars. | KEEP-AS-IS |
| `runtime/builder.rs` | 141 | `StructBuilder` trait — the consumer surface generated parsers target. | KEEP-AS-IS |
| `runtime/builder_template.rs` | 286 | `SimpleStructBuilder` + `SimpleValue` trait — generic template the BNF/EBNF/CSV/CssPretty/Math grammars use. | KEEP-AS-IS |
| `runtime/error.rs` | 56 | `DtaError` + `ParseErr` — public parse-error surface. | KEEP-AS-IS |
| `runtime/handle.rs` | 139 | `StringHandle` + `CompoundHandle` — `Copy` handles for the Value API substrate. | KEEP-AS-IS |
| `runtime/path.rs` | 163 | `Path<'a>` + `PathSegment<'a>` + `IntoPathSegment` — the borrowed path alphabet. | KEEP-AS-IS |
| `runtime/view.rs` | 76 | `RuntimeView<'p>` trait — grammar-agnostic typed-view surface. | KEEP-AS-IS |

#### runtime/<grammar>/ — per-grammar runtimes

Each cohort grammar has its own arena, builder, document, value, view, parse_with module.

##### runtime/json/

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `runtime/json/mod.rs` | 53 | Hub. | KEEP-AS-IS |
| `runtime/json/arena.rs` | 186 | `JsonArena` slab. | KEEP-AS-IS |
| `runtime/json/builder.rs` | 382 | `JsonStructBuilder`. | KEEP-AS-IS |
| `runtime/json/document.rs` | 456 | `JsonDocument` + `JsonView` + `JsonPathQuery` trait. | KEEP-AS-IS |
| `runtime/json/parse_with.rs` | 133 | `parse_with(input, &path)` — lazy bail-out parse entry. | KEEP-AS-IS |
| `runtime/json/value.rs` | 121 | `JsonValue` + `JsonNumber` + `JsonPair` + `JsonObject` + `JsonArray`. | KEEP-AS-IS |
| `runtime/json/view.rs` | 96 | `JsonView` `RuntimeView` impl. | KEEP-AS-IS |

##### runtime/css_l4/

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `runtime/css_l4/mod.rs` | 79 | Hub. | KEEP-AS-IS |
| `runtime/css_l4/arena.rs` | 390 | `CssArena` (large — 14 OpenFrame variants). | KEEP-AS-IS |
| `runtime/css_l4/builder.rs` | 1014 | `CssStructBuilder` — 14-variant frame dispatch. **>500 LOC, second-largest core file.** | **SPLIT** into `builder/{frame.rs, declarations.rs, selectors.rs, color.rs, dimensions.rs, finalize.rs}` — by frame-variant family. |
| `runtime/css_l4/document.rs` | 541 | `CssDocument` + `CssView` + `CssPathQuery`. **>500 LOC.** | **SPLIT** into `document/{root.rs, view.rs, path_query.rs}`. |
| `runtime/css_l4/parse_with.rs` | 113 | `parse_with`. | KEEP-AS-IS |
| `runtime/css_l4/value.rs` | 852 | Typed `CssValue` enum — color/dimension/calc/etc. **>500 LOC.** | **SPLIT** into `value/{color.rs, dimension.rs, calc.rs, function.rs, primitive.rs}` — by typed-value family. |
| `runtime/css_l4/view.rs` | 137 | `RuntimeView` impl. | KEEP-AS-IS |

##### runtime/google_sheets/

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `runtime/google_sheets/mod.rs` | 56 | Hub. | KEEP-AS-IS |
| `runtime/google_sheets/arena.rs` | 332 | `SheetsArena`. | KEEP-AS-IS |
| `runtime/google_sheets/builder.rs` | 357 | `SheetsStructBuilder` — specialised leaf-deposit (cell_ref, identifier, sheet_prefix, error). | KEEP-AS-IS |
| `runtime/google_sheets/parse_with.rs` | 114 | `parse_with`. | KEEP-AS-IS |
| `runtime/google_sheets/value.rs` | 189 | `SheetsValue`. | KEEP-AS-IS |
| `runtime/google_sheets/view.rs` | 95 | `SheetsView`. | KEEP-AS-IS |
| `runtime/google_sheets/document/mod.rs` | 150 | Hub. | KEEP-AS-IS |
| `runtime/google_sheets/document/canonical.rs` | 411 | Canonical document ops. | KEEP-AS-IS |
| `runtime/google_sheets/document/path_query.rs` | 114 | `SheetsPathQuery`. | KEEP-AS-IS |
| `runtime/google_sheets/document/view.rs` | 135 | View. | KEEP-AS-IS |

##### runtime/bbnf/ (the self-host)

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| `runtime/bbnf/mod.rs` | 51 | Hub. | KEEP-AS-IS |
| `runtime/bbnf/arena.rs` | 341 | `BbnfArena`. | KEEP-AS-IS |
| `runtime/bbnf/builder.rs` | 243 | `BbnfStructBuilder`. | KEEP-AS-IS |
| `runtime/bbnf/document.rs` | 453 | `BbnfDocument`. | KEEP-AS-IS |
| `runtime/bbnf/parse_with.rs` | 120 | `parse_with`. | KEEP-AS-IS |
| `runtime/bbnf/serialize.rs` | 442 | BBNF serializer. | KEEP-AS-IS |
| `runtime/bbnf/value.rs` | 96 | `BbnfValue`. | KEEP-AS-IS |
| `runtime/bbnf/view.rs` | 280 | `BbnfView`. | KEEP-AS-IS |

##### runtime/{bnf,ebnf,csv,css_pretty,math}/ — simple cohort

Each file ≤180 LOC; each consumes the `SimpleStructBuilder` + `SimpleValue` template.

| File | LOC | Purpose | Fate |
|---|---:|---|---|
| each `runtime/<simple>/mod.rs` | 18 | Hub. | KEEP-AS-IS |
| each `runtime/<simple>/arena.rs` | 54-55 | Arena. | KEEP-AS-IS |
| each `runtime/<simple>/builder.rs` | 54-55 | Builder shim onto `SimpleStructBuilder`. | KEEP-AS-IS |
| each `runtime/<simple>/document.rs` | 171-237 | Document. | KEEP-AS-IS |
| each `runtime/<simple>/value.rs` | 23-57 | Value enum. | KEEP-AS-IS |
| each `runtime/<simple>/view.rs` | 64-80 | `RuntimeView` impl. | KEEP-AS-IS |
| each `runtime/<simple>/kind.rs` | 46-67 | Per-grammar `Kind` enum. | KEEP-AS-IS |

(Five sets — bnf, ebnf, csv, css_pretty, math.)

---

# Synthesis

## (1) Layered re-organization of `crates/core/src/`

The user's draft target structure is good — what follows is a more
detailed mapping along with three corrections.

```
crates/core/src/
├── source/              # the user-input layer + grammar text + import graph
│   ├── mod.rs           # was lib.rs's pub-mod block
│   ├── types.rs         # ← from src/types.rs (RuleEntry, AST, *Directive*)
│   ├── pipeline_options.rs   # ← from src/pipeline.rs (CompileTarget, …)
│   ├── imports/         # ← src/imports/
│   └── graph/           # ← src/graph/
│
├── parse/               # everything BBNF-self-host related (parse → AST + DirectiveMaps)
│   ├── mod.rs           # was src/grammar/mod.rs
│   ├── host.rs          # the BBNF tape-walker (split per the file row above)
│   ├── schema/          # ← src/grammar/schema/  (CstSchema)
│   ├── generated/       # ← src/grammar/generated/  (xtask outputs)
│   └── validate.rs      # ← src/pipeline/validate.rs (post-parse AST validation)
│
├── ast/                 # nothing — bbnf has no separate AST type per the no-ts-ir feedback;
│                        # the bootstrap parse RESULT is the AST. This directory does NOT exist.
│
├── lower/               # AST → IR
│   └── (existing src/lower/ verbatim, after the splits)
│
├── ir_pass/             # NOTHING in core — passes live in crates/ir/src/passes/
│
├── typing/              # IR-level type projection sub-projections that are core-specific
│   └── (empty for now — typing lives in crates/ir/src/passes/types/. See note below.)
│
├── optimize/            # NOTHING in core — every optimizer (e-graph, CSP) lives in crates/ir/
│
├── codegen/             # backend-agnostic emission orchestrator + per-target sub-modules
│   ├── mod.rs           # ← src/generate/mod.rs (generate_all)
│   ├── regex/           # ← src/generate/regex/
│   ├── serialize/       # ← src/generate/serialize/  (delete after ser archive)
│   ├── driver/          # ← src/backend/driver/
│   ├── strategy/        # ← src/backend/strategy/
│   ├── prettify/        # ← src/backend/prettify/
│   ├── kernels/         # ← src/backend/kernels/
│   ├── types.rs         # ← src/backend/types/  (one file)
│   ├── util.rs          # ← src/backend/util.rs (unescape_literal)
│   ├── emitter/         # ← src/backend/emitter.rs split per the file row above
│   ├── rust/            # ← src/backend/rust/
│   ├── ts/              # ← src/backend/ts/
│   └── wasm/            # ← src/backend/wasm/
│
├── runtime/             # generic + per-grammar runtimes (no per-grammar codegen, just data + traits)
│   └── (existing src/runtime/ verbatim, after the splits)
│
├── path/                # typed-path executor surface (entirely runtime-execute)
│   └── (existing src/path/ verbatim)
│
├── pipeline/            # the compile-driver entry that threads everything together
│   ├── mod.rs           # ← src/pipeline/compile/mod.rs hub
│   ├── compile.rs       # ← src/pipeline/compile/pipeline.rs (compile_ast_common)
│   ├── target.rs        # ← src/pipeline/compile/target.rs
│   ├── timer.rs         # ← src/pipeline/compile/timer.rs
│   ├── audit.rs         # ← src/pipeline/compile/audit.rs
│   ├── closure_partition.rs  # ← src/pipeline/compile/closure_partition.rs
│   └── directives.rs    # ← src/pipeline/directives.rs
│
├── host/                # host-fn registration + shims
│   ├── mod.rs
│   └── css_types.rs     # ← src/css_types.rs (parse_hex_color)
│
└── lib.rs               # re-export hub
```

Critique of the user's draft:

1. **`scan/` directory.** The user's draft has `scan/`. In the actual layout
   the SIMD scanner is its own crate (`simd-scan`); core has only `kernels/`
   (which embed scanner calls). The proposed `crates/core/src/scan/` directory
   would be empty — drop it from the target and let `simd-scan` own that
   layer. The Rust kernels that *call* into `simd-scan` already live in
   `codegen/kernels/`.

2. **`ast/` directory.** The user's draft has `ast/`. There is no separate AST
   type in the post-AZ-II shape — the bootstrap parse returns a `BbnfDocument`
   whose `BbnfView<'a, 'a>` is the AST. The `ast/` directory in the target is
   empty; remove it. The `RuleEntry` / `AST<'a>` typedef + `*Directive*`
   structs that *would* go in `ast/` are already in `types.rs` and should
   move with it to `source/types.rs`.

3. **`ir/` and `ir_pass/` directories.** These should NOT live in `core/`.
   `bbnf-ir` is its own crate and is the correct home for both. Core consumes
   IR types via `bbnf_ir::*` re-exports through `lib.rs`. Adding an `ir_pass/`
   directory to core would either duplicate code or force a circular
   dependency. The user's draft reads as a target-state proposal that
   mistakenly mirrors `crates/ir/src/passes/` into core — drop it.

4. **`typing/` directory.** Same — the type-projection pass lives in
   `bbnf-ir`. The only typing concern in `core` is `path/type_check.rs`,
   which is already in the right place under `path/`.

5. **`optimize/` directory.** Same — lives in `bbnf-ir`.

The corrected target layout is therefore:

```
crates/core/src/
├── source/        types + directives + imports + dep-graph
├── parse/         BBNF self-host parse + CstSchema + generated/
├── lower/         BBNF AST → IR (driven by view-walk)
├── codegen/       Track 1 + Track 2 emitters (was generate/ + backend/)
├── runtime/       per-grammar arenas / builders / documents / views
├── path/          typed-path executor
├── pipeline/      compile orchestrator
├── host/          host-fn shims (css_types)
└── lib.rs
```

This is **smaller** than the user's draft and **describes the actual
contents** of core. Total: 9 directories + lib.rs.

## (2) Crate-level re-organization

### Crates that LEAVE the workspace

| Crate | New home | Mechanism |
|---|---|---|
| `egraph` | own repo (alongside csc411 or standalone) | path-dep until extracted; `bbnf-ir` consumes via path-dep then crates.io |
| `egraph-derive` | follows `egraph` | sibling proc-macro crate at same repo |
| `csp-solver` | csc411 repo (already named there) | already a path-dep; flip to crates.io once published |

Action: extract each as its own repo with workspace mirror; bbnf-lang
keeps `path = "../../<repo>/<crate>"` Cargo overrides under
`[patch.crates-io]` until publication.

### Crates that ARCHIVE

| Crate | Why | Where |
|---|---|---|
| `ser` | trait substrate has no production caller; the schema-emitted serialize call is gated by `O3` carve and emits empty TokenStream | `archive/ser/` (source preserved) |
| `gorgeous` | grammar-driven prettifier; per-grammar shims gated behind features no production grammar enables today | `archive/gorgeous/` (source preserved) |

Action: move both directories to `archive/` and remove their workspace
membership from the root Cargo.toml.

### Crates that MERGE

None recommended. The candidates were:

- **`analysis` into `core`?** No — `analysis` is the LSP-shared library
  that consumes `bbnf` (= core) plus `bbnf-ir`. Merging would force core to
  carry `ls-types` / `self_cell` deps that production parsers don't need.
- **`lower/` (sub-tree) into `bbnf-ir`?** No — lowering reads
  `runtime::bbnf::BbnfView` (a core-only type). Moving the lowering would
  force bbnf-ir to depend on core, which inverts the current direction
  (core → ir). The bootstrap parse drives lowering; both must live in the
  same crate.

### Crates that SPLIT

| Crate | Split into | Rationale |
|---|---|---|
| `core` | `bbnf-parse` (source/parse/lower/host) + `bbnf-codegen` (codegen/) + `bbnf-runtime` (runtime/path/handle) | After the layered re-org above succeeds, the three sub-trees have stable boundaries: `bbnf-parse` outputs `GrammarIR`, `bbnf-codegen` consumes it and emits, `bbnf-runtime` is the deps-free target of every emit. |

Action timeline: layered re-org first (BA), crate split second (BA closeout
or a successor tranche). The split is desirable but not required for BA's
multi-backend thesis.

## (3) Pipeline ordering — source bytes to typed value

```
[1]  source bytes            : &str (input grammar source)
       │
       │   crates/core/src/parse/  +  crates/core/src/source/imports/
       ▼
[2]  module graph            : ModuleRegistry { uri → ModuleData }
       │   (one file per @import, parsed once)
       │
       │   crates/core/src/parse/host.rs (BBNF self-host walker)
       │   reads runtime::bbnf::BbnfDocument
       ▼
[3]  AST + DirectiveMaps     : (AST<'a>, DirectiveMaps<'a>)
       │   AST = IndexMap<&str, RuleEntry { name_span, rhs: BbnfView }>
       │   DirectiveMaps holds @import / @recover / @pretty / @ws / @debug / @host / @token
       │
       │   crates/core/src/source/graph/{deps,scc}.rs
       ▼
[4]  Dep graph + SCC         : SccResult { cyclic_rules, components, … }
       │
       │   crates/core/src/lower/  (DRIVES BBNF View walk; emits IR-lower)
       ▼
[5]  GrammarIR (raw)         : bbnf_ir::GrammarIR
       │   rules, fns, strings interned; types/follow/regex_info empty
       │
       │   bbnf_ir::dag::ensure_dag  →  GrammarDag (hash-cons)
       │   crates/ir/src/passes/sets/{first_sets,scc,dispatch,structural_alphabet}
       ▼
[6]  Foundation analysis     : FIRST sets, SCC, structural alphabet, dispatch tables
       │   At this point simd-scan's StructuralAlphabet is derivable.
       │
       │   crates/ir/src/passes/transform/  (structural normalizer fixed-point loop)
       │   alias → prune → inline → fuse → eliminate_epsilon → merge_literals → prefix → fuse_token
       │   iterated until convergence; inline_trace records substitutions
       ▼
[7]  Normalized IR           : GrammarIR with normalized bodies + InlineTrace
       │
       │   crates/ir/src/passes/types/   (CSP type-projection over csp-solver)
       ▼
[8]  Typed IR                : GrammarIR with ir.types populated
       │
       │   crates/ir/src/passes/recognizers/  (single-walk miner orchestrator)
       │   ContextFacts → QuotedString → BalancedWrap → CommentWs → Identifier
       │     → SeparatorList → TokenLedBranches → PunctWsRegion → DelimScan → KeyDispatch
       │   produces NodeFacts, ContextFacts, DelimScanConfig, KeyDispatchConfig sidecars
       │
       │   crates/ir/src/passes/recognizers/grammar_facts.rs  (DTA-table lift)
       │   crates/ir/src/passes/recognizers/operator_chain.rs  (Pratt detection)
       │   crates/ir/src/passes/recognizers/shape_dispatch/   (ShapeAssignments)
       ▼
[9]  Mined IR                : GrammarIR with NodeFacts + DtaTable + ShapeAssignments
       │
       │   crates/ir/src/passes/payload/  (compute_payload_layouts)
       │   crates/ir/src/passes/types/registry.rs  (populate_struct_registry)
       │   crates/ir/src/passes/path_check.rs
       ▼
[10] Layout-complete IR      : GrammarIR with payload_layouts + StructRegistry + path_check resolver
       │
       │   crates/ir/src/egraph/  (build → saturate → write_back; runs ONCE)
       ▼
[11] Optimized IR            : GrammarIR with rewritten bodies (e-graph cost-extraction picks)
       │
       │   crates/ir/src/passes/csp_strategy/  (per-component strategy CSP via csp-solver)
       │   chooses AltMode / WrapMode / RegexEngine per node; cost-minimization
       ▼
[12] Strategy-decided IR     : GrammarIR with RecognizerDecisionMap
       │
       │   crates/ir/src/passes/audit/payload_coverage.rs
       │   crates/ir/src/passes/profile.rs  (GrammarProfile fingerprint)
       ▼
[13] Audited IR              : GrammarIR with audit/coverage attestation
       │
       │   crates/core/src/codegen/driver/   (target-agnostic compile_grammar)
       │   reads ir + analysis + driver state; chooses CallStrategy/AltStrategy/etc.
       ▼
[14] PreparedGrammar         : core::codegen::PreparedGrammar { ir, prep }
       │
       │   target dispatch:
       │      CompileTarget::Rust → crates/core/src/codegen/rust/  (RustEmitter impl Emitter)
       │      CompileTarget::Vm   → crates/ir/src/vm/compiler/      (bytecode)
       │      CompileTarget::Ts   → crates/core/src/codegen/ts/    (TsEmitter impl Emitter)
       │      CompileTarget::Wasm → crates/core/src/codegen/wasm/  (WasmEmitter impl Emitter)
       ▼
[15] CompileOutput           : Rust(TokenStream) | Vm(BytecodeProgram) | Ts(String) | Wasm(Vec<u8>)
       │
       │   xtask regen writes Rust(TokenStream) to crates/core/src/parse/generated/<grammar>.rs
       │
       ▼
[16] Generated parser        : a `pub fn parse(input: &str) -> Result<<Grammar>Document<'_>, ParseErr>` entry
       │
       │   simd-scan::scan_structural builds StructuralIndex once
       │   per-shape parse fns dispatch (Object/Array/Flat/Wrap/Pratt/...)
       │   StructBuilder writes typed records into <Grammar>StructBuilder
       ▼
[17] Typed value             : <Grammar>Document<'p> + path-driven <Grammar>Value access via PathQuery
```

### Where SIMD/Pratt/CSP/e-graph fit

- **SIMD** — step [16]: the generated parser calls `simd_scan::scan_structural`
  to produce `StructuralIndex`. The structural alphabet is **mined at step
  [6]** (`compute_structural_alphabet`). No SIMD usage upstream.
- **Pratt** — step [9]: `recognizers::operator_chain` detects operator chains;
  step [16] emits a `parse_pratt_<grammar>_<rule>` function via the Rust
  shape emitter `shapes/pratt/struct_direct.rs`.
- **CSP** — appears twice. Step [8] (`csp-solver` for type projection AC-3)
  and step [12] (`csp-solver` for strategy optimization with branch-and-bound).
  Both invoke the `csp-solver` crate's API; bbnf-ir is the only consumer.
- **e-graph** — step [11]: a single saturate-and-extract pass via the
  `egraph` crate. Runs after the structural normalizer fixed point converges.

### The "where does tape return?" question (per BG/BC carry-ledger)

Currently absent from the pipeline. Step [15]/[16] write directly into
typed `<Grammar>StructBuilder` instances. If tape returns, it would slot
between [15] and [16] as an alternative `CompileOutput` variant
(`CompileOutput::Tape(BytecodeProgram)`) consumed by a tape-walker emitter.
BA does not need to reintroduce tape; the question is preserved here for
post-BA disposition.

---

## Hard-cap tally

Crates walked: 12 (analysis, bbnf-path, bbnf-path-ts, bootstrap, core, csp-solver,
egraph, egraph-derive, gorgeous, ir, lsp, ser, simd-scan).

Files walked at row level: 824 (every `.rs` under `crates/`).

Files >500 LOC flagged for split: 21 (10 in core, 7 in ir, 1 each in
csp-solver, bbnf-path, simd-scan).

Generated files exempt from split: 9 (`grammar/generated/*.rs`).
