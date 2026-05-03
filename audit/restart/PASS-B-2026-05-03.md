# Pass B Synthesis — Codegen + Runtime + Optimisers

Date: 2026-05-03
Scope: codegen substrate + runtime substrate + pipeline + optimiser
sister crates + xtask + generated grammar tree.
Source: `docs/restart/PASS-B-CODEGEN-MID.md` + the six per-agent reports
under `audit/restart/per-agent/pass-b-agent-{1..6}-*.md`.

This synthesis collates the inventory, idiomaticity, lock-adherence,
architectural-transposition, replacement-design, and cross-cut findings
into the Pass-B verdict ledger and forwards the carried items to the
synthesizer-orchestrator.

---

## §1 — Verdict ledger (per-file disposition)

The codebase is corpus; every file is classified into one of:

- **KEEP-OUTRIGHT** — first-principles correct; no action
- **KEEP-MODIFY** — survives but requires surgery
- **ABROGATE-DELETE** — rip out wholesale; no successor
- **ABROGATE-MOVE** — relocate; symbol survives elsewhere
- **ABROGATE-REPLACE** — successor exists or must be designed

### §1.a — `crates/core/src/backend/` codegen substrate (119 files)

| File | Verdict | Rationale |
|---|---|---|
| `backend/mod.rs` | ABROGATE-MOVE | folds into `crates/bbnf-codegen/src/lib.rs` |
| `backend/emitter.rs` (566 LOC) | ABROGATE-REPLACE | reshape per Agent B.5 §3; 30-method trait → 8-10 method trait; per-shape methods consume typed IR |
| `backend/util.rs` | KEEP-MODIFY | move to `bbnf-codegen` |
| `backend/driver/{mod, alt, seq, repeat, wrap, reference, map, node, analysis, prettify}.rs` (10 files, ~2400 LOC) | KEEP-MODIFY | move to `bbnf-codegen/src/driver/`; scrub tape narrative |
| `backend/kernels/{mod, charclass, charset_shapes, identifier, number, balanced_wrap, prefix_class, punct_ws_region}.rs` (8 files) | KEEP-MODIFY | move to `bbnf-codegen/src/kernels/`; scrub legacy `emit_call` narratives |
| `backend/strategy/{mod, alt_strategy, seq_strategy, ref_strategy, repeat_strategy}.rs` (5 files) | KEEP-MODIFY | move to `bbnf-codegen/src/strategy/` |
| `backend/types/{mod, decisions}.rs` | KEEP-MODIFY | move to `bbnf-codegen/src/types/`; scrub "AM.3 per-branch tape surgery" narrative |
| `backend/prettify/{mod, types, plan, sep_rewrite, analysis}.rs` (5 files) | KEEP-MODIFY | move to `bbnf-codegen/src/prettify/` |
| `backend/rust/emitter/mod.rs` (379 LOC) | KEEP-MODIFY | reshape per Emitter trait collapse |
| `backend/rust/emitter/grammar.rs` (468 LOC) | KEEP-MODIFY | scrub tape narrative; move |
| `backend/rust/emitter_types.rs` (294 LOC), `ir_types.rs` (372 LOC), `ir_enums.rs` | KEEP-MODIFY | scrub tape narrative |
| `backend/rust/emitter/shapes/struct_direct.rs` (4 files: keyword, flat, wrap, pratt; total ~2553 LOC) | ABROGATE-REPLACE | fold into parent shape modules; the `struct_direct` sub-naming is orthogonal-codepath fault per Lock 1 |
| `backend/rust/emitter/shapes/{flat,dispatcher/support,wrap/struct_direct,keyword/struct_direct}.rs` (>500 LOC; 4 files) | ABROGATE-REPLACE | god modules per CENSUS §5; split + fold into per-shape methods |
| `backend/rust/emitter/shapes/array/mod.rs` (514 LOC) | KEEP-MODIFY | scrub "legacy record stream fallback" :35; split god module |
| `backend/rust/emitter/shapes/dispatcher/cross_shape.rs` (338 LOC) | KEEP-MODIFY | scrub "legacy Alt-dispatch body" :118 |
| `backend/rust/emitter/shapes/{scalar, string, number, object, arglist, unordered, hregex, cursor_param, substrate, alt_dispatch/branches, alt_dispatch/mod, dispatcher/{ref_call, mod, symbol_composition}, inline/structural_branch, keyword/payload, keyword/mod, pratt/mod, wrap/mod, flat/mod}.rs` | KEEP-MODIFY | move under reshaped Emitter trait per Agent B.5 §3 |
| `backend/rust/emitter/shapes/substrate.rs` (119 LOC) | ABROGATE-DELETE | substrate-selection vestigial decision; struct-direct is the only path per Lock 1 |
| `backend/rust/emitter/{keyword_dispatch, path_plan, precedence, profile, regex_scan_adapter, registry_emit, prettify/*}.rs` | KEEP-MODIFY | regex_scan_adapter (786 LOC) split god module; profile scrub tape |
| `backend/rust/analysis/inline/{mod, visit, constraints}.rs` | KEEP-MODIFY | mod.rs:37 inline test → tests/ per CENSUS §7 |
| `backend/rust/view/named_types.rs` | KEEP-MODIFY | move with bbnf-codegen |
| `backend/ts/*` (12 files, ~1719 LOC) | KEEP-MODIFY | reshape per Emitter trait collapse; scrub `declare function shim` :113 |
| `backend/wasm/*` (12 files, ~1408 LOC) | KEEP-MODIFY | reshape per Emitter trait collapse |

**Codegen aggregate**: 119 files; 1 ABROGATE-DELETE (substrate.rs);
~30 KEEP-MODIFY with tape/legacy narrative scrubs; 5 ABROGATE-REPLACE
(struct_direct sub-modules + over-large shape files); the rest move
to `bbnf-codegen` per Agent B.4 §Q1.

### §1.b — `crates/core/src/runtime/` runtime substrate (75 files)

| File | Verdict | Rationale |
|---|---|---|
| `runtime/mod.rs` (78 LOC) | ABROGATE-REPLACE | post-split, the aggregator becomes `bbnf-runtime/src/lib.rs` (mechanism only) + per-grammar declaration crate aggregators |
| `runtime/builder.rs` (141 LOC) | KEEP-MODIFY | trait surface survives; scrub "selection between tape and struct" narrative; move to `bbnf-runtime` |
| `runtime/builder_template.rs` (286 LOC) | ABROGATE-REPLACE | subsumed by `bbnf-runtime-template` per Agent B.5 §1 |
| `runtime/arena_template.rs` (134 LOC) | ABROGATE-REPLACE | subsumed by `bbnf-runtime-template` |
| `runtime/handle.rs`, `runtime/view.rs`, `runtime/error.rs` | KEEP-MODIFY | move to `bbnf-runtime` |
| `runtime/path.rs` (163 LOC) | ABROGATE-DELETE | duplicate of `crates/core/src/path/ir.rs::PathSegment` per CENSUS §4.1; consolidate per Lock 7 |
| `runtime/bbnf/{arena, builder, document, mod, parse_with, serialize, value, view}.rs` (8 files, ~2026 LOC) | ABROGATE-REPLACE | per Agent B.5 §1 + §9; template-emit canonical surface in `crates/bbnf/src/runtime/`; specialised content (bounds-recording, serialize, host-ext) lives in `crates/bbnf/src/specialised/` |
| `runtime/json/{arena, builder, document, mod, parse_with, value, view}.rs` (7 files, ~1427 LOC) | ABROGATE-REPLACE | template-emit at `crates/json/src/runtime/`; OpenFrame retires per direct-projection |
| `runtime/css_l4/{arena, builder, document, mod, parse_with, value, view}.rs` (7 files, ~3126 LOC; 14-variant OpenFrame) | ABROGATE-REPLACE | template-emit canonical surface at `crates/css-l4/src/runtime/`; 14-variant typed-value content lives in `crates/css-l4/src/specialised/` (split god modules per CENSUS §5) |
| `runtime/google_sheets/{arena, builder, document/{canonical, mod, path_query, view}, mod, parse_with, value, view}.rs` (10 files, ~1953 LOC) | ABROGATE-REPLACE | template-emit at `crates/sheets/src/runtime/`; canonical content at `crates/sheets/src/specialised/` |
| `runtime/{bnf, csv, ebnf, css_pretty, math}/*` (5 grammars × 7 files = 35 files, ~2400 LOC) | ABROGATE-REPLACE | trivial cohort fully template-emitted; per-grammar declaration crates carry the marker struct + maybe-empty `specialised/` module |
| `runtime/{<g>}/parse_with.rs` (4 files; bbnf, json, css_l4, google_sheets; ~480 LOC) | ABROGATE-DELETE | typed-path → legacy-path lowering retires when `runtime/path.rs` retires (CENSUS §4.1) |

**Runtime aggregate**: 75 files; 1 ABROGATE-DELETE (path.rs); 4
ABROGATE-DELETE (parse_with legacy lowering); 65 ABROGATE-REPLACE (the
9 per-grammar dirs template-emit) — Lock 14's strict reading.

### §1.c — `crates/core/src/pipeline/` + `pipeline.rs` (9 files)

| File | Verdict | Rationale |
|---|---|---|
| `pipeline.rs` (103 LOC) | ABROGATE-REPLACE | collapse into `pipeline/mod.rs` per `feedback_directory_modules` |
| `pipeline/{directives, validate}.rs` | KEEP-MODIFY | move with pipeline subtree |
| `pipeline/compile/{mod, audit, closure_partition, target, timer}.rs` | KEEP-MODIFY | move; scrub "Tape-direct ingress" narrative |
| `pipeline/compile/pipeline.rs` (481 LOC) | KEEP-MODIFY | scrub :163 narrative |

**Pipeline aggregate**: 9 files; 1 ABROGATE-REPLACE (file-form
collapse); rest KEEP-MODIFY.

### §1.d — `crates/core/src/grammar/generated/` (10 files; 168,785 LOC)

| File | Verdict | Rationale |
|---|---|---|
| `generated/mod.rs` (35 LOC) | KEEP-MODIFY | drop `pub use bbnf::*` aggregator asymmetry per CENSUS §3.1; namespace BBNF uniformly |
| `generated/<g>.rs` (9 files, 168,750 LOC) | ABROGATE-MOVE | relocate per Agent B.4 §Q6 to per-grammar declaration crate `crates/<grammar>/src/generated.rs` |

The 168,750 LOC distributes across 9 declaration crates: bbnf 21,503;
bnf 3,290; csv 1,693; css_l4 107,138; css_pretty 9,021; ebnf 7,646;
google_sheets 14,088; json 3,500; math 871. Each per-grammar crate's
generated parser becomes its sole (or primary) source artefact.

### §1.e — Optimiser sister crates

| Crate | Verdict | Rationale |
|---|---|---|
| `crates/egraph/` (16 files, ~2400 LOC) | KEEP-MODIFY | API freeze + crates.io publish per Lock 11 / Agent B.4 §Q3 |
| `crates/egraph-derive/` (1 file, 343 LOC) | KEEP-MODIFY | publish alongside egraph per Lock 11 |
| `crates/csp-solver/` (~42 files, ~6500 LOC src) | KEEP-MODIFY | API freeze + crates.io publish per Lock 11; per `feedback_csp-always-optimize` foundational library |
| `crates/simd-scan/` (10 source files, ~3400 LOC) | KEEP-OUTRIGHT | Lock 11 keeps workspace-internal explicitly |

**Optimiser aggregate**: 4 crates; 3 KEEP-MODIFY (publish-prep); 1
KEEP-OUTRIGHT (workspace-internal per Lock 11).

### §1.f — xtask

| File | Verdict | Rationale |
|---|---|---|
| `xtask/Cargo.toml` | KEEP-MODIFY | add path-dep on `bbnf-codegen`, `bbnf-runtime-template` post-split |
| `xtask/src/main.rs` (67 LOC) | KEEP-MODIFY | clap CLI; add `runtime-template` subcommand or fold into `regen` |
| `xtask/src/lib.rs` (11 LOC) | KEEP-MODIFY | re-export reshaped per Agent B.4 §Q5 |
| `xtask/src/regen.rs` (849 LOC) | ABROGATE-REPLACE | god module split per CENSUS §5: regen/{manifest, pipeline, emit, check, staged, mod}.rs |
| `xtask/tests/metadata_fail_closed.rs` | KEEP-OUTRIGHT | manifest validation gate |

**xtask aggregate**: 5 files; 1 ABROGATE-REPLACE (regen.rs split); 3
KEEP-MODIFY; 1 KEEP-OUTRIGHT.

### §1.g — Pass-B aggregate verdict

| Bucket | Count |
|---|---:|
| KEEP-OUTRIGHT | 11 |
| KEEP-MODIFY | ~120 |
| ABROGATE-DELETE | ~6 |
| ABROGATE-MOVE | ~9 (generated tree) |
| ABROGATE-REPLACE | ~140 (runtime per-grammar template + emit reshape + struct_direct + builder_template + arena_template + path duplicate + pipeline.rs + xtask/regen.rs + Emitter trait + substrate.rs) |

The largest bucket is ABROGATE-REPLACE: 140 of ~290 Pass-B files.
This is Lock-14 demand mass — per-grammar runtime emit replaces
hand-written per-grammar runtime files.

---

## §2 — Architectural transpositions ratified

The synthesis ratifies the following macro-pivots from Agent B.4:

### §2.a — Crate split: `crates/core/` → `bbnf-codegen` + `bbnf-runtime` + `crates/<grammar>/` × 9

`crates/core/` shrinks dramatically. Per-grammar declaration crates
emerge — one per grammar — carrying:
- the per-grammar generated parse fn (`src/generated.rs`)
- the per-grammar template-emitted runtime (`src/runtime/{value, document, view, kind, arena, builder, mod}.rs`)
- the per-grammar host fns (`src/host.rs`)
- the per-grammar specialised content (`src/specialised/` for the 4
  specialised cohorts; absent or empty for trivial cohort)
- the per-grammar prettify (CENSUS §2.5)
- the per-grammar tests (`tests/`)
- the per-grammar benches (`benches/`)

The generic substrate (`bbnf-codegen`, `bbnf-runtime`, `bbnf-ir`,
`crates/path/`) carries ZERO grammar-named modules. Lock 14 honoured
by construction.

`crates/core/` survives as the integration substrate (pipeline,
lower, imports, graph, types, grammar source-side). The `bbnf` library
crate remains as the user-facing aggregator.

### §2.b — `bbnf-runtime-template` for per-grammar runtime emission

Per Agent B.5 §1: xtask carries a runtime-template sub-module that
consumes (grammar source + workspace metadata + per-grammar registry)
and emits typed Rust per-grammar runtime modules. The trivial cohort
emits 100% from template; the specialised cohort emits canonical
surface from template + extension-via-host-fn from declaration crate.

The template's emission uses **direct-projection** — no OpenFrame
heap-stack; no `StructBuilder` trait surface ceremony; the call stack
holds depth; `SmallVec` holds element collections.

### §2.c — Reshape `Emitter` trait to one walking pattern

Per Agent B.4 §Q7 + Agent B.5 §3: reduce `Emitter` from 30 methods to
8-10. Rust adopts per-shape walk via per-shape `emit_*` methods. TS +
WASM follow. The `backend/rust/emitter/shapes/` sub-tree dissolves
into the trait's per-shape methods.

The struct_direct sub-module (4 files, ~2500 LOC) retires; the
substrate.rs decision file retires; the legacy / fallback / shim
narratives retire.

### §2.d — Path consolidation per Lock 7

Per Agent B.5 §11: `crates/path/` consolidates `crates/core/src/path/`
+ `crates/core/src/runtime/path.rs`. `crates/path-core/` factors
shared lex/lower/validate logic from `bbnf-path` proc-macro + `bbnf-path-ts`
cdylib. Both proc-macro shells path-dep on `path-core`.

The legacy `PathSegment<'a>` definition retires; `parse_with` legacy
lowering retires; one path alphabet across substrate.

### §2.e — Promote optimiser crates per Lock 11

Per Agent B.4 §Q3: egraph + egraph-derive + csp-solver promote to
crates.io once API freeze passes. simd-scan stays workspace-internal
per Lock 11 explicit.

### §2.f — Generated tree relocation

Per Agent B.4 §Q6: relocate `crates/core/src/grammar/generated/` to
per-grammar declaration crates `crates/<grammar>/src/generated.rs`.
The 168,750 LOC distributes across 9 declaration crates.

### §2.g — Pipeline directory module

Per CENSUS §4.3 + Agent B.5 §10: collapse `pipeline.rs` into `pipeline/mod.rs`.

### §2.h — xtask god module split

Per Agent B.4 §Q5 + CENSUS §5: split `xtask/src/regen.rs` (849 LOC)
into `xtask/src/regen/{manifest, pipeline, emit, check, staged, mod}.rs`.

---

## §3 — New facilities (carried to synthesizer)

The brand-new items the Pass-B substrate gains:

| Facility | Location | Purpose |
|---|---|---|
| `bbnf-runtime-template` | `xtask/src/runtime_template/` (or separate crate) | per-grammar runtime emission per Lock 14 |
| `crates/bbnf-codegen/` | new crate | extracted from `crates/core/src/backend/` |
| `crates/bbnf-runtime/` | new crate | extracted from `crates/core/src/runtime/` mechanism files |
| `crates/<grammar>/` × 9 | new crates | per-grammar declaration crates |
| `crates/path/` | new crate | consolidated path crate per Lock 7 |
| `crates/path-core/` | new crate | shared lex/lower/validate for proc-macro shells |
| `crates/bbnf-bench/` | new crate | vitest-style bench harness per `feedback_vitest-bench` |
| Reshaped `Emitter` trait | `bbnf-codegen/src/emitter.rs` | 8-10 methods, single walking pattern |

The synthesis catalogues NO additional facilities beyond these. The
brand-new candidates from Agent B.5 §6 (cost-model crate), §7 (pratt
crate), §8 (simd-detect crate) all default to KEEP-IN-EGRAPH /
KEEP-IN-CODEGEN per `feedback_kiss-perf-bias`.

---

## §4 — Cross-cuts (carried to synthesizer)

Per Agent B.6:

### §4.a — Codegen-runtime contract under crate split

Post-split contract:
- `bbnf-codegen` emits source code referencing trait methods
  (`StructBuilder`, `PathQuery`) and per-grammar declaration crate types
- `bbnf-runtime` provides the trait surfaces; per-grammar crates
  provide the trait impls + typed types
- The `[workspace.metadata.bbnf-strategy]` table encodes per-grammar
  declaration crate paths

### §4.b — Tape doc-residue scrub

~50 sites across Pass-B substrate carry tape narrative; the synthesis
demands a regen-clean scrub. The negative-assertion regression gate
at `crates/core/tests/struct_direct_snapshots.rs:45-53` extends to
add OpenFrame-departure assertion.

### §4.c — OpenFrame migration completeness gate

Phase-4 BA option-(a) had not landed across all 9 grammars at HEAD.
The synthesis demands: post-restart, OpenFrame appears ONLY in
`archive/`. Direct-projection emit from the runtime-template ensures
this.

### §4.d — Generated-output invariants

11 implicit invariants the codegen silently relies on (Agent B.6 §6.1).
Post-restart, an explicit invariant audit lands at `crates/<grammar>/tests/runtime_invariants.rs`
testing the per-grammar trait conformance + structural alphabet
emission + path cursor presence + marker struct + parse entry +
host-fn resolution.

---

## §5 — Pass-residues (forwarded to synthesizer)

Items the Pass-B audit identified that survive into the master plan
but require sister-pass coordination:

### §5.a — Pass A coordination

- Lock 14 violations in `crates/ir/`: `registry/strategy.rs:130-185`
  hardcoded grammar idents; `passes/audit/payload_coverage.rs:69-90`
  `GrammarAuditTag::{Json,CssL4,Sheets,Bbnf}` named variants; `passes/recognizers/shape_dict_bbnf.rs`
  per-grammar mining file. Pass-A scope; Pass-B requires the resolution.
- Layout lowering pass naming convergence per Lock 2 — Pass-A scope.
- Typed IR variant table per Phase-4 BC.W0 — Pass-A scope.

### §5.b — Pass C coordination

- ser + gorgeous archive per Lock 12 — Pass-C scope; precondition for
  per-grammar declaration crate gorgeous emit.
- gorgeous per-grammar files (CENSUS §2.5) retire alongside per-grammar
  declaration crates.
- analysis crate (LSP-facing) — Pass-C scope; per CENSUS §2.3 the
  analysis crate is BBNF-grammar-specific by construction (it implements
  LSP for BBNF source files); the Pass-C audit decides crate-membership.
- bbnf-path + bbnf-path-ts retirement-into-`crates/path/` — Pass-C
  scope per `feedback_wasm-subcrate-pattern`.

---

## §6 — Lock + precept verdicts (Pass-B summary)

### §6.a — Locks

| Lock | Pass-B verdict | Synthesis demand |
|---|---|---|
| 1 — tape dead | honoured at production-symbol level; ~50 doc residue sites; OpenFrame is the substantive question | scrub residue; retire OpenFrame via direct-projection emit |
| 2 — Layout lowering | naming honoured downstream | none Pass-B; runtime literal reconstruction retires via direct-projection |
| 3 — cursor-parse + byte-skip unified | honoured structurally; cursor consult on eager wasted | constant-fold cursor on EMPTY_PATH binding |
| 4 — per-domain orthogonal | honoured | none |
| 5 — IR + per-backend lower | partial | reshape Emitter trait per Agent B.4 §Q7 |
| 6 — xtask emits committed | honoured | none |
| 7 — `crates/path/` consolidated | violated (4 locations) | consolidate per Agent B.5 §11 |
| 8 — surpass sonic-rs etc. | substrate-side: OpenFrame + checkpoint clone are the blockers | direct-projection emit; O(1) checkpoint |
| 9 — slice-borrow primary | partial — eager arena alloc | introduce `parse(input)` slab-free; arena via `parse_in` |
| 10 — Pratt+SIMD auto-detect | honoured | none |
| 11 — path-deps for incubating sister crates | honoured; promotion candidates ready | promote egraph + csp-solver |
| 12 — ser + gorgeous archive | out of Pass-B; Pass-C scope | Pass-C executes |
| 13 — no god directories | violated archetype (`runtime/` 17 children mixed concerns); shape sub-API divergence; 11 god modules | split per Agent B.4 §Q1 |
| 14 — full grammar generalisation | violated systematically — 9 per-grammar runtime dirs × ~7 hand-written files; bbnf-ir manifest mirror (Pass-A) | template-emit per Agent B.5 §1 |

The two most consequential Pass-B lock violations are **Lock 13** and
**Lock 14**; both retire via per-grammar declaration crates +
template-emitted runtimes.

### §6.b — Precepts

| Precept | Pass-B verdict | Severity |
|---|---|---|
| direct-to-struct (Lock 1) | OpenFrame in 6 files; ~109 mentions | **critical** |
| no-orthogonal-codepaths | 4 emit-path bifurcations + struct_direct sub-modules | **critical** |
| system-cohesion (single types) | 2 PathSegment defs, 1 manifest mirror | high |
| gestalt approach (no fallback) | 8 fallback narrative sites | medium |
| single-codegen-path | substrate.rs + 4 substrate-selection sites | high |
| no-god-modules (>500 LOC) | 11 in Pass B | high |
| no-god-directories | runtime/ archetype + shape sub-API divergence | **critical** |
| no-workarounds | ~85 marker hits (most legitimate) | medium |
| clean-instrumentation | clean | — |
| clean-regen-discipline | mechanical honoured; emitter-source TODOs leak | low |
| directory-module-structure | pipeline.rs violation | medium |
| no-inline-tests | 8 violations | medium (mechanical) |
| no-backward-compat | BBNF aggregator asymmetry | medium |
| pluggable-components | over-plug Emitter trait | medium |
| KISS perf-bias | OpenFrame + builder + checkpoint over-machine | high |
| isomorphic-API across backends | partial uniformity | medium |

---

## §7 — Punch list (execution-ready surgery)

In execution order, with target path:line + verbatim edit:

### §7.1 — Pre-conditions (require Pass-A coordination)

1. **Layout lowering pass naming converges** — `bbnf-ir/src/passes/layout/`.
   Locks 2 + 5.

2. **Typed IR 22-variant table lands** — `bbnf-ir/src/typed_ir.rs` per
   Phase-4 BC.W0. Lock 5.

3. **bbnf-ir Lock-14 redress** — `crates/ir/src/registry/strategy.rs:130-185`
   PRODUCTION_MANIFEST_TABLE consults `[workspace.metadata.bbnf-strategy]`
   at xtask time; runtime side carries no hardcoded grammar idents.
   `crates/ir/src/passes/audit/payload_coverage.rs:69-90` GrammarAuditTag
   becomes `Custom(&'static str)`-only. `crates/ir/src/passes/recognizers/shape_dict_bbnf.rs`
   generalises into a structural-shape miner consuming per-grammar TOML
   recogniser config. Lock 14.

### §7.2 — Pass-B execution (post-pre-conditions)

4. **Extract `crates/bbnf-codegen/` from `crates/core/src/backend/`** —
   move all 119 backend files into the new crate; preserve module
   structure but flatten the `crates/core/src/backend/` god directory
   into `crates/bbnf-codegen/src/{driver,kernels,strategy,types,prettify,rust,ts,wasm}/`.
   Lock 13.

5. **Extract `crates/bbnf-runtime/` from `crates/core/src/runtime/`** —
   move mechanism files (builder.rs, builder_template.rs, arena_template.rs,
   error.rs, handle.rs, view.rs, mod.rs) into the new crate. NO
   per-grammar dirs in `bbnf-runtime`. Lock 13 + Lock 14.

6. **Land `xtask/src/runtime_template/` per Agent B.5 §1** — proc-macro2
   + quote generator emitting per-grammar runtime modules from (grammar
   IR + registry). Lock 14.

7. **Scaffold per-grammar declaration crates `crates/<grammar>/`
   × 9** — one per grammar; each carries `src/generated.rs` (relocated
   from `crates/core/src/grammar/generated/`), `src/runtime/` (template-emitted),
   `src/host.rs` (relocated host fns; CSS L4 takes `crates/core/src/css_types.rs`),
   `tests/`, optionally `src/specialised/`, optionally `benches/`. Lock
   13 + Lock 14.

8. **Reshape `Emitter` trait per Agent B.4 §Q7** — collapse from 30
   methods to 8-10; Rust adopts per-shape walk via `emit_alt`,
   `emit_seq`, `emit_repeat`, `emit_ref`, `emit_lit`, `emit_regex`,
   `emit_map`. Retire `backend/rust/emitter/shapes/struct_direct.rs`
   sub-modules (4 files); retire `backend/rust/emitter/shapes/substrate.rs`
   (119 LOC). Lock 5 + `feedback_no-orthogonal-codepaths`.

9. **Direct-projection emit** — runtime-template emits per Agent B.5 §9
   shape; OpenFrame + StructBuilder trait machinery + JsonStructCheckpoint
   et al. retire across all 9 grammars. Lock 1 + RESTART-SKETCH §A.7
   (86.07% samply share retirement).

10. **Consolidate path machinery per Lock 7** — `crates/path/` +
    `crates/path-core/` + retirement of `crates/core/src/runtime/path.rs`
    + retirement of per-grammar `parse_with.rs` legacy lowering. Lock 7.

11. **Pipeline directory module fix** — collapse `crates/core/src/pipeline.rs`
    into `crates/core/src/pipeline/mod.rs`. `feedback_directory_modules`.

12. **xtask/src/regen.rs split** — split god module (849 LOC) into
    `regen/{manifest, pipeline, emit, check, staged, mod}.rs`. CENSUS §5.

13. **Promote egraph + egraph-derive + csp-solver per Lock 11** —
    API freeze audit + Cargo.toml metadata + crates.io publish.
    Lock 11.

14. **Doc-comment scrub** — ~50 tape-narrative sites across runtime/
    pipeline/ backend/ scrub during regen. The negative-assertion
    regression gate at `crates/core/tests/struct_direct_snapshots.rs:45-53`
    extends to assert `OpenFrame`, `<G>StructBuilder`,
    `<G>StructCheckpoint` absence in Pass-B substrate. CENSUS §1.

15. **Inline-test moves** — 8 inline `#[cfg(test)]` blocks per CENSUS §7
    move to `tests/`. Mechanical.

16. **God-module splits** — 11 Pass-B god modules split per CENSUS §5
    + Agent B.2 §6. Lock 13 + `feedback_no-god-modules`.

17. **bbnf-bench skeleton** — `crates/bbnf-bench/` + per-grammar bench
    files at `crates/<grammar>/benches/parse.rs` per `feedback_vitest-bench`.

18. **Generated-output relocation** — `crates/core/src/grammar/generated/`
    distributes across per-grammar declaration crates. Per Agent B.4 §Q6.

19. **Lock-14 verification gate** — the 3 verification commands fire
    with ZERO matches in Pass-B substrate. Mechanical.

---

## §8 — Greenfield commitments (Pass-B substrate identity)

The Pass-B substrate identity post-restart:

### §8.1 — `bbnf-codegen` + `bbnf-runtime` + 9 per-grammar declaration crates + `crates/path/` family

The codegen / runtime / path machinery decomposes into clean
single-purpose crates. `crates/core/` shrinks to integration glue
(pipeline, lower, imports, graph, types, grammar source-side).

### §8.2 — Direct-projection emit, no OpenFrame

Per RESTART-SKETCH §B.2: the parse fns hold partial state on the
call stack; SmallVec carries element collections; arena owns interned
compound IDs. No heap-stack of OpenFrame; no `StructBuilder` trait
ceremony; no `<G>StructCheckpoint` Vec-clone. Lock 1 honoured by
mechanism, not just by symbol-naming.

### §8.3 — Per-grammar runtime template-emit

Lock 14 honoured by construction. The trivial cohort emits 100% from
template; the specialised cohort emits canonical surface from template +
extension via per-grammar declaration crate's `specialised/` module.

### §8.4 — One Emitter trait, one walking pattern

Lock 5 honoured. Rust + TS + WASM share the same emit_* methods over
the typed IR. The 30-method trait collapses to 8-10. The shape-dispatcher
retires; per-shape methods replace it.

### §8.5 — Path consolidation

Lock 7 honoured. `crates/path/` + `crates/path-core/` carry the path
machinery. The bbnf-path proc-macro + bbnf-path-ts cdylib path-dep on
path-core for shared lex/lower/validate. `runtime/path.rs` retires;
per-grammar parse_with legacy lowering retires.

### §8.6 — Optimiser crates published

Lock 11 honoured. egraph + egraph-derive + csp-solver promote to
crates.io. simd-scan stays workspace-internal per Lock 11.

### §8.7 — xtask split into per-concern sub-modules

`feedback_no-god-modules` honoured. `regen.rs` (849 LOC) splits into
6 sub-modules; behaviour identical.

### §8.8 — Generated-output as per-grammar substrate

168,750 LOC distributes across 9 per-grammar declaration crates per
Agent B.4 §Q6. No central god directory; per-grammar implicit invariants
explicitly tested per Agent B.6 §6.

### §8.9 — bbnf-bench harness

`feedback_vitest-bench` honoured. Per-grammar bench files run via a
lightweight harness; SOTA-anchored gates (sonic-rs twitter, lightning-css
bootstrap) per Lock 8.

---

## Closing posture

The Pass-B substrate identity post-restart is **multiple small crates,
each with one purpose**. The 119-file `crates/core/src/backend/` god
directory dissolves into `bbnf-codegen` with cohesive sub-modules.
The 75-file `crates/core/src/runtime/` god directory dissolves into
`bbnf-runtime` (mechanism) + 9 per-grammar declaration crates (typed
content). The 168,750 LOC generated tree distributes across the
declaration crates. The OpenFrame + StructBuilder + checkpoint
machinery retires; direct-projection emit from a single template
takes its place.

Lock 1 + Lock 13 + Lock 14 — the three most-consequential Pass-B
locks — retire together. None can land without the others; all three
land with one architectural pivot: **per-grammar declaration crates
+ template-emitted runtimes + direct-projection emit + reshaped
Emitter trait**.

The synthesis-orchestrator inherits a Pass-B punch list of 19 surgical
items in execution order, each with target path:line and a verbatim
edit. The synthesis carries Pass-A pre-conditions (3 items requiring
Pass-A coordination); the rest is Pass-B-internal.

The greenfield mandate is honoured: **no quick solutions** (the
crate split is the architectural transposition the locks demand);
**no workarounds** (OpenFrame retires by direct-projection, not by
checkpoint optimisation); **idiomatic gestalt** (one Emitter trait,
one walking pattern, one PathSegment, one per-grammar substrate);
**architectural transpositions for elegance, simplicity, performance**
(the 86.07% samply share retires by mechanism, not by patching);
**no legacy code survives uncontested** (every Pass-B file's continued
existence is justified or its successor is designed).

Hereupon Pass B closes. The synthesis-orchestrator inherits.
