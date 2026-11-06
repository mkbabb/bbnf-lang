# Totality Crate/Module Topology — Census, Faults, and Target Graph

## Census: Full tree (`crates/`)

Hand-written src LOC (generated excluded where noted).

| Crate | src LOC | Generated share | Purpose | Dependents |
|---|---|---|---|---|
| core | 216 818 total / **46 862 hand-written** | 169 956 LOC of `grammar/generated/*` (css_l4 alone = 108 406) | Frontend + lower + 4 backends + runtime support + generated grammars — a kitchen sink | analysis, bbnf-path-ts, bootstrap, gorgeous, lsp |
| ir | 33 779 | 0 | E-graph IR, passes (24 420 in `passes/`), VM, registry, cost | core, analysis, bbnf-path-ts, bootstrap, gorgeous, lsp |
| csp-solver | 5 882 | 0 | Generalized CSP solver, Python-isomorphic | ir (transitively) |
| analysis | 4 975 | 0 | AST analysis (slated for elimination into IR passes) | lsp |
| simd-scan | 2 607 | 0 | Architecture-neutral SIMD structural-bitmap kernel (neon 719, avx2/avx512 present) | core |
| egraph | 1 885 | 0 | General-purpose e-graph / equality saturation | ir, core |
| lsp | 1 772 | 0 | Language server | — |
| bbnf-path-ts | 1 012 | 0 | TS/WASM twin of `path!` cdylib | — |
| bbnf-path | 918 | 0 | `path!` proc-macro, compile-time-typed path literals | core, bbnf-path-ts |
| gorgeous | 604 | 0 | Pretty-printer config wrapper over `pprint` | core (test/format) |
| ser | 530 | 0 | Grammar-guided serialize/deserialize traits | core |
| bootstrap | 465 | 0 | Self-hosted BBNF bootstrap (proc-macro retired B2) | — |
| egraph-derive | 343 | 0 | `#[derive(Language)]` proc-macro | egraph |

## Census: Skinny tree (`skinny/crates/`)

The skinny workspace wires deps through `[workspace.dependencies]` path entries; its DAG is explicit and acyclic: `bbnf → codegen → {grammar, ir, passes} → runtime`; `runtime → {bbnf-simd, parse-that-regex}`; `passes/ir → bbnf-regex`; `parse-that-regex → bbnf-simd`. It re-imports the full tree's `egraph` and `csp-solver` by `../crates` path — confirming those two are already the shared general-infra crates.

| Crate | src LOC | Purpose | Dependents |
|---|---|---|---|
| bbnf-bench | 35 933 | Bench harness + corpora-driven profiles | — |
| codegen | 10 201 | The ONE emitter path | bbnf |
| runtime | 6 522 / **1 369 hand-written** (5 153 generated in `src/grammars/`) | Tape/scan/SIMD runtime + generated grammars | bbnf, bbnf-bench |
| parse-that-regex | 3 022 | Regex runtime matcher (number/unicode/integration) | runtime |
| passes | 2 745 | IR passes | codegen |
| bbnf-simd | 2 324 / **1 387 in `aarch64/`** | The real Apple-M NEON+ASM kernel (19 aarch64 intrinsic modules) | parse-that-regex, runtime, bbnf-bench |
| grammar | 2 142 | Grammar frontend/model | codegen, ir |
| ir | 1 020 | Cost + IR types (cost.rs + lib.rs) | grammar, passes, codegen |
| test-fixtures | 404 | Corpus loaders | bbnf-simd, bbnf-bench |
| bbnf-regex | 322 | Regex IR/HIR analysis (single lib.rs) | ir, passes |
| bbnf | 111 | Thin public façade | — |
| simd-scan | **0 (stale empty dir)** | — non-member, dead | — |

## Faults

### F1 — `core` is a five-headed god crate (Lock 14 + no-god-modules)
`crates/core/src/lib.rs` mounts `grammar`, `generate`, `backend`, `graph`, `imports`, `lower`, `path`, `pipeline`, `runtime` in one crate. That fuses (a) the BBNF frontend, (b) lowering, (c) **four** emitter backends (`backend/{rust,ts,wasm,kernels}`, 22 212 LOC), (d) the regex codegen (`generate/regex`, 4 378 LOC), and (e) per-grammar runtime support. These are five separable concerns sharing a single compilation unit and a single blast radius. Every consumer (lsp, analysis, gorgeous, bootstrap) drags the entire surface.

### F2 — Named-grammar leak persists in the generic core (Lock 14, direct violation)
`crates/core/src/grammar/generated/mod.rs:25-33` hardwires ten named modules — `bbnf, bnf, css_l4, csv, ebnf, google_sheets, json, math, …` — inside the generic crate. Worse, **hand-written, grammar-specific runtime support** sits beside them: `runtime/css_l4` (2 129 LOC, including `host.rs` and a bespoke `builder.rs`/`document.rs`/`value.rs`/`color`), `runtime/google_sheets` (1 004), `runtime/css_pretty` (404), `runtime/json` (710), and per-grammar `bnf/csv/math/ebnf` dirs. The HEAD commit (36ca10646) "collapse the named-grammar leaks … CSS host relocation" only *moved* `css_l4/host.rs` (63 LOC) within core — it did not evict the grammar from the generic crate. A grammar must be addable by a `.bbnf` file + workspace metadata with ZERO code here; today CSS et al. require hand-edits inside the generic crate.

### F3 — Grammar-specific test overfit lives in the generic crate
`crates/core/tests/` carries `css_l4_parity.rs`, `css_l4_canonical_parity.rs`, `css_l4_color_view.rs`, `css_l4_dimensions.rs`, `css_l4_named_color_parity.rs`, `css_l4_comment_probe.rs`, `css_l4_substrate.rs`, `css_l4_w5_diag.rs`, `css_l4_w6_typed_retime.rs`, `json_parity.rs`, `json_canonical_parity.rs`, `json_value_parity.rs`, `google_sheets_slab.rs`, plus `lightningcss_parity.rs`. The generic engine's test suite is dominated by two-or-three grammars — both an overfit smell and proof that CSS/JSON semantics are entangled with the engine.

### F4 — Duplicated/forked crates across trees
- **simd-scan ×2 + bbnf-simd**: full `simd-scan` (2 607, generic neon/avx2/avx512), skinny `bbnf-simd` (the *real* 1 387-LOC aarch64 ASM/NEON kernel with 19 intrinsic modules), and a **stale empty `skinny/crates/simd-scan`** that is not even a workspace member. Three artifacts, two live, one dead — and the live two are forks of the same concern. The skinny `bbnf-simd/aarch64` is materially richer than full `simd-scan/neon.rs`.
- **ir ×2**: full `ir` (33 779) vs skinny `ir` (1 020). The skinny ir is a stripped cost+types core; the full ir carries the e-graph passes. These are two divergent definitions of "the IR crate."
- **regex split ×3**: `bbnf-regex` (analysis/HIR) and `parse-that-regex` (runtime matcher) in skinny, plus `core/generate/regex` (codegen) in full. The regex concern is shattered across three locations with no single owner.

### F5 — `ir/passes/` is a 24 420-LOC sub-monolith
Inside the ir crate, `passes/` holds `recognizers` (7 207), `types` (3 378), `sets` (3 211), `transform`, `csp_strategy`, `materialization`, `payload`, `audit`. This is a god-module-in-gestation one level down — the bulk of the IR's mass in a single child.

### F6 — `gorgeous` is a thin wrapper, not a crate-worth of concern
604 LOC that mostly wraps `pprint::Printer` config. It is a candidate to fold into the pretty/format module of the codegen path rather than stand alone — unless it is genuinely backend-shared.

## Correctly-separated general infra (keep)
`egraph` + `egraph-derive` (general equality-saturation, Language derive) and `csp-solver` (Python-isomorphic generalized solver) are properly their own crates and are *already* shared into skinny via `../crates` paths. `simd-scan`/`bbnf-simd` is correctly *intended* to be standalone infra — it is only the duplication that is wrong, not the separation. `bbnf-path`/`bbnf-path-ts` (proc-macro + TS twin) are correctly isolated.

## Target topology for totality

A grammar is data; the engine is grammar-blind. The target collapses both trees into one workspace whose generic crates contain ZERO grammar names, and whose generated artifacts + per-grammar support live in a *generated* crate keyed only by workspace metadata.

```
                        ┌─────────────┐   ┌──────────────┐
   general infra ──────▶│   egraph    │   │  csp-solver  │   (unchanged, shared)
                        │ egraph-deriv│   └──────┬───────┘
                        └──────┬──────┘          │
                               ▼                 ▼
   ┌──────────┐         ┌────────────────────────────┐
   │ bbnf-simd│◀────────│            ir              │  e-graph IR, types, cost,
   │ (aarch64 │         │  (passes split out below)  │  registry, decision engine
   │  ASM/NEON│         └────────────┬───────────────┘
   │  kernel) │                      │
   └────┬─────┘                      ▼
        │                ┌────────────────────┐
        │                │      ir-passes     │  recognizers / sets / transform
        │                │  (own crate, was   │  / types / csp_strategy / payload
        │                │   24k sub-monolith)│  / materialization / audit
        │                └─────────┬──────────┘
        ▼                          ▼
   ┌──────────┐           ┌────────────────┐     ┌──────────┐
   │  regex   │◀──────────│    frontend    │────▶│ bbnf-ser │ (grammar-guided
   │ (HIR +   │  (one     │ (BBNF parse +  │     └──────────┘  serialize traits)
   │ matcher +│  crate,   │   model + lower│
   │ codegen) │  un-split)│   + imports)   │
   └────┬─────┘           └───────┬────────┘
        │                         ▼
        │                 ┌────────────────────────────┐
        └────────────────▶│          codegen           │  ONE emitter path;
                          │  decision engine drives     │  Emitter trait →
                          │  specializing emitters:     │  {rust, rust+asm hybrid,
                          │  rust / rust-asm / ts / wasm│   ts, wasm}
                          └──────────────┬─────────────┘
                                         ▼
   ┌────────────────────────────┐  ┌──────────────────┐
   │          runtime           │  │  bbnf-grammars   │  GENERATED crate:
   │ tape / scan / value layer  │◀─│  (per-grammar    │  build.rs emits from
   │ generic value-API traits   │  │   generated +    │  .bbnf + workspace meta;
   └──────────────┬─────────────┘  │   support; data) │  ZERO names in generics
                  ▼                 └──────────────────┘
        ┌──────────────────┐
        │       bbnf       │  thin public façade (frontend+codegen+runtime re-export)
        └──────────────────┘
                  ▼
   ┌────────┐ ┌──────────┐ ┌──────────────┐ ┌──────────┐
   │  lsp   │ │ gorgeous?│ │ bbnf-bench   │ │bbnf-path*│
   └────────┘ └──────────┘ │test-fixtures │ └──────────┘
                           └──────────────┘
```

Dependency arrows point toward dependencies (callee). No cycles; the engine never depends on `bbnf-grammars`.

### Crate roster (target)
- **egraph / egraph-derive / csp-solver** — unchanged general infra.
- **bbnf-simd** — the single aarch64 ASM/NEON kernel (skinny's wins). `simd-scan` retires.
- **regex** — single crate folding `bbnf-regex` (HIR/analysis) + `parse-that-regex` (matcher) + `core/generate/regex` (codegen) under one owner with `hir/`, `matcher/`, `emit/` directory modules.
- **ir** — types, cost, registry, the **decision engine** (the `strategy`/`csp_strategy` cost-driven dispatch), e-graph integration. Generic only.
- **ir-passes** — the 24k passes mass extracted into its own crate (directory modules per pass family). Removes the in-crate sub-monolith.
- **frontend** — BBNF parse + model + lower + imports (extracted from `core`).
- **codegen** — the ONE emitter path; `Emitter` trait with specializing impls for rust, **rust+ASM hybrid**, ts, wasm. The decision engine (in ir) chooses; codegen emits.
- **runtime** — tape, scan adapters, and the **generic unified value layer** (the value-API traits/cursor), grammar-blind.
- **bbnf-grammars** — a GENERATED crate; `build.rs` reads workspace `[metadata.bbnf.grammars.*]` and emits each grammar's parser + per-grammar runtime support. This is the *only* place a grammar name appears. css_l4/json/sheets support moves here.
- **bbnf-ser** — kept; grammar-guided serialize/deserialize.
- **bbnf** — thin façade.
- **lsp / bbnf-path / bbnf-path-ts** — kept as today.
- **gorgeous** — fold into `codegen`'s format module unless it proves to be a backend-shared printer; as a 604-LOC `pprint` wrapper it does not earn a crate.
- **bbnf-bench / test-fixtures** — kept; bench harness and corpus loaders, grammar-data only.

## Migration map

### Skinny crates
- **bbnf-simd** → becomes the canonical SIMD crate (replaces full `simd-scan`).
- **bbnf-regex + parse-that-regex** → fold-into the unified **regex** crate.
- **codegen** → becomes the canonical **codegen** (the ONE path); absorbs full `core/backend/*`.
- **grammar** → folds-into **frontend**.
- **ir** (skinny) → folds-into **ir** (its cost/types reconcile with the full ir's authoritative version).
- **passes** → reconciles into **ir-passes**.
- **runtime** → canonical **runtime** + its `src/grammars/` generated content migrates to **bbnf-grammars**.
- **bbnf** → canonical thin **bbnf** façade.
- **bbnf-bench / test-fixtures** → kept.
- **simd-scan** (skinny, empty) → **retire** (delete dead non-member dir).

### Full crates
- **core** → **splits** into `frontend` (parse/lower/imports), `codegen` (backend/*), `regex` (generate/regex → unified regex), `runtime` (tape/scan/value), and `bbnf-grammars` (grammar/generated + runtime/{css_l4,json,google_sheets,…} support + grammar-specific tests). After the split, `core` ceases to exist as a name.
- **ir** → **splits**: keep types/cost/registry/decision-engine/egraph-glue in `ir`; extract `passes/` into `ir-passes`.
- **simd-scan** → **retires** into `bbnf-simd`.
- **analysis** → **absorbs into** `ir-passes` (per the standing "eliminate AST analysis; single source of truth in IR passes" direction); `lsp` then depends on `ir-passes`.
- **gorgeous** → **absorbs into** `codegen` format module (or justified as standalone shared printer).
- **egraph / egraph-derive / csp-solver / ser / bbnf-path / bbnf-path-ts / lsp / bootstrap** → **stay** (bootstrap may retire once self-hosting lands fully; it is already a vestige with its proc-macro retired).

## Verdict
The skinny tree already demonstrates the correct acyclic, single-responsibility DAG the totality should adopt; the full tree's `core` is a five-concern god crate that still hardwires ten grammar names and ~6 000 LOC of grammar-specific runtime support plus a CSS/JSON-saturated test suite — a live Lock 14 violation. The target evicts every grammar name into a single *generated* `bbnf-grammars` crate, splits `core` into frontend/codegen/regex/runtime, lifts `ir/passes` out of its in-crate monolith, unifies the three-way regex split and the two SIMD forks (retiring the dead skinny `simd-scan`), and folds `analysis` into IR passes — leaving generic crates that are entirely grammar-blind.
