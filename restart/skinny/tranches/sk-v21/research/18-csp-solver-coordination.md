# 18 — csp-solver coordination

Audit of the csp-solver seam between bbnf-lang and its canonical home,
`~/Programming/csc411/CSC411_HW2_ProgrammingQuestion` (workspace members `csp-solver`,
`csp-solver/wasm`, `csp-solver/morph-core`, `csp-solver/wasm-morph`). Scope: what the canonical
tranche set is doing, how far it has moved from the vendored copy, what a real CSP formulation of
kernel selection would need, and the coordination seam (re-vendor cadence, Lock 4, ownership).

Repo positions at audit time: canonical HEAD `dc689795` (2026-07-05 14:40); vendored copy tracked at
canonical `b70098676` (declared verbatim in `crates/csp-solver/Cargo.toml:1`); bbnf-lang HEAD
`05302109d`.

## 0. Headline

**The canonical solver *source* has not moved a single byte since the vendor point.**
`diff -rq crates/csp-solver/src` against the canonical `csp-solver/src` is empty — the vendored copy
is byte-identical to canonical master. Every commit on canonical since `b70098676` is either release
packaging, wasm artifact regeneration, or documentation; none touch `csp-solver/*/src`. The "active
tranche set" the NOW-STATE points at is `docs/tranches/2026-07-grand-uplift/` — a 14-wave *plan*
committed 2026-07-05 (`dc689795`), authored from a four-pass ~90-agent campaign, that has landed
**zero lines of code**. It is a plan with the code staged in a tarball, not merged work.

Consequently the "how far has canonical moved" question resolves to: **not at all, in substance —
but a large soundness fix and API-breaking changes are queued and imminent.** The coordination risk is
not drift-already-happened; it is a fault-line about to open the moment csc411 lands its W1.

## 1. What the active tranche set is doing (`2026-07-grand-uplift`)

The tranche is a product-and-hardening campaign for the CSP-solver *stack as a puzzle app*
(Sudoku/Futoshiki, PyO3 FastAPI service, wasm deploy, Vue pencil-art frontend, cross-repo release
train). Its center of gravity is the csc411 product, **not** bbnf's use of the solver. Fourteen waves
W0–W13 (`docs/tranches/2026-07-grand-uplift/README.md:30-45`); the ones that touch the kernel or the
bbnf seam:

- **W1 — kernel** (`README.md:33`): lands a composed v2 tree carrying **the P0 soundness fix**, plus
  invariance tests and a `ThreadSafe` bound. Fast-follows explicitly named: **`backjumping` deletion**,
  **`DomWdeg → Mrv`** (default variable ordering rename), and a `lib.rs` split. These are API-visible.
- **W2 — GAC/search** (`README.md:34`): GAC default-ON; builder `AcFc → Ac3`; solver excisions.
- **W12 — release train** (`README.md:44`): a single ratified cross-repo window bumping **csp-solver
  to 0.2.0**, republishing morph, and — the only two bbnf touchpoints — a **bbnf sync-gate `--verify`**
  and **one coordinated skinny edit** (see §5, OD-7).

### 1a. The P0 soundness fix — the substantive kernel change

The campaign's defining defect (`README.md:22`): `ac3_from_variable`'s `Revision::Unsatisfiable` arm
returned without recording the just-mutated constraint scope on the undo `Trail`, so prunes leaked
into sibling branches — silent false-UNSAT on 26/113 corpus boards, and silently dropped solutions
under `Pruning::Ac3` enumerate (queens8 → 45 or 5, not 92). This bug is **present, verbatim, in the
vendored copy**: `crates/csp-solver/src/solver/ac3.rs:128` reads `Revision::Unsatisfiable => return
true` with no trail push, matching the pre-fix canonical exactly. It is **also still present on
canonical master** — the fix lives only inside `artifacts/composed-csp-solver-v2.tgz` (W1's seed,
`README.md:102`), not on any committed line. The described fix is 3 lines (`README.md:20`).

Materiality to bbnf **today**: **low**. Skinny's sole consumer (`decision_csp.rs:86`) runs
`Pruning::ForwardChecking`, not `Ac3`, over a single-variable/5-constraint model, so the false-UNSAT
path is not exercised there. The full `crates/` tree, however, uses AC-3 heavily
(`crates/ir/src/passes/span.rs`, `crates/core/src/backend/rust/analysis/inline/plan.rs`,
`crates/egraph/src/csp_scheduler.rs`) — if any of those run `ac3_from_variable` inside backtracking
search with enumeration, they inherit the same silent-drop hazard. This should be verified
crate-by-crate before TW4/TW5 lean on the solver harder; the fix must ride the next re-vendor
regardless.

### 1b. Solver API surface (already in the vendored copy)

All of the COP/GAC machinery predates the vendor point, so the vendored copy already carries:
`CostFiniteDomain` (assignment-COP cost domains), `AssignmentBuilder` (bipartite assignment COPs),
`AllDifferentExcept<V>` (sentinel-aware, Régin's GAC), `ImplicationConstraint`/`CardinalityConstraint`,
`SoftConstraint` + branch-and-bound, `node_budget`, `OptimizationMode`, `LambdaConstraint`. `Ordering`
today exposes `FailFirst`/`DomWdeg`-family (the W1 `DomWdeg→Mrv` rename will change this enum). The
propagators in tree: `ac3`, `gac_alldiff`, `gac_alldiff_except`, `backtrack`, `backjump`, `nogoods`,
`local_search`, `monotonic`, `optimize`, `propagate`.

### 1c. morph-core / wasm-morph purpose

Orthogonal to bbnf entirely. `morph-core` (`csc411 5658984f`, `e161e377`, `3870769f`) ports geometry
primitives — bezier, contour, resample, **procrustes** — and a shape-signature aligner that reuses the
solver's `AssignmentBuilder` to solve the **contour-point-correspondence assignment problem** for SVG
shape morphing (B↔b mascot morph). `wasm-morph` is its wasm binding, consumed by `bbnf-buddy` (the
mascot) via a `file:` dep. W11 excises morph to its own sibling repo `mkbabb/morph`. **This is a
csc411/mascot product feature; it places no demand on the bbnf parser generator.** It is worth noting
only as evidence that the canonical solver is being generalized as an assignment/COP substrate — which
is exactly the capability a real kernel-selection formulation would draw on (§4).

## 2. Diff-in-substance: vendored vs canonical

| Surface | Vendored (`crates/csp-solver`) | Canonical `dc689795` | Substantive delta |
|---|---|---|---|
| `src/**` (solver, domains, constraints, puzzles) | b70098676 state | byte-identical | **none** |
| Release scaffolding | absent | `CHANGELOG`/`CONTRIBUTING`/`LICENSE`/`README`/CI+release workflows, published to crates.io (`aef9eae9`) | packaging only |
| wasm/wasm-morph `pkg/` | n/a (not vendored) | regenerated build artifacts | build output only |
| P0 ac3 soundness fix | **carries the bug** (`ac3.rs:128`) | **also carries the bug** (staged in tarball, unmerged) | queued, not landed |
| `docs/tranches/2026-07-grand-uplift` | n/a | 14-wave plan (`dc689795`) | plan only |

`git diff --stat b70098676 HEAD -- csp-solver/` confirms: the only tracked changes under `csp-solver/`
are the release files, the two `Cargo.toml` metadata bumps, and wasm `pkg/` regeneration. **No `.rs`
under `csp-solver/*/src` changed.** The vendor is not stale in code; it is stale only against an
imminent, not-yet-committed W1.

## 3. How bbnf actually binds the crate (a latent hazard)

The vendored crate is version `0.1.0` (`crates/csp-solver/Cargo.toml`). Consumers split into two
binding styles:

- **Path/workspace**: top `Cargo.toml:2` lists `crates/csp-solver` as a member; `skinny/Cargo.toml:37`
  binds `csp-solver = { path = "../crates/csp-solver" }`; `skinny/crates/passes/Cargo.toml:11` uses
  `csp-solver.workspace = true`.
- **Bare version**: `crates/egraph/Cargo.toml:11`, `crates/core/Cargo.toml:20`, `crates/ir/Cargo.toml:18`
  each declare `csp-solver = "0.1"`. These resolve to the in-workspace member today (cargo prefers a
  workspace member whose version satisfies the requirement).

The hazard: **W12 bumps canonical csp-solver to 0.2.0.** When the next re-vendor lands 0.2.0 into
`crates/csp-solver`, every `csp-solver = "0.1"` requirement in `core`/`egraph`/`ir` **fails to
resolve** until swept to `"0.2"`. This is a mechanical break the re-vendor must carry in the same
commit. It compounds the SolveConfig break of §5.

## 4. Implications for bbnf: the decision engine (TW4) and type plane (TW5)

### 4a. The current CSP is tautological — confirmed at source

`skinny/crates/passes/src/decision_csp.rs` is a decision *theater*, not a decision *engine*. The proof,
line by line:

1. `selected_index` is read out of `active.facts.selected_candidate_id` (`:34-42`) — the answer the
   e-graph already chose, *before* the CSP is built.
2. Costs are assigned `0.0` to that index and `1.0` to all others (`:44-47`) — the objective is
   constructed from the answer.
3. `add_selected_constraint` (`:52`, `:194-207`) adds a hard constraint pinning the variable to
   `selected_index`.
4. The five `add_candidate_constraint` predicates (`:53-81`) are true of every admitted shape by
   construction (every shape `matches!` the full shape set; every id non-empty; capacity ≤ 1).
5. `csp.solve_optimized` (`:93`) can only return `selected_index`; on empty it falls back to
   `active.facts.selected_candidate_id` anyway (`:112`); `selected_shape: active.shape` passes the
   input straight through (`:153`); the test asserts exactly this preservation (`:264`).

The solver **recovers its own input**. Its output is a 40-field `DecisionCspFacts` bag of `String`
status constants ("pass"/"evidence-only"/…) — telemetry, not computation. SK-V20 research 24 reaches
the identical verdict (`research/24-decision-engine.md:63-72`) and the SPEC disposition is **retire,
not port** (`SPEC.md:117-118`, `research/24-decision-engine.md:220`).

### 4b. What TW4 wants instead (measured KernelCostTable)

SK-V20 TW4 (`SPEC.md:115-135`) is unambiguous: one engine unifying skinny's `BackendShape` 5-shape
canon with core's `csp_strategy` into a **single extraction over `(BackendShape × KernelStrategy)`**;
the tautological CSP and the cosmetic e-graph rewrite retired; the cost model is **DATA** — a
checked-in, versioned `KernelCostTable: (KernelId, Uarch) → measured cycles/64B`, populated by a
checkasm/PMU harness, with `perf_cost` as the lead axis and every hardcoded threshold (8/16 kernel
bounds, `alt_branch ≥ 8`, `tiny_string_cap`) migrated into it.

### 4c. What a REAL CSP formulation of kernel selection would need from the solver

Research 24 lays out the honest formulation (`research/24-decision-engine.md:150-203`). Mapped onto the
solver's actual capabilities:

- **The decision is an extraction, not a satisfiability probe.** The natural object is a single
  extraction over a product language: a rule's e-class holds nodes keyed by `(BackendShape,
  KernelStrategy)`; hand-asm kernels become alternatives in one e-class behind one `Kernel` contract;
  the extractor picks the cheapest admissible cell. Total cost is `Σ frequency(leaf) ×
  table_cost(kernel(leaf), uarch)` — the iburg DP form (`:150-154`). This is fundamentally an
  **e-graph extraction with a real cost model**, which is why research 24 says keep the
  `Extractor`/`Lattice`/`CostModel` and fix only the cost upstream (`:156-165`).
- **Where the CSP earns its keep** (not tautologically): `cfg!(target_arch)` becomes an
  **admissibility filter** (a NEON node hard-pruned off aarch64), never the selector (`:176-177`);
  cross-leaf coupling constraints (shared substrate/union invariants, "same-substrate union", capacity
  budgets) are genuine constraints once candidates are no longer trivially all-true. For that, the
  formulation needs: a **cost domain over real cells** (`CostFiniteDomain` already exists and is what
  `decision_csp` misuses); **soft/optimization constraints with branch-and-bound** (present:
  `SoftConstraint`, `OptimizationMode`, `node_budget`); and **all-different / cardinality** over shared
  resources if kernel selection is jointly constrained across a rule set (present:
  `AllDifferentExcept`, `CardinalityConstraint`). The solver's API is **already sufficient** for a real
  formulation — what is missing is entirely bbnf-side: a non-zero measured `perf_cost` and a product
  e-class, not a pinned single variable. The blocker is the cost data and the modeling, **not the
  solver**.
- **The record must be a real manifest, not a String bag** (`:180-192`): the Lock-16 manifest —
  extractor-emitted, per-grammar, one row per selected leaf `(rule_id, shape, kernel_id, uarch,
  table_cost_cell, table_version, scalar_oracle, checkasm_cmd, rejected_alternatives_with_costs)`,
  byte-reproducible. This **subsumes and deletes** `DecisionCspFacts`/`ActiveCostFacts`.

**Bottom line for SK-V21:** the solver is not the bottleneck and needs nothing new from csc411 to
support real kernel selection. The bbnf work is (i) build the KernelCostTable, (ii) promote
`kernel_shape` selection into cost-table lookups, (iii) make `perf_cost` the lead cost axis feeding the
existing e-graph extractor, (iv) demote the CSP to an admissibility/coupling filter or retire it from
the *selection* path entirely. The one genuine solver dependency is soundness: if the extraction or any
coupling constraint runs AC-3 enumeration, the §1a P0 fix must be vendored first.

### 4d. TW5 type plane

TW5 (`SPEC.md:137-148`) is explicit about the seam: **"CSP remains the layout/strategy solver; egraph
remains typed-rewrite + cost extraction; the type lattice re-homes off the csp-solver."** Today
`crates/ir/src/passes/csp_domains.rs` and `span.rs` implement type/span lattices *as* csp-solver
`LatticeDomain`s. TW5 wants the genuine HM core (type variables, unification, principal schemes) for
type inference, with the csp-solver confined to layout/strategy. So the SK-V21 direction is to **narrow**
the solver's role, not widen it: it is the strategy/layout/coupling solver, and the type plane moves to
a purpose-built HM engine. This is congruent with the canonical tranche's own trajectory (generalizing
the solver as an assignment/COP substrate, §1c) — the two repos are pulling the solver toward "general
COP substrate," and bbnf simply consumes fewer of its faces (drop the type-lattice face).

## 5. The coordination seam

### 5a. Lock 4 — egraph→csp direct dependency violation

Confirmed live. `crates/egraph/Cargo.toml:11` declares `csp-solver = "0.1"` directly, and
`crates/egraph/src/csp_scheduler.rs:30` imports `csp_solver::{Csp, constraint, domain, variable}` to
model equality-saturation dirty-frontier propagation as a CSP (`csp_scheduler.rs:1-23`). SK-V20 logs
this as a standing violation: DEFERRED-LEDGER N11 — *"Lock 4 (egraph→csp direct dep)"* — dispositioned
to **TW0** as a two-line fix; SPEC.md:54 prescribes the remedy: **"Lock-4 fix (egraph→csp via the
passes bridge)."** The e-graph is a general-infra crate and must not reach into the domain solver
directly; the CSP scheduling must be injected through the passes layer (dependency-inverted), so egraph
depends only on a scheduler trait and `passes` wires the csp-backed implementation. This is a bbnf-side
architectural fix; it does not touch canonical. It should land in TW0 as specified and be re-verified
in SK-V21.

### 5b. OD-7 — the one coordinated skinny edit (the SolveConfig break)

The canonical tranche's own owner-decision ledger names the seam from the other side. **OD-7**
(`README.md:61`): *"append `..Default::default()` to
`bbnf-lang/skinny/crates/passes/src/decision_csp.rs:85`'s exhaustive `SolveConfig` literal … no
csp-side fix exists — `#[non_exhaustive]` breaks it harder."* This is a real, verified break:
`decision_csp.rs:85-92` is an exhaustive six-field `SolveConfig { pruning, ordering, max_solutions,
backjumping, optimization_mode, node_budget }` literal. When W1 adds fields **and deletes
`backjumping`** (fast-follow, `README.md:33`) and W12 ships 0.2.0, this literal fails to compile in two
independent ways: unlisted new field, and `backjumping: false` (`:89`) referencing a deleted field.
The `..Default::default()` spread does not save the deleted-field case — the `backjumping: false` line
itself must be removed. csc411 authored this in the mirror and verified it compiles + runs
(`solutions: 1`). The standing engineering rule is codified: *"any `SolveConfig`/`SolveStats` field
change sweeps all exhaustive literals or uses `..Default::default()`"* (`README.md:76`).

### 5c. Re-vendor cadence and ownership

- **Ownership** is clean and declared. `crates/csp-solver/Cargo.toml:1` states the source-of-truth is
  csc411; the `py` feature "is declared for cfg-recognition only … the PyO3 surface … is csc411-only
  and unused by bbnf-lang." Algorithm evolution, propagator design, and the puzzle/morph/service stack
  are **csc411 territory**; bbnf is a **consumer** that re-vendors `src/` and never edits it in place
  (the vendored copy is byte-identical, proving the discipline has held).
- **Cadence** is event-driven and currently *paused-pending-W1*. There is nothing to re-vendor now (no
  source delta). The **next** re-vendor is gated on csc411 landing W1 (the P0 fix + `backjumping`
  deletion + `DomWdeg→Mrv`) and, if bundled, W12's 0.2.0 bump. That single re-vendor must carry, in one
  bbnf commit: (i) the new `src/`; (ii) the OD-7 `decision_csp.rs` edit (remove `backjumping: false`,
  add `..Default::default()`); (iii) the `= "0.1"` → `= "0.2"` sweep across `core`/`egraph`/`ir`
  (§3); (iv) a `cargo test --workspace` gate. The canonical side plans a **bbnf sync-gate `--verify`**
  in W12 (`README.md:44`) as the coordination handshake.
- **Constraint from canonical**: *"NEVER push bbnf-lang origin"* (`README.md:70`) — the skinny edit is
  authorized to land locally under the never-push discipline in the W12 window. bbnf-lang's crates.io
  identity is 73 commits stale by their count; the re-vendor stays local.

### 5d. A framing correction for SK-V21

The NOW-STATE phrasing "csp-solver is being worked on with a latest tranche set" is true but easily
over-read. The *solver algorithm* is **not** being actively rewritten right now — it is frozen at the
vendor point with a fix queued. What is being worked, energetically, is the **csc411 product stack
around it** (puzzles, service, wasm, frontend, morph excision, release train). For SK-V21 the actionable
truths are narrow and firm:

1. **Nothing forces an immediate re-vendor** — no source delta exists (§2).
2. **A re-vendor is imminent and API-breaking** once csc411 lands W1/W12; pre-stage the OD-7 edit and
   the `0.1→0.2` sweep so the fold is mechanical (§5b/§5c).
3. **The P0 fix must ride that re-vendor** and the full-tree AC-3 enumeration sites should be audited
   for the same false-UNSAT before TW4/TW5 lean on them (§1a).
4. **Lock 4 is a bbnf-only TW0 fix** (dependency-invert egraph→csp through passes), independent of any
   canonical motion (§5a).
5. **The solver needs nothing new for real kernel selection** — the entire gap (measured cost table,
   product e-class, non-zero `perf_cost`, Lock-16 manifest) is bbnf-side TW4 work; the CSP is demoted
   from tautological selector to admissibility/coupling filter, and the type-lattice face re-homes off
   the solver in TW5 (§4).

The one thing SK-V21 must not do is re-import the tautology under a new name. The measured cost table is
the load-bearing net-new; the solver is a settled, well-owned dependency, not a research frontier.
