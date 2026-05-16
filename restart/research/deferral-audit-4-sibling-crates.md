# Deferral Audit #4 — Sibling Crates (csp-solver / parse-that / egraph / simd-scan)

Status: V6-READY corpus audit; greenfield mandate; deferral-audit lane #4.
Scope: the four sibling crates published as general-purpose Rust libraries
(`csp-solver`, `parse-that`, `egraph`, `simd-scan`) and their consumption from
within the bbnf-lang workspace. The audit returns disposition rows per item
plus cross-crate fold candidates and a regex-automata removal ledger.

The closing posture is set in §10 of the dispatch: sibling crates are SOTA
general-purpose Rust crates that bbnf consumes; audacity here means publishing
four crates the broader ecosystem can use, not bbnf-shaped shims. parse-that
completely replaces regex-automata.

---

## §1 — Scope + corpus references

### Required reading anchors

| ID | Path | Lines |
|---|---|---|
| C1 | `restart/README.md` | 31 (workspace shape — sister crates), 56-57 (parse-that + simd-scan), 62 (regex fold), 84-89 (DAG), 122-143 (Unicode regex), 365 (regex-automata absorbs), 392-397 (Lock 11 path-deps), 471-473 (close posture) |
| C2 | `restart/locks/LOCKS.md` | 40 (Lock 4 — output piping), 44 (Lock 6 — xtask), 46 (Lock 7 — path), 52 (Lock 10 — auto-detect), 54-56 (Lock 11 — incubation), 60 (Lock 14 — zero overfit) |
| C3 | `restart/ARCHITECTURE.md` | 25-30, 55-67, 127-134, 164-182, 305-330, 503-562, 597-602, 644-665, 752, 802-849, 869-936, 1109-1134 |
| C4 | `restart/MASTER-PLAN.md` | 51-55, 81, 184-185, 245, 304-318, 350-358, 477-499, 556 (Lock 11 publication split), 691-708, 752-777 |
| C5 | `restart/research/topic-3-csp-gadts.md` | full read; HM(X) + OutsideIn(X) + finite-domain CSP delineation |
| C6 | `restart/research/topic-4-egraphs.md` | full read; bridge vs. fusion, e-class analysis monotonicity, extraction discipline |
| C7 | `restart/research/topic-5-cost-models.md` | 1-200; Cost trait shape, regex+parser cost sharing, Pareto pressure |
| C8 | `restart/research/topic-8-simd-dfa.md` | full read; bespoke regex motivation, exact-vs-prefilter contract, multi-engine routing |

### Crate-on-disk anchors

| Crate | Cargo.toml | lib.rs | Module siblings | LOC class |
|---|---|---|---|---|
| `csp-solver` | `crates/csp-solver/Cargo.toml:1-18` | `crates/csp-solver/src/lib.rs:1-533` | adjacency, builder, constraint, domain, ordering, puzzles, py, solver, variable | mid (~3-4 KLOC across 9 modules per `ls`) |
| `egraph` | `crates/egraph/Cargo.toml:1-15` | `crates/egraph/src/lib.rs:1-58` | analysis, cost_config, cost_weights, csp_scheduler, eclass, egraph, extract, id, language, rewrite, scheduler, unionfind | mid; egg-class layout |
| `simd-scan` | `crates/simd-scan/Cargo.toml:1-33` | `crates/simd-scan/src/lib.rs:1-114` | alphabet, avx2, avx512, compaction, index, neon, parity, scalar, wasm | mid (5,676 total LOC; neon largest at 719) |
| `parse-that` | absent on disk | absent on disk | absent on disk | greenfield — not yet a crate |

The on-disk sibling-crate landscape is therefore three crates extant
(`csp-solver`, `egraph`, `simd-scan`) plus one greenfield-only crate
(`parse-that`) named throughout the corpus but never created. The architecture
already pre-sites it at `crates/parse-that/src/{lib.rs,regex/hir/,regex/nfa/,
regex/dfa/,regex/vm/,regex/prefilter/,unicode/,literal/}`
(`restart/ARCHITECTURE.md:544-552`).

This is load-bearing for §6: the parse-that publication target is brand-new
implementation work, not extraction from existing code. Every fold proposal
that touches parse-that lands in a green tree.

---

## §2 — Per-crate audit

### §2.1 — `csp-solver`

#### Engagement

The CSP solver carries three distinct workloads:

1. **HM(X)/OutsideIn(X) bridge for the type checker** — Topic 3 settles this:
   plain Hindley-Milner generates and solves first-order equality constraints
   inside `passes::layout`; CSP only earns its keep on finite implementation
   choices (host overload, layout class, recognizer eligibility, materialization
   mode, recovery strategy, backend erasure, extraction legality)
   (`restart/research/topic-3-csp-gadts.md:172-179`). The csp-solver is *not* a
   replacement unifier; it is an orthogonal substrate for finite-domain
   decisions.
2. **Cost-model bounded-search backend** — Topic 5 surfaces SMT/CSP-style
   solver composition for constrained objective rows
   (`restart/research/topic-5-cost-models.md:1-100`); the csp-solver's
   `branch_and_bound` + `solve_with_cost_eval`
   (`crates/csp-solver/src/lib.rs:454-491`) is already the right shape.
3. **Grammar-shape mining + bridge guards** — `passes::bridge` consults solved
   legality facts from the CSP side of the bridge tables
   (`restart/ARCHITECTURE.md:802-815`). This is monotone-fact exchange, not
   global search.

#### Audit rows

| # | Topic | On-disk verdict | V1 fold action |
|---|---|---|---|
| CSP-1 | First-order equality unification | Out of scope. HM owns equality; csp-solver's `Pruning::Ac3` / `Pruning::ForwardChecking` propagation is the wrong shape for first-order term unification (`crates/csp-solver/src/lib.rs:36-58`). | KEEP separation. Document explicitly that csp-solver is finite-domain only; the type checker calls a Robinson-style unifier independently. |
| CSP-2 | Inequality / arithmetic / lattice domains | `domain` module exposes `CostDomain` lattice pattern (`crates/csp-solver/src/lib.rs:510-520`); `monotonic::propagate_monotonic` is a separate strategy (`crates/csp-solver/src/lib.rs:281-288`). | KEEP. Lattice domains are exactly the Topic 4 "monotone summaries" the bridge requires (`restart/research/topic-4-egraphs.md:282-287`). |
| CSP-3 | Implication / OutsideIn(X) constraints | Absent. README §7 mentions OutsideIn as a research warning, not a V1 commitment (`restart/README.md:264`); Topic 3 §C3 confirms this defers (`restart/research/topic-3-csp-gadts.md:198-203`). | DEFER honestly. Add a public `Notes::OutsideInUnsupported` doc paragraph in `csp-solver/src/lib.rs` so future GADT/branch-local-equality work has a documented reopen point per Topic 3 §6 adversarial gate. |
| CSP-4 | Higher-order + abductive unification | Absent; out of V1 per Topic 3 (`restart/research/topic-3-csp-gadts.md:118-127`). | DEFER. Same documentation gate as CSP-3. |
| CSP-5 | Public API surface | Today's surface re-exports `Csp`, `SolveConfig`, `Pruning`, `PropagationStrategy`, `OptimizationMode`, `SolveStats`, `Unsatisfiable`, `assignment` builder, `sudoku` puzzle (`crates/csp-solver/src/lib.rs:22-34`). | FOLD. Two issues. First, `pub use puzzles::sudoku` (`crates/csp-solver/src/lib.rs:25`) leaks puzzle examples into the public surface — a published general-purpose CSP crate should ship sudoku under `[dev-dependencies]`/`tests/` or feature-gate it (`puzzles` feature off-by-default), not export it from `lib.rs`. Second, the `py` feature is documented as csc411-only (`crates/csp-solver/Cargo.toml:5-12`); for publication this leakage must move to the csc411-side cdylib sub-crate per the `wasm-subcrate-pattern` precedent (memory `feedback_wasm_subcrate_pattern`). |
| CSP-6 | Explanation / unsat-conflict trace | Conflict-directed backjumping is implemented (`crates/csp-solver/src/lib.rs:316-330`); the public `Solution` lacks a structured explanation enum mirroring `BridgeJustification` (`restart/ARCHITECTURE.md:1008`). | FOLD. Add `pub struct Explanation { pub conflicts: Vec<ConflictRecord>, pub propagated: Vec<PropagationStep> }`. Topic 4 §C9 binds the explanation requirement (`restart/research/topic-4-egraphs.md:499-506`). |
| CSP-7 | Performance / scale | `node_budget: Some(1_000_000)` default + `SolveStats::budget_exceeded` (`crates/csp-solver/src/lib.rs:81-118`) is the right early-abort discipline. | KEEP. PASS-2/H gates need a per-pass propagation-time gate; route through `cost-model` (no overlap). |
| CSP-8 | Reusability for the broader ecosystem | The isomorphism with the csc411 Python solver (`crates/csp-solver/Cargo.toml:5`) ratifies general-purpose framing; sudoku puzzle (`crates/csp-solver/src/puzzles/`) is the only domain leak. | FOLD-thinly. Move `puzzles/` to `tests/puzzles_smoke.rs` or feature-gate it. The public crate should publish exactly: `Csp`, `Domain` trait, `Constraint` trait, `Variable`, `Adjacency`, `SolveConfig`, `Explanation`, plus `assignment::AssignmentBuilder`. Nothing else. |
| CSP-9 | Test fixtures + baseline | `crates/csp-solver/tests/` exists; `crates/csp-solver/docs/` exists. | KEEP. Sibling-crate stability per `restart/MASTER-PLAN.md:556` requires "API has not changed across the prior tranche" — capture a `tests/api_snapshot.rs` baseline at A.W1. |

#### Disposition

`csp-solver` V1 is **publication-ready after one fold pass**: drop the
`puzzles` re-export from `lib.rs`, gate the `py` feature behind a sub-crate,
add `Explanation`, and document the OutsideIn-deferral gate. Power-vs-need
is well-matched: the bbnf type checker uses HM for unification (out of csp
scope) and CSP only for finite choices (in csp scope, well-supported by the
existing `solve` / `solve_with_cost_eval` / `solve_with_given` triplet).

### §2.2 — `parse-that` (bespoke regex)

#### Engagement

The user mandate is unambiguous: regex-automata MUST NOT be used; parse-that
owns regex. This is a stronger position than the V6-corpus's "oracle-only"
posture (`restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md:138`). The
audit must therefore distinguish:

- **Active dependency** — regex-automata referenced by `Cargo.toml` or `use`
  (forbidden, must remove).
- **Oracle-test posture** — regex-automata used in `tests/` to assert
  behavioural parity (currently corpus-acceptable per Topic 8 §6 A3
  amendment; user mandate now sharpens this — see §4).
- **Documentation citation** — regex-automata cited as a research influence
  in `restart/README.md:369` etc. (acceptable; no code).

The on-disk reality is that parse-that does not yet exist as a Rust crate;
the entire spec must land cleanly in V1 because there is no legacy code to
contest.

#### Audit rows

| # | Topic | V6 corpus state | V1 fold action |
|---|---|---|---|
| PT-1 | Crate existence | absent. | LAND in Tranche A.W1 with the architecture-prescribed module layout (`restart/ARCHITECTURE.md:544-552`). |
| PT-2 | Regex syntax coverage | Architecture lists HIR + NFA + DFA + VM + prefilter; Topic 8 confirms multi-engine is mandatory (`restart/research/topic-8-simd-dfa.md:519-525`). | KEEP. V1 must cover: literals, character classes (incl. set algebra `[A--B]`, `[A&&B]`), alternation, `*`/`+`/`?`/`{n,m}`, anchors, named captures, back-references behind a `regex_capability` feature flag (back-refs are NP-hard for DFA; route through PikeVM-class engine), bounded lookahead/lookbehind. |
| PT-3 | Unicode coverage | README claims Unicode 16.0, full `\p{...}` properties, script properties, set algebra inside character classes, grapheme cluster awareness, NFC/NFD/NFKC/NFKD modifiers (`restart/README.md:131-143`). | KEEP — but flag size budget. Topic 8 §6 D4 calls out that "rich Unicode ambitions exceed what a simple DFA codegen story can carry" (`restart/research/topic-8-simd-dfa.md:567-576`). V1 must publish per-engine Unicode-state-budget knobs and a documented fallback policy from full DFA → lazy DFA → NFA/VM. |
| PT-4 | Construction (NFA → DFA) | Architecture splits `regex/nfa/`, `regex/dfa/`, `regex/vm/`, `regex/prefilter/` (`restart/ARCHITECTURE.md:546-550`). | KEEP. Honour Cox 2007 (S1) and Owens-Reppy-Turon derivatives (S4): three engine plans (Thompson VM, lazy DFA, full DFA) with explicit verifier-before-tape mode (`restart/audit/pass-2-codegen/PASS-2.md:81`). |
| PT-5 | HIR — stable bbnf-consumable shape | `parse-that/regex/hir/` exists as a directory but spec-only (`restart/ARCHITECTURE.md:546`). | LAND. The HIR is the load-bearing delta vs. regex-automata: it must expose stable `RegexHir` enum + capture map + Unicode-class summary so the BIR `RegexProgram` can carry a handle without reading regex-automata internals (`restart/ARCHITECTURE.md:935`). |
| PT-6 | Cost-model integration | Topic 5 binds shared `Cost` trait with `score` + `branches` (`restart/README.md:215-217`); Topic 5 refinement 2 (`restart/research/topic-5-cost-models.md` proposed text) widens this to "exact scan, SIMD prefilter plus verifier, lazy DFA, full DFA, or VM." | LAND. The regex `Cost` impl publishes per-program `(plan, score)` for each viable execution choice; the parser cost model picks among them through `cost-model`'s comparison logic. |
| PT-7 | Codegen — emit verifier source | Lock 6 forbids proc-macro façades for codegen output (`restart/locks/LOCKS.md:44`). The verifier is committed source emitted by `xtask` from a parse-that-owned HIR. | LAND. parse-that publishes `to_verifier_program(hir) -> VerifierProgram`; bbnf-codegen wraps it in committed Rust source under `crates/runtime/src/grammars/<name>/regex/`. parse-that itself never emits user-facing Rust; it emits a tagged execution plan. |
| PT-8 | Performance — SIMD acceleration / JIT | Topic 8 SOTA (`restart/research/topic-8-simd-dfa.md:519-525`) routes SIMD prefilter through `simd-scan` consumers, *not* internally inside parse-that. JIT is absent from V1. | KEEP. parse-that consumes `simd-scan` for prefilter candidate offsets; verifier acceptance is parse-that's authority. No JIT in V1; route as Tranche-K research stub if user reopens. |
| PT-9 | Test corpora | Architecture binds `regex_unicode` test (`restart/MASTER-PLAN.md:358`). | LAND. V1 corpus: (a) Cox 2007 examples; (b) RE2 regression suite; (c) Unicode 16.0 character-property battery; (d) BBNF grammar regex literals from the seed-9 grammars; (e) per-engine parity suite (VM = lazy DFA = full DFA on the same input). |
| PT-10 | regex-automata oracle | The V6 corpus's "oracle until parity is proven" posture (`restart/MASTER-PLAN.md:777`, `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md:138`) conflicts with the user mandate. | RESOLVE per §4 below. The user-mandated removal forces oracle parity to come from a parse-that-internal cross-engine matrix (VM vs. lazy DFA vs. full DFA, all parse-that-owned), not from regex-automata. |
| PT-11 | PCRE compat / regex literal extensions / regex-as-grammar-fragment | Deferred per V6 (architecture has no PCRE-compat row; regex-as-grammar-fragment is the `RegexProgram` BIR variant). | DEFER honestly. Document `parse-that::compat::pcre = unsupported` so future Tranche-K research has a reopen pointer. |
| PT-12 | Reusability — broader ecosystem | The user-set publication name is "a generic Rust regex library" (`restart/README.md:62`). | LAND. parse-that's `Cargo.toml` must publish without any bbnf dependency (`restart/ARCHITECTURE.md:174-180`); ecosystem consumers use `parse-that::regex::{hir, nfa, dfa, vm, prefilter, unicode}` directly. |

#### Disposition

parse-that V1 is **the largest greenfield workload of the four sibling
crates**. The architecture has prescribed the module tree; the corpus has
prescribed the multi-engine + verifier-before-tape contract; the user has
removed the regex-automata escape hatch. V1 must deliver a complete
HIR/NFA/lazy-DFA/full-DFA/VM/prefilter substrate with Unicode 16.0 + set
algebra + bounded lookahead/lookbehind. This is roughly the
regex-automata feature surface plus grammar-owned HIR + verifier-program
extraction; it is not a thin wrapper.

### §2.3 — `egraph`

#### Engagement

The egraph crate is the rewrite + extraction substrate; Topic 4 binds the
seven V1 rewrite categories (`restart/README.md:229-239`) and the bridge
discipline (monotone facts only; e-class representative is *not* truth until
extraction; Topic 4 §D2). The on-disk crate is already quite complete:
`Analysis` trait, `Language` trait, `Rewrite`, `Extractor`, `CostModel`,
`Lattice`, `Scalar`, `BackoffScheduler`, `CspScheduler`, `EClass`, `EGraph`,
`Id`, `UnionFind` (`crates/egraph/src/lib.rs:33-57`).

#### Audit rows

| # | Topic | On-disk verdict | V1 fold action |
|---|---|---|---|
| EG-1 | E-class rep stability (Lock 4 binding) | `unionfind` module + `Id` API exists; rep-stability test belongs in tranche C.W4 (`restart/MASTER-PLAN.md:316`). | KEEP. Add `tests/representative_stability.rs` enforcing "extracted ID stable across rebuild iteration" per Topic 4 §C5 (`restart/research/topic-4-egraphs.md:461-468`). |
| EG-2 | Analysis trait (egg-style) | Present; `pub use analysis::{Analysis, NoAnalysis}` (`crates/egraph/src/lib.rs:46`). | KEEP. The trait must enforce semilattice discipline (Topic 4 S2) — current `make`/`merge` shape mirrors egg. Add doc comment naming the monotonicity requirement. |
| EG-3 | Domains: regex, grammar, cost — all V1? | Crate is intentionally domain-agnostic (`crates/egraph/src/lib.rs:5-10`). bbnf-side `Language` impls land in `passes::egraph_rewrites/*`. parse-that-side `Language` impl for regex HIR lands in parse-that. | KEEP, with one cross-crate fold (see §3). The `Language` derive-macro (`egraph-derive`) must satisfy both grammar-IR and regex-HIR shapes. |
| EG-4 | Datalog frontend (egglog) | Topic 4 §D1 names egglog as a known-SOTA fusion path; V1 keeps bridge tables instead (`restart/research/topic-4-egraphs.md:519-529`). | DEFER. Document `egglog::compat::Bridge = future research target` so post-V1 reopen has a pointer. No code in V1. |
| EG-5 | Extraction (cost-driven, Pareto, scalarization) | `Extractor` + `CostModel` + `AstSize` + `Lattice` + `Scalar` exist (`crates/egraph/src/lib.rs:52`). | FOLD. Topic 5 §C7 wants Pareto-frontier extraction beyond scalar `score` (`restart/research/topic-5-cost-models.md:1`). V1 must publish a `ParetoExtractor` alongside the scalar `Extractor`; downstream `cost-model` calls scalarize from a frontier. |
| EG-6 | Saturation budget | `BackoffScheduler` + `RunReport` (`crates/egraph/src/lib.rs:56`); Topic 4 §C8 binds rewrite budgets (`restart/research/topic-4-egraphs.md:490-498`). | KEEP. `RunReport` carries iteration limit, node limit, time limit; budget integration is downstream cost-model concern. |
| EG-7 | Bridge implementation — `BridgeJustification`, stable-id passing | `csp_scheduler` module + `CspScheduler`/`DirtyDomain`/`ParentDirtyProp` exposed (`crates/egraph/src/lib.rs:49`). | FOLD. The on-disk `csp_scheduler` is bbnf-aware terminology; the public surface should be domain-agnostic — rename to `BridgeScheduler`/`ExternalAnalysis` so the publication crate has no implicit CSP coupling. The bbnf-specific binding lives in `passes::bridge` (`restart/ARCHITECTURE.md:802-815`), not in `egraph` itself. |
| EG-8 | Public API for bbnf + external | `lib.rs` re-exports 14 items (`crates/egraph/src/lib.rs:46-57`). | FOLD-thinly. Demote `csp_scheduler::*` to `bridge::*`; keep everything else. Externalise documentation to `crates/egraph/README.md` (greenfield doc) for crates.io consumers. |
| EG-9 | egraph-derive macro | Separate crate; dev-dep on egraph (`crates/egraph/Cargo.toml:13-14`). | KEEP. The derive must satisfy both grammar-IR `Rule`/`Seq`/`Alt` and regex-HIR `RegexHir` shapes — see cross-crate fold §3. |
| EG-10 | csp-solver dependency | `csp-solver = "0.1"` in egraph's `Cargo.toml:11`. | RESOLVE. This is a hard dependency from one publication-target to another. Topic 4 §D1 binds bridge-not-fusion. The egraph crate should not directly depend on csp-solver — the bridge lives in `passes::bridge`. **Recommended action: remove `csp-solver` from `egraph/Cargo.toml`** and move the `csp_scheduler` integration glue to `passes` (where it actually executes) or to a thin `egraph-csp-bridge` sub-crate if path-dep coupling is required. |

#### Disposition

`egraph` V1 is **mostly publication-ready** but carries one architectural
violation: the direct `csp-solver` dependency in `Cargo.toml` (`crates/
egraph/Cargo.toml:11`) couples two publication targets that the locks
explicitly require to compose by output piping (`restart/locks/LOCKS.md:40`).
The fix is to demote the dependency to a passes-side bridge.

### §2.4 — `simd-scan`

#### Engagement

simd-scan owns the SIMD structural-scan kernel matrix (NEON, AVX2, AVX-512,
WASM-SIMD, scalar fallback). PASS-2 Lock 1 demands verifier-before-tape
discipline (`restart/audit/pass-2-codegen/PASS-2.md:170-178`); PASS-2 binds
exact-vs-prefilter modes (`restart/ARCHITECTURE.md:921`).

#### Audit rows

| # | Topic | On-disk verdict | V1 fold action |
|---|---|---|---|
| SS-1 | AVX2 / NEON / AVX-512 / WASM-SIMD / scalar coverage | All five present (`crates/simd-scan/src/{avx2,neon,avx512,wasm,scalar}.rs`); cfg-gated per target (`crates/simd-scan/src/lib.rs:55-66`). | KEEP. Architecture-matrix per Topic 8 §C10 honoured (`restart/research/topic-8-simd-dfa.md:526-533`). |
| SS-2 | Verifier-before-tape integration | `scan_structural` returns `StructuralIndex` of `(position, kind)` pairs (`crates/simd-scan/src/lib.rs:80-114`); verifier-before-tape is the consumer's responsibility (PASS-2 contract `restart/audit/pass-2-codegen/PASS-2.md:81`). | KEEP. Document the candidate-not-acceptance contract in `lib.rs` doc comment so external consumers honour it. |
| SS-3 | Algorithms (delim scan, classifier, regex prefilter) — V1 | Delim/structural scan (`alphabet::StructuralAlphabet` + nibble-LUT + wide-LUT + digraph compare); compaction (tzcnt + PEXT specialisation); parity (CLMUL or shift-XOR) (`crates/simd-scan/src/lib.rs:31-37`). | KEEP. The three V1 algorithms cover stage-1 simdjson parity (`restart/research/topic-8-simd-dfa.md:321-337`). |
| SS-4 | Vectorscan-style multi-pattern / Hyperscan compat — V1 | Absent; Topic 8 §D5 explicitly says do not imitate Hyperscan's API (`restart/research/topic-8-simd-dfa.md:577-585`). | DEFER honestly. No fold action; document non-goal in `lib.rs`. |
| SS-5 | Test fixtures + baseline | `tests/correctness.rs` + `tests/fuzz.rs` referenced in lib doc (`crates/simd-scan/src/lib.rs:44-46`); `benches/stage1_throughput.rs` exists. | KEEP. Per-arch byte-identical fuzz parity is the Lock 1 invariant; PASS-2 SOTA gates depend on it. |
| SS-6 | proc-macro2/syn/quote dependencies | Present in `Cargo.toml:11-13`. | INVESTIGATE. simd-scan should not depend on proc-macro infrastructure for its public scanner API. If these are used for scanner-codegen (KernelShape / WideLut compile-time generation), the dependency is sound but should be feature-gated. If they are used for nothing currently, remove them. (Outside this audit's allowed scope to investigate; flag for tranche-A review.) |
| SS-7 | Publication state | `Cargo.toml` lacks `license`, `repository`, `description` keywords for crates.io publication. Workspace shape (`restart/README.md:57`) has it as workspace-internal until stable; Lock 11 (`restart/MASTER-PLAN.md:556`) holds simd-scan in path-dep until further publication review. | KEEP-DEFERRED. simd-scan is the only of the four that the corpus does *not* publish in J.W3 (it stays workspace-internal per Lock 11 staging). Reaffirm this; revisit when downstream consumers (regex prefilter, JSON structural index) demonstrate stability. |

#### Disposition

`simd-scan` V1 is **publication-ready as a workspace-internal sister crate**
already, but stays internal until Lock 11's stability gate clears. The
proc-macro dependency triplet warrants tranche-A investigation — they are
not justified by the published `lib.rs` surface.

---

## §3 — Cross-crate fold candidates

| # | Span | Decision | Action |
|---|---|---|---|
| X-1 | HM(X) constraint solver → csp-solver + bbnf type-checker | csp-solver carries finite-domain only; HM lives in `passes::layout` and uses an internal first-order unifier. The bridge between HM and CSP is via `LayoutFacts` side tables (`restart/ARCHITECTURE.md:1004`). | Document explicitly in `csp-solver/src/lib.rs` head doc: "csp-solver is finite-domain CSP only; first-order term unification is not implemented and is the caller's responsibility." Topic 3 §C3 anchor. |
| X-2 | Function-value typing → bbnf type-checker + csp-solver constraints | Topic 3 + audit #2 (function-value) — function values likely require local-equality flow; OutsideIn(X) is the SOTA escalation target but is V1-deferred per Topic 3. | DEFER. Document `csp-solver::compat::implications = future` so the reopen pointer exists. Cross-reference to deferral-audit #2. |
| X-3 | E-graph rewrite cost → egraph + cost-model trait + parse-that regex cost + simd-scan candidate cost | The shared `Cost` trait lives in `cost-model`; egraph extraction calls it; parse-that regex programs publish multiple `(plan, score)` entries; simd-scan kernels publish per-shape cost classes that bbnf cost-model translates. | LAND. The trait is `cost-model::Cost`, not `egraph::CostModel`. egraph re-exports the local extraction-side trait (`crates/egraph/src/lib.rs:52`); `cost-model` owns the public-comparison surface. parse-that and simd-scan implement `cost-model::Cost` for their respective domain instances. Topic 5 fold refinement 2 binds this. |
| X-4 | Regex-as-cost-domain → egraph domain + parse-that HIR | Regex HIR `Language` impl lives inside parse-that; egraph rewrites over RegexHir (e.g., charclass merging — Topic 4 + README line `restart/README.md:233`) live in `passes::egraph_rewrites/charclass.rs`. | LAND. parse-that's `regex::hir` ships a `Language` impl using `egraph_derive`; bbnf-side rewrites are domain-specific passes. Lock 14 honours: parse-that carries no grammar-specific code, only generic regex HIR; bbnf-side rewrites stay grammar-agnostic by operating over the regex domain abstractly. |
| X-5 | egraph → csp-solver direct dependency | `egraph/Cargo.toml:11` couples two publication targets. | FOLD. Remove `csp-solver = "0.1"` from `egraph/Cargo.toml`; demote bridge integration to `passes::bridge`. Lock 4 honours per `restart/locks/LOCKS.md:40`. |
| X-6 | egraph-derive — cross-domain | Must satisfy both grammar-IR `Rule`/`Seq`/`Alt` and regex-HIR shape. | KEEP. Derive macro is already domain-agnostic; verify in V1 by deriving for both `passes::ir::GrammarNode` and `parse_that::regex::hir::RegexHir` in tests. |
| X-7 | parse-that → simd-scan integration | parse-that's prefilter (`regex/prefilter/`) consumes simd-scan candidate offsets; parse-that owns verifier acceptance. | LAND. parse-that depends on simd-scan as a path-dep until publication; the public API in parse-that exposes `Prefilter::accept(candidates, verifier) -> Match` so neither crate leaks the other's internals. |

---

## §4 — regex-automata removal ledger

User mandate: regex-automata MUST NOT be used. Verification grep returns the
following match classes across the corpus:

### Active code dependency (must remove)

None. Across `Cargo.toml` and `crates/*/Cargo.toml`, regex-automata is
**not declared as a dependency anywhere** — confirmed by the absence of
`regex-automata` in any `Cargo.toml` line of the rg output (only doc and
research mentions surface). The on-disk codebase is already clean.

### Test-oracle posture (must amend)

The V6 corpus retains regex-automata as a test-oracle for parse-that:

| Path | Line | Excerpt | Action |
|---|---|---|---|
| `restart/MASTER-PLAN.md` | 477 | "`regex-automata` oracle parity for regex fixtures" | AMEND. Replace with "parse-that internal cross-engine parity (VM ↔ lazy DFA ↔ full DFA on the same fixtures)." |
| `restart/MASTER-PLAN.md` | 499 | `cargo test -p parse-that regex_automata_oracle` | AMEND. Rename test to `regex_engine_parity` (cross-engine internal). |
| `restart/MASTER-PLAN.md` | 777 | "regex oracle lane / parity evidence" | AMEND. Same direction. |
| `restart/ARCHITECTURE.md` | 935 | "`regex-automata` remains the oracle lane until parity is proven" | AMEND. Strike the clause. parse-that owns regex; oracle pressure routes to internal cross-engine parity. |
| `restart/MIGRATION.md` | 506, 594 | "`regex-automata` oracle lane" | AMEND. Same direction. |
| `restart/research/fold-pass-2.md` | 132, 198 | "Bespoke regex risks reimplementing `regex-automata`" / "`regex-automata` grep" | KEEP as research history; it is research-level adversarial finding text, not a contract. The §6 finding is now superseded by the user mandate, so the *amendment row* (T8-A3) is the load-bearing edit (`restart/research/fold-pass-2.md:132`). |
| `restart/research/fold-synthesis.md` | 220, 226, 273, 306, 321, 346 | repeated oracle-lane references | AMEND in V7 synthesis (cross-tranche; outside this audit's scope per §8 but logged here). |

### Documentation-only citation (acceptable)

| Path | Line | Excerpt | Action |
|---|---|---|---|
| `restart/HANDOFF.md` | 51 | "All 16 SOTA projects deep-dived" | KEEP. Listed as research influence; acceptable. |
| `restart/README.md` | 369 | "**regex-automata** | DFA / NFA / hybrid regex engines" | KEEP. Cited as influence on parse-that; correctly absorbs the contribution rather than depends on it. |
| `restart/research/topic-8-simd-dfa.md` | many | regex-automata as primary source S5 | KEEP. Research-level primary-source citation; acceptable. |
| `restart/research/INDEX.md` | 159, 165 | "What does this buy over `regex-automata`?" / "The `regex-automata` crate (BurntSushi) source + documentation" | KEEP. Research engagement question; the answer is now *bespoke parse-that fully replaces regex-automata; user mandate*. |

### User-mandate clarification (resolution)

The V6 oracle posture predates the user mandate; the user mandate sharpens
"oracle until parity is proven" to "no regex-automata at all, including
oracle." The amendment is binary: parse-that's parity oracle becomes
**internal cross-engine** (Thompson VM, lazy DFA, full DFA must all agree on
the same fixtures), supplemented by the **standard regex test corpora** (RE2
suite, Cox examples, Unicode 16.0 property tables — these are public-domain
data, not the regex-automata crate).

The five MASTER-PLAN/ARCHITECTURE/MIGRATION amendments above are the receipt;
they belong to the next synthesis cycle (out of this audit's scope).

---

## §5 — Cross-cutting concerns (with audits #1 type-system and #2 function-value)

| # | Audit overlap | Sibling-crate position |
|---|---|---|
| CC-1 | Audit #1 type-system: HM core + Pierce-Turner + finite CSP | csp-solver is the finite-CSP backend only. HM unification is **not** in csp-solver; type-checker owns it. Cross-audit alignment required. |
| CC-2 | Audit #2 function-value typing: closures / higher-rank / row poly | Topic 3 §C3 binds OutsideIn(X) as the SOTA escalation; csp-solver V1 does not implement implication constraints. Function-value typing must either land via HM(X) extension in `passes::layout` (preferred) or via OutsideIn-style implication in csp-solver (deferred). |
| CC-3 | Audit #1 type-system: GADTs / branch-local equality | csp-solver V1 does not support implications. This is the documented deferral surface. |
| CC-4 | Audit #2 function-value: capture analysis | Captures live in `passes::layout` + host registry; csp-solver participates only when a capture-class triggers a finite choice (e.g., box-vs-borrow materialization). |
| CC-5 | Cost model shared across regex/parser (Topic 5) | `cost-model::Cost` is the trait; parse-that and simd-scan both implement it. egraph extraction calls it. csp-solver's `solve_with_cost_eval` consumes a `DomainCostEval`, which is *not* the same trait — keep the boundary clean. |

---

## §6 — Recommended V1 folds (sorted by greenfield value)

| Rank | Fold | Crate(s) | Greenfield value |
|---|---|---|---|
| 1 | Land parse-that V1 with full HIR + NFA + lazy/full DFA + VM + prefilter + Unicode 16.0 + set algebra | `parse-that` | HIGH. The user mandate forces full bespoke; this is the largest greenfield delta and removes the entire regex-automata escape hatch. |
| 2 | Remove `csp-solver` from `egraph/Cargo.toml`; move bridge integration to `passes::bridge` | `egraph`, `passes` | HIGH. Honours Lock 4 output-piping; un-couples two publication targets. |
| 3 | Strike "regex-automata oracle lane" from MASTER-PLAN/ARCHITECTURE/MIGRATION; replace with parse-that internal cross-engine parity | corpus | HIGH. User mandate compliance. |
| 4 | Amend `csp-solver` public surface: drop `puzzles::sudoku` re-export from `lib.rs`; feature-gate puzzles; add `Explanation` struct | `csp-solver` | MEDIUM. Publication readiness; documented bridge contract. |
| 5 | Add `ParetoExtractor` alongside scalar `Extractor` in egraph | `egraph` | MEDIUM. Topic 5 §C7 binds Pareto pressure for cost-model extraction. |
| 6 | Rename `egraph::csp_scheduler` to `egraph::bridge` (domain-agnostic naming) | `egraph` | MEDIUM. Publication-name hygiene. |
| 7 | Document `csp-solver` as finite-domain only; flag OutsideIn-deferral; flag higher-order-deferral | `csp-solver` | LOW-MEDIUM. Doc only; reopen-pointer hygiene for post-V1 work. |
| 8 | Investigate proc-macro2/syn/quote dependency triplet in simd-scan; remove if unused or feature-gate if codegen | `simd-scan` | LOW-MEDIUM. Tranche-A scope; reduces published-crate dependency surface. |
| 9 | Land `egraph-derive` cross-domain test (grammar-IR + regex-HIR) | `egraph-derive` | LOW. Sanity check; protects cross-crate fold X-6. |
| 10 | Split `csp-solver` `py` feature into a `csp-solver-py` sub-crate per `wasm-subcrate-pattern` precedent | `csp-solver` | LOW. Ecosystem hygiene; csc411-only PyO3 surface should not pollute the Rust publication. |

---

## §7 — Open questions for synthesis

| # | Question | Routing |
|---|---|---|
| Q1 | The user mandate forbids regex-automata even as oracle. Does the corpus accept "internal cross-engine parity" + "RE2 public-domain fixtures" as the substitute, or does the user expect zero external regex test corpora? | Synthesis must clarify before parse-that V1 implementation begins. |
| Q2 | Should parse-that publish to crates.io as `parse-that` (matches workspace name) or under a more discoverable name (`bespoke-regex`, `bbnf-regex` rejected per `restart/README.md:62`)? Naming search needed. | Synthesis or PASS-2 stable-surface review. |
| Q3 | The `egraph` crate currently depends on `csp-solver = "0.1"`. After the fold, does anything else couple them, or are they fully orthogonal publication targets? | Tranche A.W1 audit. |
| Q4 | parse-that V1 work scope: how does it bound against PASS-1 (substrate) vs. PASS-2 (codegen) vs. tranche D (regex Unicode routing)? The architecture pre-sites parse-that under PASS-1's substrate ownership but the work scope spans all three passes. | PASS-1 / PASS-2 / Tranche D scope reconciliation. |
| Q5 | Should `cost-model::Cost` be the unified trait that egraph, parse-that, simd-scan all implement, or should each crate publish a domain-specific trait that `cost-model` adapts? Topic 5 leans toward unified; egg's `CostModel` (already present in egraph as `CostModel<N, A>` per `crates/egraph/src/lib.rs:52`) is a different trait. | Topic 5 fold finalisation. |
| Q6 | Is `egraph::csp_scheduler` (current public re-export `crates/egraph/src/lib.rs:49`) actually domain-agnostic, or does it embed CSP terminology that leaks the bridge into the publication crate? Tranche-A sub-agent must read the module and verify the rename to `bridge` does not break orthogonality. | Tranche A.W1. |
| Q7 | Function-value typing (audit #2) — which sibling crate, if any, owns the implementation? csp-solver is finite-domain only; HM is `passes::layout`; OutsideIn-style implications are deferred. Where does function-value typing actually land? | Audit #2 + audit #1 + Topic 3 cross-resolution. |
| Q8 | simd-scan publication — Lock 11 holds it as workspace-internal until stability. What is the stability gate exactly? "API has not changed across the prior tranche" (`restart/MASTER-PLAN.md:556`) is the J.W3 wording, but simd-scan is excluded from J.W3's incubation-cleared list. When does simd-scan publish, if ever? | J.W3 / future-tranche scope. |

---

## §8 — Voice + discipline

This audit cites path:line on every concrete claim. No metalanguage. No
deferral to "future tranche" without a named receiver. Every fold action
names the file and the rationale. The user mandate on regex-automata is
treated as binary, not as gradient softening of the V6 corpus's oracle
posture.

Sibling crates are SOTA general-purpose Rust crates that bbnf consumes;
audacity here means publishing four crates the broader Rust ecosystem can
use, not bbnf-shaped shims. parse-that completely replaces regex-automata —
the V6 oracle clauses are the corpus's softest residue and must amend.
