# 36 — csp-solver P0: bbnf-side severity derivation

The csc411 grand-uplift campaign flags a P0 soundness defect in `ac3_from_variable`'s
`Revision::Unsatisfiable` arm — present byte-identical in the vendored copy at
`crates/csp-solver/src/solver/ac3.rs:128` (`Revision::Unsatisfiable => return true`). The canonical
severity (silent false-UNSAT on 26/113 corpus boards; dropped solutions under `Pruning::Ac3`
enumerate) was imported from csc411 docs, never derived against bbnf call paths. This note traces
every bbnf entry into the vendored solver and resolves the transfer.

## Headline verdict

**UNREACHABLE from bbnf today. NOT P0-for-bbnf. LATENT-only behind two independent config changes
that no bbnf site makes.** The imported severity does not transfer.

## 1. Where the bug lives — the reachable set

`ac3_from_variable` (the function containing line 128) is called from **exactly three** sites, and
every one is gated behind the `Pruning::Ac3` match arm inside backtracking / branch-and-bound
**search**:

- `crates/csp-solver/src/solver/backtrack.rs:154` — `Pruning::Ac3 => ac3::ac3_from_variable(...)`
- `crates/csp-solver/src/solver/backjump.rs:154` — same, backjumping search
- `crates/csp-solver/src/solver/optimize.rs:361` — same, branch-and-bound

`ac3_full` (the pure-propagation cousin, `ac3.rs:57`) has **no** early-return-without-trail hazard:
its Unsatisfiable arm (`ac3.rs:78`) returns `Err(Unsatisfiable)` from a one-shot fixpoint with no
sibling branches to leak into. The defect is specific to the search-time seeded variant.

So reaching line 128 requires a bbnf caller that (a) enters **search** (`solve` / `solve_optimized`),
AND (b) selects `pruning: Pruning::Ac3`.

## 2. Every bbnf entry into the solver

**Propagation-only sites (never enter search → never touch `ac3_from_variable`):** all call
`csp.propagate()`, which dispatches (`lib.rs:259-289`) to `ac3_full` or the monotonic sweep, never the
seeded variant:

- `crates/ir/src/passes/span.rs:61`
- `crates/ir/src/passes/sets/first_sets.rs:71`, `sets/follow.rs:70`, `sets/dispatch/eligibility.rs:67`
- `crates/ir/src/passes/types/mod.rs:67,127`
- `crates/egraph/src/csp_scheduler.rs:235`
- `crates/core/src/backend/rust/analysis/inline/plan.rs:167`

This **resolves research 18 §1a's hedge** ("the full crates/ tree uses AC-3 heavily … if any run
`ac3_from_variable` inside backtracking search with enumeration, they inherit the hazard"): the
crate-by-crate audit is done — none of these run search at all. They are `ac3_full` fixpoints, which
do not contain the buggy arm.

**Search sites (the only two in all of bbnf):**

1. `skinny/crates/passes/src/decision_csp.rs:93` — `csp.solve_optimized(&config)`, config at
   `:85-92`: `pruning: Pruning::ForwardChecking`, `max_solutions: 1`, single variable, 5 constraints.
2. `crates/ir/src/passes/csp_strategy/mod.rs:603` — `csp.solve_optimized(&config)`, config at
   `:598-601`: `pruning: Pruning::ForwardChecking` (explicit) with `..SolveConfig::default()` (whose
   default is also `Pruning::ForwardChecking`, `lib.rs:97`).

**Neither selects `Pruning::Ac3`.** A repo-wide check confirms zero `Pruning::Ac3` selections in bbnf
(`rg "pruning:" crates/ skinny/` yields only the two `ForwardChecking` lines above). Therefore the
`Pruning::Ac3 =>` arm at all three call sites is dead for bbnf, and **line 128 is unreachable from
every bbnf call path.**

## 3. What reachability would cost — the latent envelope

To make line 128 reachable a bbnf site must, simultaneously:
- (a) enter search — only 2 sites qualify, and
- (b) flip `pruning` to `Pruning::Ac3` — 0 sites do.
The dropped-solutions variant additionally needs `max_solutions > 1` — 0 bbnf sites do (both are 1;
default is 1).

Even under the worst hypothetical flip (skinny → `Pruning::Ac3`): the model is 1 variable / 5
constraints / `max_solutions: 1`, and it is tautological by construction (research 18 §4a — the CSP
recovers its own input). A false-UNSAT would set `csp_status = "unsat"`, but `selected_candidate_id`
falls back to `active.facts.selected_candidate_id` regardless (`decision_csp.rs:110-115`). Blast radius
is a wrong **String telemetry field**, never codegen correctness. `csp_strategy/mod.rs` similarly
guards with `node_budget` + a trivial-pick fallback (`:606-620`).

**Verdict: UNREACHABLE today; LATENT-only behind two independent, currently-unmade config changes;
NOT P0-for-bbnf.** The fix must still ride the next re-vendor (it is correct and free), but it gates
nothing in SK-V21 and blocks no TW.

## 4. The 0.2.0 API break (backjumping deletion) — bbnf blast radius

W1 deletes the `SolveConfig.backjumping` field; W12 bumps to 0.2.0. bbnf call sites affected:

- **`backjumping` field reference — exactly one site:**
  `skinny/crates/passes/src/decision_csp.rs:89` (`backjumping: false`) inside the exhaustive six-field
  `SolveConfig { .. }` literal at `:85-92`. Field deletion makes this line reference a nonexistent
  field → compile error. **`..Default::default()` does NOT rescue it** — the `backjumping: false` line
  must itself be removed. This is precisely csc411 OD-7 (research 18 §5b).
- **The other `SolveConfig` literal is SAFE:** `crates/ir/src/passes/csp_strategy/mod.rs:598-601`
  already uses `..SolveConfig::default()` and lists only `pruning` + `optimization_mode`, referencing
  neither `backjumping` nor any about-to-be-added field. It survives the field churn untouched.
- **New W1 fields:** the exhaustive `decision_csp.rs:85-92` literal breaks on any added field unless
  `..Default::default()` is appended; `csp_strategy` is already spread-safe.
- **`DomWdeg → Mrv` ordering rename:** does NOT break bbnf. bbnf uses only `Ordering::FailFirst`
  (`decision_csp.rs:87`) and `Ordering::Chronological` (via default in `csp_strategy`) — neither is in
  the `DomWdeg` family.
- **Version-requirement resolution break (0.2.0):** `csp-solver = "0.1"` in `crates/egraph/Cargo.toml:11`,
  `crates/core/Cargo.toml:20`, `crates/ir/Cargo.toml:18` fail to resolve against 0.2.0 → sweep to
  `"0.2"` (3 files). skinny binds by `path`/`workspace` (unaffected).

**Net 0.2.0 fold for bbnf:** one source edit (remove `decision_csp.rs:89`, add `..Default::default()`
to the `:85-92` literal) + three Cargo.toml version sweeps. Mechanical; pre-stageable exactly as
research 18 §5c prescribes.
