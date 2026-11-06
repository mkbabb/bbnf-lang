# SK-V21 — PASS-ALPHA: the convergence-loop re-founding

Date opened: 2026-07-05. HEAD at opening: `05302109d` (SK-V20 close; nothing has landed since —
cutover commit 0 / the D-G reconcile was never executed; the TW manifest is intact but dormant).

## Charter

A fresh-eyes gestalt research pass over the project in totality. The aim is unchanged and absolute: a
fully generalized parser generator that bests every SOTA parser for the targeted structured-data
languages (CSS, JSON, …) by a perfected union of backend-agnostic code with an emitter specializing in
a hybrid of Rust and hand-rolled ASM, in the dav1d discipline — parse speed and optimization of first-order
priority.

What is NEW over SK-V20 (which stands, committed, at `restart/skinny/tranches/sk-v20/`):

1. **The restart itself is in question.** Not merely skinny-vs-totality: the restart's crate topology,
   MASTER-PLAN, and locks are themselves auditable. Prototype, profile, debug, and research SOTA to
   *find the lacuna* — what the restart missed.
2. **The Rust stack is to be questioned wholesale.** The 2021 edition is to be totally abrogated
   (skinny is the holdout: `edition = "2021"`, `rust-version = "1.78"`; `crates/` is already 2024).
   The toolchain pin `nightly-2026-04-11` is ~12 weeks stale against its own 4–6-week cadence. Every
   dependency is to be SOTA and cutting-edge. All concepts — grammars, parsers, type solvers, CSP
   facilities — cutting-edge.
3. **The archaeological dig.** All 49 original tranche directories (`docs/tranches/`: AA…BD, W–Z,
   B0–B7, AY/AZ multi-part, archive, meta-audit; 668 files, 14 MB) are to be excavated: what went so
   wrong, what local minima and bottlenecks were faced, what deserves resurrection. Realism about what
   is achievable, while constantly challenging the frontier.
4. **csp-solver has an active tranche set — hear it.** The canonical repo is
   `~/Programming/csc411/CSC411_HW2_ProgrammingQuestion/` (members: csp-solver, wasm, morph-core,
   wasm-morph); the vendored `crates/csp-solver` was last re-vendored at csc411@b70098676. Coordinate;
   do not clobber.
5. **parse-that has moved.** 1.0.0 cut 2026-07-03 (packrat epoch armed, *Span excised, SpanParser
   killed); a coordination letter (VALUEJS-PT-E) dated 2026-07-05. Both `rust/` and `typescript/`
   trees. SK-V20's report 17 is stale; refresh what folds in/out.

## The pass protocol (the convergence loop)

Each pass, sequentially:

1. Up to 8 agents in parallel research the web, the extant codebase, the tranches hitherto.
   (Pass 0 — the audit substrate — runs wide at 32 per the standing directive.)
2. One synthesis agent folds the results into a cogent specification and plan.
3. A fleet of prototyping agents — sometimes with concrete implementation, sometimes with
   prototype-augmented specification — greenfield-brainstorms and test-implements against that
   specification. Prototypes are *analysis instruments*: worktree-isolated, never landed on master.
4. A fleet of critique agents hardens, challenges, and refines each begotten item; each returns a
   **percentage of convergence** alongside a critical analysis.
5. A final synthesizing agent agglomerates and the loop re-enters with newfound context.

**Whereupon 100% convergence: stop, and develop out that exact tranche plan / wave set(s) to
implement, refine, and align.**

## Standing constraints (unchanged, binding)

- This is NOT an implementation phase. Tranche development only. Prototypes inform; they do not land.
- NO quick solutions, NO workarounds, NO legacy code. Idiomatic, gestalt approaches; architectural
  transpositions for elegance, simplicity, and performance are necessary and desirable.
- No overfitting or over-emphasis of JSON/CSS; full grammar generalization, no contrivances.
- Chronically deferred items fold into THIS tranche (the SK-V20 DEFERRED-LEDGER is the opening state).
- All prior prompts and precepts recapped and verified addressed (SK-V20 PRECEPTS-RECAP is the
  opening scorecard; re-verify, do not re-derive).
- Benchmarks: cold-only, loadavg-quiet, single-invocation; aarch64/Apple-M only.
- Orchestration/design/synthesis on the core model; workflow fanout on Opus/Sonnet.
- One cargo invocation in flight per target dir; profiling via samply (debug=true, interactive).

## Pass 0 design (this document's dispatch)

32 agents: 15 dig shards (D1–D15 over the original tranche corpus) · 6 now-state auditors (HEAD delta
re-verification; parse-that refresh; csp-solver coordination; Rust-stack abrogation census;
restart-lacuna architecture hunt; restart-lacuna profiling hunt) · 8 SOTA frontier researchers
(simdjson-class; sonic-rs/yyjson; lightningcss/cssparser; dav1d/NEON-on-Apple-M; data-parallel
parser-generation literature; e-graph+CSP; type-inference frontier; Rust 2024/nightly) · 2 adversarial
verifiers · 1 completeness critic. Reports land at `research/01..32-*.md` herein.

## Deliverables (at 100% convergence)

- `SPEC.md` — the exact tranche plan / wave set(s), superseding-or-amending the SK-V20 TW manifest
  with the dig's lessons, the lacunae found, the stack abrogation, and the coordination seams.
- `GESTALT-SYNTHESIS.md`, `DEFERRED-LEDGER.md` (updated fold-in), `CONVERGENCE-LEDGER.md` (per-pass
  convergence percentages and what moved them).
- The forward prompt: actionable, executable, honest.
