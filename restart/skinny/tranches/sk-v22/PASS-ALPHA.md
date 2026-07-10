# SK-V22 — PASS-ALPHA: the graduation and the great archival

Date opened: 2026-07-09. HEAD at opening: the SK-V21 addendum commit. The SK-V21 SPEC remains the
performance/architecture plan of record; THIS tranche amends its execution substrate on the
user's direction and dispositions the repository itself.

## The pivot (supersedes SK-V21 TW1a's legacy-split-in-place)

**Skinny matures into the tree of record.** The graduation inverts the porting direction: rather
than splitting the legacy 247K-LOC `crates/core` in place and landing waves into it, the skinny
workspace — the only tree whose architecture survived the SK-V21 adjudications (un-forked
structural dispatch, grammar-derived projections, the lazy cursor, the fused sink prototype, the
NEON kernels) — is hardened into the totality implementation, and the ~40K-line legacy hand seam
(research/38) is ported INTO it selectively. Everything else archives.

**The great archival.** The repository is a four-generation midden: `crates/` (legacy totality),
`audit/`, `playground/`, `wasm/`, `scripts/`, `server/`, `extension/`, `data/`, `docs/` (49-dir
pre-restart corpus), `restart/` (600K-line doc layer), `restart-archive-2026-05-04/`, root
`node_modules`/`Makefile`/`.profiles`, plus a sibling shelf of stale checkouts
(`bbnf-lang-json-threaded-fork`, `bbnf-lang-skv5/6/15-*`, `bbnf-skv16-p1`, `bbnf-wt-b5-*`).
Every one of them receives an archival disposition (branch/tag/subtree/delete) and the graduated
tree gets a clean top-level shape. The SK-V21 evidence corpus (research/01-41, prototypes,
critiques, ledgers, SPEC) is NOT waste — it is the cached intelligence this tranche's leverage
plan distills into the graduated tree's durable documentation.

## Standing corrections (from the user, binding)

1. **The playground archives outright.** It will be re-invented; its docs are out of date and
   poor. The TW-P "rebuild the existing SPA" framing in ADDENDUM-GREENFIELD-PRUNE §2 is
   superseded: the future shopwindow is greenfield; only the skinny-backed wasm prerequisite
   survives as a concept.
2. **bbnf-buddy EXISTS** — at `~/Programming/bbnf-buddy` (its own repo, active: 2026-07-06 morph
   bump, glass-ui, own SPEC/DESIGN, playwright baselines). The SK-V21 census claim ("memory-only
   aspiration") was scoped to this tree and is corrected: the mascot is a live sibling project.
3. **parse-that's TypeScript implementation is a constellation backbone** — consumed by value.js,
   latex-paper, and keyframes. It is not bbnf's to fold in or retire; bbnf's seams to it are the
   canonical Rust regex engine (consumed), the NO-GO letter (dated), and the PT-E1 diagnostics
   design (mirrored when bbnf grows an error surface).
4. **We must profile — under load, with accounting.** The loadavg<2 gate being environmentally
   unachievable does not excuse profiling: PMU cycles/byte with paired per-sample interleaving,
   per-block loadavg recording, multi-window repetition, and load-robust ratio certification are
   the method (validated at SK-V21 pass 2). A load-accounted profile of skinny at HEAD is this
   tranche's empirical leg and the graduation baseline.

## The questions this tranche answers

- What does skinny LACK to be the tree of record (walls in-tree, value plane, kernels, fleet,
  oracles, xtask/regen, edition 2024), and which legacy hand-seam pieces port in (bbnf-path? the
  LSP/VM constellation? egraph/csp seams?) — with the SK-V21 wave manifest re-mapped onto the
  graduation path (what survives unchanged, what re-homes, what dies).
- What of xtask: the regen-host pattern (B2's cure) rebuilt against the one generator; the legacy
  per-grammar fork archives with the legacy tree.
- The archive mechanics: branch + tags vs subtree vs deletion, per surface; the new top-level
  shape; the new documentation home (restart/ archives too — tranche docs need a durable home).
- The leverage plan: how the 41-report corpus, prototypes, ledgers, and workflow journals become
  the graduated tree's ARCHITECTURE / PERFORMANCE-THESIS / NEGATIVE-RESULTS / BENCH-PROTOCOL
  documents and a navigational evidence index — so no lesson is re-learned and no dead end re-run.

## Constraints (unchanged, binding)

NOT an implementation phase — tranche development only; prototypes worktree-isolated, never
landed. NO workarounds, NO legacy code carried into the graduated tree. Batches of three agents
in parallel. Core model orchestrates and synthesizes; Opus/Sonnet fan out. Critique + hardening
before freeze; deferred items folded, none re-deferred without a named wave. Benchmarks cold,
ratio-certified, loadavg-recorded.
