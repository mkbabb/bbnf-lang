# Era III — Grammar / Regex / CSP Substrate (2026-03-16 → 2026-04-09)

Era III is the tranche era where *the IR becomes a compiler*. The pass
structure, the cost model, the e-graph rewrite engine, the CSP-scheduled
optimiser, the bespoke regex HIR, and the NodeId-keyed analysis lattice
all land in Era III. Every downstream era builds on this substrate.

Commit density: ~280 commits. Tranches F, G, H, I, J, K, L, M, N, O,
P, Q, R, S, T, U, V, W all belong here (18 letter-codes, many single-
commit, some tens of commits). Tranche discipline is *one-letter-per-
cohesive-refactor* — a tranche is a commit-message tag, not yet a
dispatch-and-waves protocol.

## Architectural thesis

Every optimiser decision should be CSP-scheduled against a shared
CostModel, with the e-graph as the permanent secondary optimiser and
the hand-written normalizer as the primary. Per-rule decisions live
in `NodeFacts` keyed by `NodeId`; pointer identity is purged. The
regex engine is built in-house (`bbnf-regex`) with a HIR layer so
grammar transformations can fuse and canonicalise regexes like any
other IR node.

The thesis is encoded across three inflection commits:

- `bfa50f25` (2026-04-08) `refactor(ir): four-layer optimizer —
  normalizer primary, e-graph permanent secondary` — establishes the
  optimisation layer cake.
- `ce9d213b` (2026-04-08) `refactor(ir,backend): NodeId-keyed
  node_facts and alt_strategies` — pointer identity purged from
  analysis passes.
- `6becbf8b` (2026-04-09) `fix(egraph): scheduler correctness —
  measure real work done (Tranche J)` — LLVM-style `Changed` bool
  replaces structural-hash fixed-point (see feedback memory
  `changed-flag-convergence`).

## Tranche ledger

| Letter | Commits | Headline | Verdict |
|---|---:|---|---|
| F | single | `refactor(backend): pre-solve delim_scan + key_dispatch per-grammar` (`a3fadf56`) | Worked — subsumed into later compile-time CSP. |
| G | bulk | `bbnf-derive` split, `egraph-derive` crate | Worked — still live in 2026-04-22. |
| H (H-5, H-7, H-DAG) | ~4 | E-graph rewrite rules: factor_common_prefixes, merge_regex_alts, inline_acyclic, EliminateEpsilon | Worked — rewrite rules durable. |
| I | ~3 | IR orphan-entry-point purge + DAG-build invariants + bench sweep | Worked. |
| J | 1 | E-graph scheduler correctness — measure real work done (`6becbf8b`) | Worked — anchors the `changed-flag-convergence` memory. |
| K | 1 | CSP-scheduled e-graph rewrite execution (`a5991bac`) | Worked — CSP-as-scheduler thesis. |
| L | 1 | `NodeId everywhere — purge pointer-identity from passes` (`f6119e0b`) | Worked — invariant held. |
| M | 1 | Delete dead `GrammarAnalysis` lattice (`359eb068`) | Worked. |
| N | 1 | Deterministic codegen via `IndexMap` (`9932d348`) | Worked — non-determinism eliminated. |
| O, P | dir-only | No tranche-tagged commits; work bundled into adjacent letters. | N/A |
| Q | 1 | bbnf-ir god-module splits (6 files → directories) (`5e408f04`) | Worked. |
| R | 1 | bbnf core god-module splits (`2d326760`) | Worked — anchors the `no-god-modules` + `directory-module-structure` memories. |
| S | 1 | bbnf analysis + ser god-module splits (`b2c26511`) | Worked. |
| T | dir-only | | N/A |
| U | 1 | Documentation alignment + post-N bench baseline (`bab4405f`) | Worked. |
| V | 9 | Recognizer mining pipeline — `patterns`, `recognizers`, `csp_recognizers` with `NodeFacts.recognizer` (`fd7c2ce1` → `47661314`) | Worked — mining + decision substrate. |
| W | 11 | CSP-driven cost model + real `MinimizeCost` replaces `csp_recognizers`; kernel family modules; per-NodeId recognizer decisions; hot-path clone elimination; `derive(Copy)` on parser enum | Worked — CSP substrate goes live. |
| X | 20 | Spans over prior-tranche, plus Lever 4 decisions + bench scaffolding | Worked; some items reappear in AX.W0b carve. |

## What landed durably

- **E-graph rewrite system** (`555456ff`, `bf53c93d`, `03cbba99`,
  `1a640855`, `bb9e8aea`, `a1e096d2`, `9b828b1f`, `1bac615e`,
  `6345dbcd`) — eight rewrite rules landed in a single day. All
  remain active through AX.
- **CostModel + CostWeights substrate** (`88d8b239`) — shared weights
  for cost-aware scheduling.
- **NodeFacts + recognizer mining** — the `Flat | Wrap | ArgList |
  HRegex | Pratt | Unordered` shape vocabulary that AU-AX gate
  predicates consume is downstream of V's mining output.
- **Bespoke regex HIR** (referenced by feedback memory
  `bespoke-regex-hir`; HIR landed as part of `bbnf-regex` crate in W
  and spans to X) — replaces `regex-syntax` with an explicit
  `Negated` flag and hand-written parser; the "regex HIR" is one of
  the two named durable breakthroughs per `perf-breakthrough-
  accuracy` memory.
- **Delim-scan pre-solved per-grammar** (`a3fadf56`) — the other
  named durable breakthrough; compile-time CSP decides delimiter
  classes that become inner-loop constants.
- **`IndexMap`-backed deterministic codegen** — `generated.rs` bytes
  stabilise; clean-regen discipline (`clean-regen-discipline` memory)
  has a substrate from N onward.

## What was reverted or superseded

- **`simplify_regex_algebra` + early `merge_regex_alts`** deleted at
  `c7269f6b` (Tranche H-7) once the e-graph subsumes them.
- **`project_types` test-time fallback** deleted at `834dccf1`
  (H-DAG) once DAG ordering makes it unnecessary.
- **`recognize_patterns` standalone pass** replaced by the V.4 mining
  pipeline (`ba61a25e`).

## Salvageable artefacts (all still present at 2026-04-22)

- `crates/ir/src/passes/` — the pass layer cake.
- `crates/ir/src/egraph/` — rewrite engine + rules.
- `crates/egraph-derive/` — `Language` impl derive macro.
- `crates/bbnf-regex/` — bespoke HIR crate.
- `crates/csp-solver/` — general-purpose CSP solver (the `csp-solver-
  crate` project memory: split into csc411 at some point; used here
  as local path-dep).
- `NodeId` keying throughout pass surface.

## Transition into Era IV

Era III closes with commit `f86be004` (2026-04-09) `docs(bench):
post-W baseline (Tranche W complete)`. The CSP + cost-model + e-graph
substrate is in place but the runtime is still a recursive
`fn __<rule>` descent over `Parsed` values.

The decision that opened Era IV is visible in tranches Y + Z + AA:
Y splits tape columns, Z lands the tape-first buffer idea in embryo,
and AA interns `TypeDesc` values. Together they form the thesis:
*treat the parse output as a columnar tape rather than a tree of
allocated `Value` nodes*. Era IV is the execution of that thesis.
