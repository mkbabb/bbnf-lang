# B7 — Per-Agent Dispatch Templates

This document carries one concrete dispatch template per agent
across the three waves. Each template is self-contained: an agent
dispatched with the template plus the wave spec at `waves/W<N>.md`
plus `docs/instructions/README.md` plus
`docs/instructions/tranche/SPEC.md` has every input it needs.

The orchestrator substitutes the worktree path field per dispatch
and reviews the wave's hard-gate items before dispatch to confirm
the agent has runtime evidence for each.

## 10-agent dispatch table

| Agent | Wave | Repo | Scope | Wall cap | Allow-list |
|-------|:----:|------|-------|---------:|------------|
| **A1** | W0 | `bbnf-lang` | simd-scan divan migration | 30 min | `crates/simd-scan/{Cargo.toml,benches/stage1_throughput.rs}` |
| **A9** | W0 | `bbnf-lang` | Test-surface partition (re-route `bench_lsp_actions` from `crates/lsp/tests/bench_lsp.rs` to `crates/lsp/benches/`; sample-cap simd-scan proptest fuzzers via `PROPTEST_CASES=64` env or `proptest-config` attribute) + T1.a doc scrub (AZ-II.W1+W2 merge) | 3 h | `crates/lsp/{Cargo.toml, tests/bench_lsp.rs, benches/}`, `crates/simd-scan/{Cargo.toml, tests/fuzz.rs}`, `docs/tranches/AZ-II/{AZ-II.md, waves/W1.md, waves/W2.md}` |
| **A10** | W0 | `bbnf-lang` | T1.b (BB.W0 e-graph hit-rate gate) + T1.c (AY-III deferred status; AZ-I.W4 + AZ-II.W2 absorb-clauses) + B7 plan docs (B7.md, PROGRESS, AGENT_DISPATCH, waves/{W0,W1,W2}.md, FINAL.md skeleton) | 90 min | `docs/tranches/BB/{BB.md,waves/W0.md}`, `docs/tranches/AY-III/{AY-III.md,PROGRESS.md}`, `docs/tranches/AZ-I/AZ-I.md`, `docs/tranches/AZ-II/AZ-II.md`, `docs/tranches/B7/**` (create), `docs/tranches/REMAINING-TRAJECTORY.md` |
| **A2** | W1 | `parse-that` | Divan migration of 8 bench targets (combinator, micro_parse_that, css, competitor_serde, competitor_pest, competitor_nom, competitor_chumsky, competitor_winnow) | 4 h | `rust/parse_that/Cargo.toml` (divan dep + bench `harness = false` lines), `rust/parse_that/benches/{combinator,micro_parse_that,css,competitor_serde,competitor_pest,competitor_nom,competitor_chumsky,competitor_winnow}.rs` |
| **A3** | W1 | `parse-that` | Divan migration of remaining 8 bench targets (disjoint with A2 by file name) | 4 h | `rust/parse_that/benches/{remaining-8-files}.rs` |
| **A4** | W1 | `parse-that` | CI rewire (nextest + pinned toolchain reading `rust-toolchain.toml`) + `.cargo/config.toml` profile parity (`ay-final`, `bench-ci`) | 90 min | `.github/workflows/ci.yml`, `.cargo/config.toml` |
| **A5** | W1 | `parse-that` | Inline-`#[test]` cleanup: 69 inline tests → `tests/` directory | 3 h | `rust/parse_that/src/**/*.rs`, `rust/parse_that/tests/` |
| **A6** | W2 | `pprint` | Divan migration: `benches/pprint.rs` (~24 bench fns) | 4 h | `rust/Cargo.toml`, `rust/benches/pprint.rs` |
| **A7** | W2 | `pprint` | Divan migration: `benches/digit_count.rs` (~14 bench fns) | 2 h | `rust/benches/digit_count.rs` |
| **A8** | W2 | `pprint` | CI nextest + `.cargo/config.toml` patch resolution (delete redundant `bbnf-ser` patch) | 2 h | `.github/workflows/ci.yml`, `rust/.cargo/config.toml` |

Per-wave parallel-agent counts: W0 = 3, W1 = 4, W2 = 3. All ≤ SPEC
max 6.

---

## W0 dispatch — bbnf-lang internal (3 agents)

### W0.A1 — simd-scan divan migration (30 min cap)

```
You are sub-agent A1 for tranche B7. W0.A1 ports
`crates/simd-scan/benches/stage1_throughput.rs` from manual
`std::time::Instant` timing to `#[divan::bench]` macros.
HARD CAP: 30 min. At 27 min commit the deliverable; at 30 min halt.

## Worktree (ABSOLUTE ROOT — all work here)

`/tmp/b7-bbnf-lang-a1`

Never leave that directory. Never touch
`/Users/mkbabb/Programming/bbnf-lang` — that is the orchestrator's
main checkout. `target/` symlinks to main; `data/` is seeded.

## Memory discipline

Before every cargo invocation:

    export CARGO_BUILD_JOBS=4

Prefer `cargo {test,check} --profile ax-iter` during iteration.
Never run two cargo invocations concurrently.

## Read first (required, in order)

1. `docs/instructions/README.md` — operational directives.
2. `docs/instructions/tranche/SPEC.md` — §Hard gates, §Bench
   contract, §Three-tier command surface.
3. `docs/tranches/B7/B7.md` — tranche plan; invariants, wave
   summary, critical-files table.
4. `docs/tranches/B7/waves/W0.md` — your wave spec.
5. `crates/simd-scan/benches/stage1_throughput.rs` — current entry
   to port; manual `Instant` loop pattern.
6. Any existing divan bench in bbnf-lang as a reference (e.g.
   `crates/core/benches/json/monolithic.rs`).

## Scope — A1 only

1. Add divan as a dev-dep in `crates/simd-scan/Cargo.toml`;
   declare `[[bench]] name = "stage1_throughput" harness = false`.
2. Rewrite `crates/simd-scan/benches/stage1_throughput.rs` so
   every benchmark function uses `#[divan::bench]` (or
   `#[divan::bench(args = [...])]` for parameterized cases) and
   no `std::time::Instant` loop remains.
3. Verify `cargo bench -p simd-scan` runs under divan's harness.
4. Verify `cargo nextest run -p simd-scan --profile ax-iter`
   green.

## File bounds

Allow-list:
- `crates/simd-scan/Cargo.toml`
- `crates/simd-scan/benches/stage1_throughput.rs`

Forbidden:
- Any file outside the allow-list.
- No `#[allow(...)]` introductions.

## Hard gate

1. `cargo bench -p simd-scan` runs under divan harness; output
   shows divan's per-bench ns/iter format.
2. `grep -n 'std::time::Instant' crates/simd-scan/benches/
   stage1_throughput.rs` returns no hits.
3. `cargo nextest run -p simd-scan --profile ax-iter --no-fail-
   fast` exits 0.
4. bbnf-lang main checkout `cargo nextest run --workspace
   --profile ax-iter` returns 1477+/1477+ post-cherry-pick.

## Commit message template

    feat(simd-scan): port stage1_throughput to divan harness
    (B7.W0.A1)

## Return format (≤ 250 words)

- Commit SHA(s).
- divan harness output excerpt (1–2 lines per bench fn).
- Any blocker.
```

---

### W0.A9 — Test-surface partition + T1.a (3 h cap)

```
You are sub-agent A9 for tranche B7. W0.A9 partitions the test
surface so bench-class and fuzz-class tests stop driving the warm
`cargo nextest run --profile ax-iter` wall, and folds the AZ-II
W1 + W2 wave-merge doc scrub (T1.a). HARD CAP: 3 h. At 2.7 h commit
the deliverable; at 3 h halt.

## Worktree

`/tmp/b7-bbnf-lang-a9`

## Read first (required)

1. `docs/instructions/README.md`
2. `docs/instructions/tranche/SPEC.md`
3. `docs/tranches/B6/FINAL.md` — slow-test surface observation in
   §Cross-tranche debt and §W2 narrative.
4. `docs/tranches/B7/B7.md` + `docs/tranches/B7/waves/W0.md`.
5. `docs/tranches/AZ-II/AZ-II.md` + `docs/tranches/AZ-II/waves/
   W1.md` + `docs/tranches/AZ-II/waves/W2.md` — current 3-wave
   shape that merges to 2 + FINAL.

## Scope — A9 only

1. Re-route `bench_lsp::bench_lsp_actions` from
   `crates/lsp/tests/bench_lsp.rs` to a new
   `crates/lsp/benches/bench_lsp.rs` registered as `[[bench]]` in
   `crates/lsp/Cargo.toml`. Commit this re-categorization
   separately from item 2.
2. Sample-cap the simd-scan proptest fuzzers in
   `crates/simd-scan/tests/fuzz.rs`: either
   `PROPTEST_CASES=64` env via `Cargo.toml`'s `[env]` section, or
   per-fn `#[proptest_attr(cases = 64)]` per the proptest crate's
   attribute API.
3. Update `docs/tranches/AZ-II/AZ-II.md` wave-schedule to declare
   3-wave shape (W0 + W1-merged + W2 FINAL). Update
   `docs/tranches/AZ-II/waves/W1.md` to describe the merged
   Stage-A + Stage-B atomic wave with byte-equal close gate.
   Update `docs/tranches/AZ-II/waves/W2.md` to describe the FINAL
   wave (tape deletion + parity recode + BA handoff). DO NOT
   touch the §Hard gates section's W2 absorb-clause subsection
   that A10 owns.

## File bounds

Allow-list:
- `crates/lsp/Cargo.toml`, `crates/lsp/tests/bench_lsp.rs`,
  `crates/lsp/benches/`
- `crates/simd-scan/Cargo.toml`, `crates/simd-scan/tests/fuzz.rs`
- `docs/tranches/AZ-II/AZ-II.md` (wave-schedule + W1 / W2
  narrative sections only; A10 owns the §Hard gates W2 absorb-
  clause subsection)
- `docs/tranches/AZ-II/waves/W1.md`,
  `docs/tranches/AZ-II/waves/W2.md`

Forbidden:
- `docs/tranches/AZ-II/AZ-II.md` §Hard gates §AZ-II.W2 absorbs
  durable AY-III gates subsection (A10 owns).
- Any other file.

## Hard gate

1. `bbnf-lsp::bench_lsp::bench_lsp_actions` absent from `cargo
   nextest run --workspace --profile ax-iter` output.
2. simd-scan fuzz tests sample-capped (`PROPTEST_CASES=64` or
   `#[proptest_attr]`); each fuzz test runs in < 1 s.
3. Warm `cargo nextest run --workspace --profile ax-iter` 3-run
   median ≤ 14 s (post-A1 + A9 cherry-pick).
4. AZ-II.md declares 3-wave shape (W0 + W1-merged + W2 FINAL);
   waves/W1.md and waves/W2.md describe the merged shape.
5. T1.a doc scrub commits separable from the source-side
   re-categorization commit.

## Commit cadence

Three commits expected:
- One for the bench_lsp re-categorization.
- One for the simd-scan fuzz sample-cap.
- One for the AZ-II.md wave-merge doc scrub.

## Return format (≤ 250 words)

- Three commit SHAs.
- Warm nextest wall 3-run median value.
- Any blocker.
```

---

### W0.A10 — T1.b + T1.c + B7 plan docs (90 min cap)

```
You are sub-agent A10 for tranche B7. W0.A10 lands three doc
deliverables: T1.b (BB.W0 e-graph hit-rate gate), T1.c (AY-III
deferred + AZ-I.W4 + AZ-II.W2 absorb-clauses), and the B7 plan
document set (B7.md + PROGRESS + AGENT_DISPATCH + waves/
{W0,W1,W2}.md + FINAL.md skeleton). HARD CAP: 90 min. At 81 min
commit; at 90 min halt.

## Worktree

`/tmp/b7-bbnf-lang-a10`

## Read first (required)

1. `docs/instructions/README.md`
2. `docs/instructions/tranche/SPEC.md` — §Document set, §Plan
   structure.
3. `docs/tranches/B6/B6.md` + `docs/tranches/B6/PROGRESS.md` +
   `docs/tranches/B6/AGENT_DISPATCH.md` + `docs/tranches/B6/
   FINAL.md` — convention reference.
4. `docs/tranches/BB/BB.md` + `docs/tranches/BB/waves/W0.md` —
   T1.b target.
5. `docs/tranches/AY-III/AY-III.md` +
   `docs/tranches/AY-III/PROGRESS.md` — T1.c targets.
6. `docs/tranches/AZ-I/AZ-I.md` + `docs/tranches/AZ-II/AZ-II.md`
   — T1.c absorb-clause targets.
7. `docs/tranches/REMAINING-TRAJECTORY.md` — path-change diagram.

## Scope — A10 only

1. **T1.b — BB.W0 hit-rate gate.** Add a hard gate to W0:
   "E-graph rule hit-rate measurement on the corpus (per-rule
   firing count per parse, summed across the 4 primary
   grammars). Verification artefact:
   `docs/benchmarks/post-BB-W0-hit-rate.json`. Floor: rule-
   firing rate ≥ 0.1 per parse for any rule retained; rules with
   hit-rate <0.1 retire per the e-graph cost model. Cite samply
   attribution showing rule firings register in the parse hot
   path." Update BB.md wave schedule table.
2. **T1.c — AY-III deferred + absorb-clauses.** Add status
   header to AY-III.md; append deferral entry to AY-III/
   PROGRESS.md; append AZ-I.W4 absorb-clause to AZ-I.md; append
   AZ-II.W2 absorb-clause to AZ-II.md (HARD-GATES section
   only — A9 owns wave-schedule + W1/W2 narrative); annotate
   REMAINING-TRAJECTORY.md path diagram.
3. **B7 plan docs.** Author B7.md, PROGRESS.md, AGENT_DISPATCH.md,
   waves/W0.md, waves/W1.md, waves/W2.md, FINAL.md skeleton.

## File bounds

Allow-list (write):
- `docs/tranches/BB/BB.md`, `docs/tranches/BB/waves/W0.md`
- `docs/tranches/AY-III/AY-III.md` (header + status only),
  `docs/tranches/AY-III/PROGRESS.md` (append)
- `docs/tranches/AZ-I/AZ-I.md` (append W4 absorb-clause)
- `docs/tranches/AZ-II/AZ-II.md` (append W2 absorb-clause in
  hard-gates section only)
- `docs/tranches/REMAINING-TRAJECTORY.md` (annotate)
- `docs/tranches/B7/**` (create)

Forbidden: every other file.

## Hard gate

1. `find docs/tranches/B7/ -type f | wc -l` ≥ 6 (B7.md, PROGRESS,
   AGENT_DISPATCH, waves/{W0,W1,W2}.md, FINAL.md).
2. `cargo nextest run --workspace --profile ax-iter --no-fail-
   fast` returns 1477+/1477+ (no source changes; doc-only).
3. `cargo xtask regen --check` exit 0 across 9 grammars.
4. 3 commits.

## Commit cadence

Three commits expected:
- T1.b BB.W0 hit-rate gate.
- T1.c AY-III deferred + AZ-I.W4 + AZ-II.W2 + trajectory.
- B7 plan + PROGRESS + AGENT_DISPATCH + waves + FINAL skeleton.

## Return format (≤ 250 words)

- Three commit SHAs.
- Files created / edited.
- Any blocker.
```

---

## W1 dispatch — parse-that (4 agents)

A2, A3, A4, A5 dispatch in parallel into the
`/tmp/b7-parse-that-{a2,a3,a4,a5}` worktree pool. Each carries a
self-contained prompt mirroring the A1 / A9 / A10 structure above
with sibling-repo paths (`/Users/mkbabb/Programming/parse-that/...`)
and the corresponding scope from the dispatch table. The orchestrator
authors per-agent dispatch templates at dispatch time per SPEC §Agent
briefing; the table above plus `waves/W1.md` carries the
declarative scope.

## W2 dispatch — pprint (3 agents)

A6, A7, A8 dispatch in parallel into the
`/tmp/b7-pprint-{a6,a7,a8}` worktree pool. Sibling-repo paths
(`/Users/mkbabb/Programming/pprint/...`) and the corresponding
scope from the dispatch table. The orchestrator authors per-agent
dispatch templates at dispatch time per SPEC §Agent briefing.

## Cross-wave coordination

The orchestrator coordinates cross-repo activity at two boundaries:

1. **Wave open.** Each pool's worktrees are pre-created from each
   repo's master HEAD before its first dispatch.
2. **Wave close.** Per-repo cherry-pick discipline: bbnf-lang's
   master advances on W0 commits; parse-that's master advances on
   W1 commits; pprint's master advances on W2 commits. The
   orchestrator does not wait on cross-pool cherry-picks; each
   pool advances independently.

Cross-repo verification at tranche close: the bbnf-lang main
checkout's warm nextest wall reads the post-W0 lever; the
parse-that and pprint masters carry their own CI green signals;
the bbnf-ser patch graph collapses to a single source of truth
verified by `grep -F bbnf-ser /Users/mkbabb/Programming/bbnf-lang/
.cargo/config.toml /Users/mkbabb/Programming/pprint/rust/.cargo/
config.toml` — exactly one hit, in bbnf-lang.
