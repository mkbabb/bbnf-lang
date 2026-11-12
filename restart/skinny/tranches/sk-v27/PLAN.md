# SK-V27 "LOCKSTEP": the programme plan

**Status:** PLANNED on 2026-11-05. LW1 may start now. LW2 and later waves wait on the owner rulings in §2.
**Authority:** the owner's order of 2026-11-05 (quoted in §0). It resumes the sk-v programme that `sk-v25/HANDOFF.md` left paused ("transition suspended pending owner resume").
**Inputs:** seven adjudicated final designs, one per lane: compiler, rust-perf, regex, pprint, competition, consumers and programme. Each design has two critics and an adjudicator, and every objection was ACCEPTED or accepted in part.
**Unit ledger:** `LEDGER.md` in this directory. It maps every adjudicated item to a unit, a wave and a falsifier. Nothing in a design is dropped without a ledger row.
**Measured so far:** nothing. Every number in this plan is a target, a proposed floor or a recorded fact. Floors are frozen only at LW2's feasibility seam (unit LW2.CON-2).

## 0. The order

> "Ensure that we continue to work and iterate on parse-that and bbnf in a workflow, too, with those tranches, and our extant items from our megatranche and in flight work, uninoed thereof. What of our perf? What of our compiler? What of our rust implementation and CSP, egraph, etc--optimization passes? What of our new IR? What of our competition with the other compilers and combinator/parser-generators we're to beat, and what of our beating of simd-json and the like with our deep DAV1D-like SIMD heuristics leveraging esoteric and black-magic ASM? We should have a perfected backend for the bbnf compiler system. What of our regex implemnetaoin for both parse-that and bbnf? what of our pprint system and formatting? Spawn many workflows for these analysis in a series of waves for those systems in perfected lock-step without losing track of the extant daraksha, value, fourier, or keyframes.js work." (owner, 2026-11-05)

Standing law carried into every seat:
- "NO quick solutions, NO workarounds: idiomatic, gestalt approaches."
- "No custom grammar, unless it's BBNF."
- parse-that is a general library and carries no BBNF vocabulary.
- The TS BBNF compiler lives in `bbnf-lang/typescript/`.
- Opus 5.5 for every seat (M-15: Codex abrogated). Every spawn declares its model explicitly, and the recorded model is checked against the declaration (the sk-v25 §0 model-truth gate never retires).
- The begin-word authorizes push and publish. Publishing still follows the release DAG in §5 and the ownership holds in §1.

## 1. Facts recorded at planning (re-verified 2026-11-05)

| Fact | Value | Consequence |
|---|---|---|
| bbnf-lang local `master` | `af15f63e0`, 71 ahead and 38 behind `origin/master` `c9b93adb4` | Fast-forward is impossible. The order's "fetch and fast-forward" step cannot be carried out as written. The lawful route is push to a lineage branch, then an ordinary merge (LW1.TOPO-3, LW1.TOPO-5). |
| bbnf-lang primary checkout | 243 dirty entries, including the untracked sk-v25 corpus (~1.7 GiB) | Read-only. It is captured, never mutated, and its branch is never switched. |
| sk-v25 / sk-v26 docs | sk-v19..sk-v25 are on local `master` only; sk-v26 is filesystem-only (its own STATE.md says "FILESYSTEM_ONLY") | Custody comes first (LW1.TOPO-3). |
| sk-v26 branches | `codex/sk-v26-bbnf` `b5fd0ca98` (worktree `~/Programming/bbnf-lang-skv26`); `codex/sk-v26-parse-that` `e31fbfe` (worktree `~/Programming/parse-that-skv26`). Neither is on origin. | Push both in LW1. Mine them; do not adopt them wholesale (rust-perf R0-d). |
| parse-that `origin/master` | `47e928acc` (not the `7098c86` the order cites); 35 dirty paths in the primary checkout | Every SHA in this plan is re-derived from the LW1 heads receipt. |
| pprint `master` | `2b4d2d4`, 39 ahead of origin | Pushed to a lineage branch in LW1. |
| Toolchain | bbnf-lang pins `nightly-2026-04-11` plus cranelift. `.cargo/config.toml` forces `rustc-wrapper = "sccache"` and `[build] target = "aarch64-apple-darwin"`. | No x86 CI job can execute x86 code today. LW2.TOPO-2 fixes it. |
| CI | bbnf-lang runs on `macos-14` only; parse-that runs on `ubuntu-latest` only | Each repo gets a runner matrix in LW2. |
| Repo cycle | parse-that `rust/regex` takes `egraph`, `egraph-derive` and `csp-solver` 0.1 from crates.io; all three live in bbnf-lang, and bbnf-lang takes `parse_that` 0.4 | The crate graph is acyclic, but the repo graph has a cycle. LW2 removes it (ruling X-5). |
| CSP | `let _ = csp.propagate()` at 6 sites; `InlineDomain::join` is first-writer-wins and not commutative | Born-RED in LW1, cured in LW2. |
| Records on file | skinny `RESULTS.md` canada rows win, but they were built NATIVE plus scalar (`simd=Scalar`) | No record proves anything about SIMD dispatch. Competition starting position: **no certified wins**. |
| wf_d837f96c (xp-w9-w10-continue) | value.js COHESION §0hz says "The xp-w9-w10-continue workflow is DONE" | LW1.TOPO-2 confirms the close by record. Until then the holds below stay in force. |
| value.js | `latest` 4.2.1 on npm; the 4.3.0 release commit `d5a8fbc0c` is cleared to cut (§0hy) but unpublished | The consumers lane observes 4.3.0 and never publishes value.js. |

**Ownership holds (from competition C2-2).** Until wf_d837f96c's close is on record, only that workflow edits:
- `bbnf-lang/typescript/**`
- `bbnf-lang/.github/workflows/*release*`
- `parse-that/.github/workflows/**`
- `parse-that/typescript/package.json` and `parse-that/CHANGELOG.md`
- value.js `package.json` and `pnpm-lock.yaml` (the BBNF and parse-that pins), `gen-grammar.mjs` and the generated grammar modules

An edit to a held path lives on branch `contest/ci`. It merges only after the close commit, or after an ACK commit on wf_d837f96c's branch that names the `contest/ci` SHA.

**App-work holds.** megatranche-realization, x-0hr-residuals and daraksha-v3 own their repos' app work. This programme touches value.js only to append COHESION pointer lines, and never touches value.js `scripts/dev/dev.sh`.

## 2. Owner rulings

The seven designs disagree on five structural points. Each disagreement is stated below with the default this plan adopts. A default is binding only once the owner confirms it. LW1 depends on none of them.

| # | Question | Where the designs disagree | Default (this plan) | Needed by |
|---|---|---|---|---|
| **X-1** | Compiler of record | compiler R1 and programme R-4: the sk-v26 spine. rust-perf R0-e: skinny's `ir`, `passes`, `codegen` and `runtime`. | **The sk-v26 spine is the carrier.** skinny is a lab: its IR, kernels and passes are candidates that the LW1 crate census (LW1.COMP-1) admits by row. The root pipeline is a quarry, re-admitted only as passes that win a trial (C2-5: a root trial can only *delete*). | LW2.COMP |
| **X-2** | Regex engine: home, name and analysis | compiler C2-3: rename to `parse-that-regex`, with a `RegexFacts` adapter in bbnf-lang. rust-perf: one crate, `parse-that/rust/regex`. regex design: `bbnf-regex` owns Hir, RegexInfo and Plan. consumers #3: lift `crates/core/src/generate/regex` into a bbnf-lang `crates/regex`. programme C1-3: the engine stays in parse-that. | **One engine package, in parse-that, generically named `parse-that-regex`.** It owns the parser (BBNF-generated), Hir, `to_source`, RegexInfo (including `end_unambiguous`), Plan and Budget. bbnf-lang keeps a thin adapter (`bbnf-regex-facts`) and the emitters: the DFA emitter lifted out of `crates/core/src/generate/regex` and the JS RegExp printer. Falsifier: exactly one regex engine package in the combined workspace graph, and 0 `RegexInfo` definitions outside it. | LW2.RX |
| **X-3** | SIMD kernel library home | compiler C2-2: merge into `bbnf-kernels`. rust-perf R0-f: `parse-that/rust/simd`. competition C1-3: `crates/simd-scan` becomes the library. regex: `parse_that::scan`. | **`parse-that/rust/simd` (crate `parse-that-simd`)**, formed by merging `crates/simd-scan`, `skinny/crates/simd-scan` and `skinny/crates/bbnf-simd`. It is the only `core::arch` code in either repo. `parse_that::scan` is its public face. | LW2.PERF |
| **X-4** | x86 policy | compiler R2: an x86 kernel needs a measured win on its runner or it is deleted. programme R-1, competition C2-6 and consumers O-8: x86 for correctness only; performance is claimed only on this Mac. regex C1-7 and the order itself ask for `is_x86_feature_detected!`, which conflicts with O033. | **One `OnceLock` dispatch table: NEON (primary), AVX2, AVX-512 and wasm32-simd128, all over one scalar oracle.** An x86 tier merges only if CI executes it, with its oracle and fuzz target, on hardware that has the ISA (ubuntu-latest for AVX2; Intel SDE for AVX-512, or the AVX-512 kernel is deleted). x86 is never certifying. compiler R2's "measured win" clause is read as follows: an x86 tier with no measured win on its runner (shared-runner numbers, labelled observational) and no correctness role is deleted. O033 is retired by addendum. | LW2.PERF |
| **X-5** | egraph, egraph-derive and csp-solver home (the repo DAG) | rust-perf R0-f: egraph to parse-that, csp-solver stays in bbnf-lang. competition U1a: a new repo, `mkbabb/egraph-csp`. programme R-8: all three into the parse-that workspace. regex §A: generic published leaves. | **programme R-8: all three move into the parse-that workspace as standalone general crates.** Repo DAG: `pprint → parse-that → bbnf-lang`. The alternative, a leaf repo `mkbabb/egraph-csp`, is recorded for the owner. | LW2.TOPO |
| X-6 | Disposition of local master and the dirty tree | rust-perf R0-a gives options (a) merge all 71 commits and (b) cherry-pick `-x`. R0-b: patch plus tarball. compiler R4: commit the dirty tree to a preserve branch. | **(a): an ordinary merge of all 71.** The dirty tree is captured read-only in LW1. A preserve commit is made through an alternate index, with no checkout switch and no stash. The owner chooses commit-on-branch or discard per entry. | LW1.TOPO-5 merge to master |
| X-7 | Bench window | rust-perf R0-g, competition C1-7 and C2-3, programme C1-6, pprint C2-O8c | **(i): the orchestrator schedules the window.** While a `guarded --exclusive bench` seat holds the lock, no heavy seat is dispatched. A record waits at most 60 min for a 1-minute load below 8; after that it runs and is labelled NOISY. Each certification unit has a 72 h budget and is re-queued once. A guard `--drain` class is the owner's machine law and is not built by this programme. | LW2.CON |
| X-8 | Canonical CSS grammar | consumers P-1 | value.js's six WPT-gated grammars are canonical. bbnf-lang `grammar/css/l4` is retired in favour of `grammar/corpus/value-js/`, pinned to a released value.js tarball with checked hashes. | LW3.USE |
| X-9 | Workflow slots | programme R-7, against the 4-workflow cap | This programme holds at most 2 slots while megatranche-realization, x-0hr-residuals and daraksha-v3 are open. Waves run in series; lanes within a wave run as seats, not as separate workflows. | every wave |
| X-10 | Formatter-parity diff budget (G7/G8, gorgeous against Prettier/Biome) | pprint C1-O4 | Set by the owner before any throughput claim in the formatter-parity plan (LW4.PP-4). | LW4 |

## 3. Lanes

| Lane | Scope | Design |
|---|---|---|
| **TOPO** | custody, heads and ownership receipts, history merge, toolchain, CI matrices, repo DAG, releases | programme, rust-perf §0, competition U0/U1 |
| **COMP** | the BBNF compiler backend: Syntax → Program → Graph → TypeSolution → AnalysisFacts → OptIR → sealed LoweringPlan → emitters; CSP, e-graph, left recursion, oracles; consumers of the IR (LSP, wasm, playground, extension) | compiler |
| **PERF** | the Rust runtime and the `parse-that-simd` kernels: dispatch, NEON, AVX2/AVX-512, simd128, `asm!`, the structural indexer, canada attribution | rust-perf |
| **RX** | the regex engine for parse-that and bbnf: semantics, oracles, determinizer (OR-7), Plan, codegen, convergence | regex |
| **PP** | pprint 0.4.0: ABI v2, Wadler semantics, trivia, renderer, `pprint-ser`, the stream format, gorgeous, `@pretty` | pprint |
| **CON** | the contest: harness, pins, PREREG, planes, certification against simd-json, sonic-rs, serde_json, winnow, nom, pest, chumsky, tree-sitter, cssparser, lightningcss, Lezer and the JS tokenizers | competition, plus the statistics rulings of every lane |
| **USE** | consumers: value.js, keyframes.js, fourier, glass-ui, daraksha, sci-report; the benefit table; consumer cures | consumers |

## 4. The lockstep gate (identical at the end of every wave)

A wave closes only when every row is GREEN, read twice, at SHAs from that wave's heads receipt. A row that is RED is either cured inside the wave or carried as a named ledger row with an owner ruling. A row is never waived silently.

| # | Row | Measure |
|---|---|---|
| LG-a | Cross-repo build | pprint → parse-that → bbnf-lang build and test from clean clones at pinned git revs (LW1, LW2) or published versions (LW6 onward), with no `[patch]` path overrides and no sibling checkout. From LW2 on: `cargo +stable` rc 0. |
| LG-b | TS↔Rust parity | The parity corpus (grammar `MANIFEST` hash) gives the same accept/reject, the same `obs(tree)` and the same failure contract (furthest offset plus sorted expected set) on the TS 0.3.x package and the Rust backend. LW1 records the baseline; from LW3 on, 0 unruled divergences. |
| LG-c | Consumer gates | value.js V-C (unruled misses do not rise from 66), MIRROR-DEFECTS 0, E-6 within 15,200 / 6,100 gz, self-contained 0/0; keyframes.js check, lint and vitest; fourier's own suite (`COMPUTE_CONCURRENCY=1`). Each is read on the consumer's own CI or under `guarded`, never by this programme editing the consumer. |
| LG-d | Benefit table | One row per consumer: bytes (raw and gz), parse ms (median and IQR, NOISY or QUIET), conformance (WPT and V-C denominators), hand code deleted (lines). Each row is a delta from the LW1 baseline. |
| LG-e | Competition table re-read | Every cell is re-read under its plane, build matrix (PORTABLE or NATIVE), track (`generated` or `hand`) and status (CERTIFIED, OBSERVATIONAL, NOISY, NOT-COMPARABLE or DESCRIPTIVE). No cell is promoted without a PREREG entry and a Holm-adjusted CI that clears the minimum effect (3%). |
| LG-f | Machine law | Every heavy run went through `guarded`, bench seats used `--cpu-cores 0` and `--exclusive bench`, and no stray processes remain (process census at wave close). |
| LG-g | Custody | All wave branches are pushed, the ledger is updated, and the COHESION pointer is appended (value.js §0hx.N) along with parse-that `docs/tranches/SK-V27.md`. |

**Downstream release order (whenever a wave releases):** bbnf-lang → parse-that → value.js → keyframes.js. Each consumer is re-pinned by PR, its gates are re-read, and the consumer is notified (COHESION pointer line plus a commit on its branch). The crates.io DAG under it is fixed by X-5: egraph/csp leaves → pprint → parse-that → bbnf-lang. No crates.io publish happens before LW6 (U-REL).

## 5. Waves

Each wave lists its units by lane. Unit ids are `LWn.LANE-k`. The falsifier for each unit is in `LEDGER.md`.

### LW1: truth seal and custody (starts now; no ruling needed)

Purpose: every repo's history is safe on origin, every claim has a SHA, every known defect has a born-RED test, and every lane has a baseline. No product behaviour changes in this wave.

- **TOPO**
  - TOPO-0: file the seven full adjudicated design texts in `designs/`. The dispatch copies were cut off at compiler §E, rust-perf §1, regex §A, pprint L9, competition L4t, consumers G-J1 and programme R-8.
  - TOPO-1: heads receipt. `git ls-remote` on all three repos plus local heads go into `~/.dev-logs/bbnf-lockstep/heads-W0.json` and `CUSTODY-RECEIPT.tsv`.
  - TOPO-2: ownership record (`ownership-W0.json`). Confirm wf_d837f96c's close by record and the state of the 63 `.claude/worktrees` and wf_fce2ec2b.
  - TOPO-3: custody push. `af15f63e0` → `sk-lineage/local-master`; `codex/sk-v26-bbnf`; `codex/sk-v26-parse-that`; pprint `master` → `sk-lineage/pprint-local`. The dirty trees are captured read-only: a binary patch, a tarball of untracked files, and a sha256 manifest in `~/.dev-logs/bbnf-lockstep/custody/`. Blobs over 5 MB go to release assets on tag `custody-sk-v25`. The sk-v26 packet is committed on its branch.
  - TOPO-4: divergence classification (`git log` in both directions) and `grammar/MANIFEST` (path plus content hash; excludes `.claude/worktrees`, `restart/` and `target/`).
  - TOPO-5: the history merge. In worktree `~/Programming/bbnf-lang-wt/merge-skv25`, branch `lockstep/merge-skv25` off `origin/master`, make an ordinary merge of `sk-lineage/local-master`. The only expected conflict is `.gitignore`. It merges to master after CI.
- **COMP**
  - COMP-1 (CB-0.5): census of every crate in the root workspace, skinny and sk-v26, with fate (absorb, keep or delete) and owning unit. Includes the 15 sk-v26 compiler modules.
  - COMP-2: Opus re-certification of the sk-v26 spine. R-3 code receipts (W2 spine, ABI-3) are re-proved by their tests at the merged HEAD.
  - COMP-3: born-RED tests. The contradictory-constraint grammar at the 6 `propagate` sites; the `LatticeDomain` law test (commutative, associative, idempotent, monotone) RED on `InlineDomain`; the I7 generality lint RED, if any plan decision reads a rule name.
  - COMP-4 (CB-1a): hermetic build. `git = …, rev = <sha>` edges and no `../` paths.
- **PERF**
  - PERF-1: a born-RED test that each dispatch tier is reachable on a default build. It is RED today, because `simd-scan`'s `cfg(target_feature)` gates hide the AVX2 and AVX-512 modules.
  - PERF-2: `cargo +stable build` probe on both repos. The RED is recorded; the cure is in LW2.
  - PERF-3: RP-13 canada 1.56× trace-or-retract. Find the plane, API and HEAD it was measured on, or retract it.
- **RX**
  - RX-1: oracle harness. V8 (node) and JSC (bun), one guarded subprocess per run, on valid Unicode scalar values only, with UTF-16↔UTF-8 offset maps.
  - RX-2: born-RED tests. `/\d+(?!\.\d)/y` on `"12.5"` (C1-1); fused `Alt(/a/,/ab/)` on `"ab"`; `SupersetAbsorbAlt` on `[é]` / `[α-ω]`; Kelvin under `/i` and `/iu`.
  - RX-3: lookaround census of the 121 value.js leaves and the sk-v24 56.
  - RX-4: re-ask the UNREAD objections C1-9 (G-P1) and C2-6 ("Changing semantics breaks e…").
- **PP**
  - PP-1: re-grep `Mediumline` across gorgeous, the emitters and `typescript/`.
  - PP-2: born-RED tests. `flat_width` u16 overflow (group > 65,535); L3′ counterexamples (a fill ancestor; a `HAS_HARD` sibling).
  - PP-3: start the Wadler 2003 §§3–5 plus Leijen `align`/`fill` oracle. Each function names the equation it transcribes.
- **CON**
  - CON-1 (CB-B0): harness and manifest schema. One rep is one fresh process; QoS user-interactive; `pmset -g therm`; load before and after; records keyed by tree hash plus commit sha.
  - CON-2: pins resolved against the registries (newest release ≥ 14 days old), written to `W0-BENCH-PINS.tsv` v2. Upstream competitor parsers pinned by commit, with any departure recorded as a diff.
  - CON-3: check whether `guarded --exclusive` accepts the key `bench`.
  - CON-4: NOISY baselines for the root pipeline and skinny on both build matrices.
  - CON-5: `bench/PREREG.md` skeleton (families, Holm, α = 0.01, minimum effect 3%).
- **USE**
  - USE-1: parse-site census across value, keyframes, fourier, glass, daraksha and sci-report.
  - USE-2: the benefit-table baseline (LG-d row 0).
  - USE-3: intake of the 66 V-C css-color misses (§0hy) as lane rows.
  - USE-4: pin census. kf and daraksha are at 4.2.0; one resolved copy per app lockfile.
  - USE-5: the value.js `files` gap (no `.bbnf` in the tarball), filed for after the close.

**LG-1 adds:** the custody receipt verifies (`shasum -c`); `git merge-base --is-ancestor af15f63e0 origin/master` is true after TOPO-5; every born-RED test reads RED ×2 for the stated reason.

### LW2: topology, toolchain, ABI freezes and the feasibility seam (needs X-1..X-5, X-7)

- **TOPO**
  - TOPO-1: repo DAG per X-5. egraph, egraph-derive and csp-solver move into the parse-that workspace; `[patch.crates-io]` leaves the committed manifests.
  - TOPO-2: de-nightly. `build.target`, `rustc-wrapper` and the `-Z` flags move to the developer's `~/.cargo/config.toml` or to CI env; the committed config keeps only portable profiles and an explicit `+nightly` alias.
  - TOPO-3: CI matrices (bbnf-lang adds ubuntu-latest; parse-that adds macos-14), a required `cargo +stable` job, and Intel SDE for AVX-512. Held workflow files go through `contest/ci`.
  - TOPO-4: `cargo publish --dry-run` in topological order.
- **COMP**
  - COMP-1: root-pipeline trials under CB-B0. e-graph (P5) on against off, and CSP against greedy, with microbench and compile-artifact gates and deadlines. An UNDECIDED trial goes to the simpler organ; the loser is kept on a tagged branch.
  - COMP-2: CSP Unsat cure. `let _ = csp.propagate` goes from 6 to 0, and a Decided/Decided conflict becomes ⊤.
  - COMP-3: the inline plan and the CSP move into `crates/ir` (passes).
  - COMP-4: CB-5.5, a minimal plan-driven JSON emitter slice that gives trials an end-to-end harness.
- **PERF**
  - PERF-1: form `parse-that-simd` (X-3) from the three crates. `OnceLock` table; `BBNF_SCAN_TIER` / `force-scalar` overrides; a scalar oracle per kernel; a differential fuzz target per kernel; a guard-page test (PROT_NONE trailing page); Miri on the scalar and dispatch code.
  - PERF-2: the dispatch-tier-selected test GREEN on every CI arm.
- **RX**
  - RX-1 (R1.5 / OR-7): spike Route A (regex-automata as determinizer) against Route B (in-house). Measure fidelity, compile time, state counts and emitted-code speed on 121 + 56 leaves. The owner rules OR-7.
  - RX-2 (R1.6): freeze the Hir ABI, plus the state type under Route B.
  - RX-3: `Hir::to_source` (R6) as the round-trip oracle, with V8 and JSC agreeing.
  - RX-4: `FuseAltRegexBranches` disabled for Rust until R3.
- **PP**
  - PP-1: op.rs ABI v2 (≤ 24 B, const-asserted; `Text(&'a str)` kept; `GroupHdr` side table with u32 fields).
  - PP-2: builder `finish()` with the monotone-stack `trailing` (L3′); unbalanced input returns `Err`; `Mediumline` deleted.
  - PP-3: the oracle complete. L1–L8 property tests against it.
- **CON**
  - CON-1: per-plane semantic specs (UTF-8 validation, escapes, number fidelity, duplicate keys, error offsets) and an output-parity class plus work checklist for every row.
  - CON-2: the feasibility seam. Measured baseline, self-vs-self noise and a roofline per goal; floors PROPOSED; the owner rules; frozen in a hashed decision-plan commit (U6s).
- **USE**
  - USE-1: G-S1 / G-S2 pre-registration (ABBA order, seed, corpus hash; tiers TOKENIZE, COLOR→RGBA and FULL-VALUE).
  - USE-2: the equal-work proof for base `4038dce5`.

### LW3: the spine and semantics

- **COMP**
  - COMP-1: merge the sk-v26 spine (ordinary merge, `integration/backend`); port only the LW2 winners.
  - COMP-2: R3 left recursion. Pratt tables for operator grammars and an iterative left-fold for the rest, validated against the seed-growing oracle. Zero-consumption cycles stay a `RecursiveRule` diagnostic.
  - COMP-3: I8 independent oracles. TS 0.3.0; JSONTestSuite plus serde_json; value.js's 121 leaves.
  - COMP-4: I9 `obs()` classes and the failure contract.
  - COMP-5: I7 lint GREEN; TOML and CSV held-out grammars.
- **PERF**
  - PERF-1: our own structural indexer (an extension of `simd-scan/index.rs`) as an LIR `KernelId` primitive. simdjson and simd-json are never linked.
- **RX**
  - RX-1: R2 then R3 in sequence. Leftmost-first sticky semantics under u/v. `end_unambiguous`; the product construction for regular lookahead; the linear-lookaround tier.
  - RX-2: the ADJ-2 migration ledger. Every in-tree grammar plus the value.js corpus under both semantics; every changed parse is a corrected bug or a root grammar edit. Shims are banned.
  - RX-3: R4 in parallel with the other units.
- **PP**
  - PP-1: the one-loop renderer (bit-vector mode stack, zero `unsafe`); `width.rs` (an ASCII fast path through the `parse-that-simd` all-ASCII kernel; `unicode-width`).
  - PP-2: U4b trivia (`LineSuffix`, `LineSuffixBoundary`, `BreakParent`); comment-bearing corpora.
  - PP-3: freeze the stream format; publish `0.4.0-alpha.1`.
- **CON**
  - CON-1: Wave D cells first: CSS P2 (G-C1a cssparser; G-C1b lightningcss on the typed-coverage manifest only) and regex. Exploratory reps ≥ 10.
- **USE**
  - USE-1: P-1 corpus `grammar/corpus/value-js/` pinned to a released tarball (X-8).
  - USE-2: the svg-path consumer (G-L1) and the glass colour regexes (G-R1, `rgb(1 2 3 / 50%)` gives α 0.5) on branches.
  - USE-3: P-IR-TS. The Rust front end emits `plan.json` plus `plan.hash`, and the TS emitter reads it at generation time. Gated on wf_d837f96c's close; carried out by U7b's successor release workflow.

**LG-3 adds:** G1 = 0 divergences under `obs()` plus the failure contract, across the 9 + 3 grammars and every independent oracle; the migration ledger closed.

### LW4: emitters, kernels and formatting

- **COMP**
  - COMP-1 (CB-10): the plan-driven Rust emitter over parse-that ABI-3, with direct-to-struct through `bbnf-ser` / `pprint-ser` layouts. Parity gate: at least root `backend/rust` on all 9 grammars, beyond noise or tied. Root `backend/rust` stays as a second oracle until then.
  - COMP-2 (CB-10b): LSP, `wasm/`, `server/`, `playground/` and `extension/` move to Program/Facts/Plan. Born-RED: `cargo tree -i bbnf-ir` / `-i bbnf-analysis` is non-empty.
  - COMP-3 (CB-11): a delete-unless-wins trial for the Wasm parse bytecode VM only. The gorgeous `vm` feature is judged in PP.
- **PERF**
  - PERF-1: NEON kernels (class runs via shufti/truffle, accel self-loops, UTF-8 valid prefix, structural index); AVX2 conditional under X-4; `asm!` only where it beats intrinsics beyond the floor on the recorded M-series generation, with the measurement cited in the block. ASan and `cargo careful` in CI.
- **RX**
  - RX-1: R7 codegen against the final state type.
  - RX-2: R8 convergence. Delete the duplicates in `crates/ir/src/passes/regex_info.rs`, `crates/core/src/generate/regex/*` and `parse-that/rust/regex/src/info/*` down to the one home; delete the skinny regex crates. The rules in `crates/ir/src/egraph/rules/regex.rs` are ported onto `FirstSet` with the precondition `min_len == max_len == 1` and no observable branch identity.
  - RX-3: R9. Teddy only for named unanchored consumers; dropped if none is on a hot path.
- **PP**
  - PP-1: gorgeous re-based onto Syntax→Doc through `FmtBuilder`/`FmtOp`, with `@fmt` rules in BBNF only.
  - PP-2: formatting output byte-identical with the optimizer on and off. The IR/passes seats own this invariant.
  - PP-3: P-PP-TS. The TS emitter emits a Wadler printer from `@pretty` (gated as USE-3 in LW3).
  - PP-4: the formatter-parity plan (G7/G8) chartered, with diff budget X-10.
- **CON**
  - CON-1: JSON P1 and P4 cells (typed value and lossless CST against tree-sitter, in the CST class only). Every contestant runs a checksum walk inside the timed region.
- **USE**
  - USE-1: G-P1. `formatCSSKeyframeString` byte-identical through the emitted printer; the value.js `serialize` round-trip at 100%; a ruling row for each model serializer.

### LW5: optimization passes and certification

- **COMP**
  - COMP-1: the e-graph over a sound rewrite set (each rule has a property-tested witness); CSP results consumed by the scheduler; inlining and FIRST-set dispatch fusion driven by a measured cost model.
  - COMP-2 (CB-13): keep/delete verdicts recorded against the new spine (I6).
- **PERF**
  - PERF-1 (U10): canada attribution (lexing, float conversion, tape writes, allocation), then the remedy. A lazy-number tape is its own plane.
- **RX**
  - RX-1: G-P1 (ADJ-1). The same DFA, ours against the interpretive dense DFA; non-DFA Plan tiers in a separate row.
  - RX-2: G-R1 fidelity at 0 ENGINE divergences.
- **PP**
  - PP-1: G6a (always-break, byte-equal to `to_string_pretty`) and G6b (width-fitting, no win claimed); dtoa reported separately; G4 budget (CI upper bound of v2/greedy ≤ 1.05).
- **CON**
  - CON-1: certification runs. PREREG frozen; N ≥ 200 pairs per certification cell; ≥ 30 processes; BCa CI and Hodges–Lehmann; Holm within each family. PORTABLE is the headline matrix and NATIVE is labelled. Claims read "on Apple M-series (aarch64)".
- **USE**
  - USE-1: G-S1 non-inferiority on all 14 cells; G-S2 tiers with CIs; G-L1 p95 < 1 ms (NEON, quiet window).

### LW6: the release cascade (U-REL; needs wf_d837f96c closed)

- **TOPO**
  - TOPO-1: publish in DAG order: egraph, egraph-derive and csp-solver → pprint 0.4.0 (after the U9, U10 and U12 conformance) → parse-that 0.5 (with `parse-that-regex` and `parse-that-simd`) → bbnf-lang crates.
  - TOPO-2: U7b. The TS BIR-S adoption release, the only unit allowed in `typescript/`; bbnf-lang TS 0.4.0.
- **USE**
  - USE-1: value.js re-pin by PR (the `files` fix, E-6 re-read); value.js publishes through its own workflow.
  - USE-2: keyframes.js re-pin (exact for apps; caret peer ranges for libraries); fourier and daraksha notified.
  - USE-3: `npm ls @mkbabb/value.js` shows one copy per app.

### LW7: deletion and close

- **COMP**
  - COMP-1 (CB-14): delete root `backend/rust`, the Rust `backend/ts` (R-5; its divergences captured first as parity failures), the absorbed root crates and `skinny/` (GN-0). Losers live on tagged branches only.
- **CON**
  - CON-1: Wave F stretch (JSON P3, P5 and P6, serialization).
  - CON-2: opponent bump (W13) with every claim re-measured.
- **ALL**
  - Tri-fold close review; the final competition table; the final benefit table; the carried tail re-homed.

## 6. Review regime and seat law

- Tri-fold review (two critics and an adjudicator, all Opus 5.5) at six seams: architecture (LW1 close), API freeze (LW2 close), cutover (LW4 COMP-1), perf claim (LW5), the feasibility seam (LW2.CON-2) and close (LW7). Every other unit gets a single critic plus its mechanical gate.
- Every `unsafe` or SIMD site needs: a scalar oracle, a differential fuzz target, ASan, the guard-page test, and Miri where Miri applies. No UB.
- Fuzzing: local runs are smoke only, in shards of at most 80 min, with the corpus kept at `~/.dev-logs/bbnf-lockstep/fuzz/<target>`. Long runs are scheduled CI jobs. Admission needs 10⁵ inputs per grammar per pass; close needs 10⁶.
- Machine law: every build, test, fuzz and bench goes through `~/.claude/guard/guarded`, with at most one heavy run per seat. Bench seats use `--cpu-cores 0 --exclusive bench`. Durable scratch goes in `~/.dev-logs/bbnf-lockstep/`, never `/tmp`.
- Git law: worktrees under `~/Programming/{bbnf-lang,parse-that,pprint}-wt/<unit>` off `origin/master`. Commit by pathspec, push branches, merge to master by an ordinary merge after CI. Never stash, reset, rebase or force. Primary checkouts are never switched.
- Benchmark honesty: published releases with their recommended flags, the same inputs, ≥ 10 reps (certification ≥ 200 pairs), medians with dispersion, and no win claimed inside noise.

## 7. Addenda

Sections 0–6 are sealed and are not edited. Each row below amends them by addendum, and where a row and the sealed text differ, the row wins. Rows are added at the end, never edited in place.

### 7.1 2026-11-12, DIG-G1: no trial verdict deletes an organ (LW3 R0-FOLD, under RULINGS 2026-11-12a DIG-G1; delegate, owner may overturn)

**The rule.** No trial verdict, at any wave, deletes or drops the CSP solver, the e-graph, shape mining or the kernels (a kernel slot). A LOSS or UNDECIDED changes the order of work, never the scope: it may switch one named rewrite family, constraint family or kernel variant off by default, and it opens a redesign unit with a re-trial. A NOISY record decides nothing in either direction. "Tagged the loser" names a branch tag that keeps the losing arm's code for a re-trial; it never authorises a deletion. DIG-G1 replaces OWN-1 (2026-11-11e). Each passage below deletes, or drops, a trial loser; its amending text follows. The passages' sealed text is not edited; `bench/PREREG.md` stays byte-for-byte as sealed under `DECISION-PLAN.lw2.json` (its decision rule is unchanged; only the consequence of a verdict for the organ's existence is amended here).

| Row | Passage | Sealed text | Amended by DIG-G1 to read |
|---|---|---|---|
| G1-P01 | `designs/compiler.md:32` (C1-4) | "the verdict is UNDECIDED and the simpler organ wins: greedy over CSP, direct over e-graph, scalar over kernel" | DIG-G1: an UNDECIDED-by-deadline verdict decides the default for the next build only (greedy, direct or scalar on by default); the organ stays on the spine and its redesign unit and re-trial open. |
| G1-P02 | `designs/compiler.md:33` (C1-4 amendment) | "an UNDECIDED deletion stays reversible until CB-14" | DIG-G1: there is no UNDECIDED deletion of the CSP, the e-graph, shape mining or a kernel. The tagged branch keeps the trial arm for the re-trial; CB-14 deletes no organ. |
| G1-P03 | `designs/compiler.md:38` (C1-5) | "The final keep/delete verdict is recorded in CB-13." | DIG-G1: CB-13 records the final on-by-default/off-by-default verdict per named family or variant, never a keep/delete verdict on an organ. |
| G1-P04 | `designs/compiler.md:89` (C2-4 guard) | "a root trial result can only *delete*" | DIG-G1: a root trial result can only withhold a port of the root implementation (LW3.COMP-1f); it cannot delete the organ, which LW5.COMP-1 builds on the spine regardless. |
| G1-P05 | `designs/compiler.md:125` (I6 amended) | "a deletion takes effect only after the organ loses a parity or paired trial" | DIG-G1: I6 does not reach the CSP, the e-graph, shape mining or the kernels. A lost trial switches the named family or variant off by default and opens a redesign unit; I6's "never a second path on master" still holds for that family. |
| G1-P06 | `designs/compiler.md:163` (step 8) | "the verdict is UNDECIDED and the simpler organ wins, reversibly until CB-14" | DIG-G1: as G1-P01; UNDECIDED sets a default and opens a re-trial, and nothing is deleted at CB-14. |
| G1-P07 | `designs/compiler.md:175` (CB-T) | "loser or UNDECIDED marked for deletion; only winners enter the port list" | DIG-G1: a loser or UNDECIDED is withheld from the LW3 port list and tagged for re-trial, never marked for deletion. |
| G1-P08 | `designs/compiler.md:182` (CB-7) | "**CB-7 E-graph port** (only if CB-T kept it)" | DIG-G1: CB-7 is unconditional. The e-graph is built on the spine (LW5.COMP-1) whatever CB-T rules; CB-T decides only whether the root implementation is ported in LW3. |
| G1-P09 | `designs/compiler.md:183` (CB-8) | "**CB-8 Global decisions port** (only if CB-T kept CSP)" | DIG-G1: CB-8 is unconditional, as G1-P08, for the CSP (LW5.COMP-1, LW5.COMP-3). |
| G1-P10 | `designs/compiler.md:189` (CB-13) | "final keep/delete verdicts for CB-7, CB-8 and CB-9" | DIG-G1: CB-13 gives final on/off-by-default verdicts per named family (CB-7, CB-8) and per kernel variant (CB-9), each with its redesign unit when it loses; it gives no delete verdict. |
| G1-P11 | `designs/compiler.md:190` (CB-14) | "trial losers" | DIG-G1: CB-14 deletes no trial loser of the CSP, the e-graph, shape mining or a kernel; their tagged branches stay until each re-trial is decided on a QUIET record. |
| G1-P12 | `designs/rust-perf.md:131` (G-K1) | "or the kernel is deleted" | DIG-G1: a kernel below its G-K1 bar, or one that moves no G-J or G-C cell, is not selected by default and opens a redesign unit with a re-trial; its slot, scalar oracle and harness stay. |
| G1-P13 | `designs/rust-perf.md:132` (G-Q1) | "is off by default or removed" | DIG-G1: a pass that moves neither G-J1 nor G-C1 beyond noise is off by default, never removed; the stage it belongs to stays. |
| G1-P14 | `designs/rust-perf.md:190` (U-gate) | "G-K1 holds or the kernel is deleted" | DIG-G1: as G1-P12. |
| G1-P15 | `designs/rust-perf.md:227` (U-gate, G-Q1 A/B) | "a measured effect, or the pass ships off by default" | DIG-G1: "the pass" is one named rewrite family, constraint family or kernel variant, never the e-graph, CSP or shape-mining stage. |
| G1-P16 | `designs/rust-perf.md:248` (U12) | "or the kernel is withdrawn" | DIG-G1: as G1-P12; the assembly kernel is not selected by default and its record stands, with a redesign unit (DIG-K-C10). |
| G1-P17 | `designs/programme.md:414` (O012) | "A pass that does not beat it on compile time or parse throughput is retired (O012)." | DIG-G1: such a pass is off by default with a redesign unit and a re-trial; the CSP and the e-graph are not retired (O012 is amended). |
| G1-P18 | `bench/PREREG.md:135` (§6 verdict rule) | "LOSS and UNDECIDED keep the simpler arm (e-graph off; greedy), and the organ is tagged the loser." | DIG-G1: the decision rule (rules 1–4) and every registered cell are unchanged. "Keep the simpler arm" governs the default of the LW3 port only, and "the organ is tagged the loser" names the re-trial tag; the organ stays in the programme and on the spine. |
| G1-P19 | `PLAN.md:151` (LW2.COMP-1) | "An UNDECIDED trial goes to the simpler organ; the loser is kept on a tagged branch." | DIG-G1: an UNDECIDED trial sets the simpler arm as the LW3 default; the organ is not dropped and is built on the spine at LW5. |
| G1-P20 | `PLAN.md:227` (LW5.COMP-2, CB-13) | "COMP-2 (CB-13): keep/delete verdicts recorded against the new spine (I6)." | DIG-G1: LW5.COMP-2 records on/off-by-default verdicts per named family and kernel variant against the new spine, each loss with its redesign unit; I6 deletes no organ (G1-P05). |
| G1-P21 | `PLAN.md:253` (LW7.COMP-1, CB-14) | "Losers live on tagged branches only." | DIG-G1: LW7.COMP-1 deletes no CSP, e-graph, shape-mining or kernel code as a trial loser; root code they replace goes only once the spine carries the organ (DIG-H1's pin tests GREEN). |
| G1-P22 | `LW3-ORDER.md:107` (LW3.COMP-1f) | "LOSS or UNDECIDED-by-deadline ports nothing in LW3 and tags `trial/lw2-<organ>-loser`." | DIG-G1: unchanged in effect (the tag is a re-trial tag); a LOSS moves the organ's work later, never out (LW5.COMP-1). |

**Read, not amended.** Two passages delete on a trial and are outside DIG-G1's four organs, so they are left to the asks filed by seat R0-FOLD: `PLAN.md:206` (LW4.COMP-3, the delete-unless-wins trial of the Wasm parse VM; DIG-C16 asks that it may switch the VM off and never delete the wasm face without an owner ruling) and `PLAN.md:59` (X-4: an x86 kernel without a measured win is deleted; the owner's own words on x86 are the dig's O-2).

### 7.2 2026-11-12, units the dig names, homed to a wave (LW3 R0-FOLD, under RULINGS 2026-11-12a DIG-A9 and 2026-11-11e ADD-2; delegate, owner may overturn)

Each unit's falsifier is the one written in `AMENDMENTS-FROM-THE-DIG.md` (cited by row id) or in LEDGER amendment 9; the wave's order opens it by name. The first four rows are the units RULINGS 2026-11-12a DIG-A9 names.

| Row | Unit | Wave | Work | Source |
|---|---|---|---|---|
| A-01 | LW5.COMP-1-MINE | LW5 | Shape mining as a stage on the spine: every recognizer of `crates/ir/src/passes/recognizers/` becomes a `ShapeMiner`, a constraint emitter or a widening e-graph rule; the March techniques survive with a pin test each; miner hints feed the shared cost model; no language name in any miner. | DIG-SM-1..4, H23 |
| A-02 | LW5.COMP-1-BRIDGE | LW5 | The CSP↔e-graph bridge at `passes/csp_egraph_bridge.rs`, with a justification recorded per accepted or denied rewrite (justification count = accepted + denied). | DIG-CSP-D12, H2 |
| A-03 | LW5.COMP-5 | LW5 | The measured cost table: kernel × microarchitecture cycles per 64 bytes filled by the harness, the e-graph cost fitted from hardware counters on real hit distributions, objectives that read it (a coefficient-flip mutant changes `plan.json`; the 648-run sweep re-run non-null). | DIG-CSP-C11, DIG-EG-C11, DIG-K-D13, H4 |
| A-04 | LW5.COMP-1-OPTIR | LW5 | Amendment 9's OptIR stage, extended: **the e-graph runs BEFORE the normalizer** and subsumes the destructive passes (inline, merge, factor, ε-elimination, the G1–G4 duplicates become e-graph rules); saturation also runs in the OptIR islands between Facts and Plan; a destructive pass kept outside the e-graph carries a recorded, measured "not befitting" verdict. Falsifier: the destructive-pass list against a verdict file, `missing=0`, and the stage order read from the spine's pass registry (e-graph before normalizer). | DIG-EG-C9, amendment 9 |
| A-05 | LW4.TOPO-PIN | LW4 | bbnf-lang and parse-that re-pinned to a csp-solver release that carries the CH-69 cure `58014efd`. | DIG-CSP-A2, H8 |
| A-06 | LW4.COMP-5 | LW4 | Refutations first, as failing tests, then repair or removal of the rule (never the stage): `WrapOfEpsilonScalar`, `FuseAltRegexBranches`, `SupersetAbsorbAlt`, `UnionMergeAlt` (both halves), `CommonSuffixFactor`, `RepeatOfSingle`, `ConcatLiterals`, the HIR repetition merge. | DIG-EG-A3, N-q |
| A-07 | LW4.COMP-6 | LW4 | The value API: `as_<T>()`, `try_into()`, typed property, visitor, `path!` and `select!` over one path core, lazy and eager; `bbnf-path` and `bbnf-path-ts` ported. | DIG-T-19, H20, H21 |
| A-08 | LW4.PERF-3 | LW4 | msac-style cross-chunk refill against its intrinsic twin, a QUIET paired record, win or recorded negative. | DIG-K-C10, H13 |
| A-09 | LW4.PERF-4 | LW4 | PROVE landings: each PROVE GO result (01, 03, 06, 07, 10, 12) lands as its own unit and is re-measured on the same plane. | DIG-K-PROVE, H25, N-n |
| A-10 | LW5.COMP-1-PAT | LW5 | One CSP pass for all structural pattern recognition (one CSP instance per grammar carries every pattern variable). | DIG-CSP-B6, H1 |
| A-11 | LW5.COMP-4 | LW5 | The type-system CSP (finite choice for overloads, layout, materialisation, recogniser eligibility, recovery, backend erasure), with inference on the CSP substrate; scope is the owner's O-5. | DIG-CSP-B7, H3 |
| A-12 | LW5.COMP-6 | LW5 | Tuning, validation and visibility of optimizer decisions (`CostDecision` count = decision count; playground view). | DIG-CSP-E16, H16 |
| A-13 | LW4.PERF-2 | LW4 | The tape union (amendment 9), widened to DIG-T-14..18: one tape, node identity shared with direct-to-struct, all five shapes chosen by the CSP, lazy numbers a flavour, every consumer on one node id. | amendment 9, DIG-T-14..18, N-f |
| A-14 | LW4.COMP-4 | LW4 | LIR completion (amendment 9): `memo`, `plane` and `alloc` join `dispatch`, `RegexPlan` and `KernelId`. | amendment 9, DIG-C4 |
| A-15 | LW4.CON-2 | LW4 | The same-plane simdjson C++ arm on the PORTABLE build plane (amendment 9). | amendment 9 |
| A-16 | LW4.PERF-1-U5 | LW4 | `parsers/scan/{number_simd,lanes}.rs` behind `rust/simd` (amendment 9). | amendment 9, DIG-CERT-NUM |
| A-17 | LW4.CON-1-J | LW4 | The JSON and F-COMB witness modes and adapters (amendment 9). | amendment 9 |
| A-18 | LW4.RX-2-BR | LW4 | The BackRef bounded tier (amendment 9). | amendment 9 |
| A-19 | LW4.RX-2-DOC | LW4 | `Hir::to_doc` round-trip through pprint (amendment 9). | amendment 9 |
| A-20 | LW4.PP-1-NAH | LW4 | Nest, Align and Hang with their laws (amendment 9). | amendment 9 |
| A-21 | LW4.PP-1-LL | LW4 | `LiteralLine` with its laws (amendment 9). | amendment 9 |
| A-22 | LW4.PP-3-TS | LW4 | The TS renderer, byte-equal to the Rust renderer on STREAM-v1 (amendment 9). | amendment 9, DIG-C8 |
| A-23 | LW5.COMP-3 | LW5 | CSP completeness (amendment 9), widened to DIG-CSP-A1: nogoods, conflict-directed backjumping, Luby restarts, `bounded_sum` (from the DIG-CUST-1 custody refs), cardinality and global-sum, soft constraints, LCV, lex-leader symmetry breaking, incremental re-solve, min-conflicts, Unsat explanations; plus the coupled solve of DIG-CSP-C9. | amendment 9, DIG-CSP-A1, C9, N-d, N-e |
| A-24 | LW5.PP-1-H | LW5 | The pprint bench harness LW5.PP-1 needs (amendment 9). | amendment 9 |
| A-25 | LW7.CON-1-REQ | LW7 | JSON P3, P5 and P6 required certification cells, not stretch (amendment 9), with P7 (DIG-CERT-P7). | amendment 9, DIG-CERT-P3..P7 |

### 7.3 2026-11-12, a quiet certification window (LW3 R0-FOLD, under RULINGS 2026-11-12a DIG-QW; delegate, owner may overturn)

§6 "Benchmark honesty" and LW5.CON-1 gain: a timing read certifies only inside a scheduled quiet certification window (X-7's load rule; the QoS reading of 2026-11-11); a record outside it is labelled NOISY and decides nothing. The owner's own ruling on the window is the dig's O-7.
