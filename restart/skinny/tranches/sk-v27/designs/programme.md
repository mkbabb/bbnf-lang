# BBNF / parse-that / pprint: the sk-v lockstep programme. Adjudicated final design (2026-11-05)

**Status:** adjudicated. The owner rulings R-1..R-7 in §0 must be confirmed before W2 starts. W0 and W1 may start at once.

**Measurement status:** nothing in this design was measured. Every number is a target or a proposed floor until W2 freezes it.

**Read-only facts I checked myself at adjudication:**
- bbnf-lang:
  - Local HEAD is `af15f63e0`. `origin/master` is `c9b93adb4`. `af15f63e0` is NOT an ancestor of `c9b93adb4`: the two have diverged.
  - `ci.yml` and `release.yml` run only on `runs-on: macos-14`.
  - `.cargo/config.toml:40-41` sets `rustc-wrapper = "sccache"` and `target = "aarch64-apple-darwin"`, plus nightly `-Z` flags.
  - `.claude/worktrees/` holds 63 entries.
- parse-that:
  - `origin/master` is `47e928acc`, not `7098c86`.
  - CI runs only on `ubuntu-latest`.
  - `rust/regex/Cargo.toml:35-36` depends on `egraph = "0.1"` and `egraph-derive = "0.1"`.
- pprint: `master` is 39 commits ahead of origin.

**Durable scratch:** `/Users/mkbabb/.dev-logs/bbnf-lockstep/`.

---

## A. Rulings on the objections

### Critic 1

**C1-1. The CI premise is false. ACCEPTED (verified).**
- bbnf-lang CI is aarch64-only, and parse-that CI is x86-only. The committed `build.target` would turn every x86 job into a cross-build, so the x86 tiers would never execute.
- R-1 and W1 are rewritten (§0 and §4):
  - `build.target` comes out of the committed config.
  - Each repo gets a per-repo runner matrix.
  - The W1 gate becomes: every dispatch tier executes on at least one CI job, proved by a test that asserts which tier was selected at runtime.

**C1-2. The sibling path patches and the egraph cycle. ACCEPTED, with one clarification and a ruling.**
- The cycle is at the repo level, not the crate level:
  - `egraph` does not depend on parse-that.
  - Even so, a repo cycle makes CI and publication order fragile, so it is a real defect.
- RULING (new R-8): the repo DAG must be acyclic, in the order `pprint` → `parse-that` → `bbnf-lang`. To get there, `egraph`, `egraph-derive` and `csp-solver` move into the parse-that workspace as standalone general crates (parse-that stays a general library, and these are general crates). Separately, `[patch.crates-io]` path patches leave the committed manifests.
- In W1a, CI builds against published versions or pinned git revs.
- W1a gate: `cargo publish --dry-run` succeeds for every crate, in topological order.

**C1-3. The regex-home ruling comes too late. ACCEPTED.**
- The IF-5 home is decided in W1a, alongside the topology work.
- RULING: `bbnf-regex` stays in parse-that, the lower layer that consumes it at runtime. bbnf-lang depends on it.
- W6 only implements.

**C1-4. The floors have no baseline. ACCEPTED, merged with C2-B6.**
- The floors in §3 become PROPOSED.
- W2 adds a feasibility seam: measured baseline, measured self-vs-self noise, and a roofline estimate per goal. The owner confirms or rules each floor, and it is frozen before W4.
- This is pre-registration, not re-scoping. O018 holds after the freeze.
- G-JSON is split per plane and per corpus.

**C1-5. Unequal head-to-heads. ACCEPTED, merged with C2-B3.**
- IF-8 gains an "equivalent work" column.
- A per-plane semantic spec is written before any measurement.
- Lightning CSS is timed twice: `StyleSheet::parse` against our parse, and parse+print against our parse+print.
- The opponent-advocate seat reviews both W9 and W10.

**C1-6. The noise gate cannot hold on this machine. ACCEPTED.**
- Bench seats run under a bench mutex (§2, IF-8):
  - W2 first checks whether `guarded --exclusive` takes an arbitrary key. If it does not, the seat uses `--cpu-cores 0` plus a lock file at `~/.dev-logs/bbnf-lockstep/bench.lock`, and the record notes that machine-wide exclusion was not available.
  - Changing the guard itself is the owner's act. No seat edits `~/.claude/guard`.
- Interleaved ABAB reps.
- Load and process census at start and at end. If either check fails, the record is void.
- If W2 cannot reach the noise gate in three attempts, the programme falls back to cycle counts (kperf) and says so.

**C1-7. The fuzz budget is unbudgeted. ACCEPTED, merged with C2-B5.**
- Long fuzzing runs as scheduled CI jobs, x86 and aarch64, with a persisted corpus.
- Local fuzzing is short smoke only: shards of at most 80 minutes, corpus kept at `~/.dev-logs/bbnf-lockstep/fuzz/<target>`.
- Admission gate: 10⁵ inputs per grammar per pass. Close gate: 10⁶.
- Kernel gate: a minimum exec count plus coverage saturation (no new edges for N CPU-hours, summed from shard receipts). A raw wall-hour total is not the gate.
- The per-unit budget table is in §6.

**C1-8. The grammar census is stale. ACCEPTED (verified: 63 worktrees).**
- W0 commits `grammar/MANIFEST` (path plus content hash), excluding `.claude/worktrees`, `restart/` and `target/`.
- IF-1, IF-2 and the R-4 falsifier refer to the manifest hash, never to the number 43.

**C1-9. No ancestry check (truncated in the critique). ACCEPTED (verified: diverged).**
- Fast-forwarding `af15f63e0` to `c9b93adb4` is impossible, and the LAW forbids reset, rebase, force and stash.
- RULING:
  - Push `af15f63e0` to `sk-lineage/local-master`.
  - Leave the dirty primary checkout's branch ref untouched.
  - Start all new work in fresh worktrees under `~/Programming/bbnf-lang-wt/<unit>` (outside the repo, so no census pollution), branched from `origin/master`.
  - Classify the divergence (`git log c9b93adb4..af15f63e0`, and the reverse).
  - Bring the unique local commits in by an ordinary merge on a branch, after CI.

### Critic 2

**C2-A1. The parse-that SHA is stale. ACCEPTED (verified: `47e928acc`).**
- W0 records every repo's origin and local heads with `git ls-remote` into `~/.dev-logs/bbnf-lockstep/heads-W0.json`.
- No SHA written in a design is trusted.
- W5's merge base is re-derived from that record.

**C2-A2. The census and the worktrees. ACCEPTED (same as C1-8).**
- Before anything touches `.claude/worktrees/wf_fce2ec2b-*`, W0 establishes whether wf_fce2ec2b is still live, using the ListAgents and worktree lists.

**C2-A3. The sccache "fix" is backwards. ACCEPTED. This overrides the design's W1 choice.**
- The gestalt fix is that the repo builds anywhere on stable.
- `rustc-wrapper`, the `-Z` flags and `build.target` move to the developer's `~/.cargo/config.toml`, or into CI env (`RUSTC_WRAPPER`).
- The committed config keeps only portable profiles and aliases.
- A nightly-only alias, if kept, lives behind an explicit `+nightly` alias.

**C2-B1. The claim statistics are unsound. ACCEPTED.**
- Claim-grade records use:
  - in-process ABAB interleaving;
  - QoS user-interactive for P-core bias (macOS has no hard pinning; the bias is recorded as a confound);
  - a block bootstrap by session;
  - at least 3 sessions on different days;
  - a Holm correction pre-registered per goal family.
- G-RX-1 is restated: no pattern's ratio CI upper bound is below 0.95, and the geomean CI lower bound must exceed the floor.

**C2-B2. The opponents are not fixed by the claim protocol. ACCEPTED.**
- Opponents are pinned once at W2 by crates.io or npm tarball hash. They are bumped only at W13, and every claim is re-measured when they are.
- Both sides share one flag profile: `target-cpu=native`, LTO and codegen-units, matching the opponents' published guidance.
- PGO or BOLT is offered to both sides, or results are reported with and without it.
- Each cell gets a time budget: at least 2 s of measured iterations per rep, at least 10 reps per session, at least 3 sessions.

**C2-B3. The output-equivalence planes are under-defined. ACCEPTED (merged with C1-5).**
- Each plane's spec covers:
  - UTF-8 validation
  - escape decoding
  - number fidelity
  - duplicate keys
  - error offsets
- A cell whose opponent mode fails the spec is marked NOT-COMPARABLE, never counted as a win.
- G-CSS uses an AST-equivalence oracle on a declarations/tokens projection, or the ratio is dropped.

**C2-B4. Hidden concurrency owners and the truncated §5. ACCEPTED.**
- §5 is completed below.
- W0 attributes only paths that no live workflow owns. Ownership is an owner record in a file (`~/.dev-logs/bbnf-lockstep/ownership-W0.json`), not a judgment.
- Paths under an active worktree or file set are skipped.

**C2-B5. Fuzz versus the machine law, and the limits of Miri. ACCEPTED.**
- Miri cannot execute most `std::arch` intrinsics. Its gate is therefore stated as covering scalar, fallback and pointer-arithmetic code only.
- The SIMD paths are covered instead by:
  - the differential fuzz against the scalar oracle;
  - ASan on aarch64 (nightly `-Zsanitizer=address`, in a CI job);
  - a guard-page test for the padded-input contract (mmap with a PROT_NONE trailing page).

**C2-B6. The floors are not derived. ACCEPTED (merged with C1-4).**
- A roofline figure (bytes per cycle from memory bandwidth, kernel cycles per byte, measured gap) goes beside every G-* at the W2 feasibility seam.

**Adjudicator's own addition. ACCEPTED as the new R-7 (workflow cap versus "spawn many workflows").** The owner's standing 4-concurrent-workflow cap (2026-07-24) is already used up by:
- wf_d837f96c
- megatranche-realization
- x-0hr-residuals
- daraksha-v3

"Many workflows in a series of waves" is met by running the waves in series inside the cap. The cap is never broken without an explicit owner lift.

---

## 0. Owner rulings (confirm before W2)

**R-1. x86 tiers plus NEON. The later, explicit order wins.**
- One dispatch table, chosen once and stored in a `OnceLock`. Tiers:
  - aarch64 NEON (SVE2 and Apple-specific later)
  - x86_64 AVX2
  - AVX-512 (VBMI where present)
  - portable scalar
- Every tier runs against the same scalar oracle.
- An env override (`BBNF_SCAN_TIER=scalar|neon|avx2|avx512`) forces a tier, so every tier is tested on every host that can run it.
- Performance claims are made only on this Mac's aarch64.
- O033 is retired by addendum.

**CI matrix (W1):**

| Repo | Runners | Tier coverage |
|---|---|---|
| bbnf-lang | adds `ubuntu-latest` | AVX2 and scalar; AVX-512/VBMI under Intel SDE |
| parse-that | adds `macos-14` | NEON |

**R-2. Review regime.**
- Tri-fold review (2 critics plus an adjudicator, Opus 5.5) at six seams: architecture, API freeze, cutover, perf claim, close, and the W2 feasibility seam.
- Every other unit gets a single critic plus a mechanical gate.
- Every `unsafe` or SIMD site gets mechanical proof: oracle, differential fuzz, ASan, guard-page test, and Miri where Miri applies.

**R-3. The Codex-era receipts (M-15) are pass-0 input.**
- Receipts that carry code (W2 spine, ABI-3) are re-proved by their tests at the new HEAD.
- Judgment-only receipts are re-adjudicated or retired at the unit that consumes them.

**R-4. The spine is the sk-v26 clean-break compiler.**
- The published pipeline (e-graph, CSP, FIRST, memo, inlining) is a quarry, re-admitted as certified passes at W4.
- `skinny/` is a lab. Its kernels migrate at W7, and it is deleted at W13 (GN-0).
- Falsifier: a construct in any `grammar/MANIFEST` entry that the spine cannot express without a semantic workaround.

**R-5. One TS emitter of record: `typescript/` (`@mkbabb/bbnf-lang`).**
- The Rust `backend/ts` emitter is deleted at W12, after its divergences are captured as parity failures.
- W12 decides the long-term frontend by a measured gate (bundle bytes, cold compile, parity): WASM of the Rust frontend, or two frontends plus a parity suite.

**R-6. BA–BD are retired as plans; their gates are carried as rows.**
- The carried rows are BA-G1a, BB-G1..4, BC-G4, BD-G5 and BD-G7.
- T-TAIL-9 rows are homed per unit.
- parse-that T/U is the authority for parse-that's waves: U.W1→W5, U.W2→W7, U.W3-5→W6, and U.W7 is already retired.

**R-7. The workflow cap of 4 holds.** The sk-v programme claims slots only as the four current owners close, and takes at most 2 slots while any of them remains open (see §5).

**R-8. The repo DAG is `pprint` → `parse-that` → `bbnf-lang`, and it is acyclic.**
- `egraph`, `egraph-derive` and `csp-solver` move to the parse-that workspace.
- `bbnf-regex` stays in parse-that.
- No committed `[patch]` path entries.

---

## 1. Target architecture

The layers I1–I8 are unchanged from the proposal:

- **I1:** the immutable Grammar AST.
- **I2:** the serialisable Semantic Plan, with the lattice TypeDomain.
- **I3:** certified Plan→Plan′ passes.
- **I4:** the LoweringPlan.
- **I5:** the Rust, TS and WASM backends, plus printer derivation.
- **I6:** the RegexTablePlan.
- **I7:** the ScanKernel ABI.
- **I8:** the Value/Tape planes P1..P7.
- **I9:** the pprint Doc algebra.

**Physical homes, per R-8:**
- **pprint:** I9, the leaf.
- **parse-that:** the I5 runtime, I6 (`bbnf-regex`), I7 (`simd-scan`), and `egraph`, `egraph-derive` and `csp-solver`. It remains a general library.
- **bbnf-lang:** I1–I5 codegen, printer derivation, WASM, and the TS emitter of record.

## 2. Interfaces

Each interface has a contract file, a version and one proving gate.

**IF-1. AST → Plan**
- Contract: `docs/codegen-IR-CONTRACT.md` (BC-G4) plus the schema JSON.
- Gate: serialise/deserialise round-trip identity over every `grammar/MANIFEST` entry, and the plan hash stable across 2 clean builds.

**IF-2. Pass trait**
- Contract: `trait Pass { fn run(&Plan) -> (Plan, Certificate) }`.
  - Passes are pure.
  - Budget exhaustion returns the input plan unchanged, with an `Exhausted` certificate.
  - Silent fallback is forbidden.
- Gate: unoptimised and optimised plans accept the same language and produce the same values on corpus plus grammar-directed fuzz. 10⁵ inputs per grammar per pass for admission, 10⁶ for close.

**IF-3. Plan′ → LoweringPlan**
- Contract: a total function; an unhandled node is a compile error.
- Gate: exhaustive matches plus a coverage table.

**IF-4. LoweringPlan → backends**
- Contract: a single `TypeDesc` → value-shape table.
- Gate: the W12 parity matrix. The 9 BD-G5 grammars × 3 fixtures × {Rust, TS, WASM} must give byte-identical serialised values and identical error offsets.

**IF-5. Regex**
- Contract: `RegexTablePlan` in parse-that's `bbnf-regex`. TS uses emitted DFA tables or a sticky irregexp pattern, chosen per pattern by a measured rule.
- Gate: a shared TS/Rust law-vector corpus (match, span, longest-match), and DFA parity 126/126.

**IF-6. ScanKernel**
- Contract: the `Kernels` table in a `OnceLock`. Each entry has a `scalar::` twin, and inputs follow an explicit padding contract.
- Gate:
  - a checkasm-style harness: lengths 0..4096, every alignment, every tail, every tier, forced via `BBNF_SCAN_TIER`;
  - cargo-fuzz differential against the scalar twin;
  - ASan;
  - the guard-page padding test;
  - Miri on scalar and fallback code only, stated as such.

**IF-7. Printer**
- Contract: gorgeous derives `Plan → (Value → pprint::Doc)`.
- Gate: `parse(print(v)) == v`, and print idempotence, on all corpora.

**IF-8. Bench record**
- Location: `bench/records/*.json` in the repo that owns the claim.
- Fields:
  - opponent name, version and tarball hash
  - the shared flag profile
  - corpus hash
  - "equivalent work" (the timed region, with code links)
  - plane-spec conformance
  - session IDs and dates
  - reps per session, and measured seconds per rep
  - median, IQR, and block-bootstrap 95% CI of the ratio
  - the Holm-adjusted family
  - load and process census at start and end
  - bench-mutex receipt, `--cpu-cores 0` receipt, QoS
  - kperf cycles where available
- Gate:
  - the record validates against the schema;
  - a claim needs at least 3 sessions on different days, ABAB interleaving, quiet start and end (load < 8, waited for), and a Holm-adjusted CI lower bound above the frozen floor;
  - anything else is labelled NOISY or NOT-COMPARABLE.

## 3. Goals

Every floor below is PROPOSED until the W2 feasibility seam freezes it. Ratios are competitor ÷ ours on this Mac's aarch64, in a quiet window.

| Goal | Opponent | Plane / scope | Proposed floor / aspiration |
|---|---|---|---|
| G-JSON-1 | sonic-rs | P4 DOM, per corpus (twitter, citm, canada, github_events, gsoc-2018), never averaged | ≥1.10 / 2.0. canada is reported on its own row. |
| G-JSON-2 | simd-json tape | P3 tape, each P3 cell against its true tape competitor, under the plane spec | ≥1.10 / 1.5 |
| G-JSON-3 | serde_json, jiter | P5 typed direct-to-struct | ≥1.5 / 3.0 |
| G-CSS-1 | Lightning CSS | two rows: parse-only against `StyleSheet::parse`, and parse+print against parse+print, under the AST-projection oracle; no per-corpus regression | ≥5.0 / 6.0, with the animate +13.4% row carried |
| G-RX-1 | the `regex` crate, TS irregexp | the 126 grammar patterns plus a rebar subset | no pattern's CI upper bound below 0.95; geomean ≥2.0 with prefilter |
| G-GEN-1 | pest, nom, winnow 1.x, chumsky, lalrpop, tree-sitter | JSON, CSS and expression grammars, each tool in its idiomatic best form | ≥1.5 geomean; compile time and code size reported |
| G-GEN-2 | chevrotain, peggy, ohm, nearley, arcsecond, and parse-that 0.8.2 as self-baseline | same three grammars, Node LTS | ≥1.5 geomean warm; cold ≤ the best competitor |
| G-PP-1 | prettyplease, the `pretty` crate | printing the JSON and CSS corpora | ≥1.0; idempotence 100% |
| G-HYG | (our own code) | all three repos | master CI green on x86_64 and aarch64; 0 unsafe blocks without an oracle; Miri, ASan and the guard-page test clean; 0 unattributed dirty paths |

A goal that is not met is reported as a loss, with its numbers. Held-out corpora are fixed at W2 and never tuned against.

## 4. Ordered units

### W0. Durability and ground truth (no timing)

**Work:**
1. Record `git ls-remote` and local heads for bbnf-lang, parse-that, pprint and both `-skv26` worktrees into `~/.dev-logs/bbnf-lockstep/heads-W0.json`.
2. Census the live workflows (ListAgents, `git worktree list`, the run markers in `~/.claude/guard/runs`). Find out whether wf_fce2ec2b is still live. Write `ownership-W0.json`.
3. Push to remote branches:
   - `af15f63e0` → `sk-lineage/local-master`
   - the `codex/sk-v26-*` branches in both repos
   - pprint's 39 local commits → `sk-lineage/pprint-local`. They are merged to master in W1, after CI.
4. Classify the bbnf divergence both ways (`c9b93adb4..af15f63e0` and `af15f63e0..c9b93adb4`).
5. Open the merge branch `sk/w0-reconcile` from `origin/master` in an external worktree. Merging waits for W1 CI.
6. Make the sk-v25 ledger durable in git (LFS, or a pruned index plus a tarball manifest).
7. Commit `grammar/MANIFEST`.
8. Attribute the dirty paths that no live workflow owns (243 in bbnf, 35 in parse-that) to provenance branches, by pathspec. Paths a live workflow owns are listed and skipped. Dead workflow worktrees are attributed, then removed with `git worktree remove` (no force) once their contents are remote-contained.

**Falsifier:**
- `git branch -r --contains <sha>` is empty for any SHA recorded in W0;
- or a path owned by a live workflow was committed.

**Gate:**
- every recorded SHA is remote-contained;
- the MANIFEST is committed;
- each primary checkout's dirty count is 0 or covered by `ownership-W0.json`.

### W1. Dependency topology and CI to green

**W1a. Topology:**
- Write the crate DAG document.
- Carry out R-8: move `egraph`, `egraph-derive` and `csp-solver` to parse-that; keep `bbnf-regex` in parse-that.
- Remove the committed `[patch]` path entries.
- Publish the leaves bottom-up: pprint, then egraph/egraph-derive/csp-solver, then the parse-that crates. Each publication needs a green CI on its branch. These are pre-1.0 bumps, and the parse-that release workflow is owned by wf_d837f96c (see §5).

**W1b. Portable build:**
- Strip `rustc-wrapper`, `-Z` and `build.target` from the committed `.cargo/config.toml`, per C2-A3.
- Keep the profiles and aliases.
- Add a `rust-toolchain.toml` only if nightly is truly required, and justify it in the file.

**W1c. CI:**
- Runner matrix: bbnf adds `ubuntu-latest` and an SDE AVX-512 job; parse-that adds `macos-14`.
- Add the tier-selection assertion test.
- Fix origin `parser_test.rs` importing the absent module.
- Merge `sk/w0-reconcile` and pprint's `sk-lineage/pprint-local` by ordinary merge after green.

**Falsifier:**
- any red Rust job on master;
- any tier that never executes in CI;
- a publish dry-run failure.

**Gate:**
- 2 consecutive green master runs per repo, on x86_64 and aarch64;
- every tier asserted executed;
- `cargo publish --dry-run` green in topological order.

### W2. Rulings, bench constitution, baseline, feasibility seam

**Work:**
- Record R-1..R-8 as addenda.
- Write the per-plane semantic specs (P3, P4, P5, CSS projection).
- Build the IF-8 schema and the quiet-window runner:
  - `guarded --cpu-cores 0` plus the bench mutex;
  - wait for load < 8;
  - ABAB interleaving;
  - QoS;
  - the load and process census at start and end;
  - kperf.
- Pin the opponents by tarball hash and set the shared flag profile.
- Fix the held-out corpora.
- Take the baseline for every G-*.
- Re-measure PROVE-01..14 at HEAD.
- Run the roofline per G-*.
- **Feasibility seam (tri-fold):** the owner confirms or rules each floor, then the floors are frozen.

**Falsifier:** self-vs-self CV above 3%, or a self-vs-self CI that excludes 1.0.

**Gate:**
- a self-vs-self CI containing 1.0 with width ≤ 0.04 (falling back to cycle counts after 3 failed attempts, declared);
- baseline records committed;
- floors frozen in `bench/FLOORS.json`.

### W3. IR freeze (completes sk-v26 W3)

Seam: architecture plus API freeze.

**Work:**
- Migrate consumers off `ModuleInput`, `compile_module` and edit sessions, then delete them.
- Write the IF-1 contract doc.
- Lattice laws as proptests.
- The held-out grammar.
- sk-v26 W3 itself: public-witness, regen and zero-diff gates, and the freeze receipt.
- Re-prove the Codex W2-spine receipts under R-3.

**Falsifier:**
- a non-commutative, non-associative or non-idempotent `join`;
- an unstable plan hash;
- a MANIFEST construct the spine cannot express (R-4 reopens).

**Gate:**
- IF-1 green;
- zero-diff regen;
- the old `crates/ir` has no live consumer;
- the held-out grammar compiles first try.

### W4. Certified passes

Seam: architecture.

**Work:**
- Re-admit, one at a time, as IF-2 passes: FIRST dispatch, memo selection, inlining (cost model), left-rec→Pratt, CSP (typed, all-or-nothing) and e-graph islands (bounded; each rule proved or fuzz-certified).
- Delete the unsound G3 `Alt([x,ε])→x` rule.
- Delete `let _ = csp.propagate()`, the all-dirty fallback and the budget/unsat write-back.
- Measure CSP and the e-graph against a direct-worklist baseline. A pass that does not beat it on compile time or parse throughput is retired (O012).

**Falsifier:** a differential failure, or a certificate that does not re-check.

**Gate:**
- every admitted pass is clean at 10⁵ inputs locally and 10⁶ in CI;
- its throughput delta is recorded with a CI;
- its compile-time cost is recorded.

### W5. parse-that kernel reconciliation (U.W1)

Seam: cutover.

**Work:**
- Merge ABI-3 (0.5.0) with origin, re-derived from `heads-W0.json`. The base is `47e928acc` or later, already containing x-p-w9-rel.
- Version 0.6.0 (no existing number is reused).
- Rule on SpanParser and the Rust CSS parser: they move to a bbnf-generated grammar or are deleted. No custom grammar.
- Build the shared TS/Rust law-vector corpus.
- Carry the T-family RED defects (SCC/bundle, artifact-skip, loader identity, dispatch-domain, diagnostics, EOF) as gates.
- Re-prove ABI-3 under R-3.

**Falsifier:** any law-vector divergence between TS and Rust.

**Gate:**
- law vectors 100% on both faces;
- T criteria 1–7 GREEN.

**Ownership:** the release waits for wf_d837f96c to hand off.

### W6. Regex, one home (U.W3–5, GRAFT-3, the TS Dfa port, R010)

**Work:**
- Implement on the W1a home.
- One dispatch surface for class-scan versus DFA.
- A TS table emitter.
- The prefilter. The SIMD prefilter consumes W7 kernels, so its sub-gate closes after W7.

**Falsifier:** a G-RX-1 violation under the restated rule, or a law-vector mismatch.

**Gate:**
- G-RX-1 floor met;
- 126/126 parity;
- a single crate home.

### W7. SIMD scanner kernels and dispatch (U.W2, 4B padded-input, 9C Sheng)

**Work:**
- Migrate the kernels from `skinny/crates/bbnf-simd` into parse-that `simd-scan` under the IF-6 ABI.
- Add the R-1 tiers.
- Every `unsafe` block gets a `// SAFETY:` note, a scalar twin, a fuzz target, ASan, the guard-page test, and Miri on scalar and fallback (`-Zmiri-symbolic-alignment-check`).

**Falsifier:** a fuzz divergence, an ASan or Miri finding, or a guard-page fault.

**Gate:**
- the checkasm harness green on every tier in CI;
- coverage saturation plus a minimum exec count per kernel per tier, summed from CI and shard receipts;
- cycles per byte recorded per kernel in a quiet window;
- shufti ≤ 0.115 cyc/B, re-measured.

### W8. ASM seam, dav1d-style (O010)

Seam: perf claim.

**Work:**
- Hand-written `.S` (aarch64 first; x86_64 only for tiers W7 already proved), dispatched through the same IF-6 table.
- Written only for a kernel whose intrinsic version measures at least 15% below its W2 roofline.
- Each `.S` entry passes the W7 checkasm harness and differential fuzz against both the scalar twin and the intrinsic twin.
- No inline `asm!` outside the seam module.

**Falsifier:** an `.S` kernel that does not beat its intrinsic twin by a Holm-adjusted CI. It is deleted. ASM earns its place or goes.

**Gate:**
- every kept `.S` kernel has a claim-grade IF-8 record against its intrinsic twin;
- the harness is green on every tier.

### W9. Value/Tape planes P1..P7 (G-JSON)

**Work:**
- Build the lossless tape, DOM, typed direct-to-struct, lazy get and transform/print planes on W7/W8 kernels, generated from the BBNF JSON grammar.
- The opponent-advocate seat (an Opus critic whose brief is to make the opponents fastest and fair) reviews the setup before measurement.

**Falsifier:** a plane-spec violation, or an output mismatch against the opponent.

**Gate:**
- the G-JSON-1..3 cells reported per plane and per corpus, each as a claim-grade win, a loss with numbers, or NOT-COMPARABLE.

**Seam:** perf claim.

### W10. Competition: generators and combinators (G-GEN-1/2, G-CSS-1)

**Work:**
- Write each opponent's idiomatic best form; the opponent-advocate seat writes or reviews them.
- Same inputs and the shared flag profile.
- Run the Lightning CSS rows on the AST-projection oracle.

**Falsifier:** the advocate shows a faster idiomatic opponent form than the one measured.

**Gate:**
- every G-GEN and G-CSS row reported, with compile time and code size.

**Seam:** perf claim.

### W11. Printer and formatting (IF-7, G-PP-1)

**Work:**
- gorgeous printer derivation from the Plan to `pprint::Doc`.
- The pprint layout engine: the Wadler/Leijen algebra plus the zero-unsafe `Serializer<'a>` line.
- Formatter CLI entry points for the JSON and CSS grammars.

**Falsifier:** a round-trip or idempotence failure on any corpus.

**Gate:**
- the IF-7 laws at 100%;
- a G-PP-1 record.

### W12. Backend parity and the TS emitter (R-5, BD-G5)

**Work:**
- Capture the Rust `backend/ts` divergences as parity failures, then delete it.
- The IF-4 parity matrix across {Rust, TS, WASM}.
- Make the measured R-5 frontend decision (bundle bytes, cold compile, parity).
- The TS regex tables from W6.
- **Ownership:** the TS release touches `typescript/`, which wf_d837f96c owns. W12 builds on a branch beside it and releases only after the hand-off.

**Falsifier:** any parity-cell mismatch.

**Gate:**
- the parity matrix is byte-identical;
- the R-5 decision is recorded with its numbers.

### W13. Publication and close (BD-G7, GN-0)

Seam: close.

**Work:**
- Re-pin the opponents and re-measure every claim.
- Delete `skinny/` (GN-0).
- Publish to crates.io and npm bottom-up in R-8 order.
- Bump value.js's BBNF pin through its owner (see §5).
- Write the closing ledger.

**Falsifier:**
- a re-measured claim that falls inside noise (it is downgraded and reported);
- a publish out of topological order.

**Gate:**
- G-HYG green across all three repos;
- every claim either re-confirmed or reported as a loss;
- the close receipt.

## 5. Lockstep and concurrency contract (completes the truncated section)

**Owners until they close. sk-v seats never write their files.**

| Owner | Owns |
|---|---|
| wf_d837f96c (xp-w9-w10-continue) | bbnf-lang `typescript/` releases, parse-that's release workflow, value.js's BBNF pin |
| megatranche-realization | value.js app work |
| x-0hr-residuals | value.js app work |
| daraksha-v3 | daraksha repo |

- fourier-analysis and keyframes.js are touched by no sk-v unit.
- value.js `scripts/dev/dev.sh` is never touched.

**Coordination:**
- Commits on branches. A unit that needs a file owned by someone else commits a request note to `~/.dev-logs/bbnf-lockstep/requests/` and waits for the owner's merge.
- At its start, every unit re-reads `ownership-W0.json` and the live census, refreshed per wave.
- When wf_d837f96c closes, its handoff is recorded, and W5's, W12's and W13's release steps become unblocked.

**Slots (R-7):**
- At most 2 sk-v workflows while any of the four owners is open, and at most 4 in total with no other workflows running.
- At most one heavy run per seat.
- Timing seats need the bench mutex. They may be scheduled to wait for the other owners' heavy runs to drain, and that wait is waited for, never claimed.

**Wave series (each wave is one or two workflows within the cap):**

| Wave | Units |
|---|---|
| α | W0 |
| β | W1 (a, b, c) |
| γ | W2 + W3 |
| δ | W4 + W5 |
| ε | W7 + W6 (W6's prefilter sub-gate after W7) |
| ζ | W8 + W11 |
| η | W9 + W10 |
| θ | W12 |
| ι | W13 |

- W11 depends only on W3, so it may be pulled forward into δ or ε if a slot is free.

**The megatranche union:**
- value.js X.P records (`docs/tranches/X/parse-that/`, COHESION §0ht–§0hw) are inputs to W5, W6 and W12.
- Each X.P row is homed to a unit in the W2 addendum. Nothing is dropped, and anything left over goes to W13.

## 6. Fuzz and timing budget (per C1-7)

| Unit | Local (guarded shards, ≤ 80 min each) | CI (scheduled nightly) | Expected calendar time |
|---|---|---|---|
| W4 | 10⁵ per grammar per pass, admission smoke | 10⁶ per grammar per pass, close | ~1–2 weeks of nightlies |
| W7 | smoke per kernel per tier | exec minimum plus coverage saturation, x86 and aarch64 | ~2 weeks |
| W8 | smoke | ASM entries added to the W7 jobs | ~1 week |
| W2, W9, W10, W13 timing | ≥ 3 sessions on different days per claim family, each session in the bench mutex | none (not usable for perf claims) | ≥ 3 days per family |

Corpora are kept in `~/.dev-logs/bbnf-lockstep/fuzz/<target>` and in CI caches. The gates count summed shard receipts.

---

**Artifacts this adjudication will rely on:**
- `/Users/mkbabb/.dev-logs/bbnf-lockstep/heads-W0.json` (W0)
- `/Users/mkbabb/.dev-logs/bbnf-lockstep/ownership-W0.json` (W0)
- `/Users/mkbabb/Programming/bbnf-lang/grammar/MANIFEST` (W0)
- `/Users/mkbabb/Programming/bbnf-lang/.cargo/config.toml` (lines 40–41 to change in W1b)
- `/Users/mkbabb/Programming/parse-that/rust/regex/Cargo.toml` (lines 35–36, the egraph dependency resolved in W1a)