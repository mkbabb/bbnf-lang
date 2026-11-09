# rust-perf: adjudicated final design (2026-11-05)

**Adjudicator's read-only checks** (no source changed, nothing built or run):

- **bbnf-lang branches.** Local `master` is at `af15f63e0` and `origin/master` at `c9b93adb4`. They have diverged.
- **bbnf-lang `.cargo/config.toml`.** It sets `rustc-wrapper = "sccache"` and `[build] target = "aarch64-apple-darwin"`. Both are mandatory as committed.
- **bbnf-lang `rust-toolchain.toml`.** It pins `nightly-2026-04-11` plus `rustc-codegen-cranelift`, and calls itself "nightly-only".
- **Two workspaces.** The root members are `crates/{core,analysis,ir,lsp,ser,gorgeous,bootstrap,egraph,egraph-derive,csp-solver,simd-scan,bbnf-path,bbnf-path-ts}`. `skinny/` is a separate workspace with `ir, passes, codegen, runtime, bbnf-regex, parse-that-regex, bbnf-simd, simd-scan, bbnf-bench, grammar`. `skinny/passes` depends on `bbnf-regex`, `csp-solver`, `egraph` and `ir`.
- **Dependency cycle.** `crates/core` takes `parse_that = "0.4"` from crates.io. parse-that `rust/regex` takes `egraph = "0.1"` and `egraph-derive = "0.1"` from crates.io, and both crates live in bbnf-lang. The cycle is real.
- **skinny/RESULTS.md, canada rows.** Every row shows Track 1 winning:
  - `parse_only` 16709.9 Mbps against sonic `Skipper` 12970.9.
  - `direct_to_struct` 4749.6 against 2733.7.
  - `real_typed_struct` 4761.9 against 2736.4.
- **How those rows were built.** Each one records `rustflags=-C target-cpu=native`, `simd=Scalar` and `not-applicable:no-simd-or-asm`. So the winning numbers we have on record come from scalar code built with native flags. They do not come from the SIMD tier.
- **The guard.** `~/.claude/guard/guarded --exclusive NAME` already exists. It takes a machine-wide named lock, so it keeps same-name runs from overlapping. It does not hold back other heavy runs.

## A. Rulings on the objections

| # | Objection | Ruling | Reason / disposition |
|---|---|---|---|
| C1 O-1 · C2-1 | Two compilers; the design plans for one | **ACCEPT** | Verified. Adds ruling R0-e and unit U0. The "new IR" the owner names is taken to be `skinny/crates/ir`. U6, U8 and G-Q1 can only be stated after U0. |
| C1 O-2 · C2-3 | The kernel library is in the wrong repo; the repos form a cycle | **ACCEPT** | Verified (parse-that→egraph and bbnf→parse_that). The kernel library and egraph move into parse-that's Rust workspace, which is the general base. The release DAG becomes pprint → parse-that{simd, egraph, regex, runtime} → bbnf-lang. Adds R0-f. |
| C1 O-1 · C2-3 (regex) | Three regex crates | **ACCEPT** | One crate: `parse-that/rust/regex`. skinny's `bbnf-regex` and `parse-that-regex` are folded into it by named commits in U0, then deleted. |
| C1 O-3 (target) | The hard-coded `build.target` makes x86 CI fake | **ACCEPT** | Verified. U1 deletes `[build] target`. Without that, R0-c's "tested on ubuntu CI" is false. |
| C1 O-3 · C2-2 | Nightly in the root, with no unit that fixes it | **ACCEPT** | Verified. Invariant 3 is restated as: shipped crates compile on stable, proven by a required `cargo +stable` CI job in both repos. Nightly, cranelift, `-Z` flags and sccache move to an opt-in env/profile layer. The root de-nightly goes into U1. parse-that's part stays in U5 because it needs the kernel substitution. |
| C1 O-3 (AVX-512) | GitHub runners may lack AVX-512 | **ACCEPT** | AVX-512 checkasm and fuzz run under Intel SDE in CI. If SDE cannot be wired in U4, the AVX-512 kernels are deleted, not shipped untested. |
| C1 O-4 | R0-a cannot be done as written; fast-forward is impossible | **ACCEPT** | An ordinary merge brings in every ancestor, and a cherry-pick makes new SHAs. R0-a is restated as two options (§0). The order's "fetch and fast-forward" step cannot be met on the diverged local master; this is said explicitly. All `rp/*` work happens in `git worktree`s off `origin/master`, and the primary checkout is never switched. |
| C1 O-5 · C2-7 (multiplicity) | No multiple-comparison correction | **ACCEPT** | Headline cells are pre-registered per record. Holm–Bonferroni is applied across them at a family-wise α = 0.01. Cells that are not pre-registered are labelled exploratory and can never be cited as wins. |
| C1 O-5 (Apple silicon) | Load < 8 is not enough | **ACCEPT** | The arms are interleaved in randomized ABAB order inside one session window. The reported figure is a paired within-session ratio. Runs are discarded when `pmset -g therm` shows a CPU speed limit below 100, which needs no root. `powermetrics` (root) is optional. |
| C2-7 (rep definition) | Iterations inside one process are not independent | **ACCEPT** | One rep = one fresh process. In-process iterations are aggregated into that rep's single sample. |
| C2-7 (core placement) | macOS cannot pin a thread to a P-core | **ACCEPT** | The harness sets the QoS class to user-interactive and records `taskpolicy`/QoS. The core type is recorded where it is observable; otherwise the field says it is unobservable. |
| C1 O-5 · C2-7 (counters) | No root counters on macOS | **ACCEPT** | The instruction-count cross-check runs on Linux `perf stat` (CI or a native x86/arm Linux host). There it is a required field. On the Mac, kperf counters are optional and recorded only when available. |
| C2-7 (record sha) | The HEAD sha changes when the record is committed | **ACCEPT** | Records are keyed by the tree hash excluding `docs/benchmarks/records/`, and the commit sha is also stored. |
| C2-7 (platform) | Platform scope of claims | **ACCEPT** | Every claim reads "on Apple M-series (aarch64)". Shared-runner numbers never certify x86 speed; x86 CI proves correctness only. |
| C1 O-5 · C2-5 | G-J1 has no output contract | **ACCEPT** | Planes are defined in §2.1. Each cell names the exact competitor API and includes a checksum walk inside the timed region for every contestant. |
| C2-5 · C2-6 (canada) | The 1.56× loss has no plane; U10 aims at the wrong cause | **ACCEPT** | Verified: skinny wins all three recorded canada planes. The 1.56× (RP-13) must be traced to the plane, API and HEAD where it was measured, or retracted. U10 becomes attribution first (cycles split across lexing, float conversion, tape writes, allocation), then the remedy. A lazy-number tape is its own plane and never crosses canada. |
| C1 O-5 (G-C1) | lightningcss parse does not do the same work | **ACCEPT** | G-C1a: cssparser, tokenize plus rule tree. G-C1b: lightningcss, only against a typed-output grammar for the same property set; otherwise the cell is labelled ASYMMETRIC and cannot be a headline. |
| C1 O-5 (G-P1) | ≥ 1.5× and ±10% contradict each other; we would write the reference parser | **ACCEPT** | The ±10% hand-written-winnow clause is dropped. References are competitor-authored only: winnow's and nom's examples, pest's published JSON grammar, tree-sitter-json, chumsky's examples. Where no reference exists for a grammar, the cell is DESCRIPTIVE and carries no target. |
| C2-5 (track) | RESULTS.md mixes Track 1 and Track 2 | **ACCEPT** | Every G-* cell is tagged `generated` or `hand`. Only `generated` cells count toward goals. |
| C2-9 | Build flags can be rigged | **ACCEPT** (text truncated; ruled on its stated intent) | Two build matrices, identical across contestants inside each. PORTABLE: no `target-cpu`; ours goes through runtime dispatch; this is the headline plane. NATIVE: `-C target-cpu=native` for everyone; it satisfies the law's "recommended flags" and is labelled NATIVE. Verified: today's records are NATIVE plus scalar, so none of them proves anything about dispatch. |
| C1 O-6 | The regex oracle is the wrong dialect | **ACCEPT** | BBNF regex leaves are ECMAScript. The dialect is specified as a BBNF grammar (the BBNF-only law). Primary oracle: `regress` (ECMAScript). Secondary: the `regex` crate, only on the fragment where semantics coincide. The fuzzer generates the full syntax; each case scores agree-with-oracle or rejected-with-diagnostic. Adds a cross-backend parity cell (TS against Rust; read-only on TS while xp-w9-w10 is open). |
| C1 O-6 (adjudicator corollary) | Rejecting lookaround could break real grammars | **ACCEPT (added)** | U7 first counts lookaround in the 121 value.js leaves. If any leaf uses it, rejecting it breaks a shipped grammar, and a lookaround charter becomes a gate for G-R1 instead of being out of scope. |
| C2-4 | Releases collide with xp-w9-w10 | **ACCEPT** | No crates.io publish happens before unit U-REL. Before that, cross-repo edges are `git = …, rev = <sha>` dependencies. U-REL is gated on wf_d837f96c being closed and publishes in DAG order. U1's "publish egraph" is withdrawn. |
| C2-8 | The quiet window will be starved | **ACCEPT, modified** | `guarded --exclusive bench` already exists and serializes bench seats. It does not hold back other heavy runs, so the orchestrator schedules a bench window by not dispatching heavy seats while a record seat holds the lock. A guard-level drain mode would change machine law, so it goes to the owner as an ask (R0-g). Time box: after 4 h of waiting, the record runs and is labelled NOISY; it is never certified. |

Nothing was rejected. Every objection was either verified in the tree or follows from the law as written.

## 0. R-0 gate: rulings the owner must make first

| # | Question | Recommended default |
|---|---|---|
| R0-a | Topology. Local master has 71 commits not on origin and origin has 38 not local; fast-forward is impossible. | **(a)** Push the 71 local commits as they stand to `archive/sk-v25-local`, then make an ordinary merge of it in full into `rp/merge-sk-v25` (off `origin/master`, in a worktree), after review. No SHA is rewritten, and `restart/` docs are already the repo's convention. **(b)**, only if the owner wants the docs out of master: `cherry-pick -x` of the code commits (sk-v18-G1..G6 incl. `369f4ffe7`, plus `c6b9245c3`'s `json_canon_bench.rs`), with provenance. This creates new SHAs, so it needs the owner's explicit allowance. |
| R0-b | Uncommitted July work (bbnf-lang 243 files, parse-that 35). | Read-only capture: `git diff --binary` to `~/.dev-logs/bbnf-lockstep/wip/<repo>-2026-07-20.patch`, plus a tarball of the untracked files. The tree is left untouched. The owner then picks: commit on `wip/2026-07-20-x86-purge`, or discard. |
| R0-c | x86 law conflict. | Keep x86 under runtime dispatch, with NEON primary. An x86 kernel survives only with an oracle, a fuzz target and native ubuntu CI (after the U1 target fix). AVX-512 additionally needs SDE. Any x86 kernel without an oracle is deleted. |
| R0-d | sk-v26 Codex branches (`b5fd0ca98`, `e31fbfe`). | Mine them, do not adopt them. Three separately reviewed lifts: the stable de-nightly (feeds U1 and U5), sealed-span and transactional kernels (feed U2), and the SCC / min-consumption analysis (feeds U0 and U8). Do not take the 26.8k-line deletion. |
| **R0-e (new)** | Compiler of record. | **skinny's `ir`, `passes`, `codegen` and `runtime` become the backend of record.** They are merged into the root workspace, and root `crates/ir` is replaced by skinny's IR. `crates/core` is reduced to the front end (parse and analysis), the LSP, the TS/other emitters and the bootstrap. This is confirmed or overturned by the U0 census before any merge. |
| **R0-f (new)** | Home of the kernel library and egraph. | `parse-that/rust/simd` (crate `parse-that-simd`) and `parse-that/rust/egraph{,-derive}`, moved out of bbnf-lang. `csp-solver` stays in bbnf-lang, since only the compiler uses it. |
| **R0-g (new)** | Bench window. | Pick one: (i) the orchestrator schedules the bench window (default, no machine-law change); or (ii) a guard `--drain` class that holds back new heavy admissions while a `bench` lock is held. Option (ii) is the owner's machine law. |

## 1. Target architecture

```
 pprint ──────────────┐ (formats emitted source; one pinned API)
                      ▼
 parse-that (general library, Rust workspace)
   rust/simd   — the ONLY core::arch code; NEON | AVX2 | AVX-512(SDE-tested) | simd128 | scalar
   rust/egraph, rust/egraph-derive
   rust/regex  — the ONE regex crate: ECMAScript-dialect HIR → NFA → min-DFA → ByteClass/accel/prefilter
   rust/parse_that — runtime: sealed Span, sealed PaddedInput, scanners call rust/simd
                      ▼  (git rev deps until U-REL; crates.io after)
 bbnf-lang
   crates/core (front end + LSP + TS emit) ──▶ bbnf_ir (skinny ir, promoted)
       ──▶ passes (egraph saturation [gated], CSP solve [consumed], FIRST/nullable/SCC/min-consumption, ByteClass)
       ──▶ LoweringPlan ──▶ rust codegen (from skinny) ──▶ generated parser ──▶ parse-that runtime + simd
   crates/csp-solver
```

Invariants:

1. **One kernel library, in parse-that.** No other crate contains `core::arch` code, and generated code never inlines SIMD.
2. **Dispatch.** An `OnceLock<KernelTable>` is filled by `is_aarch64_feature_detected!` / `is_x86_feature_detected!`; on aarch64-apple it resolves statically. `--cfg bbnf_force_scalar` and `cfg(miri)` both select the scalar table.
3. **Stable.** Shipped crates compile on stable, proven by a required `cargo +stable` CI job in both repos. Nightly, cranelift, `-Z` flags and sccache are opt-in env/profiles only. There is no committed `build.target`.
4. **Soundness.** No safe API can reach UB. Every `unsafe` block has a `// SAFETY:` comment witnessed by a test. Miri runs over the scalar tables and the whole safe runtime surface.
5. **ASM.** `asm!` is allowed only by charter (U12): a disassembly diff must show at least 10% left on the table, and the kernel ships with a scalar oracle, checkasm and fuzz.
6. **Acyclic release DAG.** pprint → parse-that → bbnf-lang. No edge points back.

## 2. Measurable goals

### 2.1 Bench-of-record protocol (U3)

**Setup and process**
- The run holds `guarded --exclusive bench --cpu-cores 0`, and starts only once the 1-minute load is below 8, waited for.
- Throttled runs are discarded (`pmset -g therm`).
- QoS is set to user-interactive.
- At least 10 reps, each rep a fresh process. Arms are interleaved in randomized ABAB order.

**Statistics**
- Report: median, IQR, and the paired within-session ratio with its 95% CI.
- A win requires all three: non-overlapping IQRs, a Holm-corrected Mann-Whitney test at family α = 0.01 over the pre-registered headline cells, and an effect of at least 5%.

**Build matrices and contestants**
- PORTABLE is the headline matrix; NATIVE is labelled as such. Flags are identical across contestants within a matrix.
- Competitors are built from current crates.io releases with their README-recommended features. A profile/flag assertion catches debug or rigged builds.

**Measurement and records**
- Every contestant gets a checksum walk inside the timed region.
- Instruction counts come from Linux `perf stat` (required there). kperf counters on the Mac are optional.
- Records are keyed by the tree hash excluding the records directory, with the commit sha stored too. Every cell is tagged `generated` or `hand`, and only `generated` cells count.
- Claims are scoped to "Apple M-series".
- After 4 h without a quiet window, the run is labelled NOISY and is never certified.

**JSON planes (G-J\*)**
- **P-DOM:** an owned tree, strings unescaped, numbers parsed to i64/u64/f64. Contestants: `sonic_rs::from_slice::<sonic_rs::Value>`, `simd_json::to_owned_value`, `serde_json::from_slice::<Value>`.
- **P-TAPE:** a borrowed tape, strings unescaped, numbers parsed. Contestant: `simd_json::to_tape`. sonic-rs has no entrant here.
- **P-SKIP:** validate and skip. Contestants: sonic `Skipper`/`LazyValue`, simd-json stage 1.
- **P-STRUCT:** a typed serde struct. Contestants: `sonic_rs::from_slice::<T>`, `serde_json`, `simd_json::serde`.
- **P-LAZYNUM:** a lazy-number tape. It is its own plane and can never stand in for P-DOM or P-TAPE.

### 2.2 Goal table

| ID | Plane | Opponent | Inputs | Target at p50, PORTABLE, `generated` |
|---|---|---|---|---|
| G-J1 | P-DOM, P-TAPE | sonic-rs 0.5.10 (DOM), simd-json 0.19.0 (DOM and tape), serde_json | twitter, citm, canada, github_events, gsoc-2018, mesh | ≥ 1.15× sonic DOM on twitter and citm; ≥ 1.30× simd-json on all six. **The canada target holds until RP-13's 1.56× is traced to a named plane and API (U10 step 1).** |
| G-J2 | P-SKIP and path get | sonic `get`, jiter 0.18.0 | the six inputs × 3 paths | ≥ 1.30× sonic get; ≥ 2× jiter |
| G-J3 | Validation | simdutf8, simd-json stage 1 | the six inputs plus 64 MB synthetic | ≥ 1.0× simdutf8; ≥ 1.2× simd-json stage 1 |
| G-C1a | tokenize plus rule tree | cssparser (latest) | bootstrap, tailwind-full, value.js corpus | ≥ 1.5× |
| G-C1b | typed property values | lightningcss 1.0.0-alpha.72 | the same, restricted to an equivalent typed property set | ≥ 2.0×, else labelled ASYMMETRIC and not a headline |
| G-P1 | same grammar | competitor-authored reference parsers: pest, nom 8, winnow 1.0.5, chumsky 1.x, lalrpop, tree-sitter | JSON, CSV (RFC 4180), arithmetic (1 MB), CSS subset | ≥ 1.5× the fastest reference; grammars with no reference are DESCRIPTIVE |
| G-R1 | regex correctness | `regress` (ECMAScript, primary); `regex` 1.x (coinciding fragment) | 11-case probe, 121 value.js leaves, 10k full-syntax fuzz | 0 disagreements; every unsupported construct is rejected with a diagnostic naming its leaf; TS↔Rust parity cell at 0 diffs |
| G-R2 | leaf throughput | regex-automata dense DFA | leaf corpus over CSS inputs | ≥ 1.0× find; ≥ 1.5× anchored leaf match |
| G-K1 | kernel micro | the kernel's own scalar oracle | random, corpus, adversarial | NEON ≥ 3× oracle, or the kernel is deleted; **the kernel must also move at least one G-J or G-C cell beyond noise, or it is deleted** (today's winning rows are `simd=Scalar`) |
| G-Q1 | compiler quality | itself, A/B per pass | JSON and CSS grammars | a pass that does not move G-J1 or G-C1 beyond noise is off by default or removed |
| G-PP1 | pprint | prettyplease, plus the previous record | 50k-line generated parser | ≥ 1.0× wall; linear memory; byte-identical output across runs |

Every existing SPEC §1 headline is reclassified NOISY / NATIVE-only / stale-base until a PORTABLE `generated` record reproduces it at the merged HEAD.

## 3. Ordered units

Each unit has a falsifier (RED before the unit, GREEN after, run twice) and a gate that must hold before dependent units start. All work happens in worktrees off `origin/master` and is committed by pathspec.

**U0. One compiler, one IR, one regex, one simd** (depends on R0-a, R0-e, R0-f)
- **What:**
  - Census, root pipeline against skinny: IR types, passes, codegen, runtime, tests, benches.
  - Owner ruling on the census.
  - Promote skinny `ir`, `passes`, `codegen` and `runtime` into the root workspace as `bbnf_ir` and siblings. Repoint `crates/core` at them. Retire root `crates/ir` and its consumers by named commits.
  - Fold skinny `bbnf-regex` and `parse-that-regex` into `parse-that/rust/regex`.
  - Fold skinny `bbnf-simd` and both `simd-scan` crates into `parse-that/rust/simd` (the move itself; dispatch and fuzz come in U4).
  - Move egraph and egraph-derive to parse-that.
  - Cross-repo edges become git rev deps.
- **Falsifier:** a grep census shows 2 IR crates, 3 regex crates and 2 simd-scan crates today; 1/1/1 after. The skinny JSON/CSS parity tests pass unchanged on the merged workspace. The dependency graph has no cycle (`cargo metadata` check in CI).
- **Gate:** one root workspace; skinny is no longer a separate workspace; the release DAG is acyclic.

**U1. CI truth** (depends on U0)
- **What:**
  - Delete `[build] target`. Make sccache an opt-in `RUSTC_WRAPPER`. Move `-Z`, cranelift and threads settings to an opt-in profile/env.
  - Move the root workspace to stable (MSRV recorded). The nightly pin becomes an optional dev toolchain file.
  - Add required jobs: `cargo +stable build/test` on ubuntu x86 (native) and macos-14 arm64; arm64 checkasm.
  - Port or delete parse-that `tests/regex/parser_test.rs` (RP-11).
  - Resolve egraph via U0's move (no crates.io publish).
- **Falsifier:** CI on `c9b93adb4` and `47e928a` is red today, and `cargo +stable build` fails today. Both go green.
- **Gate:** both masters green on stable across x86 and arm64; at least 61 kernel tests on arm64.

**U2. Soundness** (depends on U1; parallel with U3)
- **What:**
  - Seal `Span` (private constructor, checked `as_str`, `get_unchecked` only behind a constructor-proven invariant).
  - Seal `PaddedView` with a padding constructor.
  - Fix BumpSlab alignment and drop, `lazy` re-entry, re-entrant Clone in `alloc_slice_clone`.
  - Replace the erased `*const ()` context with a typed handle.
  - Add a Miri CI job.
- **Falsifier:** the 10 PT-K counterexamples (RP-10) as tests: UB under Miri before, pass or compile-fail after.
- **Gate:** Miri clean over the safe runtime surface; inventory of unsafe sites, each with a SAFETY comment.

**U3. Bench of record** (depends on U1)
- **What:** the `benches/record` crate plus `xtask bench-record`, implementing §2.1 exactly: planes, matrices, ABAB, fresh-process reps, Holm, thermal discard, the `--exclusive bench` lock, the NOISY time box, tree-hash keys, Linux `perf stat` instruction counts. Competitor pins: simd-json 0.19.0, sonic-rs 0.5.10, jiter 0.18.0, winnow 1.0.5, lightningcss alpha.72, cssparser latest. The literature plane is yyjson and RapidJSON via `cc` at their release tags (descriptive only).
- **Falsifier, all four must hold:**
  - a run under load ≥ 8 refuses or labels itself NOISY;
  - a debug-built competitor is caught;
  - a contestant that skips the checksum walk is caught;
  - a NATIVE build in the PORTABLE matrix is caught.
- **Gate:** a first certified baseline at the merged HEAD covering every G-* cell, wins and losses alike. The RP-13 canada 1.56× is traced to its plane and API or retracted.

**U4. One kernel library: dispatch and proof** (depends on U0, U1, R0-c)
- **What:**
  - In `parse-that/rust/simd`: the OnceLock dispatch table and `bbnf_force_scalar`.
  - Move the checkasm fixture out of `restart/…/sk-v12/research/w1b/` into `rust/simd/tests/fixtures/`.
  - One `cargo-fuzz` differential target per kernel against its scalar twin.
  - AVX-512 under SDE, or delete it.
  - Measure and settle the `build_lo6_table` hoist (RP-06).
- **Falsifier:** an injected 1-bit fault is caught by checkasm and by fuzz within 60 s; force-scalar output is bit-identical on the corpus.
- **Gate:** checkasm green on arm64 and x86; 10 min fuzz per target with no finding; G-K1 holds or the kernel is deleted.

**U5. parse-that onto the kernels; de-nightly parse-that** (depends on U4, U2)
- **What:** replace `quoted_simd`, `number_simd`, `quote_parity`, `structural_bitmap`, `ws_comment`, `balanced` and the decode scanners with calls into `rust/simd`. Remove `#![feature(portable_simd)]`. Toolchain goes to stable; nightly is dev-only.
- **Falsifier:** `cargo +stable test` fails today and passes after, with the existing oracle tests unchanged.
- **Gate:** U3 record shows no regression beyond noise on G-J1 or G-C1.

**U6. Codegen routes to kernels** (depends on U0, U4, U8's interface)
- **What:** skinny codegen (now root) stops emitting `std::simd u8x16` and compile-time-only AVX2, and emits calls through the `ScanPlan`→kernel ABI. `xtask regen` regenerates all grammars.
- **Falsifier:**
  - the emitted JSON parser contains `std::simd` today and has zero hits after;
  - every generated parser's output equals the force-scalar build on the corpus.
- **Gate:** PORTABLE G-J1 twitter and citm move by at least the PROVE-09-refuted margin. If they do not, the result is written up as a measured negative.

**U7. Regex: dialect, correctness, then speed** (depends on U0, U1)
- **What, in order:**
  - (0) Write the ECMAScript-subset dialect as a BBNF grammar. Count lookaround and backreferences in the 121 value.js leaves.
  - (a) Fix the 5 DFA disagreements (anchors, alternation priority, lazy quantifiers, case-fold, `\b`), judged by ECMAScript semantics via `regress`.
  - (b) Fix the non-ASCII panic.
  - (c) Lookaround: compile-time rejection naming the leaf. If (0) found any leaf using it, a lookaround charter becomes a gate for G-R1.
  - (d) Literal/ByteClass prefilter for `Dfa::find` (PROVE-10 rebased).
  - (e) Commit the 56/56 parity suite.
  - (f) TS↔Rust parity cell, read-only on TS while xp-w9-w10 is open.
- **Falsifier:** the probe fails 5 of 11 today, target 0; 10k full-syntax fuzz with 0 disagreements and 0 silent mis-matches.
- **Gate:** G-R1, then a G-R2 record.

**U8. IR and passes that decide performance** (depends on U0; mines sk-v26)
- **What, on `bbnf_ir`:**
  - Delete or repair the G3 `WrapOfEpsilonScalar` rewrite.
  - Make `TypeDomain::join` a real lattice join.
  - Make the scheduler consume the CSP solution into `LoweringPlan` (no `let _ = csp.propagate()`).
  - Gate saturation: an unsaturated or over-budget e-graph falls back to the un-rewritten IR with a diagnostic.
  - Add SCC / min-consumption analysis.
- **Falsifier:** one property test per defect, RED today:
  - join is commutative and idempotent;
  - a CSP-infeasible grammar changes the plan;
  - an epsilon wrap preserves semantics.
- **Gate:** G-Q1 A/B per pass: a measured effect, or the pass ships off by default.

**U9. PROVE patches, one per unit** (depends on U4–U8)
- **Order:** 01 (NEON shufti/truffle) → 03 (vshrn fused string scan) → 05 (CSS shufti) → 12 (const-fold delimiter mask) → 06 (stop-early get) → 07 (direct-to-struct) → 08 (type plane) → 13 (CSV) → 14 (offset tape).
- **Out of scope here:** 10 lands in U7; 11 is TS and belongs to xp-w9-w10. The RP-16 negatives (02, 04, 09) are not re-attempted without new evidence.
- **Gate:** each patch merges only with a certified PORTABLE win, or a neutral result that simplifies the code. A loss shelves the patch and keeps its record.

**U10. Canada, attribution first** (depends on U3's traced cell, U9's 07 and 08)
- **What:**
  - Step 1: profile the losing plane. Split cycles across number lexing, float conversion, tape writes and allocation.
  - Step 2: choose the remedy from that split: tape layout, allocation, number-token materialization, or float conversion (Eisel-Lemire plus a correctly rounded fallback only if conversion dominates).
  - A lazy-number tape is P-LAZYNUM and never counts for the canada crossing.
- **Falsifier:** the attribution itself, committed. Then: bit-exact against `f64::from_str` on canada plus 10M random floats plus the edge corpus.
- **Gate:** canada at least 1.00× the named opponent on the traced plane, or a measured negative.

**U11. CSS and parser-generator competition** (depends on U6, U7)
- **What:** G-C1a, G-C1b (or the ASYMMETRIC label), and G-P1 against competitor-authored references, all `generated`.
- **Gate:** certified record; losses kept.

**U12. ASM charter** (depends on U4 and U9; optional)
- **What:** per candidate kernel, a disassembly diff against the intrinsics codegen. `asm!` only where at least 10% is proven left on the table. It ships behind dispatch with oracle, checkasm and fuzz (Miri cannot see it).
- **Gate:** G-K1 plus a cell moved beyond noise, or the kernel is withdrawn.

**U13. pprint** (depends on U0)
- **What:** codegen formats emitted source through a pprint `Doc`. One pinned pprint API, as a git rev dependency, shared by the parse-that and bbnf pretty-printers.
- **Falsifier:** byte-identical output across runs and against the previous record.
- **Gate:** G-PP1.

**U-REL. Releases in DAG order** (depends on U1–U13 for each crate, and on **xp-w9-w10 (wf_d837f96c) closed**)
- **What:** publish pprint → parse-that-simd, egraph, egraph-derive → parse-that-regex, parse_that → bbnf crates. Swap git rev deps for crates.io versions. The begin-word authorizes publishing.
- **Falsifier:** a clean `cargo publish --dry-run` chain in DAG order; a fresh crate depending only on crates.io builds on stable.

## 4. Concurrency and standing law
- wf_d837f96c owns bbnf `typescript/`, parse-that's release workflow and value.js's BBNF pin until it closes. U7(f) only reads TS, and U-REL waits for it.
- Never stash, reset, rebase or force. Commits go by pathspec on branches, and branches merge to master by ordinary merge after CI. Never touch value.js `scripts/dev/dev.sh`.
- Every build, test and bench goes through `guarded`. cargo builds are heavy, with one heavy run per seat. Bench seats use `--cpu-cores 0 --exclusive bench`.
- Durable scratch goes in `~/.dev-logs/bbnf-lockstep/`.
- The SIMD and soundness laws are as stated in §1.