# sk-v27 "Contest": adjudication and final design

**Role:** adjudicator for the competition design. I rule on every objection from both critics, then give the final design. Where a dispute was about facts, I checked them read-only (git and file reads, no builds); those checks are marked [V].

- [V] bbnf-lang: `origin/master` is `c9b93adb4`; local `master` is `af15f63e0`.
- [V] parse-that: `origin/master` is `47e928a` ("Merge PR #2 x-p-w9-rel", 2026-11-04), not the `7098c86` the order cites.
- [V] `.cargo/config.toml:40` sets `rustc-wrapper = "sccache"` with no condition.
- [V] `rust-toolchain.toml` pins `nightly-2026-04-11`. Cranelift is used for dev builds only; the release profile keeps LLVM.
- [V] `parse-that/rust/regex/Cargo.toml:35-37` needs `egraph`, `egraph-derive` and `csp-solver` 0.1 from the registry.
- [V] Those three crates live in `bbnf-lang/crates/`, and they are true leaves at the crate level:
  - `egraph` depends on smallvec, rustc-hash and csp-solver;
  - `csp-solver` depends on include_dir;
  - `egraph-derive` depends on syn, quote and proc-macro2.
- [V] `bbnf-lang/crates/simd-scan` exists. It has `scalar.rs`, `neon.rs`, `wasm.rs`, `parity.rs`, `index.rs`, `alphabet.rs` and `compaction.rs`.

---

## Part 1: rulings

### Critic 1

**C1-1, the dependency cycle across the two repos: ACCEPTED, with the diagnosis narrowed.**
- [V] The crate graph has no cycle. `egraph`, `egraph-derive` and `csp-solver` do not depend on parse_that or bbnf. The cycle is between the two repos, and the `[patch]` overrides in `.cargo/config.toml` hide it.
- Even so, the critic's conclusion holds. Today neither repo builds from a clean clone without the other checked out beside it, and their CIs are coupled.
- **Ruling: new unit U1a.**
  - Move `egraph`, `egraph-derive` and `csp-solver` into their own leaf repo, `mkbabb/egraph-csp`. They are general libraries, not BBNF and not parsing, so they belong in neither parse-that nor bbnf-lang.
  - Publish bottom-up: the leaf crates, then pprint, then parse_that (with bbnf-regex), then bbnf.
- **Rejected alternative:** moving bbnf-regex's HIR e-graph pass into bbnf-lang. That would make parse-that's regex depend on the compiler's optimizer story, which inverts the intended layering.

**C1-2, toolchain and build-plane fairness: ACCEPTED.** I merged it with C2-1 into one unit, U1b (see below).

**C1-3, `simd-scan` already exists: ACCEPTED.** [V]
- `crates/simd-scan` already has scalar, NEON and WASM variants plus a parity module. It becomes the I-5 kernel library. No `bbnf-simd` crate gets created.
- The plane-census falsifier is adopted.

**C1-4, custody is incomplete: ACCEPTED in full.**
- Content manifest with sha256 for every path.
- 5 MB split threshold.
- pprint and both `codex/sk-v26-*` branches are pushed inside U0 itself.
- The gate is a script, not a check by eye.
- Out-of-git store: GitHub release assets on a `custody-sk-v25` tag, mirrored to `~/.dev-logs/bbnf-lockstep/custody/blobs/` and indexed by hash.

**C1-5, G-J1 has no comparator, and gates were set as if a trend were known: ACCEPTED.**
- Every P1 row must name an exact API and carry a work-equivalence note, or be labelled "no comparator".
- Every numeric gate in this document is PROVISIONAL. Gates are sealed after U6 and before any optimization unit opens, in a hashed decision-plan commit (unit U6s).

**C1-6, pins conflict and none are verified: ACCEPTED.**
- Version numbers typed into a design are not pins. U4 opens by resolving each version against crates.io, npm or the upstream tag, and records checksum and publish date in `W0-BENCH-PINS.tsv` v2.
- Policy: use the newest published release that is at least 14 days old on the day pins are resolved, and freeze it for the contest epoch.

**C1-7, CPU frequency and the quiet window are not measurable on Apple Silicon: ACCEPTED** (the text was cut off; I ruled on the stated premise).
- Frequency cannot be pinned, so the harness instead:
  - runs timed threads at `QOS_CLASS_USER_INTERACTIVE`, so they are steered to P-cores;
  - records per-sample core class where kperf/PMU allows, otherwise records `pthread_qos` plus the thermal state from `pmset -g therm`;
  - samples 1-minute load before and after every block;
  - discards any block where load is ≥ 8 or thermal state is not nominal;
  - records the M-series generation and macOS build in the manifest.
- "Quiet" is the measured condition on each block, not a claim about the window.

### Critic 2

**C2-1, toolchain fairness and publishability: ACCEPTED, merged with C1-2 into U1b.**
- Certified path:
  - stable rustc only;
  - one shared bench workspace with a single `[profile.bench]` (`lto="fat"`, `codegen-units=1`, `panic="abort"`, `target-cpu=apple-m1` baseline, or whatever both sides use, recorded);
  - the same allocator on both sides.
- `target-cpu=native` on only one side is forbidden.
- Each competitor gets a second, observational row under its own published profile.
- Nightly stays only for dev ergonomics (cranelift, parallel front end).
- Gate: `cargo +stable build --release -p bbnf -p parse_that -p pprint -p simd-scan` returns rc 0.
- `portable_simd` and every other unstable feature are banned from the certified path. `asm!` and `std::arch` are stable and allowed.

**C2-2, ownership facts are stale: ACCEPTED.** [V] parse-that `origin/master` moved to `47e928a` on 2026-11-04.
- U0 opens with `CUSTODY-RECEIPT.tsv`: every remote branch SHA in all three repos, re-fetched at the time of the receipt. Every falsifier names a SHA from it.
- **Ownership globs held by wf_d837f96c until it closes:**
  - `bbnf-lang/typescript/**`
  - `bbnf-lang/.github/workflows/*release*`
  - `parse-that/.github/workflows/**`
  - `parse-that/typescript/package.json` and `parse-that/CHANGELOG.md`
  - value.js `package.json` and `pnpm-lock.yaml` (the BBNF/parse-that pins)
- **Protocol for any edit to a held glob:**
  - The edit lives on branch `contest/ci`.
  - It merges only after wf_d837f96c's close commit, or after an explicit ACK commit on wf_d837f96c's branch that names the `contest/ci` SHA.
  - Nothing else counts as coordination.

**C2-3, the statistics cannot be met within the machine budget: ACCEPTED.**
- Pre-registered certification set: for each goal, 2 corpus families and 1 primary competitor. Holm correction applies only within that goal's family. Every other cell is OBSERVATIONAL.
- N ≥ 200 pairs applies to certification cells only. Exploratory cells use ≥ 10 reps with median and IQR.
- Each bench unit gets a 72-hour wall-clock budget. If no admissible quiet blocks arrive within it, the unit closes NOISY and is re-queued once in the next wave, not retried indefinitely.
- Cold/hot and fresh/reuse variants are dropped except where the variant is the goal.

**C2-4, several goals are not falsifiable: ACCEPTED in full.**
- **G-C2:** the coverage manifest is Lightning CSS's own property list intersected with BBNF typed coverage, committed and hashed.
- **G-G1/G-G2 adapters:** start from upstream example code where it exists. Each is reviewed by a seat that did not write it, and the review is recorded.
- **G-P2:** byte-equal output on a declared subset. Any difference removes that subset from eligibility.
- **G-J5:** schema is the citm and twitter serde structs from the simd-json and sonic-rs bench suites, at pinned commits.
- **G-G3:** bbnf emit time and rustc time on the generated code are reported separately, both cold.

**C2-5, the JSON goals are on the wrong side of the risk curve: ACCEPTED with one amendment.**
- Wave D now runs CSS P2 and regex first. They have PROVE-05 and PROVE-10 evidence, and value.js consumes them.
- JSON is limited to P1 and P4 at first. P3, P5, P6 and serialization become stretch units in Wave F.
- **Amendment:** the "bind an existing kernel library" route means our own stage-1 structural indexer, an extension of `simd-scan/index.rs`, exposed as an LIR primitive the generator targets. It does not mean linking simdjson or simd-json. Linking either would make the contestant not our parser.

**C2-6, SIMD discipline is incomplete: ACCEPTED, with OR-1 reconciled to the owner's order.** The order requires runtime dispatch including x86, so a default of "not merged" cannot be total.
- **Ruling:**
  - An x86 variant merges only if CI executes it, with its checkasm and fuzz targets, on a runner that actually has the ISA. ubuntu-latest x86_64 covers AVX2. AVX-512 variants are not merged until such a runner exists, with the CPU flags verified in the job log.
  - x86 is never certifying, because there is no quiet x86 host.
  - A `BBNF_SIMD=scalar|neon|avx2|auto` override, plus a cargo feature `force-scalar`, is exercised by checkasm and fuzz in CI. This tests dispatch, not just the scalar path.
  - Miri runs on the scalar path and the dispatch logic.
  - `cargo careful` and ASan (`-Zsanitizer=address`, nightly, CI-only and non-certifying) run on the intrinsic paths.
  - `asm!` lands only where it beats the intrinsic version by more than the practical floor on the recorded M-series generation. The asm block carries the measurement reference.

**C2-7, hidden coupling between U7, U16 and the TS face: ACCEPTED.**
- U7 compares TS output against the Rust canonical BIR-S in a side harness, `bbnf-lang/contest/ir/`, until wf_d837f96c closes.
- New unit **U7b, "TS BIR-S adoption release"**, owned by the successor release workflow that is chartered when wf_d837f96c closes. It is the only unit allowed to touch `typescript/`.

**C2-8, missing units** (the text was cut off; ruled on the parts given).
- **(a) Error-path quality: ACCEPTED** as new unit U15e, goal G-E1.
- **(b) WASM (L4w): ACCEPTED as "cut from the gated scope."** The slot stays in the architecture and `simd-scan/wasm.rs` stays under parity tests. There is no goal and no unit in this contest, and there is no implicit claim either.
- **(c) Incremental parsing: ACCEPTED as scoping.** tree-sitter and Lezer rows are labelled "batch mode; their incremental advantage is not measured". Incremental parsing is out of scope for sk-v27, and that is recorded in the ledger so it is not lost.

**Net result:** 17 objections, 17 accepted. Four of them were narrowed or amended: C1-1, C1-7, C2-5 and C2-6.

---

## Part 2: final design, sk-v27 "Contest"

### 0. Starting position

- **No certified wins.** None on any plane, against any competitor.
- **The shipped compiler loses badly.** It is 2.6x to 4,196x behind sonic-rs.
- **The trees are not fit to build on:**
  - bbnf-lang local master `af15f63e0` has diverged from origin/master `c9b93adb4` (71 ahead, 38 behind) and has 243 dirty paths.
  - parse-that `origin/master` is `47e928a` and has 35 dirty paths.
  - pprint is 39 commits ahead of its upstream.
  - `codex/sk-v26-*` is unpushed in both repos.
  - Both Rust CIs are red.
  - The repos are coupled through `[patch]`.
  - The workspace builds only on nightly.
  - Three compiler planes coexist.
- **Consequence:** custody, topology, toolchain, CI and harness come before any speed unit.

### 1. Target architecture: one compiler, five layers, one kernel library

```
BBNF source (one grammar authority; TS and Rust front-ends)
  │ I-1 GrammarAST
L1 Semantic IR BIR-S (sk-v26 spine): immutable instance, SCC, nullable/min-consumption,
   FIRST/FOLLOW, typecheck, Pratt tables; left recursion → Pratt or rejected
  │ I-2 BIR-S (canonical serialization, versioned schema, blake3 digest)
L2 Proof-carrying passes: e-graph saturation over a SOUND rewrite set (each rule has a
   property-tested witness); CSP/lattice solver whose result the scheduler CONSUMES;
   inlining and FIRST-set dispatch fusion driven by a measured cost model
  │ I-3 PassReport (JSON artifact)
L3 LIR, per rule: {dispatch: table|branch|Pratt} {RegexPlan} {KernelId}
   {memo: none|packrat|selective} {plane: P1..P7} {alloc: arena|borrowed|in-situ}
  │ I-4 LIR (plane and policy fields equal the I-8 manifest fields)
  ├─► L4r Rust emitter over parse-that ABI-3
  ├─► L4t TS emitter (shipped value.js face; typescript/ release-owned, see U7b)
  └─► L4w WASM: slot only, NOT in this contest's gated scope
        │ I-5 Kernel ABI                │ I-6 RegexPlan ABI
   simd-scan (the one kernel library:   regex engines: literal/memchr prefilter →
   scalar oracle, NEON, WASM; x86        direct-coded DFA (re2c-style) → lazy DFA →
   AVX2 only when CI-executed)           bounded NFA; TS: sticky RegExp or class loop
L5 pprint Doc IR (Wadler/Lindig) ◄── I-7 FormatSpec from BBNF annotations; both faces
```

- **Spine (needs OR-2):** sk-v26 provides L1 and L3. The e-graph and CSP return as new L2 passes. The root pipeline's known defects are not ported:
  - `WrapOfEpsilonScalar`
  - the last-writer `TypeDomain::join`
  - the discarded `csp.propagate()`
  - the saturation report that is only logged
- **`skinny/`** is a source of kernels and techniques. Its PROVE patches land one at a time into simd-scan and the LIR, each re-measured. It is then retired by an ordinary commit (OR-3).
- **Crate topology (U1a):** egraph-csp (leaf repo) → pprint → parse_that and bbnf-regex → bbnf. All are registry-resolvable. `[patch]` is for local development only and is never in effect for CI or certification.

### 2. Interfaces

Each interface has an owner, a content-addressed artifact and a guarding test.

| Id | Contract | Guarding test |
|---|---|---|
| I-1 | The TS and Rust front-ends give the same AST | AST digest equality on every repo grammar, plus value.js `css-values.bbnf` and `css-color.bbnf` |
| I-2 | BIR-S canonical serialization plus blake3 digest | Digest equality, TS vs Rust. Until U7b: side harness `contest/ir/` |
| I-3 | PassReport JSON: rules applied, e-class counts, stop reason, CSP assignments, cost inputs | Pass-on/pass-off differential: accept/reject sets, outputs and error positions identical on the fuzz corpus |
| I-4 | LIR declares the plane and the UTF-8, escape, number, duplicate-key and error policies per entry point | Plane-contract test against the reference oracle |
| I-5 | `Kernel{id, scalar, isa_variants, contract{tail, alignment, page_guard}}`, dispatched once at init; `BBNF_SIMD` override | checkasm parity per variant, cargo-fuzz differential per kernel, Miri (scalar and dispatch), cargo careful and ASan on intrinsics, page-boundary tests, override matrix in CI |
| I-6 | RegexPlan: expression, capability set, engine choice with cost rationale; TS lowering | Per terminal: `regex`/regex-automata 0.4 (Rust) and native RegExp (TS) on generated strings, plus differential fuzz |
| I-7 | FormatSpec → Doc builders; one stable `Doc`/`FmtBuilder` API in published pprint | Idempotence, round-trip on P4, width-law goldens |
| I-8 | Constitution manifest plus **build plane** (rustc and LLVM versions, opt-level, LTO, codegen-units, panic, target-cpu, allocator) plus **host plane** (M-series generation, macOS build, QoS, thermal state, load before and after each block) | The harness refuses "certified" if any field is missing or the two sides' build planes differ |

### 3. Measurable goals (PROVISIONAL until sealed at U6s)

**Measurement rules:**
- **Host:** Apple aarch64 is the only certification host.
- **Blocks:** paired and interleaved, after an A/A calibration. Blocks with load ≥ 8 or a non-nominal thermal state are discarded.
- **Certification cells** (pre-registered: 2 families × 1 primary competitor per goal):
  - N ≥ 200 pairs;
  - median, IQR and 95% paired-bootstrap LB, Holm-corrected within the goal;
  - floor = max(10%, 3× A/A noise).
- **Other cells:** ≥ 10 reps with median and IQR, labelled OBSERVATIONAL.
- **Competitors:** pins resolved in U4 (newest release at least 14 days old, checksummed). Both sides use the same build plane; each competitor's own published profile gets a second, observational row. In-situ parsers copy their buffer outside the timed region.
- **Corpus:** the constitution's controlled matrix. twitter, citm and canada are continuity fixtures only.
- **Pass condition:** a goal is met on ≥ 2 families with no regression above 3% on any non-pathological family.

**CSS goals** (first, because value.js consumes them):

| Goal | Plane | Gate | Stretch |
|---|---|---|---|
| G-C1 | P2 token/event | ≥ 1.5x cssparser (full-consumption event checksum) | 3x |
| G-C2 | P7 typed | ≥ 1.0x Lightning CSS on the hashed coverage manifest | 5.0x LB, if OR-4 keeps T's bar |

**Regex goals:**

| Goal | Gate |
|---|---|
| G-R1 | Every terminal ≥ 1.0x regex-automata 0.4 (Rust) and ≥ 1.0x native sticky RegExp (TS) |
| G-R2 | PROVE-10 prefilter wins re-taken end to end under the harness, ≥ 10% on 2 families |

**JSON goals** (the generated parser, not a hand-written one):
- **G-J1 (P1 check):** a row is admitted only with a named API and a work-equivalence note. Candidates:
  - simdjson DOM parse with the tape discarded, counted as extra work it does;
  - On-Demand with a forced full traversal;
  - simd-json and sonic-rs equivalents.
  If no equal-work API exists, the row is "no comparator".
- **G-J3 (P4 DOM):** provisional ≥ 1.1x sonic-rs and ≥ 1.0x yyjson on ≥ 5/8 corpora.
- **Stretch, Wave F:** G-J2 (P3 tape vs simd-json borrowed), G-J4 (P5 get), G-J5 (P6 typed, citm/twitter serde structs), G-J6 (serialization).

**Generator competition:**
- **G-G1 (Rust):** ≥ 1.5x the fastest of pest, nom, winnow, chumsky, lalrpop, peg and tree-sitter (batch-mode label). JSON at P4, CSS at P2.
- **G-G2 (TS):** ≥ 1.2x the fastest of chevrotain, peggy, ohm, parsimmon, @lezer/lr (batch-mode label) and antlr4. The JSON.parse ratio is tracked, not gated.
- **G-G3:** CSS L4 grammar emit ≤ 1 s cold, with rustc time reported separately and cold. Generated binary ≤ 1.5x the fastest generator's output.

**pprint:**
- **G-P1:** 100% idempotence and round-trip.
- **G-P2:** ≥ 5x prettier (TS face) and ≥ 1.0x dprint/biome (Rust face), on a declared subset with byte-equal output.

**Error paths:**
- **G-E1:** on the invalid-early, -mid and -late corpora, error byte offset equals the reference oracle on 100% of cases. Error-path throughput is ≥ 1.0x the plane's primary competitor. Message parity is reported, not gated.

**Hygiene:** no regression above 3% on any value.js consumer path (X.P gate P-5: parse-that 2.x vs its 0.8.2 paired harness).

### 4. Ordered units

Each unit closes only when its falsifier goes RED → GREEN twice at a pushed commit whose SHA is recorded in the ledger. At most 4 concurrent workflows.

**Wave A: custody and ground** (blocks everything)

**U0 Custody.**
- Work:
  - `git fetch` all three repos; write `CUSTODY-RECEIPT.tsv` with every remote branch SHA.
  - Write the content manifest (path, size, sha256) for every dirty and untracked path to `~/.dev-logs/bbnf-lockstep/custody/`.
  - Push `af15f63e0` as `custody/sk-v25-local`.
  - Commit sources and ledgers by pathspec onto `custody/sk-v25-dirty`, parented on it. Files over 5 MB go to `custody-sk-v25` release assets plus the `~/.dev-logs/.../blobs/` mirror, recorded by hash.
  - Do the same for parse-that's T and U ledgers and its 35 dirty paths.
  - Push pprint's 39 commits on a branch.
  - Push `codex/sk-v26-bbnf` and `codex/sk-v26-parse-that`.
  - Write an owner-ruling list of anything that cannot be regenerated and has no home.
  - No reset, stash, rebase or force.
- Falsifier: the custody script exits non-zero (currently true).
- Gate: every manifest hash can be fetched from a remote branch or the asset store, checked by script.

**U1a Acyclic topology.**
- Work: move egraph, egraph-derive and csp-solver to `mkbabb/egraph-csp` and publish them. Repoint bbnf-regex and bbnf to registry versions.
- Gate: `cargo publish --dry-run` passes in order (leaf → pprint → parse_that/bbnf-regex → bbnf) from clean clones with no `[patch]` in effect.

**U1b Toolchain.**
- Work: a stable certified profile and a shared `contest/bench` workspace with one `[profile.bench]`.
- Gate: `cargo +stable build --release -p bbnf -p parse_that -p pprint -p simd-scan` returns rc 0, and a grep finds no unstable feature on the certified path.

**U1 CI green** (on `contest/ci`, merged under the ownership protocol).
- Work:
  - Make `RUSTC_WRAPPER` environment-conditional, removing the `.cargo/config.toml:40` mandate.
  - Add a macos-14 arm64 job: simd-scan checkasm plus the `BBNF_SIMD` override matrix.
  - Add an ubuntu x86_64 AVX2 job, used only once x86 variants exist.
- Gate: 3 consecutive green runs on both repos.

**U2 Co-buildability.**
- Work: reconcile the published pprint `FmtBuilder`/`FmtOp` API.
- Falsifier: `cargo check` from clean clones currently gives 957 errors (re-measured at the U0 SHA).
- Gate: rc 0 from clean clones of all remotes.

**U3 Owner rulings** (asked, never assumed):
- **OR-1:** the x86 policy as ruled in C2-6.
- **OR-2:** the sk-v26 spine.
- **OR-3:** retire `skinny/` once its kernels are landed.
- **OR-4:** T's 5x Lightning CSS bar as a gate, or as stretch.
- **OR-5:** the new egraph-csp leaf repo.
- Plus the custody list of irreplaceable material.

**Wave B: the instrument**

**U4 Harness.**
- Work:
  - Pin verification (pins TSV v2).
  - Manifest emitter including the build and host planes.
  - QoS and thermal recording.
  - Paired interleaved runner, A/A calibration, bootstrap with Holm.
  - Raw samples kept under `~/.dev-logs/bbnf-lockstep/bench/`.
  - PMU counters via kperf where available.
  - Competitor adapters at the verified pins, with in-situ buffer copies outside the timed region.
  - TS face through `paired/run.mjs`.
  - Pre-registration file format.
- Falsifiers:
  - (a) an A/A run reports zero wins beyond the floor;
  - (b) an injected 15% slowdown mutant is detected;
  - (c) a skip-unescape mutant is refused as plane-inequivalent;
  - (d) a build-plane mismatch is refused.

**U5 Never-harnessed adapters.**
- Work: chumsky, lalrpop, peg, Lezer and ANTLR, plus a tree-sitter row.
- Each adapter is derived from upstream examples, has a work-equivalence note, and is reviewed by a seat that did not write it.
- Falsifier: each mutated adapter fails its oracle.
- Gate: every competitor's checksum equals the oracle.

**U6 Baseline matrix.**
- Work: the full honest board, OBSERVATIONAL. Committed as `docs/benchmarks/contest-baseline-<date>.json`.
- All earlier rows (RESULTS.md, sk-v21, PROVE-*) are relabelled observational.

**U6s Seal.**
- Work: one hashed decision-plan commit that fixes every gate value, certification cell, family, primary competitor and per-unit wall-clock budget. Nothing may change afterwards except by an addendum with owner sign-off.

**Wave C: compiler soundness**

**U7 IR unification.**
- Work: canonical BIR-S, with the TS digest compared in `contest/ir/`.
- Falsifier: a digest mismatch on any repo grammar.

**U7b TS BIR-S adoption release.**
- Owner: the successor release workflow, after wf_d837f96c closes.
- Gate: digest equality from inside `typescript/`.

**U8 Pass soundness.**
- Work:
  - Delete `WrapOfEpsilonScalar`.
  - Property-tested witness per rewrite.
  - `TypeDomain::join` lattice laws in proptest (commutative, associative, idempotent, monotone).
  - The scheduler consumes the CSP result.
  - PassReport as a durable artifact.
- Falsifier: a seeded unsound rewrite is caught by the differential.

**U9 Cost model and LIR.**
- Work: choices driven by U6 PMU data (cycles/B, branch misses); planes declared per entry point.
- Falsifier: the cost model's choice loses by more than 10% to the best alternative on a held-out family.
- Also: the 118x canada regression (215.7 ms vs 1.83 ms) is cured, or explained with a profile.

**U9c Plane census.**
- Falsifier: after cutover, a grep finds scanner or kernel entry points outside the declared crates.
- Gate: none remain.
- Then `skinny/` is retired by an ordinary commit (OR-3).

**Wave D: CSS and regex first** (value.js consumes these)

**U10 simd-scan as I-5.**
- Work:
  - Inventory what `simd-scan` already has.
  - Add the I-5 ABI, the `BBNF_SIMD` override, checkasm, fuzz, Miri, careful and ASan.
  - Land PROVE-05 (CSS table tokenizer), PROVE-10 (regex prefilter) and PROVE-03 (fused string scan), one patch at a time, each re-measured at HEAD.

**U11 Regex engines (I-6).**
- Work: prefilter → direct-coded DFA → lazy DFA → bounded NFA; TS lowering.
- Gates: G-R1, G-R2.

**U13 CSS P2 vertical.** Gate: G-C1.

**U13t CSS P7 typed vertical.** Gate: G-C2, on the coverage manifest.

**U15e Error paths.** Gate: G-E1.

**Wave E: JSON core, the generator contest, pprint**

**U12 JSON P1 and P4.**
- Work: a stage-1 structural indexer in `simd-scan/index.rs` as an LIR primitive; PROVE-06 and PROVE-07 land here.
- Gates: G-J1 (admitted rows only), G-J3.

**U14 Generator contest.**
- Work: the Rust face first. The TS face (U14-TS) follows U7b.
- Gates: G-G1, G-G2, G-G3.

**U16 pprint FormatSpec (I-7).**
- Work: BBNF annotations → Doc; Rust face first, then the TS face after U7b.
- Gates: G-P1, G-P2.

**Wave F: stretch and release**

**U18 JSON stretch planes.**
- Work: P3, P5, P6 and serialization (G-J2, G-J4, G-J5, G-J6). `asm!` lands only where it beats intrinsics per M-series generation.
- x86 AVX2 variants only when executed in CI.

**U17 Release.**
- Work: bottom-up publish of all crates and the TS face.
- Gates:
  - value.js hygiene gate P-5;
  - pins bumped by the release-owning workflow, never by contest seats;
  - every certified row reproducible from its manifest.

### 5. Lockstep with the in-flight programmes

- **Ledger:** `bbnf-lang/restart/skinny/tranches/sk-v27/LEDGER.md`. A one-line pointer goes in value.js COHESION (X.P §0ht–§0hw lineage) at each wave close.
- **value.js** is touched only via the release-owned pin, under U7b and U17.
- **Programmes not touched:** megatranche-realization, x-0hr-residuals and daraksha-v3 keep their repos. Contest seats do not touch value.js, fourier, keyframes or daraksha app files. value.js `scripts/dev/dev.sh` is never touched.
- **Workflow map within the 4-workflow cap:**
  - Wave A: {U0} → {U1a, U1b, U2} → {U1}
  - Wave B: {U4} → {U5, U6} → {U6s}
  - Wave C: {U7, U8} → {U9, U9c}
  - Wave D: {U10} → {U11, U13, U15e} → {U13t}
  - Wave E: {U12, U14, U16}
  - Wave F: {U18, U17}
- **Machine law:** every build, bench and fuzz run goes through `guarded`. cargo builds are heavy runs. Bench seats use `--cpu-cores 0` and wait for load < 8, measured per block. One heavy run per seat. Durable scratch goes only in `~/.dev-logs/bbnf-lockstep/`.