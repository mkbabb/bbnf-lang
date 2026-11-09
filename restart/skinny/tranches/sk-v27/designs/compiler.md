# BBNF compiler backend: adjudicated final design (2026-11-05)

**Adjudicator's checks (read-only):**
- bbnf-lang `crates/` holds `analysis, bbnf-path, bbnf-path-ts, bootstrap, core, csp-solver, egraph, egraph-derive, gorgeous, ir, lsp, ser, simd-scan`.
- `crates/lsp` and `wasm/` depend on `bbnf-ir` and `bbnf-analysis`. `wasm/` also pulls `gorgeous` with `features=["vm"]`.
- `gorgeous` describes itself as "Grammar-driven pretty printers auto-generated from BBNF grammars" and depends on `pprint = "0.3"`.
- sk-v26 `crates/core/src/compiler/` has 16 entries: 15 modules plus `testdata/`, so Critic 1 is correct. sk-v26 also has `skinny/crates/{bbnf-regex, bbnf-simd, parse-that-regex, ir, passes, codegen, runtime, ...}`.
- `parse-that/rust/regex` is named `bbnf-regex`.
- Both critic texts were cut off at item 8. Item 8 is ruled on what was received; anything after it was not seen and is not ruled.

## A. Rulings

### Critic 1

**C1-1 (CB-1 depends on a parse-that release owned by wf_d837f96c): ACCEPT.**
- CB-1a is a hermetic build: exact `git = …, rev = <sha>` dependencies on pushed branches, no `../` paths, sccache optional, macos-arm64 runner.
- CB-1b is the crates.io publish of pprint and parse-that 0.5. It is a coordination row executed by wf_d837f96c or after that workflow closes, because a publish cannot be undone.
- CB-2 and everything after it depend only on CB-1a.

**C1-2 (same work, and hand-written JSON under a grammar label): ACCEPT, all of (a), (b) and (c).**
- Each row gets an output-equivalence contract. Output is canonicalised and diffed before any timing, and a row with a mismatch is void.
- Generality rule: `KernelBinding` is chosen only from AnalysisFacts and RegexFacts. A test plus a lint fails the build if the selector reads a rule or grammar name.
- A held-out grammar (TOML and CSV, chosen now) must receive kernels with no grammar-specific code. Otherwise those kernels are reported as "JSON-specific" and do not count toward G5.

**C1-3 (cold-process protocol, CI-overlap test): ACCEPT, merged with C2-8 (see §4).**
- Warm in-process iterations run inside repeated cold processes, on pinned P-cores.
- The manifest records frequency and thermal state.
- The test is a ratio CI with a minimum effect size. Cold-process numbers go in a separate column.

**C1-4 (quiet window may never come): ACCEPT, with an amendment.**
- A bench-window unit (CB-BW) requests an owner-scheduled certification slot or a nightly window.
- Every keep/delete trial has a deadline. If the deadline passes with no window, the verdict is UNDECIDED and the simpler organ wins: greedy over CSP, direct over e-graph, scalar over kernel.
- Amendment: an UNDECIDED deletion stays reversible until CB-14. The losing code is kept on a tagged branch, not just in git history, so a later quiet window can reopen the trial. It is never kept as a dual path on master (I6).

**C1-5 (trials need end-to-end rows that only exist after CB-10): ACCEPT.** Both fixes are combined with C2-4.
- Each trial gets a microbench/compile-artifact gate in its current place.
- A minimal plan-driven JSON emitter slice (CB-5.5) gives the trials an end-to-end harness early.
- The final keep/delete verdict is recorded in CB-13.

**C1-6 (CB-10 deletes the fastest backend before measuring its replacement): ACCEPT.**
- CB-10 gains a parity gate: the new emitter is at least root `backend/rust` on all 9 grammars, either beyond noise or tied within it.
- Until it passes, root `backend/rust` stays as a second differential oracle.
- Its deletion moves to CB-14.

**C1-7 (the oracle shares the frontend with every backend): ACCEPT.** G1 gains independent oracles:
- the TS 0.3.0 package, for accept/reject and tree;
- JSONTestSuite verdicts and serde_json, for JSON;
- the value.js CSS parser's 121 leaves, for css_l4;
- for R3, a seed-growing interpreter over the unlowered left-recursive grammar (allowed in the oracle only), checked against the lowered output.

**C1-8 ("proves" overclaims; tree equivalence and failure semantics undefined): ACCEPT.**
- Wording: "validated", never "proved".
- Tree equivalence is a declared observation function `obs(tree)`, per rewrite class:
  - semantic projection: the typed value / AST after the declared CST-shape normalisation;
  - for the lossless Syntax layer, exact equality.
- Failure semantics: furthest-failure offset plus a sorted expected set. That is the parse-that error contract, and it is now what G1 compares.
- Depth beyond k=6: bounded exhaustive k=6 stays, plus grammar-aware fuzz with recursion depth up to 64, plus left-recursion and memo stress seeds. The k=6 bound is documented as a limit, not a guarantee.

### Critic 2

**C2-1 (LSP, wasm, playground and extension depend on crates CB-14 deletes): ACCEPT, checked.**
- New unit CB-10b, consumer migration. Its born-RED test: `cargo tree -i bbnf-ir` and `cargo tree -i bbnf-analysis` are not empty.
- CB-14 is gated on both going green.
- CB-11 must name the VM. The "delete unless it wins" row applies only to the Wasm parse bytecode VM. The `gorgeous` `vm` feature is a formatter dependency; it is judged in CB-12 and is not deleted in CB-11.

**C2-2 (gorgeous, simd-scan and ser already exist): ACCEPT, checked.**
- New CB-0.5 crate census: one row per crate with its fate (absorb, keep or delete) and its owning unit.
- CB-12 is re-based: gorgeous moves onto Syntax→Doc and gets certified.
- CB-9 merges `crates/simd-scan`, `skinny/crates/simd-scan` and `skinny/crates/bbnf-simd` into `bbnf-kernels`. The wasm32-SIMD target is kept as a third dispatch arm.
- CB-10 names `bbnf-ser` as the direct-to-struct runtime.

**C2-3 (two crates named `bbnf-regex`, and the home is in the wrong library): ACCEPT, checked.**
- The parse-that engine is renamed `parse-that-regex`, a generic name.
- Both skinny regex crates are deleted.
- `RegexFacts` becomes a bbnf-side adapter, so parse-that imports no bbnf vocabulary.
- CB-6's born-RED test also asserts that exactly one regex engine package exists in the combined workspace graph.
- The rename is a parse-that API change. It ships on a branch and is released only through, or after, wf_d837f96c, under the same rule as CB-1b.

**C2-4 (circular gating on numbers that only exist at CB-13): ACCEPT.**
- CB-B0 is placed right after CB-1a. It holds:
  - the harness and the manifest schema;
  - current competitor pins;
  - upstream competitor parsers frozen at pinned commits;
  - baselines for today's root pipeline and skinny.
- Every trial cites a CB-B0-format record. CB-13 stays as the final certification.

**C2-5 (run the deletion trials on the root pipeline before porting): ACCEPT, with a guard.**
- Trials P5 (e-graph) and CSP-vs-greedy run on the root pipeline under CB-B0 before any port. Only the winners are ported into sk-v26.
- Guard: a root trial result can only *delete*. A port that has to re-earn its place, against the new spine at CB-13, is still subject to I6. This prevents the root pipeline's quirks from certifying an organ on the new spine.

**C2-6 (outputs are not the same work): ACCEPT, merged with C1-2.**
- Each row declares an output-parity class: typed value, DOM, lossless CST or events. It also carries a work checklist: UTF-8 validation, unescape, float rounding (correct versus lazy), duplicate-key policy, and whether the in-place copy is timed. The checklist must be identical across the row.
- Rows compare only within one class. The "≥3.0× tree-sitter" target moves to the lossless-CST class against our Syntax layer; a typed-value comparison against tree-sitter is never claimed.

**C2-7 (who writes the competitor parsers, and which flags): ACCEPT.**
- Competitor parsers are upstream examples or reference parsers pinned by commit: winnow and nom JSON examples, pest's JSON grammar, chumsky's JSON example, tree-sitter-json. Any departure from upstream is a recorded diff.
- One profile per run, used for every row, ours included: RUSTFLAGS (`target-cpu=native` for all rows or none), LTO, codegen-units and allocator.
- A PGO or profile-fit plan gets its own labelled row. No competitor PGO row is offered unless that competitor documents PGO.

**C2-8 (statistics unsound): ACCEPT.**
- Each process runs N warmed iterations, with N calibrated to about 100 ms of work, over at least 30 processes.
- Report the median, IQR, BCa bootstrap CI of the ratio, and a Hodges–Lehmann estimate.
- Minimum effect size: 3%.
- The row list and thresholds are pre-registered in a frozen `bench/PREREG.md` before CB-13, with Holm correction across the family.

**Rejected:** none. One fact is corrected: the module count is 15, not 16.

## B. Owner rulings (unchanged, refined)

- **R1, carrier: sk-v26's spine, adopted as material.**
  - An Opus audit re-certifies it, and it is merged by an ordinary merge.
  - It is amended by C2-5: trials first, on the root pipeline, then only the winners are ported.
  - Root pipeline and skinny deletions wait for CB-10b and CB-14.
- **R2, SIMD.** aarch64 NEON is first-class, and wasm32-SIMD is kept (C2-2). x86 AVX2/AVX-512 may exist only with all four of: runtime dispatch, a scalar oracle, an x86 CI runner that executes it, and a measured win on that runner. A kernel missing any of the four is deleted. This order supersedes O033 on those conditions.
- **R3, left recursion.** Pratt tables for operator grammars. Other left recursion lowers to an iterative left-fold. It is validated, not proved, against the seed-growing oracle (C1-7). Zero-consumption cycles remain the `RecursiveRule` diagnostic.
- **R4, topology.** Commit the dirty tree by pathspec on `preserve/local-master-2026-07` and push it. Push `codex/sk-v26-bbnf` and the parse-that-skv26 branch. Reconcile by ordinary merge on `integration/backend`. No stash, reset, rebase or force.

Nothing from CB-3 onward starts before R1 and R2 are ruled. CB-0, CB-0.5, CB-1a and CB-B0 may run now.

## C. Architecture and invariants

The §1 pipeline is unchanged: Syntax (lossless CST) → Program (interned, content-hashed IDs) → Graph → TypeSolution (lattice) → AnalysisFacts (+RegexFacts adapter, profile counts) → OptIR (bounded islands) → sealed LoweringPlan (`plan_abi`) → emitters (Rust/parse-that ABI-3 via `bbnf-ser` layouts, Wasm, TS-facts export). The reference interpreter sits over Program + TypeSolution.

- **I1–I6** are kept.
- **I6 amended:** a deletion takes effect only after the organ loses a parity or paired trial, or after UNDECIDED-by-deadline (C1-4). Until CB-14 the deleted code survives as a tagged branch, never as a second path on master.
- **I7 (new), generality.** No plan decision may read a grammar or rule name. Lint plus test.
- **I8 (new), independent oracles.** G1 holds only when the reference interpreter *and* at least one independent oracle per grammar agree (C1-7).
- **I9 (new), declared observation.** Every validated rewrite names its `obs()` class and the failure contract it preserves (furthest offset plus expected set).

## D. Interfaces (amendments to §2)

- **passes ↔ regex.** The engine is `parse-that-regex`, with a generic API. `bbnf_regex_facts::RegexFacts` is an adapter in bbnf-lang. Exactly one engine exists in the graph.
- **codegen ↔ SIMD.** `bbnf-kernels`, merged from three crates, dispatches NEON, AVX2 (conditional) and wasm32-SIMD over a scalar oracle, through an init-time `OnceLock` function-pointer table.
- **plan ↔ codegen.** Direct-to-struct targets `bbnf-ser`.
- **CST ↔ pprint.** gorgeous is re-based onto Syntax→Doc through `FmtBuilder`/`FmtOp`. Formatting rules are written as `@fmt` annotations in BBNF only.
- **Consumers.** LSP, `wasm/`, `server/`, `playground/` and `extension/` consume Program, Facts and Plan (CB-10b).
- **runtime ↔ value.js.** Frozen until wf_d837f96c closes.

## E. Measurable goals (amended)

- **G1.** 0 divergences under `obs()` plus the failure contract, against the reference interpreter *and* the independent oracles. Corpus: the 9 grammars plus 3 held-out grammars (TOML and CSV among them), frozen in PREREG before CB-4. At least 10⁶ fuzz inputs per grammar, recursion depth up to 64.
- **G2, within the DOM class and the typed class, identical work checklists:**
  - DOM ≥ 1.10× simd-json 0.19.x and ≥ 1.00× sonic-rs at p50;
  - typed ≥ 1.20× serde_json and ≥ 1.00× sonic-rs.
  - Pins are verified at CB-B0, since Critic 1 found 0.19, 1.0.5 and 0.18 unverified locally. If a pin does not exist, the latest published release is used and recorded.
- **G3, per class:**
  - typed: ≥ 2.0× pest, ≥ 1.3× winnow 1.x upstream example, ≥ 1.0× nom 8 upstream example;
  - lossless CST: our Syntax layer ≥ 1.0× tree-sitter batch. The 3.0× typed-versus-CST target is withdrawn.
  - lalrpop and chumsky: report-only.
- **G4.** The TS face is ≥ 1.0× peggy, chevrotain and ohm on the same three grammars, with no regression against 0.3.0. value.js `grammar.js`/`color.js` gz sizes do not grow.
- **G5.** A kernel is ≥ 2.0× its scalar oracle *and* moves an end-to-end row beyond the 3% minimum effect size. Kernels that apply only to JSON are reported as JSON-specific and do not count unless the held-out grammar receives them automatically.
- **G6.** Unchanged: css_l4 compiles in < 1.0 s (release, p50); plan and emitted code are byte-deterministic across 10 runs; an incremental edit replays only the dirty SCCs.

## F. Benchmark protocol (final)

1. Competitors are built from their published releases. Their parsers are upstream examples or reference parsers pinned by commit, and any departure is a recorded diff.
2. One build profile per run, used for every row (RUSTFLAGS, LTO, codegen-units, allocator). PGO and profile-fit rows are labelled separately.
3. Output-parity class plus work checklist per row. A canonicalised output diff runs before any timing; a mismatch voids the row.
4. Each of at least 30 cold processes runs N warm iterations (about 100 ms of work), pinned to P-cores via `taskpolicy` / QoS USER_INTERACTIVE. Cold-process numbers go in a separate column.
5. Report the median, IQR, BCa CI of the ratio and Hodges–Lehmann. The minimum effect is 3%, with Holm correction over the pre-registered `bench/PREREG.md` family.
6. Certifying runs use `guarded --cpu-cores 0` in a load < 8 window, waited for and never claimed. They run in a CB-BW slot. Any run outside a window is labelled NOISY and cannot certify.
7. Every row records a manifest: toolchain, crate versions and commits, CPU, frequency, thermal state, load samples and flags. Output goes to `bench/records/<unit>/` in bbnf-lang, with scratch in `~/.dev-logs/bbnf-lockstep/`.
8. Trial deadline. If no window comes before it, the verdict is UNDECIDED and the simpler organ wins, reversibly until CB-14.

## G. Ordered units (final)

| Unit | Scope | Falsifier (born RED) | Gate |
|---|---|---|---|
| **CB-0 Preserve** | R4 refs pushed | `git branch -r --contains af15f63e0` is empty | 4 refs on origin; no stash, reset or rebase |
| **CB-0.5 Crate census** | One row per crate (bbnf-lang root, skinny, sk-v26, parse-that/rust) with fate and owning unit | Census missing; 2 crates named `bbnf-regex` | Census committed; every crate has an owner unit |
| **CB-1a Hermetic build** | Git-rev pins, no `../`, sccache env-only, macos-arm64 and ubuntu-x86 runners | 957-error remote check; CI red 8/8 | `cargo test --workspace` green from a clean clone on both runners |
| **CB-1b Publish (coordination)** | pprint, parse-that 0.5 (+`parse-that-regex` rename), through or after wf_d837f96c | Not published | Published by the owning workflow; pins move from git-rev to version |
| **CB-B0 Bench baseline** | Harness, manifest schema, PREREG draft, competitor pins verified, upstream parsers frozen, root and skinny baselines | No manifest-complete row exists | Root/skinny baseline rows recorded (NOISY allowed, labelled) |
| **CB-BW Bench window** | Owner-scheduled or nightly certification slot; trial deadlines declared | No window defined | Window and deadlines ratified |
| **CB-T Root trials** (C2-5) | P5 e-graph and CSP-vs-greedy run on the root pipeline | LOCK-2: 0 effective islands; 2D-FIT: 0 executed fits | Verdict per organ: loser or UNDECIDED marked for deletion; only winners enter the port list |
| **CB-2 Spine audit and land** | Opus re-audit of sk-v26; fix the 2/82 failures; merge to `integration/backend` | w3_semantic_pratt never ran; replay at 11/13 | instance 20/20, replay 13/13, Pratt 17/17, type 16/16; no open audit defect |
| **CB-3 Sound types and verifier** | Lattice-law-checked TypeDomain; stage verifier after every pass | Proptest of join laws fails on the old domain; an injected dangling RuleId is not caught | Laws hold at 10⁵ proptest cases; verifier rejects 50/50 mutants |
| **CB-4 Reference interpreter, validator, independent oracles** | Interpreter; `obs()` classes; failure contract; seed-growing LR oracle; TS 0.3.0, JSONTestSuite and value.js-121 oracles | `WrapOfEpsilonScalar` is accepted today | Refuted with a witness; all surviving rules validated; held-out corpus frozen |
| **CB-5 FIRST/FOLLOW and dispatch** | One `Dispatch` surface; `facts.json`; I7 lint | Rust↔TS facts differential never run | 0 diffs across the 9+3 grammars; deterministic plan; I7 lint green |
| **CB-5.5 JSON emitter slice** | Minimal plan-driven Rust emitter, DOM and typed via bbnf-ser | No end-to-end harness on the new spine | Passes G1 on JSON; CB-B0 record produced |
| **CB-6 Regex single home** | `parse-that-regex`; fix the 5/11 divergences; lookaround as parser asserts; non-ASCII panic; regex test compiles; bootstrap regen | 5/11 divergences; 40/121 leaves rejected; panic; 2 engines in the graph | 0 divergences on ≥10⁵ cases vs `regex` and V8; 121/121 leaves; one engine; bootstrap fixpoint |
| **CB-7 E-graph port** (only if CB-T kept it) | Sound bounded islands, fact-driven cost model | Port absent | Changes emitted code on ≥3 grammars (artifact gate); end-to-end verdict in CB-13 |
| **CB-8 Global decisions port** (only if CB-T kept CSP) | One decision model; unsat surfaces a diagnostic; `bounded_sum` B&B; profile-fit memo trial | `let _ = csp.propagate()`; constraints dropped on unsat | Unsat-diagnostic test green; microbench gate; end-to-end verdict in CB-13 |
| **CB-9 Kernel plane** | `bbnf-kernels` = merge of 3 crates; NEON, wasm32-SIMD, and x86 under R2; stage-1 index, string/escape scan, Eisel-Lemire, whitespace skip, `vqtbl` charclass, keyword PHF; `asm!` only where intrinsics measurably lose, with clobber tests | No differential fuzz; no Miri; approximate scans | Per kernel: scalar oracle, checkasm (random, page-boundary, adversarial, lengths 0..256), 10⁷ fuzz execs clean, Miri clean on scalar paths and safe wrappers, ≥2.0× microbench; held-out auto-binding test; end-to-end verdict in CB-13 |
| **CB-10 Layout-driven emitter and regeneration** | One plan-driven generator (bbnf-ser for direct-to-struct, DOM, events); 9 runtimes regenerated | e683e22e9 left the 9 runtimes unregenerable | 9/9 regenerated and byte-deterministic; G1 holds; **parity ≥ root backend/rust on 9/9** (root kept as an oracle until then) |
| **CB-10b Consumer migration** | LSP, wasm/, server/, playground/, extension/ moved onto Program/Facts/Plan | `cargo tree -i bbnf-ir` / `-i bbnf-analysis` not empty | Both empty; playground and LSP smoke green |
| **CB-11 Wasm and TS faces** (after wf_d837f96c closes) | Wasm emitter over the plan; Wasm parse VM deleted unless it wins; Rust TsEmitter deleted; TS consumes `facts.json` | Three Wasm faces; two TS emitters | One Wasm face; one TS emitter; facts CI green in both repos; G4 met |
| **CB-12 gorgeous re-base** | gorgeous moves onto Syntax→Doc, rules as `@fmt` in BBNF; gorgeous `vm` feature judged here | gorgeous's idempotence and roundtrip are uncertified | `fmt∘fmt = fmt` and `parse(fmt x) ≅obs parse x` on the corpus plus 10⁵ fuzz cases |
| **CB-13 Competitive proof** | PREREG frozen; certified rows in a CB-BW window; final keep/delete verdicts for CB-7, CB-8 and CB-9 | No certified row exists | Every pre-registered row is certified or recorded as a miss or UNDECIDED; no claim inside noise |
| **CB-14 Apotheosis and deletion** | Delete root `crates/ir` and `analysis` (after CB-10b), `backend/rust` (after parity), trial losers, skinny (GN-0), legacy core sources, and the reversible-loser branches once their status is final; close the sk-v25 tail | 300 tracked but unbuilt files; skinny's 480 tracked files | Every remaining source file is reachable from the build; held-out proof passes; release cut through the owning workflow |

**Lanes after CB-2:**
- Spine lane: CB-3 → CB-4 → CB-5 → CB-5.5 → {CB-7, CB-8} → CB-10 → CB-10b.
- Parallel lanes: CB-6 (needs CB-1a and CB-4 for its oracle wiring) and CB-9 (needs CB-5.5 for end-to-end records).
- CB-11 waits on wf_d837f96c. CB-12 needs CB-1b, or a git-rev pprint.
- CB-13 and CB-14 come last.
- Each lane is limited to one heavy run per seat.

## H. Coordination constraints carried

- Do not touch bbnf-lang `typescript/` releases, parse-that's release workflow or value.js's BBNF pin while wf_d837f96c is open. CB-1b, the `parse-that-regex` release and CB-11 are coordination rows.
- Commit by pathspec on branches; merge after CI. Never touch value.js `scripts/dev/dev.sh`.
- Every heavy run goes through `~/.claude/guard/guarded`, and cargo builds count as heavy runs. Bench seats use `--cpu-cores 0`.
- Opus 5.5 for every seat.