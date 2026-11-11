# sk-v27 — owner rulings (2026-11-05, by the owner's delegate, value-js-e7)

The plan's §2 defaults are confirmed as written for X-1 to X-9. X-10 is set below. These rulings are binding from now.

| # | Ruling |
|---|---|
| X-1 | Confirmed. The sk-v26 spine carries the compiler; skinny is a lab whose rows are admitted by census; the root pipeline is a quarry that can only delete. |
| X-2 | Confirmed. There is one engine, `parse-that-regex`, inside parse-that, and bbnf-lang holds a thin `bbnf-regex-facts` adapter plus the emitters. Falsifier: one regex engine package in the graph, and 0 `RegexInfo` definitions outside it. |
| X-3 | Confirmed. `parse-that-simd` lives in `parse-that/rust/simd` and holds the only `core::arch` code; `parse_that::scan` is its public face. |
| X-4 | Confirmed. There is one `OnceLock` dispatch over one scalar oracle. An x86 tier merges only if CI executes it on hardware that has the ISA; x86 never certifies; O033 is retired by addendum. |
| X-5 | Confirmed (R-8). egraph, egraph-derive and csp-solver move into the parse-that workspace as standalone general crates, and the DAG runs pprint → parse-that → bbnf-lang. The `mkbabb/egraph-csp` leaf repo is not taken. |
| X-6 | Confirmed (a). All 71 commits are merged ordinarily. The dirty tree is preserved by an alternate-index commit on `sk-lineage/dirty-preserve`, nothing is discarded in this programme, and per-entry fates are ruled at LW7. |
| X-7 | Confirmed (i). The orchestrator schedules the bench window; a record waits at most 60 min for a 1-minute load below 8 and then runs labelled NOISY; each certification unit has a 72 h budget and is re-queued once. |
| X-8 | Confirmed. value.js's six WPT-gated grammars are canonical, and bbnf-lang `grammar/css/l4` is retired in favour of `grammar/corpus/value-js/`, pinned to a released tarball with checked hashes. |
| X-9 | Confirmed. The programme holds at most 1 workflow slot while megatranche-realization, x-0hr-residuals-2, daraksha-v3-repair and owner-1105-eve are open, and 2 once any of them closes. Waves run in series and lanes run as seats. |
| X-10 | **Set as follows.** Over the formatter-parity corpus (frozen at LW4.PP-4 entry), output must be AST-equivalent to Prettier/Biome on 100 % of files, with 0 semantic diffs. It must be byte-identical on ≥ 98 % of files, and every residual byte diff must be catalogued with a cited style rule and ruled KEEP or FIX. No throughput claim is made until this budget holds ×2. |

Also ruled:
- Wave LW1 runs to its LG-1 gate.
- The three UNREAD objections (regex C1-9 and C2-6, consumers critic 2 #7) are re-asked and ruled inside LW1 (LW1.RX-4 and LW1.USE).
- The leftover sk-v26 pre-commit hook (an unguarded `cargo test` of a missing test, plus a `../parse-that` path) is a defect. LW1.COMP-4 removes it, and `--no-verify` is not used after that.

## Addendum, 2026-11-06: the LG-1 rulings (by the delegate)

| # | Ruling |
|---|---|
| R-a | The private `docs/precepts` submodule is **removed** from bbnf-lang. A public library cannot carry a private submodule that every downstream cargo git edge must clone. The precepts stay in their own private repo, and bbnf-lang references them only by a plain link in its docs. X-5's crate move (LW2.TOPO-1) proceeds regardless. Falsifier: parse-that #3 and pprint #2 resolve their bbnf-lang edges on a credential-free public runner (RED today → GREEN ×2). |
| R-b | TS↔Rust parity: each of the 20 divergences is ruled row by row in LW2. Authority runs in this order: the BBNF language spec text first; where the spec is silent, the compiler of record (the sk-v26 spine, X-1) decides, and TS follows. Every ruling adds a spec sentence, so the spec stops being silent there. No divergence is closed by deleting its test. |
| R-c | The corpus artifact is a **versioned GitHub release asset with a sha256 manifest**, not LFS. LFS is ruled out because it means bandwidth quotas, lfs in every CI job, and a second custody path. It is fetched and verified by `xtask corpus fetch/verify`, locally and in CI; X-8 pins the CSS part to a released value.js tarball. |
| Z-32 | The 32 zero-caller substrates are given their fates by LW2 S2 CENSUS under X-1/X-5, each as delete, route (with a cited unit) or move with its crate. "Sanction" is not available without a named caller in LW2–LW4. |

## Addendum, 2026-11-10: the LW2 delegate rulings (by the delegate, value-js-63)

These answer S4-B (`LEDGER.md`, S4 TOOLCHAIN asks, 2026-11-09) and A-8 to A-12 (`LW2-SEAMS.md`, "Phase 4 critique adjudication", on `sk-v27-lw2`). A-9 to A-12 are ruled as S14 recommended, and S14's text there binds.

| # | Ruling |
|---|---|
| S4-B | A zero-length regex match **is a match**. parse-that's Rust `regex()`/`regex_span()` leaf follows the TS leaf (`leaf.ts`) and the 5 `debug_test::tests::test_css_*` diagnostics tests (R-b parity). Progress is guaranteed by the combinators (the repetition and `sep_by` families stop on a zero-progress iteration), not by the leaf. Falsifier: the 5 tests RED ×2 → GREEN ×2, with a guard test of a repetition over an empty-matching leaf that terminates. |
| A-8 | op.rs ABI v2 is frozen as **PROVISIONAL**: frozen in pprint (the const asserts plus `ABI-v2.md`, version line 0.4), consumer status PROVISIONAL. It closes on the consumer side once PP-U9 compiles bbnf-lang and parse-that against it (`cargo check --workspace` and `regen --check` on the 0.4 line) GREEN ×2. Any change PP-U9 forces is an `ABI-v2.md` addendum. LG-2.3 reads GREEN with that label, not without it. |
| A-9 | As S14 recommended. `\b`/`\B` are pinned and `Look` gains `WordBoundary` and `NotWordBoundary` before RX-2 takes the Hir fingerprint. ECMAScript is the leaf language: `\b` is a word boundary over the ASCII word set `[A-Za-z0-9_]` (without `u`/`i`), never a backspace. S7's born-RED tests take their expected values from the V8+JSC oracle. |
| A-10 | As S14 recommended. COMP-4's accept and reject authority is JSONTestSuite's `y_`/`n_` expectation; parity with root `backend/rust` is a second check, not the gate. Root `grammar/json/json.bbnf` becomes RFC 8259 (mandatory commas, `json = ws value ws`), the `trim_end` tolerances become exact end offsets, and `regen --check` plus nextest run ×2 in that same unit, whatever S5-A rules on the slice's home. |
| A-11 | As S14 recommended. `gorgeous` publishes from bbnf-lang: its `repository` reads `mkbabb/bbnf-lang`, and `mkbabb/gorgeous` gets a pointer to it. If the owner keeps `mkbabb/gorgeous` as the home, the crate's source moves there before TOPO-4 and the DAG check adds the repository. |
| A-12 | As S14 recommended. PREREG's 3 % margins (COMP-1 WIN, G-S1 at 1.03) stand as registered. A verdict whose 95 % interval straddles the margin is UNDECIDED and is reported as such, never re-tested under a wider margin. The FLOORS ruling governs goal claims only. |

## Addendum, 2026-11-10b: the LW2 delegate rulings after the third LG-2 reading (by the delegate, value-js-65)

The third reading of LG-2 (value.js COHESION §0hz.lw2, evidence `lw2/lg2-r3` `0379fe71a`) was RED and nothing merged. These rulings answer the asks open at that reading: RULINGS-1 and RULINGS-2, S4-A and S4-C (`LEDGER.md`, S4 TOOLCHAIN asks), A-1 to A-7 (`LW2-SEAMS.md`, interim adjudication), S1-A, S1-B, S6-A, S6-B, OR-7, S11-A, S15-A and LG-2-A. Where a row says "as S14 recommended", S14's text in `LW2-SEAMS.md` binds.

| # | Ruling |
|---|---|
| RULINGS-1 | **Yes.** `sk-v27` is merged into `sk-v27-lw2` by an ordinary merge, before LG-2 is read again, so the integration branch carries every ruling. |
| RULINGS-2 | **Yes.** PERF-3's RED set also names `css_parse_test::test_parse_bootstrap_css`, the debug stack overflow on the macOS pin. One root cure covers both recursion-depth overflows (it and `test_nested_media`); neither is cured by a larger stack or a skipped test. |
| S4-A | **Yes, as S4 recommended.** S4 merges on a held reading that names the set exactly: bbnf-lang {`substrate_audit` naming exactly `compute_inside_string_bytes` and `compact_stripe_synthetic`; `node_execute_twitter_statuses_zero_text`}; parse-that {`css_parse_test::test_nested_media` and `css_parse_test::test_parse_bootstrap_css` (PERF-3, RULINGS-2); the 5 `debug_test::tests::test_css_*` under `--features diagnostics` (S4-B)}. Any other failure is RED. `ci-ok` GREEN in full is read at S6's merge and at S16 (LG-2.2). |
| S4-C | **Yes.** The order's macos-14 arm is read on `macos-15` (Apple silicon, NEON); PERF-2's `neon` reading is taken there. |
| A-1 | As S14 recommended (the held reading, not a widened assertion; no sanction; no early crate move). |
| A-2 | As S14 recommended (zero-caller coverage moves with the crates into parse-that, doctests counted, with an uncalled-`pub fn` mutant RED ×2). |
| A-3 | As S14 recommended: ratified (the census checker matches every `pub` item form, `lw2/s2-census` `e72912eeb`). |
| A-4 | As S14 recommended (S16 runs `FATES_AT=gate substrate-fates-check.sh`; a MOVE row whose crate is still in bbnf-lang is refused). |
| A-5 | As S14 recommended (S16 re-takes `heads-LW2-gate.json` and reads `topo0_lw2_check.sh` over every record before LG-2). |
| A-6 | As S14 recommended (`parse-that-egraph` and `parse-that-egraph-derive`). |
| A-7 | As S14 recommended (one home for csp-solver, the upstream `mkbabb/csp-solver`; bbnf-lang deletes its vendored copy). The transport of that edge is S3-A's open ask (the git edge cannot resolve credential-free while upstream carries the private submodule; S3 executed the crates.io `0.4.0` edge). |
| S1-A | **A-10 as ruled.** A new unit, **LW2.TOPO-6b**, makes `json = ws value ws` the entry rule of root `grammar/json/json.bbnf`, moves the projection and the `path!` root to it, requires the commas (RFC 8259), sets exact `trim_end` end offsets, and reads `regen --check`, nextest and the iai JSON gate ×2. The TS-only cure is **not** taken. |
| S1-B | **The literal reading is the spec's** (R-b: spec text first). `( x << sep ? ) *` means what it says: each `x` may be followed by an optional `sep`. The TS and Rust emitters stop rewriting it into a strict separated list. A grammar that means a strict list writes `x (sep x)*`. Every in-repo grammar using the form is audited for intent, and each one that meant strict is rewritten, each with a falsifier (an input the two readings disagree on, RED ×2 → GREEN ×2). |
| S6-A | PERF-1 closes when the `core::arch` grep shows `rust/simd` plus **exactly the 13 named files**, each carried by a named row (U5/U6). Any other file is RED. |
| S6-B | `compact_stripe_synthetic` is **DELETED**. |
| OR-7 | **Route A**, PROVISIONAL: `regex-automata` 0.4.14, `LeftmostFirst`, an anchored dense minimized DFA, fed by our Hir. It closes when Phase 3's emitted-code cells (G-P1/G-R2, F-REGEX) show parity with Route B; a loss beyond the floor reopens it. G-P1 and G-R2 are registered in PREREG. |
| S11-A | **No.** No bench is taken before PERF-1b is banked and S6 and S7 are merged with PREREG reconciled. |
| S15-A | **No.** pprint does not merge early. |
| LG-2-A | **No.** The three LW2 PRs (bbnf-lang #12, parse-that #4, pprint #3) merge together, through the gate, after every row is GREEN ×2. |

## Addendum, 2026-11-11: the FLOORS adoption and the LW2.CON-2s seal (seat S15 FREEZE, under the delegate's pre-ruling)

The delegate (value-js-65) pre-ruled at S15's dispatch: adopt S11's floor proposal if it follows the larger-of-10 %-and-3×-A/A rule exactly, otherwise return the ask. It does, so the row below is that pre-ruling applied, not a new ruling.

| # | Ruling |
|---|---|
| FLOORS | **G-G2 (F-JS) = 10.0 %, FROZEN.** S11's proposal (`bench/FLOORS-PROPOSED.tsv`, `lw2/s11-bench` `0879d876e`) reads A/A noise 2.489 % (C-G2-AA, 220 QUIET pairs, CI99 [−0.060, +2.489] %), 3 × noise = 7.467 %, so `max(10 %, 3 × noise)` = 10.0 %, which is the proposal; U4 (a) PASS, U4 (b) PASS (C-G2-SLOW +14.08 %, LOSS). Every other goal keeps S11's state: G-J1, G-J3, G-C1, G-C2, G-G1, G-G3, G-P2, G-E1 and P-5 NOISY-UNFROZEN (no admissible A/A), G-R2 and G-P1 BLOCKED, G-R1 n/a. The floor was read from QUIET blocks under LW2-ORDER §5's QoS reading, which is not yet ruled; the plan fixes the effect of the other answer (G-G2 → NOISY-UNFROZEN, nothing else moves), so the QoS ruling needs no plan addendum. |
| CON-2s | `bench/DECISION-PLAN.lw2.json` is sealed: sha256 `9882d5a1e0048c3446b054796aa300ce7e7b4e579e521e740040c822207c76bf`. It fixes the gate values, the 76 registered cells (PREREG §6–§11, sealed over PREREG sha256 `55acca83…`), the 14 families with their primary competitors as PREREG §4 names them (none where §4 names none), the 13 goal rows and the 72 h per-unit budgets with their clock starts. Any change needs an addendum here with owner sign-off (competition U6s). |

## Addendum, 2026-11-11: the LW2 delegate rulings after the fourth LG-2 reading (by the delegate, the orchestrator)

The fourth reading of LG-2 (`lw2/lg2-r4` `0a3ccb1b7`, evidence `evidence/LW2.GATE/r4/`) was RED and nothing merged. Its one blocker is LW2.GATE TOPO-4: the publish dry-run read RED ×2 (`e06220484`). These rulings answer the asks open at that reading: TOPO-4-A and TOPO-4-B (S16's TOPO-4 row), LG-de and the QoS reading (LW2-ORDER §5). LG-2-A (2026-11-10b) stands unchanged.

| # | Ruling |
|---|---|
| TOPO-4-A | **A published crates.io version is never overwritten or reused.** Each collided crate is bumped to its next patch: `pprint` 0.3.7, `pprint_derive` 0.2.3, `bbnf-analysis` 0.1.2. The one exception is a crate that is already moving to its 0.4 line in this wave: it takes that 0.4 version instead. Falsifier: the TOPO-4 dry-run's name-and-version read against the crates.io API shows 0 versions in the publish set that crates.io already holds. |
| TOPO-4-B | **A new LW2 unit, LW2.TOPO-4b, cures it at root.** `bbnf-ir` reads its strategy table from its own `[package.metadata]` or from a crate-local file, never from the workspace-root `Cargo.toml`. The emitter writes grammar paths that sit inside the crate, and `cargo xtask regen --check` keeps the in-crate copy in step with `grammar/`. `parse_that_cli` is `publish = false` while it depends on `parse-that-corpus`. Falsifier: the TOPO-4 dry-run itself, RED ×2 → GREEN ×2. |
| LG-de | **LG-d (row-0 timings) and LG-e (cell re-reads) carry to LW3** as named ledger rows, each with its blocker and a 72 h clock. They do not block the LG-2 merge. Nothing is CERTIFIED in LW2. |
| QoS | **The order's proposed reading (LW2-ORDER §5) is adopted.** Load and thermal state decide the NOISY label. Every record states its QoS class as a disclosed host condition, and a pair whose two arms ran under different QoS accounting is discarded. The FLOORS row (above) stands as frozen; the plan's contingency (G-G2 → NOISY-UNFROZEN) does not fire. |

## Addendum, 2026-11-11b: TOPO-4-A2, after the fifth LG-2 reading (by the delegate, the orchestrator)

The fifth reading of LG-2 (`lw2/lg2-r5` `1ec2eb50e`, evidence `evidence/LW2.GATE/r5/`, `evidence/LW2.TOPO-4c/`) was RED and nothing merged or published. Its one blocker is critic objection O-1: the patch bumps break the public API of crates already on crates.io (`semver-probe.sh` RED ×2). This ruling answers ask TOPO-4-A2 and corrects TOPO-4-A's "next patch" wording, which was wrong for a breaking change. LG-de, the QoS reading and LG-2-A stand unchanged.

| # | Ruling |
|---|---|
| TOPO-4-A2 | **The version line follows the evidence, crate by crate.** A crate whose public API breaks against its newest published, non-yanked version takes its next minor (0.x) line; a crate whose API does not break takes its next patch. Each crate is decided by evidence (the semver probe plus a public-API diff), never by assumption. As recommended: `pprint` 0.4.0 and `pprint_derive` 0.3.0, with `parse_that` re-pinned to `pprint = "0.4"`; `bbnf` 0.3.0, `bbnf-ir` 0.2.0, `gorgeous` 0.2.0; `bbnf-analysis` 0.2.0 if its API breaks against 0.1.1, else 0.1.2. Every dependent's requirement is re-pinned to match, and a dependent whose own public API re-exports a broken type breaks too and takes its minor. `pprint` 0.4.0-alpha.0 leaves this wave's publish set; if the pprint integration head differs from what publishes as 0.4.0, the seat states exactly how the two lines are reconciled, never publishes two things under one version, and asks if it is not clear. A version crates.io already holds (yanked included) is never reused. **The gate gains a test:** `semver-probe.sh` GREEN ×2 with every control passing (published consumers still build on a fresh resolve), beside `topo4c.sh` GREEN ×2. Unit: **LW2.TOPO-4d**. |
