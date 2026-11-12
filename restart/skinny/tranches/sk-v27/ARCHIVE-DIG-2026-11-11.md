# The BBNF archive dig, 2023-03-03 to 2026-11-11

Seats: synthesis, then three adversarial verification seats (measured numbers; homeless and narrowed rows; the CSP and e-graph definitions of done), then a final seat that ruled every correction (§9). Written 2026-11-11. Read-only on every repo: nothing was built, benched or run.

**Why this exists.** The owner, 2026-11-11 22:25: "The CSP and e-graph items are hard requirments--perform a deep archival dig of the BBNF and it's 2000+ commits, 1000+ sessions, and our detailed tranche plan and analysis--what went wrong, right--what was true, novel, and necessary. Our deep, SOTA optimiation, DAV1D based ASM uinion'ed optimization analysis, etc." Earlier the same day: "this was a megamegatranche with MANY conestellation and check point reqs, like besting simdjson in full generality, full CSP implementation and optimization passes, backends for TS and Rust with IR and generalized IR, a tape union facility, etc." and "NO shortcuts. Full and grand implementations."

**Companion.** `AMENDMENTS-FROM-THE-DIG.md`, beside this file, turns the findings into proposed LEDGER rows for the orchestrator to rule at the LW3 R0 checkpoint. This document changes no order and no plan.

**Inputs.** The eight dig reports in this folder, read in full: `dig-history-early`, `dig-history-late`, `dig-parse-that-pprint`, `dig-csp`, `dig-egraph`, `dig-optimization-asm`, `dig-plans`, `dig-owner-voice`. The three verification reports (`verify-measured`, `verify-homeless`, `verify-csp-egraph`) are kept beside them in `~/.dev-logs/bbnf-lockstep/archive-dig/`. Light reads by the synthesis seat, to settle doubts: bbnf-lang `origin/sk-v27-lw3` `28c09a0a5` (`PLAN.md`, `LW3-ORDER.md` §7–§8, `RULINGS.md:7-11,122-156`, `LEDGER.md:267-269`), and the upstream solver at csc411 `65804fa7`.

**How to read the citations.** A commit hash or `file:line` is the dig seats' evidence, re-opened by a verification seat where §9 says so. `[dig-x]` names the report that carries the full citation. **M** = measured, with its record. **C** = claimed or planned. **UNVERIFIED** = not traced to evidence by any seat. Citations at `28c09a0a5` are to bbnf-lang `origin/sk-v27-lw3` as read by the seats; that branch has since moved (`b8e87c52a` at the final seat), and `origin/master` is `29d4fd5ef`. Shorthand used in later sections: `P` = sk-v27 `PLAN.md`, `O` = `LW3-ORDER.md`, `L` = `LEDGER.md`, `R` = `RULINGS.md`, `CC` = `evidence/LW1.COMP/CRATE-CENSUS.tsv`.

**Limits of the archive.**
- bbnf-lang holds about 4,550 commits on all refs (4,549 at the dig; 4,556–4,561 at the verify pass, growing with the day's pushes); 2,235 of them fall in April 2026 and 1,660 in May [dig-history-early].
- The owner's sessions number 301 top-level (bbnf-lang 210, parse-that 83, pprint 3, bbnf-buddy 5). "1000+" holds only if subagent sessions count; one session spawned 2,410 [dig-owner-voice].
- Almost every raw transcript is gone. One bbnf-lang root transcript survives (`67d0543b`, 2026-05-22 to 07-18/21). The owner's words before that come from `~/.claude/history.jsonl` (prompts only) and from memory files. The April sessions that built the CSP and the e-graph cannot be read.

**Four corrections the synthesis makes to the dig reports.**
1. **csp-solver 0.7.0 has no backjumping.** `dig-plans` called LW3-ORDER:299 stale because `src/cancel.rs:5` names `solver::backjump`. That line is a stale doc comment: at csc411 `65804fa7` the `solver/` folder holds `ac3, adjacency, gac, monotonic, optimize, propagate, search` and no backjump, nogood or restart module. `dig-csp`'s table is right. The completeness unit rebuilds these; it does not inherit them.
2. **The later-wave units are no longer homeless on paper.** `dig-optimization-asm` and `dig-parse-that-pprint` call the tape union and the simdjson arm homeless. Since they wrote, LEDGER amendment 9 (`LEDGER.md:269`) filed them and delegate ruling ADD-2 (`RULINGS.md:129`) admitted them as PLANNED: LW4.PERF-2, LW4.COMP-4, LW4.CON-2, LW5.COMP-1-OPTIR, LW5.COMP-3, LW7.CON-1-REQ and eight smaller rows. These are delegate rulings on branch `sk-v27-lw3` (`R:129`), marked "The owner may overturn any row". **The home is on paper only.** `PLAN.md` is unchanged since `b4e635d3e` and carries none of the fourteen ids (0 hits each). Under ruling Z-32 with LW2 amendment A5 (`L:182`), a zero-caller substrate may be kept only if a unit in PLAN.md's LW2–LW4 rows names it, and `crates/ir/tests/substrate_audit.rs:99` enforces that. So a substrate whose only consumer is an amendment-9 unit, or any LW5+ unit, can only be fated DELETE. This is the mechanism that hollowed out the solver (W8), still armed (H19).
3. **Ask S5-A has a provisional answer.** `RULINGS.md:143`: the trial slice is the spine's own JSON path, and the arms are the root pipeline with the organ on and off. The trials are therefore unblocked on paper. They still have not run.
4. **Which branch holds the rulings.** ADD-2, OWN-1, S5-A, RX-B, RX-C and QX-1 exist only on `origin/sk-v27-lw3` (addendum 2026-11-11e, `c6c5d1655`). On `origin/sk-v27-lw3-order` (`049c5929e`) RULINGS.md ends at 118 lines and amendment 9 reads OPEN. A seat that reads the sealed order branch alone sees these as unruled.

---

## 1. The history in one page

| Era | Dates | What happened | Honest verdict |
|---|---|---|---|
| 0. Extension | 2023-03 | A VSCode grammar and LSP, 25 commits. BBNF itself began inside parse-that (`9a58ecd`). | A different project. |
| I. Monorepo, IR, VM | 02-26 → 04-02 | bbnf-lang split out (`cc499979a`). Canonical IR, bytecode VM and interpreter (`1710d6f7c`, 03-15). Delimiter-driven scanning, the regex HIR compiler, a bespoke DFA engine. One codegen path (`044c1a9ad`). | Real foundations. Every JSON number before 03-23 was warm and is void. The owner called this era's numbers "exceptional" (04-03). |
| II. Backends and the CSP | 04-03 → 04-07 | One driver with Rust, TS and WASM emitters (`805705f55`). The owner's own solver ported in; types, FIRST, FOLLOW and span moved onto `Csp::propagate()` (`aae8476d2`, `951918779`, `298070b37`). Self-hosting. The TS library package deleted (`c14832a3a`). | The CSP as a lattice fixed-point engine is real and still stands. |
| III. The e-graph | 04-08 → 04-10 | Generic e-graph crate with `#[derive(Language)]` (`0c0a2e71b`). Demoted to "permanent secondary" about nine hours later (`bfa50f25f`). CSP-scheduled saturation (`a5991bac8`). First real cost-minimising solve (`cc2f10135`). | The engine is sound work. The placement starved it from day one. |
| IV. Tape-first, the peak | 04-10 → 04-15 | Tape as the only substrate. Per-component CSP solve (`0d3485564`). Tranche AU close (`3b8b757d5`). | **The measured high point.** M: twitter 1,967 MB/s, citm 2,438 MB/s on the tape-only plane (`post-AU.json`, `5281ec231`). On the same plane as sonic-rs (decoded Value), M: twitter 0.52×, canada 0.61×, both gates missed (`gate_11`, `3b8b757d5`). The "76 % / 94 % of sonic-rs" figures (AY, 04-20) divide the tape-only plane by a sonic Value run taken five days later and are cross-plane. |
| V. The collapse | 04-15 → 04-22 | Column layout, a DTA table walker and PSI replaced the per-kind emitters with no perf gate. | M: at 04-20, JSON 4.1–6.6× slower than AU, CSS ~4×, Sheets ~7× (`6516086f6`). By AZ-IV (05-02, a different bench harness, so the multiples are cross-harness): canada 118× (10.4 MB/s), CSS bootstrap 983×, tailwind 10,583× (77.6 s, watchdog halt), Sheets 20–65× (`aa31fe567`). The DTA ran at ~29 cycles per byte against 1.78 (both derived from ns at an assumed 3.5 GHz; ratio 16.4× on twitter). About 572 commits. |
| VI. Reckoning | 04-20 → 05-02 | The two audits: 9 CSP surfaces on one solver (`2d5fa0578`); 13 of 14 e-graph rules never fire (`7620cac2a`). Rule discovery built and deleted unused. | Honest. Nothing was rebuilt from what the audits found. |
| VII. Restart, twice | 05-03 → 05-04 | A 1,418-line plan, then "restart the restart" (`63560960a`). The requirement of record for both organs was written here (README §6). | The restart kept the per-grammar pattern it meant to remove (`b24232776`). |
| VIII. Skinny | 05-08 → 06-03 | A JSON-first lab. Lazy-offset tape, the SIMD crate, checkasm, a dav1d x86 layer that never ran. | v5–v11 honest negatives. v12's "2.54× lightningcss" was a hand-written file with a fake generated header. v13: 0 of 43 admitted rows survived. v17's generator "does NOT exist" (`7dbe44c22`). |
| IX. Frozen specs | 06-10 → 07-18 | Five specs frozen and never dispatched (v20–v24). PROVE-01..14 measured. | The numbers stand. The judgments are suspect: in the one run censused (sk-v24's final pt pass-3 workflow), 49 of 60 agents recorded Opus and 6 Fable (sk-v25 HANDOFF §0). PROVE ratios are certified, absolutes directional. |
| X. Codex spine | 07-28 | sk-v26: an immutable semantic spine (W2 60/60, ABI-3 92/92). | Real code with no CSP, no e-graph, no optimizer stage, no left recursion. |
| XI. sk-v27 LW1–LW2 | 11-05 → 11-11 | Custody, repo DAG, stable toolchain, `parse-that-simd`, ABI freezes, the CSP soundness cure, 14 crates published. | Verified infrastructure. 0 certified performance cells, 0 CSP or e-graph evidence, no spine integration, no tape union. |

**Cadence.** From 05-03 to 06-04, 84.7 % of 1,387 commits were docs. A ten-version audit layer of 24,248 lines never overturned a conclusion [dig-history-late].

**The one-sentence history.** In eight weeks the project built every organ the owner names, measured its highest throughput at AU (tape-only; about half of sonic-rs on the same plane), destroyed it in three days with an unbenched substrate change, and then spent six months planning, auditing and re-founding without ever again running the CSP or the e-graph against a measurement.

---

## 2. What went wrong

Each cause recurs across eras. Evidence and cost are named.

| # | Cause | Evidence | Cost |
|---|---|---|---|
| W1 | **Substrate built before its consumer.** | 14 instances across 18 tranches (AYW archaeology). The AO structural dispatch was never switched on. EmissionTier rose and was deleted (`76085303a`, −2,306 lines). Rule discovery built and deleted in four days (`26f954696` → `b82f1439b`). `crates/ir/src/rewrites/` is in the tree with zero callers. Skinny's dead kernels passed a green harness. | The largest single sink of work. Every organ acquired scaffolding that nothing read. |
| W2 | **Substrate changes shipped with parity gates only.** | AV/AW: "Both landed without perf gating" (`6516086f6`). | The AU peak: 4.1–6.6× on JSON within five days, and by AZ-IV canada 118×, CSS bootstrap 983×, tailwind a watchdog halt (`aa31fe567`, cross-harness). It led directly to the restart. |
| W3 | **The e-graph was placed where it could do nothing.** | `bfa50f25f` runs it once, after a normalizer that has already done the same canonicalisations. 13 of 14 rules fire zero times (`7620cac2a`); reproduced in November. The 648-run sweep (54 configs × 4 grammars × 3) was null on DTA state count, with wall-clock noise-dominated; it never measured parse throughput (`05364225b`). All rewrites narrow, so a class holds one form. | Seven months of an organ running on every compile with nothing to choose. It was never refuted. It was never given work. |
| W4 | **Rewrite soundness was "the tests pass".** | Gate B was "69 test binaries, 0 failures" (`c7269f6bc`). Zero property tests. Two rules confirmed unsound (`FuseAltRegexBranches`, `WrapOfEpsilonScalar`); a test asserts the unsound collapse. Three more look unsound by code reading [dig-egraph §3]. | Wrong parses were possible for seven months. Each rule must now be re-admitted from zero. |
| W5 | **A CSP in name, a cascade in fact, three times.** | `csp_recognizers` made zero solver calls (`cc2f10135`). The AltStrategy CSP "mirrors existing priority cascade" (`1e32c5850`). Skinny `decision_csp` "recovers its own input". | The owner's "strict superset" order was answered with renames. |
| W6 | **The one real optimising solve fell back without failing.** | M: CSS L4's `compile_paths_request` took 52.53 s of a 59.98 s `regen --check`. In that run the largest component (187 variables, 483 sites, 1,704 constraints) exhausted the 1,000,000-node budget and fell back to the per-variable pick (`HARDENING-2026-05-03-04-toolchain-forecast.md:28,39,69` at `9dde66ab1`). The solve's share of the 52.53 s was not isolated. The fallback logs to stderr only in debug builds or under `BBNF_CSP_REPORT`; it never fails the build (`csp_strategy/mod.rs:612-620`). | The only grammar where the solve could matter got the greedy answer, in a compile that took 52 s. |
| W7 | **The CSP failed open.** | `let _ = csp.propagate()` at 7 live sites (the contract doc counts 6). `InlineDomain` broke the lattice laws 53,722 times in 10⁴ cases (`fd5ee5e83`). Cured only at LW2 (`b39d9b78d`). | Seven months of unsound fixed points under a "lattice". |
| W8 | **The solver was hollowed out because nothing called it.** | Backjumping, nogoods, restarts, soft constraints, cardinality and local search were excised upstream on a zero-caller census (`e07ac306`, `1e65de292`), three months after the AY audit ordered nogoods wired into search. | "Full CSP" must now be rebuilt. Confirmed absent at upstream 0.7.0 (the synthesis seat's read). |
| W9 | **No measured cost was ever built.** | Weights are hand-authored. The sweep was null. The kernel cost table and the PMU fit ("2D-FIT") were designed and never executed. | Neither organ could be shown to beat greedy, because with null costs every solver returns the same answer. |
| W10 | **Chronic deferral.** | Global CSP: AL → AO.4.2 → AP.6.5 → AQ.9.5 → AW → AX, then called "OBSOLETE" with no extraction run behind it. Five e-graph rules deferred at AA.6; G5–G9 never landed; BB never ran. Both organ trials pre-registered (`7b5abd165`) and never run. | The owner named it on 04-17: "a stream of constant deferrals and lost hope". |
| W11 | **Claims ran ahead of evidence.** | Warm benches (pre-03-23). Cold against warm (`c722a581d`). Tokenise-only cssparser (`05c892933`). JSON without string decode (`8d5bd200b`). The v12 fake header. A lazy tape against sonic's full DOM (SK-V22). Megabits printed as "Mbps". Two different `twitter.json` files. A +8 % gain credited to an e-graph rule that fired zero times (`6324f717b`). | Every headline before sk-v27 is void or caveated. Certified wins today: 0. |
| W12 | **Gates passed by exclusion and labels lied.** | Lock 14 green by an exclusion list. `scan_dispatch` stamped `Scalar` after running NEON; later skinny's dispatch returned `Scalar` on every host. | The live JSON path was scalar while the record said SIMD. |
| W13 | **Each re-founding re-derived scope and dropped requirements.** | The sk-v24 SPEC never mentions CSP or e-graph. sk-v26 R6 made both "admissible only". sk-v27 made that spine the compiler of record and wrote "the simpler organ wins" for an undecided trial. Eleven narrowings carry no owner ruling [dig-plans §4]. | Hard requirements became deletable options by the hands of agents and the delegate. This dig exists because of it. |
| W14 | **Process displaced code.** | 84.7 % docs commits in May. Five frozen specs never dispatched. In parse-that, 83 docs commits against ~24 code commits from 07-29 to 08-02. | Months. The owner asked on 06-10 (repeated 07-05, 07-10, 07-15): "Are we reaching a local maxima and getting stuck?" |
| W15 | **Judgment seats ran on the wrong model.** | In the one run censused (sk-v24's final pt pass-3 workflow), 49 of 60 agents recorded Opus and 6 Fable; the judgment stages carried no model override (sk-v25 HANDOFF §0). | Every sk-v24 verdict is suspect. Only the machine facts survive. |
| W16 | **Work was written, deleted and re-derived.** | Monolithic codegen made the one path (03-31) and deleted (04-05). Columns in (04-15), out (04-20). StructRegistry deleted and resurrected. CSS built in parse-that twice and deleted twice. The pprint Doc tree deleted, then rebuilt as a test oracle. The x86 asm layer authored on a machine with no x86, never executed, deleted in 21 days. | Lost months, and lost technique: nothing guarantees AU-era methods survive the next deletion. |
| W17 | **Custody and topology churn.** | sk-v19..v25 lived on local master only. sk-v26 was filesystem-only. The solver moved csc411 → path patch → vendored → crates.io 0.4.0, and each hop lost fixes; the pinned 0.4.0 carries the CH-69 over-prune, cured only upstream at `58014efd`. | A latent unsound solver pin. Work at risk of loss on disk. |
| W18 | **The machine could not certify.** | Load 10–40 on every sk-v27 run. The guard's kernel-zone refusals stopped LW2 twice. | Every record is NOISY. One floor is frozen. |

---

## 3. What went right

| # | What | Evidence | Standing today |
|---|---|---|---|
| R1 | **The lattice passes as the single source of truth.** Types, FIRST, FOLLOW, span and dispatch eligibility on one solver, with net deletions (`951918779` −206; `ad65560fb` −404). | [dig-csp §4] | Alive on master; sound since `b39d9b78d`. |
| R2 | **The AU architecture.** A grammar-generated, tape-first parser. M: tape-only twitter 1,967 / citm 2,438 MB/s; decoded-value ratio to sonic-rs 0.52 (twitter) / 0.61 (canada). | M `post-AU.json` gate_11 | Lost in the collapse. Evidence that the approach reached about half of sonic-rs on the same plane, and the highest tape-only throughput on record. |
| R3 | **The grammar-emitted visitor path matched the hand-tuned prototype within ±5 %** (0.975–1.026). The prototype's time ratio to sonic-rs was 0.89–0.94 in run B and 0.63–1.01 in run A (twitter lost at 1.012), with sonic itself swinging up to 49 % between the runs. | M `c1e86ab32`; `post-AW-V-W2-prototype.json` (`c2f215a60` prints run B only) | Lost to an over-admission (AW-V.W6). The closest the generated path came to its own hand-written twin; parity with sonic-rs was not shown. |
| R4 | **The real March breakthroughs**: delimiter-driven flat scanning, the regex HIR compiler and its recognition of regexes as fused scanners, IIFE elimination, key/ident dispatch. | Owner's correction, 03-30; M tailwind 28 → 284 MB/s (`1385266eb`) | In the root pipeline, which is now a quarry that "can only delete". |
| R5 | **Solver engineering.** Per-component decomposition (`0d3485564`) and the branch-and-bound incremental bound. | M: CSS L4 compile 11.3 s → 42 ms, 269× (`47d6fafb9`) | Alive. |
| R6 | **The e-graph engine itself.** Generic, derived `Language`, one `CostWeights` across the grammar and regex tiers. | `0c0a2e71b`; now `parse-that-egraph` (`da3cbfe`) | Alive, published. |
| R7 | **Honest negative results.** The firing audit, the null sweep, the skinny redress log (~40 routes rejected), the SpanParser kill (−10–14 % on V8), the adversarial audits that zeroed the false numbers. | `7620cac2a`, `05364225b`, `afea5c2` | The archive's most reliable material. |
| R8 | **Measurement method.** PROVE cold-paired PMU runs; the asm-against-intrinsics question measured for one movemask kernel (PROTO-A2): algorithm choice dominates, and the spelling gap is ≤ 8 %; the sk-v27 harness (fresh processes, ABBA, Holm, BCa). | [dig-optimization-asm §4] | The first instrument able to certify anything. |
| R9 | **The checkasm discipline.** A scalar reference for every primitive, per-tier differential, fuzz, guard pages. It caught a real NEON bug on its first run and a one-byte overread in LW1. | `bb864d5`, `4762ad3` | Alive in `parse-that-simd`. |
| R10 | **The lazy-offset tape.** The only substrate with a measured cold win; the eager tape hit a ceiling at ~12.5K against sonic's ~21K (megabits per second: about 1.56 against 2.65 GB/s). | [dig-optimization-asm §4] | In skinny, which dies at LW7. |
| R11 | **pprint ABI v2.** A precomputed `trailing` width gives an O(1) break decision; agrees with an independent Wadler/Leijen reference at ≥ 10⁵ cases. | `866b5ea`, `557d9af` | Alive, frozen. |
| R12 | **Regex infrastructure.** V8+JSC reference, HIR ABI frozen by fingerprint, round-trip printer 34/34, and the `L_cut` equivalence. | `dea3807`, `8678725`, `designs/regex.md` C2-1 | Alive. |
| R13 | **The sk-v27 method.** A falsifier per unit, born-RED first, RED ×2 → GREEN ×2. It found the CSP fail-open and the lattice violation that seven months of passing tests had hidden. | `fd5ee5e83` → `b39d9b78d` | Alive. |
| R14 | **The TS emitter line.** `@mkbabb/bbnf-lang` 0.3.0, driven from value.js. | `8feeabf4d` | The only BBNF surface a consumer uses today. |
| R15 | **Build discipline (Lock 15).** | C: a thin-LTO profiling binary read 5,521 Mbps (≈ 690 MB/s) against the fat-LTO bench's 11,780 Mbps (≈ 1,473 MB/s). Confounded by the profiler and debug info, never isolated | Alive as a rule, unmeasured as a cause. |

---

## 4. Verdicts: TRUE, NOVEL, NECESSARY

- **TRUE**: measured or proven (with the record), false, or unproven.
- **NOVEL**: ours (not found in the literature or the competition, on the archive's own prior-art reading, `sk-v20/research/32`) or standard.
- **NECESSARY**: required by the owner's goals, optional, or a distraction.
- **Carrier**: the sk-v27 wave and unit. "(amd 9)" marks a unit that exists only by LEDGER amendment 9 and delegate ruling ADD-2. **NARROWED** means a unit exists but reads smaller than the requirement. **HOMELESS** means no unit.

### 4.1 CSP

| Item | TRUE | NOVEL | NECESSARY | State | Carrier |
|---|---|---|---|---|---|
| Lattice fixed points (types, FIRST, FOLLOW, span, dispatch) on one `propagate()` | Proven equivalent to the old projection; never measured faster | Ours as one substrate; each analysis is standard | Required (owner 04-06) | Alive on master (root). Absent from the spine | LW3.COMP-6 (laws, stage verifier). **AT RISK**: LW3.COMP-7 builds FIRST/FOLLOW and dispatch on the spine with no CSP, and its must-not forbids computing FIRST in two places (`O:116`); the CSP arrives only at LW5.COMP-1 (`O:19`) and can find FIRST already owned (N-w) |
| Typed Unsat, lattice laws | M: 53,722 violations → 0; GREEN ×2 | Standard | Required | Alive. Laws tested on 2 of 5 domains | LW3.COMP-6 |
| Strategy COP per component (`csp_strategy`) | Consumed and pin-tested. Benefit against greedy: **unproven**. Silent fallback on CSS L4: M | Standard technique | Required | Alive on master with the silent fallback | **NARROWED**: LW5.COMP-1 does not name the per-component strategy COP (`P:226`); its home `crates/ir/src/passes/csp_strategy` is deleted with `crates/ir` at LW7 (`CC`); LW3 ports it only on a trial win (`O:107`) (N-v). No-silent-fallback: **HOMELESS** |
| Fail-closed `csp_strategy` rewrite (+2,017/−1,225) | Unproven (never ruled) | — | Required (it is the cure for W6) | On branch `sk-lineage/dirty-preserve` `d2e169794`; "fates ruled at LW7" | **HOMELESS** until LW7, which is after LW5 builds the organ |
| Global solve beside per-component | Unproven: deferred six times. M: monolithic union solve cost 9 → 94 ms and was reverted (`ae05b4698`) | Standard | Required ("both locally and globally", owner 04-12) | Never built | LW5.COMP-3 names `bounded_sum`; the coupled sub-problem itself: **NARROWED** |
| Generated-size budget as a solve-time knapsack (`bounded_sum`) | M on synthetic costs only: greedy 200 lines busts a 120 budget, CSP finds 104 in 12 nodes | Ours | Required as a coupled-resource solve. The resource measured to bind is the hot-path instruction budget, not generated lines (§5.1 C.9; owner 04-08: "a larger binary is acceptable for now") | **Uncommitted**, in three workflow worktrees (two variants: 168 lines with 7 tests; 178 lines with 6 tests). Its old home `crates/csp-solver` no longer exists | LW5.COMP-3 (amd 9). Custody: H7 |
| PBQP for the plan | **False** as a route: objective 39.003 against a true 21.011 at ~10⁴× wall-clock | Standard | Distraction | Probe only | — |
| Solver completeness: nogoods, backjumping, restarts, soft constraints, cardinality, symmetry breaking, explanations | Existed in part, excised. Absent at upstream 0.7.0 (the synthesis seat's read) | Standard | Required ("full CSP implementation") | Deleted upstream | LW5.COMP-3 (amd 9). Symmetry breaking, soft constraints, explanations, LCV: **NARROWED** (not in the row) |
| Sound solver pin (CH-69 over-prune) | Proven present at 0.4.0, cured at csc411 `58014efd` | — | Required | bbnf-lang (`Cargo.toml:37`) and parse-that's e-graph scheduler (`rust/egraph/Cargo.toml:18`) both pin 0.4.0; latent | **HOMELESS** (S3-A covers transport only; LW5.COMP-3 releases a solver but no unit re-pins either consumer) |
| One CSP pass for all structural pattern recognition | Unproven | Ours | Required (owner 04-07, never revoked) | Never built; `StructuralPattern` has 0 hits. The detectors it would replace are `crates/ir/src/passes/recognizers/` (23 modules, 37 files, 7,179 lines) | **HOMELESS**; no unit ports the recognizers, and they are deleted with `crates/ir` at LW7 |
| CSP-scheduled saturation (dirty closure) | Works. Benefit over the backoff scheduler: **unproven**. In fact a bool reachability closure with no pruning | Ours | Required as shipped; must become real solving | Alive in `parse-that-egraph` | LW5.COMP-1 ("consumed by the scheduler"). The measurement: **HOMELESS** |
| Type-system CSP (finite choice, CHR improvement, OutsideIn implications) | Unproven | Standard parts, our combination | Required (owner 05-04: "the backbone") | Never built | **HOMELESS** |
| Regex tier selection as a weighted CSP | Unproven; LOCK-5 kept it as a local table for want of a binding witness | Ours | Required (owner 04-07) | Never built | **HOMELESS** |
| Layout, shape-dict, byte-class, slot allocation as CSPs (AYW §3) | Unproven | Ours | Optional until a cost witnesses a choice | Never built | **HOMELESS** |
| EnginePropagation as first written | **False**: it dragged SIMD scanners to per-byte DFA loops (`8061bd4f4`) | — | Distraction | Fixed | — |
| CSP against greedy on parse time | **Unproven on any grammar** | — | Required evidence | Trial registered, never run. It is JSON-only, and its "greedy" arm is the old priority cascade, not an independent per-site minimum (PREREG §6) | LW3 LG-e; a discriminating trial: **HOMELESS** (N-u) |
| Tuning, validation and visualisation of CSP and e-graph decisions | — | — | Required (owner 04-12) | Never built | **HOMELESS** |

### 4.2 E-graph

| Item | TRUE | NOVEL | NECESSARY | State | Carrier |
|---|---|---|---|---|---|
| Generic engine, derived `Language` | Proven working | Derive from existing enums: ours | Required | Alive, published | LW5.COMP-1 |
| E-graph as an optimizer on normalized IR | **False as built**: 13/14 rules dead; null sweep; no measured win | — | — | Alive, runs every compile | LW5.COMP-1-OPTIR (amd 9) moves it |
| `FuseAltRegexBranches`, `WrapOfEpsilonScalar` | **Proven unsound** | — | Remove or repair | The first leaves the DFA-leaf (leftmost-longest) rule lists (`e36244d57`) and stays default for leftmost-first leaves (`rules/mod.rs:96-103`); the second is still default | LW3.RX-1h; LW3.COMP-3b |
| `SupersetAbsorbAlt`, `UnionMergeAlt`, `CommonSuffixFactor` unsound; `RepeatOfSingle`, `ConcatLiterals`, HIR repetition merge suspect | Unproven: found by code reading, no witness run | — | Required to settle | Alive in default lists | Regex tier: LW4.RX-2 (amd 3; RX-C `R:153`) for the scalar-value, Latin-1 and equal-consumption witnesses. Grammar tier: LW3.COMP-4c (class declared, 10⁵ fuzz per grammar against the interpreter). Targeted refutations for `CommonSuffixFactor`, `RepeatOfSingle`, `ConcatLiterals`, the repetition and metacharacter halves of `UnionMergeAlt`, and the HIR repetition merge: **HOMELESS** (N-q) |
| `L_cut` priority-cut DFA-isomorphism certificate | Proven by argument; not yet built | Ours | Required | Design only | LW3.RX-1h |
| Equivalence per observation class (accept set, typed value, spans, expected set) | True as analysis | Ours in this setting | Required | Design only | LW3.COMP-4c |
| A property-tested witness per rule, with on and off arms | — | Standard | Required | Zero exist | LW3.COMP-4c (`obs()` class per rewrite, 10⁵ inputs per grammar against the reference interpreter); LW5.COMP-1 (on/off arms, seeded unsound mutant) |
| Widening rewrites and the seven README §6 categories | Unproven; about 2 of 7 exist | Ours for grammars | Required (without them the cost has nothing to choose) | Never built | LW5.COMP-1 says "sound rewrite set" only: **NARROWED** |
| Islands between Facts and Plan (OptIR) | Unproven. M: "0 child-bearing islands whose pick moves emitted code" so far | Standard (Cranelift) | Required | Never built | LW5.COMP-1-OPTIR (amd 9) |
| Cost fitted from hardware counters | Unproven. M (synthetic, uniform corpus): dispatch direction reverses between N=2 and N=8 (7/7 intervals); the registry itself (LOCK-2 §0.4) says the uniform corpus guarantees this arithmetically. The distribution-dependent fit was never run | Ours | Required | Never built | LW5.COMP-1 "measured cost model"; the fit itself: **NARROWED** |
| Sharing-aware extraction; extraction as a COP | Unproven | Standard | Optional until costs amortise sharing | Never built | **HOMELESS** |
| CSP↔e-graph bridge with justifications | Unproven | Ours | Required (README §6, the requirement of record) | 0 hits on three refs. The engine also has no e-matching pattern language and no explanations (`rewrite.rs:28`), which the justifications need | **HOMELESS** |
| Saturation outcome consumed; fallback; budgets per category | — | Standard | Required | `let _report`; no time limit | **HOMELESS** at unit level. The design text exists past the dispatch cut and never reached PLAN: `designs/programme.md:233-236` ("Silent fallback is forbidden", an `Exhausted` certificate) and `competition.md:172` (H22) |
| Ruler-style rule discovery with a VM oracle | Unproven | Adaptation | Optional | Deleted (`b82f1439b`) | **HOMELESS** |
| `crates/ir/src/rewrites/`, `path_seed.rs` | One seed rule looks wrong | — | Distraction as it stands | Alive, zero callers | **HOMELESS** (wire or delete) |
| Regex-tier e-graph isomorphic to the grammar tier | Runs by default; benefit unproven | Ours | Required (owner 04-08, 07-16) | Alive | LW4.RX-2 |
| E-graph for automaton minimisation | **False**: "wrong for automaton passes" (AY-I:170) | — | Distraction; the owner's own boundary ("should NOT attempt to shoehorn") | — | — |
| The +8 % credited to rule G3 | **False**: the emitter predicate `wrap_can_elide_compound` did it | — | Correct the record | Spot file unannotated | **HOMELESS** |

### 4.3 Runtime, SIMD, assembly, tape

| Item | TRUE | NOVEL | NECESSARY | State | Carrier |
|---|---|---|---|---|---|
| AU tape-first parser | M: tape-only 1,967 / 2,438 MB/s; same-plane decoded 0.52× / 0.61× sonic-rs. "76–94 %" is cross-plane (C) | Standard substrate | Required as the baseline to beat | Lost | **HOMELESS** as a gate |
| Grammar-emitted visitor path within ±5 % of the hand prototype | M (`c1e86ab32`). Parity with sonic-rs: not shown (two runs disagree) | Ours | Required as proof of concept | Lost | **HOMELESS** |
| DTA table walker, PSI, column tape | **False**: ~29 cycles per byte against 1.78 (derived from ns at an assumed 3.5 GHz) | Ours | Distraction | Deleted | — |
| LLVM compiles recursive descent into an automaton but cannot fold an emitted table walk | True as observed; never isolated as an experiment | — | Required knowledge | Recorded | The owner's 05-13 question ("Is our new path materially different?"): **HOMELESS** |
| Lazy-offset tape | M: the only cold win | Standard (simdjson, sonic) | Required | In skinny | LW4.PERF-2 (amd 9) |
| Tape as a navigation substrate | **False** for `get`: 150–1,700× the stop-early cost; fold-over-cursor 2.3–4.9× behind sonic | — | — | — | Informs LW4.PERF-2 |
| Tape union: one tape, shared node identity with direct-to-struct | Unproven | Ours (the single-identity union) | Required (owner 05-12, 07-05, 11-11) | Never built; two unrelated tapes | LW4.PERF-2 (amd 9) |
| Five backend shapes chosen per rule by the cost model | Unproven; 2 of 5 had consumers | Ours | Required (Lock 1) | Skinny only | LW4.PERF-2 reads "one tape as a plane": **NARROWED** |
| Tape identity consumed by LSP, DAP, path, visitor, diagnostics | Unproven | Ours | Required (PASS-3 contract) | Never built | **NARROWED** (falsifier covers struct ≡ projection only) |
| Direct-to-struct | M (verified): 0.746–0.786 time ratio, i.e. 1.27–1.34× faster than sonic-rs typed (PROVE-07; the prover read 1.30–1.65×). The 2× serde gate was unmet on twitter (1.26×) | Standard | Required | Prototype | LW4.COMP-1 |
| Stop-early `get` | M (verified) time ratios against sonic `get`: shallow 0.78–0.79, missing 0.68 (wins), deep 2.01 (a loss) (PROVE-06) | Standard | Required (lazy path API) | Prototype | **HOMELESS as a build**: LW7.CON-1-REQ only requires a recorded status for P5, so a LOSS satisfies it (`L:269`). No unit builds the lazy `get` / path Value API; the adjudicated design did (`programme.md:489-492`, W9) (H20) |
| Fused string scan | M (verified): parse_tape 0.898–0.931× cycles per byte; fused/sonic 0.643 → 0.556 (PROVE-03) | Standard | Required | Prototype | **HOMELESS** (CB-9 named string/escape scan, `compiler.md:184`; PLAN LW4.PERF-1 omits it) (H25) |
| Regex start-state prefilter | M: 8.8–29.5× (PROVE-10) | Standard | Required | Prototype | **HOMELESS**; the only prefilter-class unit, LW4.RX-3 (Teddy), may be dropped (`P:212`) (H25) |
| Const-folded delimiter mask | M: CSS 0.80–0.91× (PROVE-12) | Ours as grammar-driven folding | Required | Prototype | **HOMELESS** (H25) |
| shufti/truffle class runs | M (verified): skip-heavy ~7.5× shufti / 4.7× truffle (the prover read 8.9× / 5.5×); dense 1.03–1.15× (PROVE-01) | Standard (Hyperscan) | Required | Prototype | LW4.PERF-1 |
| NEON digit parse; multi-block pipelining; `portable_simd` shim | **False** (refuted as law) | — | Distraction | — | Stay refuted |
| Separate structural-index pass, event cursor, dispatch table in the parser, 12-byte tokens, pair fusion, one-shot presize | **False** (measured, rejected) | — | Distraction | Rejected | Note: LW3.PERF-1a builds "our own stage-1 indexer". It must meet these negatives head-on |
| `vshrn` nibble movemask | M: +36.5 % / +34.6 %. Against hand assembly of the older 8-crossing algorithm it leads by ~20–28 %; with the algorithm held equal the intrinsic leads by ~1–8 % (PROTO-A2; one kernel, min-of-7, under load) | Standard | Required | Alive in `parse-that-simd` (`rust/simd/src/neon.rs:14,366`) | LW4.PERF-1 |
| Hand assembly as a general lane | **False** on Apple M for straight-line SIMD (0–5 % headroom) | — | See §6: the owner's requirement stands | 0 `asm!` in either repo | LW4.PERF-1 "only where it beats intrinsics": **NARROWED** |
| Hybrid Rust + hand-ASM emitter (the owner's "union") | Unproven | Ours | Required (owner, five times) | Never built; the x86 layer never ran | **NARROWED**; the grammar-to-ASM path was dropped by Codex (N10) |
| msac-style cross-chunk refill | Unproven | Ours ("the one genuinely novel algorithmic lift") | Required as the flagship asm candidate | Never built | **HOMELESS** |
| Kernel table with checkasm bound to liveness | Harness M (caught a bug). Liveness binding: never built; green harness certified dead kernels | Liveness binding: ours | Required | Table alive; liveness absent | LW3.PERF-1b proves only that the stage-1 op appears in the JSON and CSS plans (`O:124`). No unit asserts each registered kernel runs on corpus bytes or that the dispatch label names the variant that ran: **NARROWED** (N-s) |
| Kernel cost table → kernel plan → emitter ("chain of consequence") | Unproven | Ours | Required (it is where the CSP meets the kernels) | Never built | LW3.PERF-1b in part: **NARROWED** |
| Grammar-derived block-SIMD structural classification | M for CSS only (`369f4ffe7`); JSON path scalar | Ours (Parabix is the only ancestor) | Required: the defensible novelty | Skinny | LW3.PERF-1a/b, LW4.PERF-1 |
| Stage-1 throughput | M, noisy: 4.7–6.2 GB/s JSON now; 3.85–5.0 (April); 8.6 canada (skinny). Unreconciled | Standard | Required | Alive | LW3.PERF-1a |
| No single opponent is fastest on this machine | M (sidecar profiles, not gated, separate binaries, MiB/s): twitter yyjson 3,687 > sonic-rs 2,782 > simdjson DOM 2,642 under load (~3,300 solo); citm simdjson 4,252 > sonic-rs 2,860 > yyjson 2,497; canada yyjson 1,549 leads. simdjson leads yyjson on 4 of 6 sidecar corpora | — | Required knowledge: the bar is per corpus. yyjson on twitter and canada, simdjson DOM on citm, apache_builds, github_events and update_center | Recorded | LW5.CON-1 |
| Exact number conversion (Eisel–Lemire) | M: canada is the standing loss, +1.48 cycles per byte | Standard | Required | In parse-that `parsers/scan` | LW4.PERF-1-U5 (amd 9); LW5.PERF-1 |
| "Beats simdjson" | **Never measured** on a like-for-like plane | — | Required | — | LW4.CON-2, LW5.CON-1, LW7.CON-1-REQ (amd 9). The owner's 05-21 pin ("every path >SOTA") was narrowed to "≥ 5/8 corpora" (`competition.md:225`) with no owner ruling (N-t) |
| CSS against lightningcss | Contested. Early seat: 4–7.5× typed, judged fair (`74ade4c6c`, `05c892933`). Later seats: 1.5–3.3×, unequal depth (`a97f5f365`) | — | Required ("the FLOOR"; "CSS is the authoritative") | — | LW4/LW5 CON; an equal-depth CSS cell: UNVERIFIED |
| x86 tiers (AVX2, AVX-512) | AVX-512 never executed | Standard | Against the owner's words (07-10) | Alive in `parse-that-simd` by delegate ruling X-4 | Owner ruling owed |

### 4.4 Compiler, IR, types, backends, regex, pprint

| Item | TRUE | NOVEL | NECESSARY | State | Carrier |
|---|---|---|---|---|---|
| sk-v26 spine (Program → Graph → TypeSolution → Facts → Plan) | M: 60/60, 92/92 | Standard design | Required as the carrier | On `b5fd0ca98`; trial merge shows 15 conflicted paths | LW3.COMP-1b/c |
| OptIR stage | Unbuilt. Contradicts the restart's "no third optimized IR" (MASTER-PLAN:417) with no supersession cited | Standard | Required | Never built | LW5.COMP-1-OPTIR (amd 9) |
| LIR per rule `{dispatch, RegexPlan, KernelId, memo, plane, alloc}` | Unbuilt | Ours | Required ("generalized IR") | Design | LW3.PERF-1b, LW3.COMP-7; LW4.COMP-4 (amd 9) |
| Left recursion and operator precedence on the spine | The spine rejects it (`RecursiveRule`) | — | Required (CSS and Sheets grammars use it) | Root only | LW3.COMP-2 (`P:178`, `O:108`): Pratt tables derived from the grammar plus an iterative left-fold for direct, indirect and mutual recursion, against a seed-growing oracle; gate `crates/oracle/tests/lr_seed.rs` (amd 4). Open until it lands |
| Type inference: HM, bidirectional, CSP-backed | M: a 221-line HM core derives schemas at zero runtime cost (PROVE-08). The documented HM core "DOES NOT EXIST" in the product (sk-v20) | Standard parts | Required (owner 05-04, 07-05, 07-16) | Prototype only | **HOMELESS**; narrowed to first-order by Codex (N3; `CC` reads `typecheck.rs` as "first-order structural types"). LOCK-1 option 1A once proposed displacing the root CSP with PROVE-08's union-find (LOCK-1 `REGISTRY.md:118,123`): a type-system unit must state that the CSP stays its substrate |
| Rust backend over the plan | — | — | Required | Root emitter alive; plan-driven one unbuilt | LW4.COMP-1 |
| TS backend over the IR | TS 0.3.0 published; parity GREEN ×2, 2 divergences carried | — | Required (owner 11-11; supersedes the 03-17 "no TS IR") | Alive | LW3.USE-3, LW4.PP-3, LW6.TOPO-2 |
| TS backend receives the optimized plan | No e-graph or CSP exists in `typescript/` | — | Required for backend parity | Design | Through `plan.json`: LW3.USE-3. Depends on LW5 producing the plan |
| WASM backend; "WASM union between BBNF and parse-that" | M (April): WASM 1.9–3.1× the VM | — | Open in the owner's words (08-03) | Emitter alive; cut from gated scope | LW4.COMP-3 is a delete-unless-wins trial: at risk |
| Bytecode VM, source maps, DAP | Built 03-15/03-25 | Standard | Optional; UNVERIFIED whether the owner still requires DAP | Root | **HOMELESS**. No sk-v27 text names DAP or source maps. The DAP code (`crates/lsp/src/dap`) steps the root VM; LW4.COMP-2's gate cuts the LSP off `bbnf-ir`, and `crates/ir` is deleted at LW7. Owner ruling owed (O-12) |
| Regex: leftmost-first semantics | M: engine is leftmost-longest; Route A 121/133 agree, Route B 114/133 | — | Required | Fix on a spike branch only | LW3.RX-1..4; OR-7 PROVISIONAL |
| Bespoke determinizer against `regex-automata` | — | — | The owner asked for bespoke (04-01, 04-02) | Route A adopts `regex-automata` provisionally | Owner ruling owed (N8) |
| BBNF-generated regex front end; delete the 799-line hand parser | A generated one exists (2,226 lines) | Ours | Required ("No custom grammar, unless it's BBNF") | Hand parser live | LW3.RX-5 (`O:145`): the hand parser is deleted; GREEN requires `git ls-files rust/regex/src/hir/parser` to be empty, and "Keep both parsers" is a must-not. Executable since RX-B's provisional ruling (`R:152`) |
| Lookaround (38 of 115 value.js leaves), `/iu` folding, backreferences | Refused or wrong today | Standard | Required by the consumer | Born-RED, ignored | LW3.RX-1..3; LW4.RX-2-BR (amd 9) |
| Regex facts feeding the grammar compiler; regexes recognised as fused scanners | M in effect (the March breakthrough) | Ours | Required | Root + parse-that | LW4.RX-1/RX-2: UNVERIFIED that the recogniser is carried |
| pprint ABI v2 (O(1) Wadler group decision) | M: laws at ≥ 10⁵ cases | Ours | Required | Alive | LW3.PP-1..3; LW4.PP-1..4 |
| pprint open defects (one unsound `unsafe`, 64-level limit, byte widths, no stream format) | Proven present | — | Required | Open | LW3.PP-1a (`unsafe` → 0, 64-level limit removed), LW3.PP-1b (display width through `unicode-width`), LW3.PP-3a (STREAM-v1) (`O:151-155`); LW4.PP-1-NAH, -LL (amd 9) |
| pprint "~10× the `pretty` crate" | M but a different product (v1 was not Wadler-correct) | — | Re-measure on v2 | — | LW5.PP-1 |

---

## 5. The CSP and the e-graph: definition of done

The owner's words that bind both:
- "The CSP and e-graph items are hard requirments" (2026-11-11 22:30).
- "full CSP implementation and optimization passes" … "NO shortcuts. Full and grand implementations." (2026-11-11).
- "Everything that can be modeled as a CSP, should; everything that can leverage the egraph, should; everything that's NOT befitting of either, should NOT attempt to shoehorn thereof in." (2026-04-08).
- "We should be optimizing both locally and globally and do so in full generality." (2026-04-12).
- "Our optimization process should be our apotheosis and shining achievement: CSP, egraph, shape mining, rooted into a shared IR, local/global cost model, and deep ties into the regex system's cost model" (2026-05-04).

**The finding that governs both.** The archive's own conclusion is that "the engineering value lives in the *rewrites that create alternatives* and the *cost that discriminates them*" (sk-v21/27). With one form per class and null costs, every solver and every extractor returns the greedy answer. Neither organ can be "done" until a measured cost exists and a real choice exists for it to make. Building the solver features alone would repeat W1.

**Existence is not on trial, at any wave.** The owner: "The CSP and e-graph items are hard requirments" (2026-11-11 22:30); "the egraph stays regardless … The e-graph CANNOT be deleted" (2026-04-08 19:13). Delegate ruling OWN-1 (`R:130`) protects only the LW3 port. These deletion paths in the sealed plan are still live and must be amended in the same ruling:
- C1-4's "the simpler organ wins: greedy over CSP, direct over e-graph, scalar over kernel" (`designs/compiler.md:32`), and I6;
- the "(only if CB-T kept …)" guards on CB-7 and CB-8 (`compiler.md:182-183`);
- CB-13's "final keep/delete verdicts for CB-7, CB-8 and CB-9" (`:189`) and CB-14's "Delete … trial losers" (`:190`);
- PLAN LW5.COMP-2, "keep/delete verdicts recorded against the new spine (I6)" (`P:227`);
- G-K1's "or the kernel is deleted" and G-Q1's "off by default or removed" (`designs/rust-perf.md:131-132`);
- programme O012, which retires a CSP or e-graph pass that does not beat a direct worklist (`programme.md:414`);
- PREREG §6's verdict rule: "LOSS and UNDECIDED keep the simpler arm … and the organ is tagged the loser" (`bench/PREREG.md:135`).

A LOSS or UNDECIDED may switch one rewrite family, constraint family or kernel variant off by default, recorded by name. It never removes the CSP stage, the e-graph stage, the shape-mining stage or a kernel slot from the spine; it opens a redesign unit with a re-trial. A NOISY record decides nothing, in either direction.

Gate: `git grep -nE 'simpler organ wins|keep/delete|trial losers|or the kernel is deleted|off by default or removed' <wave-head> -- restart/skinny/tranches/sk-v27 bench/PREREG.md`. Every hit must be amended text that cites the owner's ruling. Read RED at `28c09a0a5`.

### 5.1 CSP: done when all of these hold

**A. The solver is complete and sound (upstream `csp-solver`).**
1. Upstream `csp-solver` gains: nogood learning wired into search; conflict-directed backjumping; restarts (Luby); `bounded_sum` with a Σ-min-residual bound; cardinality and global-sum families; soft constraints or a penalty encoding; LCV value ordering; lex-leader symmetry breaking; incremental re-solve; min-conflicts local search; and Unsat explanations that name the constraints, built toward lazy clause generation. Excised code is restored from its last tree (`1e65de292^`, `e07ac306^`, csc411 `3c8eade1` for Luby) or re-derived with the excision cited. Each lands with law tests, a semver-probed release, and the AC-3/GAC notation in its doc comments (owner 04-06: "Notate the CSP items in your implementation (like AC3, etc)"). The public API passes the owner's Python test suite unchanged (owner 04-06: "we must validate that our API is 100% compatible with our python test suite"). (LW5.COMP-3; AYW R1; owner 07-05: "cutting edge".) Born-RED at csc411 `65804fa7`: `solver/` holds `ac3, adjacency, gac, monotonic, optimize, propagate, search` only.
2. Both pins (bbnf-lang and parse-that's e-graph scheduler) move to a release at or after csc411 `58014efd`, the CH-69 cure. The 0.4.0 pin already carries the `69ff7f71` trail fix. Proof: upstream `gac_kernel_beats` and `killer` pass against the pin. Transport (S3-A) ruled.
3. Solve time is measured per grammar with a ceiling (≤ 1 s per grammar, `restart/ARCHITECTURE.md` at about `:1159-1161`). Exceeding it fails the build; it never truncates the search to greedy. Component detection is in the solver, so the `ae05b4698` cliff cannot recur.

**B. It is the single inference substrate on the spine.**
4. Types, FIRST, FOLLOW, nullable, span and dispatch eligibility and the call-mode plan run on it inside the spine, through one `propagate()`. The spine's FIRST equals the root's on every grammar. LW3.COMP-7 either computes on `csp-solver`'s `propagate()` or LW5.COMP-1 states that it replaces COMP-7's fixed point (N-w).
5. Every domain has a top or a typed conflict; no result is discarded; the lattice-law oracle runs on all five domains and on each new one.
6. All structural pattern recognition is one CSP pass: detectors become constraint emitters (owner 04-07; memory `feedback_csp_unified_patterns`). Baseline: 37 files and 7,179 lines in `crates/ir/src/passes/recognizers/` at `28c09a0a5`; `StructuralPattern` 0 hits. Read by the census of §5.3-1 (`unhomed=0`) and one CSP instance per grammar carrying every pattern variable.
7. The type-system CSP exists: finite choice for overloads, layout, materialisation, recogniser eligibility, recovery and backend erasure, with improvement and implication constraints, named apart from the extraction CSP. (Owner 05-04; `restart/ARCHITECTURE.md:1822-1856`.)

**C. It decides real backend choices, locally and globally.**
8. The solved plan decides dispatch shape, regex engine and tier, materialisation, inline or call mode, payload layout, scan plan and backend shape. The emitter reads the plan and decides nothing. Removing any constraint family flips a pin test and changes emitted code.
9. Per-component exact optimisation (the `csp_strategy` COP, carried onto the spine by name), plus the bounded coupled sub-problem over resources that cross sites and are measured to bind: hot-path instruction bytes per entry point (Lock 15, ~20 KiB, read from the emitted object's symbol sizes); shared lookup-table, Teddy and shufti slots; tape and payload slots; the inline budget. A generated-size (line-count) ceiling applies only if the owner rules one (O-10): his standing word is "a larger binary is acceptable for now" (04-08). Witness: a grammar where independent per-site minimum cost busts the hot-path budget and the coupled solve finds a legal plan with lower measured cycles per byte; with a slack budget the plan equals the per-site optimum byte for byte. Real costs, in production. Born-RED: `bounded_sum` is on no pushed ref.
10. Budget exhaustion is reported and fails the build. No silent fallback: `git grep -n 'falling back to per-variable trivial pick'` reads 0 on the spine and root, and a forced-budget regen exits non-zero (born-RED: `csp_strategy/mod.rs:612-618`). The fail-closed rewrite at `d2e169794` (which also adds a `token` constraint family, `constraints/token.rs`) is adjudicated before LW5, not at LW7.
11. Objectives read a measured cost table (cycles per byte by kernel and microarchitecture, fitted to hit distribution). Flipping a coefficient changes the plan. The April null sweep does not reproduce.

**D. It is joined to the e-graph and the regex tier.**
12. Constraints live inside extraction: illegal nodes pruned, resources coupled. The CSP never receives a pre-chosen answer. Facts cross by stable IDs with a justification for each accepted or denied rewrite, at `passes/csp_egraph_bridge.rs`, carrying both e-graph and CSP explanation refs. (README §6 bridge.)
13. The saturation scheduler uses real solver results, and its gain over the plain backoff scheduler is recorded (nodes, iterations, wall time).
14. Regex tier and prefilter selection use the same solver and cost substrate as the grammar tier. (Owner 04-07, 07-16.)

**E. It is measured and visible.**
15. Both organ trials are re-registered by a PREREG addendum, before any run, on grammars where a choice exists: CSS L4 (≥ 10 competing scan sites), BBNF (≥ 3) and Google Sheets, plus the three H-1 grammars (TOML 1.0.0, GraphQL October 2021, R7RS datum) once present. JSON has one site; it stays only as a non-regression cell. The CSP trial has four arms: (a) the solver; (b) independent per-site minimum cost under the same cost table; (c) the historical priority cascade; (d) per-component only, against per-component plus the coupled sub-problem (owner 04-12: "global csp vs rule-based csp, global cost vs rule-based cost"; the archive's own acceptance, never run: "strictly-better-or-equal everywhere OR documented null-result", `43901076f`). Compile cells record solve µs, nodes, `budget_exceeded`, component count and size, and plan hash; parse cells record cycles, ns and instructions per byte. The verdict sets how the organ is used, never whether it exists.
16. Tuning, validation and visibility (owner 04-12: "a facility to better tweak, test, and validate ALL of our optimization passes … hyper parameter choosing"): an xtask that sweeps cost hyperparameters over the 9+3 grammars and records the plan hash per configuration; the April 648-run sweep re-run on the measured cost, which must not be null; a playground view of each `CostDecision` and `BridgeJustification`.

### 5.2 E-graph: done when all of these hold

**A. Every rewrite is sound by proof.**
1. Each rule declares its observation class (accept set, typed value, spans, expected-label set) and the failure contract it preserves.
2. Each rule has a property-tested witness against the reference interpreter with optimizer-on and optimizer-off arms, and a seeded unsound mutant the differential catches.
3. Each known or suspected refutation is committed as a failing test first, then the rule is repaired or removed: `WrapOfEpsilonScalar`, `FuseAltRegexBranches` (back only with the `L_cut` certificate), `SupersetAbsorbAlt`, `UnionMergeAlt` (plus unescaped metacharacters and bytes ≥ 0x80), `CommonSuffixFactor` (FIRST-disjoint guard or delete), `RepeatOfSingle`, `ConcatLiterals`, the HIR repetition merge.
4. Regex-tier rules run on `FirstSet` with single-width preconditions; the repetition-stripping helper leaves the rewrite path.
5. Formatting output is byte-identical with the optimizer on and off.

**B. The organ is honest.**
6. An unsaturated or over-budget run falls back to the un-rewritten IR with a diagnostic; the run report is a durable artifact.
6a. Budgets are numeric (ARCH §10.1): legality rewrites run to a fixpoint and abort on a cycle; normalization rewrites ≤ 1,024 steps per rule; cost-driven rewrites ≤ 256 e-class merges per rule; the existing iteration limit 64 and node limit 100,000; plus a wall-clock limit. Each fails closed.
7. Budgets per rewrite category fail closed, with a time limit and an adversarial grammar per category.
8. No surface without a consumer: `crates/ir/src/rewrites/` and the Pareto scaffold are wired or deleted.
8a. The `Map{fn}` 1e6 cost hack is removed (`4373a49dd`). A `Map{fn}` and its bare sibling are not observation-equivalent, so the class registry forbids the union.

**C. It optimises.**
9. The e-graph subsumes the destructive passes (owner 04-08 18:43: "These should SUBSUME the destructive passes. Something is wrong with our egraph approach."). The normalizer's rewrites (inline, merge, factor, ε-elimination, the G1–G4 duplicates) become e-graph rules, and saturation also runs where alternatives exist, in the OptIR islands between Facts and Plan. Any destructive pass kept outside the e-graph carries a recorded, measured "not befitting" verdict under his 04-08 19:13 rule. The `bfa50f25f` demotion was argued, never measured. Born-RED: the root normalizer runs before saturation with no verdict on file.
10. Widening rewrites create classes with two or more real forms: dispatch table against sequential choice, perfect-hash keyword sets, operator chains, repeat-loop hoisting, leaf-bounded inlining. All seven README §6 categories are built at their README paths (`passes/egraph_rewrites/{algebraic,charclass,keyword_phf,operator_chain,repeat_loop,tail_call,dead_alt}.rs`), or each is retired by a recorded, measured verdict on a QUIET record.
10a. The operator-chain and tail-call categories consume LW3.COMP-2's Pratt tables and left-fold; they do not replace that unit as the carrier of left recursion.
11. Cost is fitted from hardware counters. A pick that loses by more than 10 % on a held-out grammar family fails.
12. An island keeps the e-graph only if its pick changes under the calibrated cost and moves cycles per byte; otherwise it becomes a plain ranked pass. It changes emitted code on at least 3 grammars. The regex-algebra rewrites stay in the e-graph even where an island is demoted (owner 04-08 19:13: "for our regex algebra re-writing, we MUST keep it").
12a. Determinism: ties break by content hash, with a negative control; `cargo xtask regen --check` is byte-identical across two runs.
13. Every rule in the default set applies (non-zero union count) in the saturation of at least one production grammar, read from the per-grammar `PassReport` (`BBNF_EGRAPH_REPORT=1`; `egraph_fire_probe`), and the form it selects is reached on that grammar's parse hot path, shown by attribution and not by source grep. Rules fire when a grammar is compiled, so the old "0.1 times per parse" floor (`3fee71a4c`) was the wrong unit, and its "rules below the floor retire" clause is superseded: a rule that applies nowhere is given work by rows 9 and 10 or removed as dead code, and its rewrite family is not retired by noise. Born-RED: 13 of 14 rules apply zero times (`7620cac2a`).
14. The extractor matches the cost's shape: greedy while costs are additive; sharing-aware once shared kernels amortise.
14a. Write-back keeps the sharing that extraction chose. Today the sharing is found and then discarded (`AL/research/prototype-1.md:8`).
14b. Every decision emits a `CostDecision` (selected, rejected and dominated alternatives, objective vector, extraction method, legality evidence; README §6). Missing evidence raises `BBNF-COST-EVIDENCE-INCOMPLETE`; stale cost above 30 % rejects the wave (`ARCHITECTURE.md` at about `:1156-1170`).

**D. It is one engine across tiers and backends.**
15. One child-bearing grammar e-node on the spine; `parse-that-egraph` carries no BBNF vocabulary; e-nodes are derived.
15a. Rules are declared as patterns over derived e-nodes (an e-matching language in `parse-that-egraph`), and the engine records explanations. Today rules are "intentionally imperative rather than pattern-based" (`rewrite.rs:28`); the bridge justifications need both.
16. The grammar and regex tiers share the engine, the cost substrate and the `L_cut` equivalence, and mirror each other in "types, structure, dir structure" (owner 04-08). The TS backend receives the same optimised plan.
17. F-TRIAL-EG is re-registered on the same grammar set as §5.1-15 (as registered it is JSON-only, where 13 of 14 rules never fire). Two arms, e-graph on and off; compile cells per grammar record saturation iterations, applied unions per rule, e-class count, extracted-plan hash and emitted bytes. A third arm, normalizer-only against e-graph-subsumes-normalizer, opens once row 9 lands. The records are corrected: the +8 % spot file, `docs/GESTALT.md:58`, the `egraph_semantic.rs` docstring.

**The owner's own boundary applies.** Automaton minimisation is not e-graph work (measured, AY-I:170). Forcing it in would break his "should NOT attempt to shoehorn" rule.

### 5.3 Shape mining: done when all of these hold

The owner names three tiers every time: "our three tier optimization methodology of CSP/egraph/structural matching must be leveraged both on a per-rule and a global basis to optimize emission shape and materialization" (04-12 06:08); "What information from our optimiztaion systems (csp, egraph, structural mining) can we pool to make a more globally informed tape decision?" (04-15); "CSP, egraph, shape mining, rooted into a shared IR" (05-04). README §6 pass 4 makes it a stage (the `ShapeMiner` trait). No sk-v27 unit names it.

1. Every recognizer in `crates/ir/src/passes/recognizers/` (23 modules, 37 files, 7,179 lines at `28c09a0a5`: `key_dispatch`, `delim_scan`, `quoted_string`, `operator_chain`, `separator_list`, `shape_dispatch/` and the rest) becomes a `ShapeMiner` that emits weighted hints, or a constraint emitter into the one CSP pattern pass (§5.1-6), or a widening e-graph rule whose extracted node is the shape tag. No hand predicate decides a shape. Read by a census script listing each file with its new home: `unhomed=0`.
2. The techniques in those files survive the move: delimiter-driven scanning, key/ident dispatch, quoted-string scanning, operator chains to Pratt, separator lists. Read by a pin test per technique that its emitted form appears in a generated parser.
3. Miner hints feed the shared cost model, and the tape and backend-shape decision pools CSP, e-graph and miner facts. Removing one miner flips a pin test and changes `plan.json`.
4. No language name appears in any miner: the generality grep over the miner tree reads 0 with no exclusion list.

### 5.4 Gate rows: reader, threshold and today's reading

Readers marked † must be built by the unit that owns the row; the rest exist today. Heavy readers run through the machine guard. Every row reads RED or partial today.

| Row | Condition | Reader | GREEN | Today |
|---|---|---|---|---|
| CSP-A1 | Solver complete (§5.1-1) | csc411 `cargo test -p csp-solver`, one named test file per feature; `ls csp-solver/src/solver` | every module present, laws pass, release tag | RED (`65804fa7`) |
| CSP-A2 | Sound pin, both repos | `cargo tree -p csp-solver` in bbnf-lang and parse-that; upstream `gac_kernel_beats`, `killer` against the pin | ≥ a release containing `58014efd`; tests pass | RED (0.4.0 in both) |
| CSP-A3 | Solve ceiling | `BBNF_CSP_REPORT=1 cargo xtask regen --check` per grammar | ≤ 1 s per grammar, `budget_exceeded=false` on all 9+3 | RED (CSS L4: budget exhausted in a 52.53 s compile) |
| CSP-B4 | On the spine | `cargo tree -i csp-solver` from the spine crate; `facts.json` differential against root | non-empty; 0 diffs over 9+3 | RED (the spine has no CSP) |
| CSP-B5 | Laws on five domains | law oracle at 10⁴ cases per domain, plus a mutant | 5/5 GREEN, mutant RED | RED (2/5) |
| CSP-B6 | One pattern pass | †census script over `recognizers/` | `unhomed=0` | RED (37 files) |
| CSP-B7 | Type CSP | †type-choice suite (overload, layout, materialisation, recovery, erasure) | pass ×2 | RED (absent) |
| CSP-C8 | The plan decides | per-family removal mutant → pin test plus `plan.json` and emit diff | each family flips | partial (root pin tests `0cfc3663b`); spine RED |
| CSP-C9 | Coupled solve | witness-grammar test plus a bench cell | per-site minimum busts, the solve is legal and faster; slack equals per-site | RED (no `bounded_sum` on any pushed ref) |
| CSP-C10 | No silent fallback | the grep of §5.1-10; forced-exhaustion regen | 0 hits; exit ≠ 0 | RED |
| CSP-C11 | Measured cost | coefficient-flip mutant → `plan.json` hash changes; the sweep of §5.1-16 | changes; non-null | RED (null sweep `05364225b`) |
| CSP-D12 | Bridge | `ls passes/csp_egraph_bridge.rs`; justification count = accepted + denied rewrites | equal | RED (0 hits) |
| CSP-D13 | Scheduler measured | CSP scheduler against the backoff scheduler (nodes, iterations, wall) | recorded QUIET | RED (never measured) |
| CSP-D14 | Regex on the same solver | `cargo tree -p parse-that-regex -i csp-solver`; tier plan per leaf | non-empty | RED (local table) |
| CSP-E15 | Discriminating trial | PREREG addendum hash; `admit.py` for CSS, BBNF and Sheets cells; `decide.py` | admitted, decided QUIET | RED (JSON only) |
| CSP-E16 | Visibility | †tune xtask; `CostDecision` count = decision count | equal; sweep non-null | RED |
| EG-A1 | Observation class per rule | the COMP-4c registry read by the stage verifier | `missing=0` | RED (no `obs()` in code) |
| EG-A2 | Witness per rule | a proptest per rule, on and off arms, plus a seeded unsound mutant | all pass, mutant caught | RED (0 witnesses) |
| EG-A3 | Refutations first | born-RED tests for each suspected rule, then the cure | RED ×2 → GREEN ×2 | RED; 3 parse-that tests `#[ignore]` |
| EG-B6 | Fallback and numeric budgets | adversarial grammar per pool; `PassReport.stop_reason` | fails closed to un-rewritten IR | RED (`let _report`) |
| EG-C9 | Subsumes the normalizer | destructive-pass list against the verdict file | `missing=0` | RED |
| EG-C10 | Seven categories | `ls passes/egraph_rewrites/*.rs` plus a firing report | 7 present and applying, or a QUIET verdict each | RED (~2/7) |
| EG-C11 | Counter-fitted cost | held-out grammar-family cell | no pick loses > 10 % | RED (hand weights) |
| EG-C13 | Rules apply | `egraph_fire_probe` or the spine `PassReport` | every default rule > 0 on ≥ 1 grammar | RED (1/14) |
| EG-C14 | Changes emitted code | regen diff, on against off | ≥ 3 grammars | RED |
| EG-D15 | One e-node, patterns, explanations | `git grep -c 'enum GrammarENode\|struct DecisionNode'`; pattern-declared rule count | 1; all | RED (2; imperative) |
| EG-D17 | Discriminating on/off trial | as CSP-E15 | decided QUIET | RED |
| SM-1..4 | Shape mining (§5.3) | census, pin tests, miner-removal mutant, generality grep | all GREEN | RED |
| K-B5 | Kernel harness (§6 B.5) | checkasm, 10⁷ fuzz, Miri, ASan job id, per-primitive status TSV | all clean | partial |
| K-B6 | Liveness | †generated-parser trace counts each kernel on corpus bytes; dispatch label equals the variant run | every planned kernel > 0 | RED (JSON path scalar) |
| K-C9 | Assembly path exists | `git grep -cF 'asm!('` in the seam module; a claim-grade record per block | per O-3 | RED (0) |
| K-C10a | ISA census | census TSV against `sysctl hw.optional.arm` | `unverdicted=0` | RED |
| K-D13 | Kernel cost table | †table file read by the CSP; mutating one entry changes `plan.json` | changes | RED |
| T-14..19 | Tape union (§6 E) | P4 witnesses plus fuzz; per-consumer ID witnesses; the greps | all GREEN | RED (two unrelated tapes) |
| H-20..22 | Carry list, AU perf gate, PROVE landings (§6 H) | pin tests; paired AU cell in CI; PROVE re-measure cells | all GREEN | RED |

---

## 6. The optimization and ASM programme: definition of done for "besting simdjson in full generality"

The owner's words:
- "a perfected union between backend agnostic code, with an emitter that specializes in a hybrid of Rust and hand-rolled ASM, inspired by notions from the DAV1D project" (06-10, repeated 07-05, 07-10, 07-15).
- "beating of simd-json and the like with our deep DAV1D-like SIMD heuristics leveraging esoteric and black-magic ASM" (11-05).
- "a robust Value API, that matches and beats lightningcss, simdjson, yyjson, sonic-rs, using lazy and eager evaluation, alongside a unified tape and backend layout, alongside a unified path and direct to struct API is a first class requirement" (06-10, repeated 07-05, 07-10, 07-15).
- "this NEEDS to generalize to other arbitrary grammars, NOT just JSON … Absolutely NO contrivances, NO new directives" (05-12).
- "do not give me the 'sonic-rs is hyperoptimized with magical and arcane SIMD'--no" (04-18).
- "This MUST get all of the JSON numbers for every path >SOTA. Full semantic parity with lightningcss. Aggressive. Indefatigable." (05-21, USER-PIN addendum; 51 JSON rows above sonic-rs strict) [dig-plans].
- "…within our ASM hybrid union perfection--ensure that's for both parse-that (regex engine, etc) and herein." (07-10).
- "what commands within our host arch might we leverage in ASM? What more undocumented or esoteric items might we use?" (05-12).

**Where the record stands.** simdjson has never been measured in a like-for-like harness. The JSON wins on record are scalar and lazy. The AU peak reached 0.52–0.61× of sonic-rs on the same plane. Live, executed hand assembly across the whole programme: zero. Certified cells: 0.

**Done when all of these hold.**

**A. The claim can be certified.**
1. Same-plane opponent arms exist: simdjson C++ (On-Demand and DOM) and simd-json 0.19 (DOM and `to_tape`), built with the same flags, with a checksum walk inside the timed region. sonic-rs and yyjson are in the same table. On this machine no single opponent is fastest: yyjson leads on twitter and canada, simdjson DOM on citm, apache_builds, github_events and update_center. Owned is compared with owned and borrowed with borrowed (owner 03-17: "If it borrows string, why is the fair comparison not BBNF borrow?"); output is validated structurally against a reference parse inside the harness (owner 04-13); the allocator is named in every record. (LW4.CON-2.)
2. "Full generality" means: the generated track only (never a hand prototype); every value plane P1–P7, each against its own opponent plane (a lazy plane never against a DOM); every corpus, including escapes, unicode and numbers; Holm-corrected; ≥ 200 pairs. P3, P5 and P6 are required cells, and each needs a builder as well as a cell (H20). The owner's 05-21 pin reads "every path >SOTA"; "≥ 5/8 corpora" (`competition.md:225`) is a narrowing he has not ruled (O-13). (LW5.CON-1, LW7.CON-1-REQ.)
3. Generality across grammars: the same kernels, driven by generated tables, on JSON, CSS and the three H-1 grammars (TOML 1.0.0, GraphQL October 2021, R7RS datum syntax), each generated, with no `@generated` verbatim blob (`verbatim_blob_present` false at close; `LOCKS.md:637`). No grammar names in generic crates, enforced by a lint that cannot pass by exclusion. CSS carries its own equal-depth cell against lightningcss's typed tree.
4. Records are cold, same-run, pinned, taken in a quiet window. MB/s comes from `ns_per_byte`; megabits and megabytes are never mixed; one corpus file per name.

**B. The kernels follow the dav1d discipline, bound to the live path.**
5. One kernel table chosen once at start-up, with a scalar oracle per kernel. Checkasm iterates every registered variant over random, corpus and adversarial inputs, lengths 0..256, a page-boundary and alignment sweep, a stack canary and guard pages. Fuzzing reaches 10⁷ clean executions per target; Miri is clean on scalar paths and safe wrappers; a nightly ASan job runs; clobber tests cover every `asm!` block (CB-9, `compiler.md:184`). Every aarch64 primitive reports one of `wired`, `scalar-delegated`, `source-present-blocked`, `strict-checkasm-admitted` or `deleted`, with its `hw.optional.arm.FEAT_*` gate (Lock 16/8, `LOCKS.md:635`).
6. A liveness assertion: every planned kernel is reached on real corpus bytes in the generated parser. The dispatch label reports the variant that ran. A kernel variant with no live consumer in its own wave is switched off and opens a redesign unit; no kernel slot is deleted by a trial (§5).
7. The kernel set: grammar-derived stage-1 structural classification with the `vshrn` nibble movemask; whitespace skip; `vqtbl` byte-class tables; shufti/truffle class runs; accel self-loops; keyword PHF; fused string scan; UTF-8 validation; escape mask and `\uXXXX` decode; exact number conversion (Eisel–Lemire with a correctly rounded fallback); prefix-XOR string mask; stop-early `get`; and for `parse-that-regex` the start-state prefilter (PROVE-10), Teddy and the DFA inner loop. Tiers: NEON first and wasm32-simd128, with x86 per O-2. One speed threshold over the scalar oracle is ruled once (G-K1 says ≥ 3×, CB-9 says ≥ 2.0×; O-11), and each kernel must move a JSON, CSS or regex cell. A miss on a QUIET record opens a redesign unit; a NOISY record decides nothing.
8. The measured negatives stay negative unless new evidence appears: NEON digit parse, multi-block pipelining, a separate index prepass with no amortisation, one-shot presize.

**C. The hybrid Rust + ASM emitter exists.**
9. Generated Rust calls kernels through one kernel ABI. An assembly emission path exists and is exercised. Each assembly kernel has a claim-grade record against its intrinsic twin.
10. The measured charter for where assembly is admitted: no intrinsic exists, a disassembly diff shows ≥ 10 % left (U12, `rust-perf.md:246`), or state must cross a chunk boundary. Assembly lives only in the seam module. The flagship candidate is msac-style cross-chunk refill. The adjudicated design had a hand-written `.S` seam with a 15 % roofline trigger (programme W8, `programme.md:473-487`); PLAN kept neither, and the adjudication marked the charter "optional".
10a. An ISA census of the host: every `hw.optional.arm.FEAT_*` that reads 1 (PMULL, DotProd, I8MM, CSSC, SHA3 and the rest of the probe; SVE2 is absent, `LOCKS.md:635`) has a named kernel candidate and a recorded verdict, adopted with a record or a measured negative. Candidates include PMULL prefix-XOR, a `udot` digit multiply-accumulate, CSSC `cnt`/`ctz` for mask walks, and `prfm`/`stnp` hints. This answers his "esoteric" requirement by measurement.
11. **The owner must rule what "union" obliges** (see §8, O-3): a hybrid emitter able to emit assembly under that charter, or at least one live assembly kernel as a condition of done. PROTO-A2 answers one kernel. A better algorithm in intrinsics (`vshrn`, 4 crossings) beats hand assembly of the older 8-crossing algorithm by ~20–28 %. With the algorithm held equal, the intrinsic leads by only ~1–8 %. Single kernel, min-of-7, under load. It does not retire his requirement.
12. The 05-13 question ("Is our new path matierally different? For both skinny and greater?") is answered by measurement: why this path will not repeat the "automaton overhead that … the LLVM optimizer could not compile away".

**D. The cost model chooses the kernels.**
13. A kernel cost table (kernel × microarchitecture → measured cycles per 64 bytes), filled by the harness, read by the CSP and the extractor to choose kernel, backend shape and inlining. This is the "chain of consequence" sk-v20 named as the missing piece, and it is where §5 and §6 meet.

**E. The tape union exists.**
14. One tape: `TapeToken{kind, flags, start, end, payload, sibling_skip}`, a payload arena, a `ValueRef` cursor, `Document{tape, root, metadata}`, optional trace events.
15. Generated direct-to-struct values carry the tape node identity. The struct built directly equals the struct projected from the tape on every P4 witness and under fuzz. The one exception is `SinkOnly`, which by Lock 1 retains no document identity; its authority is the source span.
16. All five backend shapes (offset tape, event tape, sink-only, collapsed stage, eager tape) are members of the union. The CSP chooses one per rule from grammar facts, e-graph extraction and miner hints (owner 04-15), and every extraction result declares its `substrate_target`. No directive or workspace key selects a shape.
17. Lazy numbers are a flavour, not a second tape. No sidecar, no column layout, no parallel offset vectors: `ParserState.structural_offsets` and `TapeAssembler.offsets` are deleted, with the scan writing through into the tape's own offsets. The root `TapeRec` and skinny's tape are retired into the union.
18. LSP, DAP (if O-12 keeps it), path/select, visitor, diagnostics, playground and bench attribution key on the same node IDs. Mutation only through the visitor. One witness per consumer resolves the same ID.
19. The value API is built, not only measured: the four materialisation surfaces (`as_<T>()`, `try_into()`, typed property, visitor; README §8); `path!` and `select!` over one path core (today's `bbnf-path` and `bbnf-path-ts`, 918 and 1,012 lines, are deleted by census default unless `StructRegistry` gains a home in the plan); lazy and eager evaluation. For JSON every value is direct-to-projection (owner 04-12). The tape is not the navigation substrate for `get` (measured: 150–1,700× the stop-early cost).

**F. Build discipline.** Fat LTO, one codegen unit, forced inlining of hot rules, a ~20 KiB hot-function ceiling with a diagnostic, a panic-free hot path.

**G. Every perf unit opens with a committed cycle split** (stage 1, structural walk, strings, numbers, allocation, tape emit) and closes with a certified record or a recorded negative. The AU table (tape-only and decoded planes, kept apart) and the verified PROVE ratios are the baseline any new claim must beat on the same plane.

**H. Nothing proven is lost.**
20. A carry list, each item with a pin test on emitted code: sibling skip; the grammar-emitted visitor path (`c1e86ab32`); delimiter-driven flat scanning; regex-as-fused-scanner recognition; key/ident dispatch; IIFE elimination; the lazy-offset tape; Lock 15. Owner 04-17: "do not lose any optimization or implementation in this restructuring".
21. Every substrate or emitter-shape change carries a paired cell against the AU table (`docs/benchmarks/post-AU.json`) on the same plane before merge, on JSON, CSS, BBNF and Sheets, in both bbnf-lang and parse-that. A regression beyond the floor blocks the merge. LW3.CI-IAI-0/1 covers only `json_callgrind` on bbnf-lang PRs against the previous head.
22. Each PROVE GO result (01, 03, 06, 07, 10, 12) names its landing unit, and its re-measure at HEAD on the same plane is at least its verified PROVE figure, or a QUIET negative is recorded.

---

## 7. Constellation and checkpoint requirements

| # | Requirement | Source | State |
|---|---|---|---|
| C1 | Best simdjson in full generality | Owner 11-05, 11-11 | **Unmet.** 0 certified cells; never measured like-for-like. Units: LW3.PERF-1a/b, LW4.PERF-1, LW4.CON-1/2, LW5.CON-1, LW7.CON-1-REQ |
| C2 | Full CSP implementation | Owner 11-11 | **Unmet.** Sound as a fixed-point engine on root. Absent from the spine. Solver hollowed out. Units: LW3.COMP-6, LW5.COMP-1, LW5.COMP-3. Threat: LW5.COMP-2 (CB-13) still issues keep/delete verdicts |
| C3 | Optimization passes (e-graph) | Owner 11-11 | **Unmet.** No measured win; two rules proven unsound; F-TRIAL-EG cannot discriminate as registered. Shape mining, the third tier, has no unit. Units: LW3.COMP-3b/4c, RX-1h, LW5.COMP-1, LW5.COMP-1-OPTIR |
| C4 | Backends for TS and Rust over the IR and a generalized IR | Owner 11-11 | **In part.** TS 0.3.0 published; parity GREEN ×2. Plan-driven Rust emitter and LIR unbuilt. Units: LW3.COMP-1b/c, COMP-7, USE-3, LW4.COMP-1/4, LW6.TOPO-2 |
| C5 | Tape union facility | Owner 11-11 | **Unmet.** Never built. Unit: LW4.PERF-2 (amd 9) |
| C6 | The new IR as the build | Owner 11-05 | **Unmet.** Spine unmerged, 15 conflicted paths. Unit: LW3.COMP-1b |
| C7 | Regex for parse-that and bbnf | Owner 11-05 | **In part.** One engine, HIR frozen. Wrong match semantics live; Route A provisional; hand parser live |
| C8 | pprint and formatting | Owner 11-05 | **In part.** ABI v2 frozen and correct. No stream format; formatter-parity budget X-10 not set by the owner |
| C9 | Type inference SOTA, CSP-backed | Owner 05-04, 07-16 | **Unmet and narrowed** to first-order by Codex. No unit |
| C10 | Everything grammar-derived; no custom grammar unless BBNF | Owner, many times; 09-23 | **In part.** The regex hand parser breaks it. Proving parsers still carry a generalisation debt (owner 05-22: "at that exactly inflection point") |
| C11 | DAV1D-style Rust + ASM union; x86 abrogation | Owner 06-10, 07-10, 11-05 | **Unmet.** 0 `asm!`. x86 tiers kept by delegate ruling X-4 against his words |
| C12 | The TS emitter benefits value.js and keyframes.js | Owner 11-05 | **In part.** USE-1..4 GREEN at LW1; LG-d (timings) carried to LW3 |
| C13 | Release cascade bbnf-lang → parse-that → value.js → keyframes.js | PLAN LW6 | **Not started.** value.js 4.3.0 cut-cleared, unpublished. Departure: 14 crates published at LW2 against PLAN:93 "No crates.io publish happens before LW6" |
| C14 | value.js release condition RC-P (six conjuncts); E-6 size ceilings 15,200 / 6,100 gz; V-C 66 unruled misses; mirror defects 0 | value.js `RELEASE-CONDITION.md` | Open. Ceilings set by the delegate. UNVERIFIED beyond `dig-plans` R11 |
| C15 | Parser speed floor for value.js | Megatranche commission 08-03 | **Narrowed.** The ≥ 10× floor was retired; the 3× / 2× replacement is unratified |
| C16 | WASM union between BBNF and parse-that | Owner 08-03 | **Open, unproven.** WASM cut from gated scope |
| C17 | Lockstep gate at each wave | PLAN §4 | LG-1 never read GREEN. LG-2 taken at its eighth reading. LG-d and LG-e carried into LW3 |
| C18 | Organ trials (e-graph on/off, CSP against greedy) | LW2.COMP-1 | **Registered, never run.** As registered, neither trial can discriminate: both are JSON-only (PREREG §6) |
| C19 | Certification seam | LW2.CON-2 | One floor frozen (G-G2 = 10.0 %). Every other goal noisy or blocked. A quiet window needs an owner ruling |
| C20 | Model-tier receipts on every seat | sk-v25; memory M-12 | Standing law after the 49/60 finding |
| C21 | TS ↔ Rust isomorphism; regex and low-level single-homed in parse-that | Owner 07-18 | **In part.** 21 divergences cured, 2 carried |
| C22 | No V2, no deferral, no stubs, no dual paths | Owner, many times; 11-11 | Standing law. LW7.CON-1 "stretch" breached it until amendment 9 |

---

## 8. At risk of being lost

True, novel or necessary, and HOMELESS or narrowed. Ordered by how much is at stake.

### 8.1 Homeless: no unit carries it

| # | Item | Why it matters | Evidence |
|---|---|---|---|
| H1 | **One CSP pass for all structural pattern recognition** | Owner ruling of 04-07, never revoked; dropped silently at the restart | memory `feedback_csp_unified_patterns`; 0 hits for `StructuralPattern`. The ad-hoc detectors it would replace are 23 modules in `crates/ir/src/passes/recognizers/`, with no port unit, deleted with `crates/ir` at LW7 (`CC`) |
| H2 | **The CSP↔e-graph bridge with justifications** | The requirement of record (README §6); 0 hits on three refs. The engine also lacks e-matching and explanations (`rewrite.rs:28`), the substrate the justifications need | [dig-history-late §3] |
| H3 | **The type system: HM, bidirectional, CSP-backed** | Owner 05-04, 07-05, 07-16. A measured 221-line core exists as a prototype. LOCK-1's 1A route once proposed displacing the root CSP with that union-find; a type-system unit must state that the CSP stays its substrate | PROVE-08; sk-v26 R6 |
| H4 | **The measured cost table and its fit** | Without it neither organ can be shown to matter (§5). LW5.COMP-1 presumes "a measured cost model" and no unit builds one | LOCK-2 2D "zero executed fit" |
| H5 | **A CSP trial that can discriminate** | The registered trial runs on JSON, which has one competing site | PROTO-2 Step 0; PREREG §6 |
| H6 | **No silent greedy fallback, and the fail-closed `csp_strategy` rewrite** | The cure for W6 sits on `sk-lineage/dirty-preserve` `d2e169794` with its fate set for LW7, after LW5 builds the organ | [dig-csp §7, REQ-CSP-7]; the commit also adds `constraints/token.rs` (+110), a token constraint family found nowhere else |
| H7 | **`bounded_sum` code** | **Uncommitted** in three workflow worktrees under `bbnf-lang/.claude/worktrees/`, which are liable to cleanup: `wf_63419230-36d-28` (staged; 168 lines, 7 tests, with `examples/scan_plan_budget.rs`) and `wf_63419230-36d-30` (staged; 178 lines, 6 tests), both at `d2b32f6e3`; `wf_fce2ec2b-53a-32` (unstaged; the same 178 lines, 6 tests, with `examples/scan_plan_solve.rs`) at `ce51c36fc`. LW1 custody captured none of it. Its old home `crates/csp-solver` no longer exists. The final seat copied the files, with hashes, to `~/.dev-logs/bbnf-lockstep/archive-dig/custody-bounded-sum/`; that is a stopgap, not custody. `soft.rs` and `cardinality.rs` are recoverable from `d2b32f6e3` | LOCK-2 REGISTRY §0.3, `:101,236`; `git status --porcelain` in each worktree (final seat's own read) |
| H8 | **A sound solver pin** | bbnf-lang (`Cargo.toml:37`) and parse-that's e-graph scheduler (`rust/egraph/Cargo.toml:18`) both pin 0.4.0 with the CH-69 over-prune; the moment an all-different constraint is added it is live. LW5.COMP-3 releases a new solver but no unit re-pins either consumer | `02965a56`; cure `58014efd` |
| H10 | **The AU-era and March techniques as an explicit carry list** | sibling skip, the visitor path at sonic parity, delimiter-driven scanning, regex-to-fused-scanner recognition, key/ident dispatch. All live in the root pipeline, which "can only delete" and is retired at LW4.COMP-1. The code is `crates/ir/src/passes/recognizers/` (23 modules); no unit ports it to the spine. Owner 04-17: "do not lose any optimization or implementation in this restructuring" | `3b8b757d5`, `c1e86ab32`; ruling X-1 |
| H13 | **msac-style cross-chunk refill** | "the one genuinely novel algorithmic lift beyond simdjson … unbuilt" | `19-dav1d…:193-216` |
| H14 | **The 05-13 question, answered by measurement** | The hinge for any automaton or assembly lane | [dig-owner-voice §2.2] |
| H15 | **Regex tier selection as a weighted CSP; the type-layer CSP** | Owner 04-07, 05-04 | [dig-csp REQ-13, REQ-14] |
| H16 | **Tuning, validation and visualisation of optimizer decisions** | Owner 04-12 | [dig-owner-voice §1.C] |
| H17 | **Records that still mislead** | The +8 % spot file; `GESTALT.md:58`; the `egraph_semantic.rs` docstring; ledger prose quoting megabits | [dig-egraph EG-20] |
| H18 | **Rule discovery with a VM oracle** | Sound idea, deleted unused. Optional; returns only with a consumer | `26f954696`, `b82f1439b` |
| H19 | **Amendment-9 units cannot keep a substrate alive** | The census checker reads PLAN.md's LW2–LW4 rows only; every amendment-9 id greps 0 there, and LW5 and later are outside the window. A substrate whose only consumer is one of those units can only be fated DELETE | `L:182`; `R:30`; `substrate_audit.rs:99` |
| H20 | **The lazy and eager Value API with path semantics (P5)** | Owner 06-10: "a unified path and direct to struct API is a first class requirement". Only a certification cell names it, and a LOSS satisfies that cell | `L:269`; `programme.md:489-492` |
| H21 | **The existing path facility** | `bbnf-path` (918 lines) and `bbnf-path-ts` (1,012) are the only built path API. Census default is delete unless `StructRegistry` gains a plan-side home, which no unit gives | `CC:13-14` |
| H22 | **The post-cut design crosswalk was never done** | PLAN was formed from truncated dispatch copies (`L:132`). TOPO-0 promised crosswalk rows for the cut text (`L:171`); none exist. Unhomed: programme IF-2 (no silent fallback, `Exhausted` certificate), W4 (certified passes), W8 (the `.S` assembly seam), W9 (the value planes, lazy `get` included), and `competition.md:173` ("PROVE patches land one at a time … each re-measured") | `designs/programme.md`, `competition.md` |
| H23 | **Shape mining as a stage** | Owner 04-12, 04-15, 05-04; README §6 pass 4. The spine has no miner stage, and the recognizer estate is quarry that "can only delete" | §5.3 |
| H24 | **DAP and source maps** | Nothing in sk-v27 names them; LW4.COMP-2 cuts the LSP off the VM that DAP steps, and `crates/ir` is deleted at LW7 | `P:205-206`; `CC` |
| H25 | **Landing units for PROVE-03, 06, 10 and 12** | 0 hits for them in the plan, order, amendments or rulings. CB-9's string/escape scan, keyword PHF and `vqtbl` kernels were dropped from LW4.PERF-1; the only prefilter-class unit (LW4.RX-3) may be dropped | `compiler.md:184`; `P:208,212` |

### 8.2 Narrowed: a unit exists and reads smaller than the requirement

| # | Item | The narrowing | By whom |
|---|---|---|---|
| N-a | E-graph and CSP as mandatory stages | Trial organs, "the simpler organ wins". OWN-1 repairs existence for these two only | Codex, then delegate. Owner opposed 11-11 |
| N-b | "Scalar over kernel", G-Q1 and O012 | LW5.COMP-2 (CB-13) issues keep/delete verdicts for CB-9, the kernel plane, under the undecided-means-simpler rule on a machine that reads NOISY; G-Q1 turns off any pass that does not move G-J1 or G-C1; programme O012 retires a CSP or e-graph pass that does not beat a direct worklist, outside OWN-1's I6 wording; PREREG:135 still tags the organ as the loser | Delegate and design text; not covered by OWN-1 |
| N-c | Widening rewrites and the seven categories | LW5.COMP-1 says "a sound rewrite set" | Plan text |
| N-d | Global CSP | Reduced to a `bounded_sum` primitive in LW5.COMP-3; the coupled solve and its witness grammar are unnamed | Amendment 9 |
| N-e | Solver completeness | Amendment 9 omits symmetry breaking, soft constraints, explanations and value ordering | Amendment 9 |
| N-f | Tape union | "One tape as a plane"; the five shapes, the consumers (LSP, DAP, path, visitor) and the falsifier beyond struct ≡ projection are unnamed | Amendment 9 |
| N-g | Hybrid Rust + ASM emitter | "`asm!` only where it beats intrinsics"; the grammar-to-ASM path dropped | Codex (sk-v26 R7); the rust-perf adjudication marked the ASM charter "optional" (U12, `rust-perf.md:246`); PLAN LW4.PERF-1 dropped programme W8's `.S` seam and roofline trigger at the dispatch cut |
| N-h | x86 abrogation | AVX2 and AVX-512 tiers kept, "never certifying" | Delegate (X-4) |
| N-i | Bespoke regex determinizer | `regex-automata` adopted, provisional | Delegate (OR-7 Route A) |
| N-j | Type system | First-order shape inference | Codex, adopted by X-1 |
| N-k | "No third IR" | OptIR introduced with no supersession cited | Uncited |
| N-l | "No TS IR" | TS reads `plan.json`. The owner's later words favour this; record it as a named supersession | Uncited |
| N-m | value.js parser floor | ≥ 10× retired; 3× / 2× unratified | Commission 08-03 |
| N-n | PROVE wins | "Re-measure PROVE-01..14 at HEAD". No unit lands PROVE-03, 06, 10 or 12 (H25); `competition.md:173`'s "PROVE patches land one at a time" has no unit | Programme design |
| N-o | WASM | A delete-unless-wins trial for the VM; the "WASM union" is open | Plan |
| N-q | Witnesses for suspected unsound rules | Only generic fuzz (LW3.COMP-4c) and three regex-tier tests (LW4.RX-2). Targeted refutations for `CommonSuffixFactor` (`Alt(a c, ab c)` on "abc"), `RepeatOfSingle`, `ConcatLiterals`, two halves of `UnionMergeAlt` and the HIR repetition merge have no born-RED test; random fuzz need not produce them | Order text |
| N-r | Perf gate on substrate change | LW3.CI-IAI-0/1: instruction count ±1 % on bbnf-lang PRs, `json_callgrind` only. No CSS, BBNF or Sheets bench, no parse-that kernel gate, and the baseline is the previous head (a slow drift passes), not the AU table | Order text |
| N-s | Checkasm bound to liveness | LW3.PERF-1b proves the stage-1 op is in the JSON and CSS plans. No unit asserts every registered kernel is reached on corpus bytes, or that the dispatch label names the variant that ran | Order text |
| N-t | Every JSON path above SOTA (owner 05-21, 51 rows) | G-J3 passes on "≥ 5/8 corpora" (`competition.md:225`) | Design adjudication; no owner ruling |
| N-u | Organ trials | Both are JSON-only (PREREG §6), and the CSP trial's greedy arm is the priority cascade | S5 COMP (LW2.COMP-1). Owner unconsulted |
| N-v | Per-component strategy COP (`csp_strategy`) | Not named by LW5.COMP-1; its only home is deleted at LW7; LW3 ports it only on a trial win | Plan text |
| N-w | One `propagate()` for FIRST/FOLLOW on the spine | LW3.COMP-7 builds them with no CSP and forbids a second computation; the CSP arrives at LW5 | Order text |

### 8.3 Rulings owed by the owner himself

Each is a place where an agent or the delegate chose for him.

- **O-1. Existence of both organs.** Confirm OWN-1 as his ruling and extend it to every deletion path listed in §5 (C1-4, I6, the CB-7/8 guards, CB-9, CB-13, CB-14, LW5.COMP-2, G-K1, G-Q1, O012, the PREREG §6 verdict rule), and to the shape-mining stage and the kernel slots. A loss means redesign.
- **O-2. x86.** His words are "no x86" (05-31) and "Full X86 abrogation" (07-10). X-4 keeps the tiers.
- **O-3. What the ASM "union" obliges.** An emitter able to emit assembly under the measured charter, or at least one live assembly kernel as a condition of done.
- **O-4. Regex determinizer.** Bespoke, or `regex-automata` (Route A).
- **O-5. Type system scope.** Full HM with CSP-backed choice, or first-order.
- **O-6. Amendment 9.** The fourteen later-wave units are PLANNED by the delegate. His ruling makes them law; §8.2 lists where their text is still too small. Either write the fourteen ids into PLAN.md or extend Z-32's window to LW7 (H19).
- **O-7. A quiet certification window.** Nothing certifies under load 10–40.
- **O-8. The early publish.** 14 crates went to crates.io at LW2 against PLAN:93.
- **O-9. The value.js parser floor.** 3× / 2×, or restore ≥ 10×.
- **O-10. A generated-size ceiling.** Whether any line-count or binary-size ceiling binds the solve, given "a larger binary is acceptable for now" (04-08).
- **O-11. The kernel speed threshold.** ≥ 3× the scalar oracle (G-K1) or ≥ 2.0× (CB-9).
- **O-12. DAP and source maps.** Kept on the spine, or let go with the root VM.
- **O-13. "Every path >SOTA".** His 05-21 pin, or "≥ 5/8 corpora".

### 8.4 Not traced to evidence (UNVERIFIED)

- Whether the regex-to-fused-scanner recogniser and the Eisel–Lemire code survive the move behind `rust/simd`.
- Where the PROVE-08 HM core lives today.
- Whether csc411 runs the owner's Python test suite against the Rust solver today.
- The exact line of the "≤ 1 s per grammar" ceiling in `restart/ARCHITECTURE.md` (read at about 1159–1161).
- Whether "per parse" in `3fee71a4c` meant runtime hit counts of the emitted forms; §5.2-13 reads it as compile-time firing.
- `~572 commits` in Era V; Route A 121/133 and Route B 114/133; R9's NEON bug and one-byte overread; the 04-07 date of the unified-pattern ruling (the memory file is undated). None was re-opened by a verification seat.
- The value.js constellation rows (C14) beyond `dig-plans` R11.
- The CSS-against-lightningcss record is contested between seats (4–7.5× "judged fair" in April; "NOT equal-work, NOT equal-depth" in June). No equal-depth cell has been run.
- Stage-1 throughput has three unreconciled readings (3.85–5.0, 8.6, 4.7–6.2 GB/s).
- The trial cell counts (14 for the e-graph, 7 for the CSP) are as the seats reported them.
- Any owner words before 2026-05-22 beyond prompts in `history.jsonl` and memory files: the transcripts are gone.


---

## 9. Corrections ruled by the final seat

Three verification seats filed corrections against the first synthesis: `verify-measured` (V1–V17), `verify-homeless` (A1–A4, B1–B15, D1–D6, E) and `verify-csp-egraph` (K1–K17, plus the gate rows now in §5.4). The final seat spot-checked the load-bearing ones by its own read-only reads (the three `bounded_sum` worktrees; `PLAN.md` hit counts for four amendment-9 ids; `compiler.md:32,182-190`; `rust-perf.md:131-132`; `bench/PREREG.md:92,135`; the recognizer file count; the fallback string at `csp_strategy/mod.rs:617`; both solver pins; the ref holding `d2e169794`). All of those held. The rest are accepted on the verifying seat's evidence.

| Ruling | Corrections | Note |
|---|---|---|
| **Applied as filed** | V1–V7, V9–V17; A1–A4; B1–B11, B13–B15; D1–D6; E; K1–K5, K7, K9–K14, K16, K17 | V1 and V2 change the history's headline: the AU peak was about half of sonic-rs on the same plane, and the collapse was far deeper than first stated |
| **Applied with a change** | V8 | Applied; R3's "standing" cell now also says parity with sonic-rs was not shown, since the two runs disagree on direction for twitter |
| | K6 | Applied, minus its fourth piece of evidence (the thin-LTO figure), which V4 downgrades to confounded |
| | K8, B12 | The two seats disagreed (two worktrees and 6 tests; three worktrees). My read: three worktrees, two variants. `-28` holds 168 lines with 7 tests; `-30` and `-32` hold a byte-identical 178 lines with 6 tests. So the first synthesis's "7/7" and K8's "6" are both right for different copies. H7 states all three |
| | K15 | Six of seven rows applied. Its row "the operator-chain and tail-call categories carry left recursion onto the spine" is **rejected as stated**: A1 shows the carrier is LW3.COMP-2. Row 10a says the categories consume that unit's tables |
| | K3 | Applied. Its explanation that the April one-pass behaviour was a scheduler bug (`6becbf8b5`) is the verifier's reading and is not asserted here; the owner's "SUBSUME" words carry the row |
| | K4, B1 | Both asked for the id H19. B1 keeps H19; shape mining is H23. Likewise B4 keeps N-q and K2's trial row is N-u |
| **Rejected** | none in whole | The two partial rejections are K15's left-recursion row and K6's thin-LTO evidence, above |

**Standing limits.** No seat built or ran anything, so every "M" is a figure read from a record in the archive, not a re-measurement. Records taken under load are directional. The owner's words before 2026-05-22 come from prompt history and memory files only.