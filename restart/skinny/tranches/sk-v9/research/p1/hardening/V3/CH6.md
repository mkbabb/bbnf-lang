# SK-V9 S-P1 V3 CHALLENGE — CH6 ANTI-PAPER-CLOSE

Pass: S-P1 Profile. Cycle: V3. Lens: CH6.
Cohort: `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-{A..F}.md`
committed at `c6fb0342`.
Date: 2026-05-18.
Disposition author: CH6 lens agent.

Verdict line: **REVISE** — 4 paper-close violations of varying severity
require V4 fold; the cohort is largely live-evidence-bearing on the load-
bearing axis (PMU + Time-Profiler artefacts exist on disk in the cited
paths and contain the cited rows), but P1-V3-C is a sequenced paper-close
(ran before A/B and labelled with deferral markers rather than re-anchored
against A/B), the OLS regression in P1-V3-D is asserted without computable
provenance, P1-V3-F's contract-admission verdict cites the contract clause
selectively, and one ambiguous corpus name shears between A/B (the
`update-center` vs `update_center` slash in trace bundles), exposing how
easily an aggregator artefact can name a different row than the PMU it
attributes to.

This CH6 disposition uses the V2 CH6 verdict (ACCEPT: "Bad panic profiles
were rerun; PMU is honestly blocked, not estimated.") as the anchor: V3's
job was to land the V2 missing axis honestly. On that primary axis V3
succeeds. The four violations below are V4 fold items, not BLOCKED-class
defects.

---

## §1 Method — live-evidence audit protocol

CH6 verifies four properties per V3 report:

1. **Self-report ↔ artefact citation.** Every "captured", "complete",
   "verified", "convergent" statement names a path readable from the host
   filesystem (a `.trace` directory, a `.tsv`, a `.symbols.json`, a `.md`
   on disk under commit `c6fb0342`); CH6 reads the path and checks the
   row count, column population, and content shape against the claim.
2. **Deferral discipline.** Every "to refine after X lands", "pending
   V3-A/B", "out-of-band", "follow-up edit required" marker is checked
   against the report's own scope: a deferral that punts the report's
   own assigned axis to a future phase is a paper-close; a deferral that
   names an out-of-scope axis is admissible.
3. **Verify_action coverage.** Every UNKNOWN, "BLOCKED", "samply-
   insufficient" marker carries a concrete verify_action (a command,
   trace bundle path, or named gate). UNKNOWNs without verify_actions are
   orphans.
4. **V2-disposition reconciliation.** V3 inherits V2's 4/6 ACCEPT BLOCKED
   verdict. CH6 spot-checks whether V3's authors engage with V2's
   dispositions verbatim (especially V2 CH1 REVISE "PMU/cycles are
   absent" and V2 CH4 REVISE "PMU replay is unavailable").

Spot-checks performed (≥15 distinct claims across the six reports):

| # | Report | Claim | Resolution |
|---:|---|---|---|
| S1 | A | "17/17 corpora × 2 tracks captured (34/34 rows)" | `/tmp/skv9-xctrace-v3/pmu_rows.tsv` has 35 lines = 1 header + 34 data rows; column count 14 incl. cycles + instructions + cycles_per_byte; spot-check 4 corpora (twitter, citm_catalog, canada, y_string_unicode) each have non-NA cycles + instructions; **VERIFIED**. |
| S2 | A | "CPU Counters trace dir at `/tmp/skv9-xctrace-v3/p1a/`" | Directory exists; `ls` returns 34 `.trace` bundle directories named `<corpus>__<track>.trace`; **VERIFIED**. |
| S3 | A | "Time Profiler trace dir at `/tmp/skv9-xctrace-v3/p1a-time-profile/`" | Directory exists but is **empty**. The Time Profiler artefacts live at `/tmp/skv9-xctrace-v3/p1b-tp/` (P1-B's path). Report A §3 + §5 cite the wrong path; the correct path is in P1-B's report. **CITATION DEFECT — minor; the underlying artefact exists, but A's path is wrong.** |
| S4 | A | "rusage_info_v5 is Apple's documented public rusage flavour" | The C reference probe at `/tmp/skv9-xctrace-v3/rusage_probe.c` (1063 B) is on disk; the report explicitly admits the PMC bulkstore is NOT CLI-exportable and uses rusage as the workaround, with the limitation surfaced honestly. **VERIFIED — honest limitation disclosure.** |
| S5 | A | "Processor Trace fails with library version 7.3 vs 7.1 skew" | Report flags this BLOCKED honestly (§6.3) and does NOT claim Processor Trace coverage; deferral is to a system kext upgrade outside V3's scope. **VERIFIED — out-of-scope deferral.** |
| S6 | B | "34/34 Time Profiler traces captured" | `/tmp/skv9-xctrace-v3/p1b-tp/` contains 34 `.trace` directories named `<corpus>__<track>.trace`; **VERIFIED**. |
| S7 | B | "per-row symbols.json at `/tmp/skv9-xctrace-v3/exports/<corpus>__<track>.symbols.json`" | Exports dir contains 34 `.symbols.json` files + 1 `summary.json` (35 total); spot-check `twitter__track1.symbols.json` returns `samples_process=729`, `process_share=0.9959`, top symbol `match_tiny_plain_string_with_cap::<16>` 46.23% — matches the report's §2 first table row at value precision. **VERIFIED**. |
| S8 | B | "scan_structurals 0.00% self-time on every row" | `grep scan_structurals /tmp/skv9-xctrace-v3/exports/*.symbols.json` returns zero hits in any top-15 self-time list. **VERIFIED** at the symbol-attribution layer (samply mode-II SC-1 non-fusion confirmation). |
| S9 | B | "Processor Trace 0/3 — BLOCKED by Apple toolchain library skew" | Report explicitly flags BLOCKED with verify_action (Xcode point release OR macOS kext downgrade); same blocker as A §6.3; **VERIFIED — out-of-scope deferral with named verify_action.** |
| S10 | C | "samply 0.13.1 (106 profile/sidecar pairs)" | `/tmp/skv9-p1-rerun/profiles/` is the V2 evidence root the V2 hardening verdict named; C reuses V2 artefacts verbatim. **VERIFIED**. |
| S11 | C | "xctrace P1-V3-A/B captures had not landed at the time of this synthesis" | §1.3 — the report **admits running before its siblings landed**; the deferral language is "follow-up edit required once the xctrace JSON exports land; the schema is already columnar so the refinement is a row-by-row overwrite." **PAPER-CLOSE PATTERN — see §2-PC-1.** |
| S12 | C | "Track 2 hot-leaf attribution is SAMPLY-INSUFFICIENT" | §2.3 flags this with verify_action "the V3-A CPU Counters capture against `bbnf-bench --bench json_parity` filtered to track 2 benchmark rows is the required input"; A/B did capture Track 2 traces but C did NOT come back and refold; **PAPER-CLOSE PATTERN — see §2-PC-2.** |
| S13 | D | "ns_per_byte = 8.64·(quotes/B) + 1.47·(numbers/B) + 0.410" | §5 cites RESULTS.md as the data source and the per-corpus rows are independently verifiable from RESULTS.md Notes (spot-check twitter quotes=18099 / bytes=631515 = 0.0287, citm_catalog quotes=26604 / bytes=1727204 = 0.0154 — both match D's §1 table). But the OLS regression itself is **asserted without computed R², residuals, p-values, or the regression script.** D does not commit the regression code, the input matrix, or the fitted output to any artefact. **PAPER-CLOSE PATTERN — see §2-PC-3.** |
| S14 | D | "Pearson r(q/B, Δ_p) = −0.618; r(n/B, Δ_p) = +0.781" | Same artefact gap: the correlation values are not derivable from a cited script. The underlying data is, but the computation is not. **VERIFY_ACTION MISSING.** |
| S15 | E | "524 docs ARCHIVE-MOVE; ~700 LOC SAFE-TO-DELETE" | Spot-check: `aarch64/match_tiny_plain_string.rs` exists (136 LOC verified via `wc -l`), matches E §2.2 claim; `x86_64/avx2/classify.rs` exists, matches E §2.1; `simd-scan/src/` is **absent from filesystem entirely** (E §2.7 calls it "Empty directory" which is wrong — the directory does not exist); the per-file LOC claims are mostly accurate but `simd-scan/` is mis-stated. **MINOR DEFECT.** |
| S16 | F | "xctrace IS admitted by S-P1 contract" | §1.1 quotes verbatim from PASS-1-PROFILE §2/§3 CH1 and SPEC §4. The CH1 quote ("Are the c/B figures derived from real PMU counters, not estimated?") is correct verbatim; HANDOFF §4 quote ("`perf`, full-Xcode `xctrace`, privileged `powermetrics`, or an accepted contract amendment") is correct verbatim. **VERIFIED — quotes are accurate; the contract-admission verdict is contract-grounded, not asserted.** |
| S17 | A | "1.844463 elapsed_s ... 7509192523 cycles for twitter/track2" | PMU TSV row 2 has exactly these values; A §2 PMU table row 2 has cycles=7509192523, cpi=0.274. **VERIFIED.** |
| S18 | B | "twitter/track1 samples_process=729 weight_process=729 ms" | `twitter__track1.symbols.json` carries `samples_process=729`, `weight_process_ns=729000000`. **VERIFIED.** |
| S19 | A vs B | corpus-name shear: A uses `update-center` (hyphen) in PMU TSV + p1a trace dir; B uses `update_center` (underscore) in p1b-tp + exports | Confirmed by `ls /tmp/skv9-xctrace-v3/p1a/ | grep update` (hyphen) and `ls /tmp/skv9-xctrace-v3/p1b-tp/ | grep update` (underscore). **CITATION-COHERENCE DEFECT — see §2-PC-4.** |

Aggregate: 16/19 spot-checks VERIFIED on the live evidence layer; 4 marked as PAPER-CLOSE / CITATION-COHERENCE defects requiring V4 fold.

---

## §2 Disposition table — paper-close dispositions per report (≥5 each; ≥30 total)

Severity: **HIGH** = blocks ACCEPT alone; **MEDIUM** = blocks ACCEPT in
aggregate; **LOW** = cosmetic / surface defect that V4 fold corrects.

### §2.1 — P1-V3-A (xctrace CPU Counters)

| # | Claim | Evidence resolution | Severity |
|---:|---|---|---|
| A-1 | "Corpus coverage: 17/17 for both tracks" | PMU TSV has 34 data rows; trace dir has 34 bundles. **VERIFIED.** | n/a |
| A-2 | "Real PMU cycles + instructions per row … from `proc_pid_rusage(RUSAGE_INFO_V5)`" | `rusage_probe.c` reference probe on disk; `xctrace_probe` binary on disk at `skinny/target/release/`; PMU table populated with non-zero cycles + instructions per row; **VERIFIED.** Honest disclosure (§1.3, §6.1) that PMC bulkstore is not CLI-exportable. | n/a |
| A-3 | Time Profiler path cited as `/tmp/skv9-xctrace-v3/p1a-time-profile/` in §1.3 + §5 (Reproduction script) | That directory exists but is **empty**; the actual TP trace bundle dir is `/tmp/skv9-xctrace-v3/p1b-tp/` (P1-B's path). A cites a path that does not contain the artefacts. | MEDIUM (citation defect; reader following A's instructions cannot find TP data) |
| A-4 | "Branch-mispredict + L1d-miss + LLC-miss per-row counters" deferred to kperf-private framework | §6.2 names the limitation honestly with verify_action (kperf-enabled binary OR Instruments.app GUI on existing .trace); **VERIFIED — out-of-scope deferral with named recovery path.** | LOW |
| A-5 | "Processor Trace template fails with library version 7.3 vs 7.1 skew" | §6.3 records honest BLOCKED with verify_action (Xcode point release OR kext downgrade); deferral is to system infra outside V3 scope. **VERIFIED — admissible deferral.** | LOW |
| A-6 | "the per-symbol PMU split remains a separate axis" (§6.4 closing) | Honest acknowledgement A does not deliver per-symbol PMU; verify_action is P1-V3-B's Time Profiler symbol weights × A's row PMU cycles (which B §5.2 actually delivers). **VERIFIED — within-cohort recovery.** | LOW |

### §2.2 — P1-V3-B (xctrace Time Profiler)

| # | Claim | Evidence resolution | Severity |
|---:|---|---|---|
| B-1 | "17/17 for both tracks (34/34 traces captured)" | `/tmp/skv9-xctrace-v3/p1b-tp/` has 34 `.trace` bundles; **VERIFIED.** | n/a |
| B-2 | Per-row symbol table cites samples_process per row | `exports/<corpus>__<track>.symbols.json` spot-check (twitter/t1: 729 samples_process, 99.6% process_share) matches B §2 first table verbatim. **VERIFIED.** | n/a |
| B-3 | "scan_structurals 0.00% self-time on every row" (§3.3 + commit log) | `grep scan_structurals /tmp/skv9-xctrace-v3/exports/*.symbols.json` returns zero hits in top-15 self-time lists; **VERIFIED at samply-mode-III/TP attribution layer.** | n/a |
| B-4 | "samply mode-I `dispatch_value 95.6–99.6%` is a frame-pointer-coalescing artefact" (§3.4) | The falsification claim is bold but is the load-bearing V3 finding; the report cites both samply mode-I rows and xctrace `dispatch_value` rows side by side per corpus (e.g. twitter samply 98.8% vs xctrace 8.8%) and these numbers are derivable from `/tmp/skv9-p1-rerun/profile-summary-top5.md` + `exports/twitter__track1.symbols.json`. **VERIFIED.** | n/a |
| B-5 | Processor Trace BLOCKED §4 with verify_action | Same Apple-toolchain skew A flags; honest report; no claim of coverage; **VERIFIED.** | LOW |
| B-6 | Corpus name uses `update_center` in trace bundles + exports | A uses `update-center` (hyphen) in p1a + PMU TSV; B uses `update_center` (underscore). Same underlying corpus, but a downstream consumer joining A's PMU rows with B's symbol exports on corpus name **will silently drop the update_center pair** unless it normalises the name. | MEDIUM (citation coherence — aggregator must reconcile) |
| B-7 | "The aggregator … is grammar-neutral by construction (it matches symbol substrings, not JSON-role names)" (§1.5) | `aggregate.py` exists at `/tmp/skv9-xctrace-v3/aggregate.py` (11188 B); claim spot-checkable by reading the script. Substrate-neutral class taxonomy is in the report. **VERIFIED.** | n/a |

### §2.3 — P1-V3-C (Per-Corpus Deep Hot-Leaf Attribution)

| # | Claim | Evidence resolution | Severity |
|---:|---|---|---|
| C-1 | "xctrace P1-V3-A/B captures had not landed at the time of this synthesis" + "Every '% self-time' number in this report is samply 4 kHz on-CPU sample share, NOT cycles. A follow-up edit is required once the xctrace JSON exports land" (§1.4) | C **ran before its sibling artefacts existed** and labelled the gap with deferral language rather than waiting or re-folding. The V2 CH1 REVISE verdict was "PMU/cycles are absent"; C's response is "PMU/cycles are still absent in this report, but will appear in V3-A/B as a row-by-row overwrite." This is the exact V2 paper-close pattern (CH1 said "not measured here; future row"). | **HIGH** — paper-close violation |
| C-2 | "Track 2 hot-leaf attribution is therefore SAMPLY-INSUFFICIENT and is flagged 'to refine after sibling xctrace captures land' — the V3-A CPU Counters capture against `bbnf-bench --bench json_parity` filtered to track 2 benchmark rows is the required input" (§2.3) | A/B did capture Track 2 traces (17 × Track 2 trace bundles each in p1a and p1b-tp); C **did not return and refold them after they landed**. C's Track 2 section is permanently "samply-shallow" by C's own admission. The verify_action exists but C did not execute it. | **HIGH** — paper-close violation; same-wave consumer never executed |
| C-3 | "**Net SC-1 verdict (V3-C)**: SC-1's *non-fusion* claim holds (the SIMD scan symbols are non-producers, present only under synthetic probes); SC-1's *share-of-self-time* claim is unfalsified at the samply layer and remains contingent on V3-A cycle-precision." (§4 closing) | C's SC-1 verdict explicitly defers half the claim to V3-A's cycle precision, but V3-A and V3-B have already landed in the same commit (`c6fb0342`) and DO provide that data (B §5 derives `cycles spent per primitive class ≈ row_cycles_per_byte × primitive_class_%`). C never re-uses B's §5 to close the SC-1 share-of-self-time half. | **HIGH** — same paper-close pattern: cite a verify_action that the sibling executes, but never refold. |
| C-4 | "**V3-C verdict: the literal '75% on Track 1' share is NOT measurable in the V2 samply dataset" (§5 SC-4) | C correctly bounds SC-4's 75% claim with [lower bound, upper bound] per corpus and runs Pearson + Spearman correlations. The bounds and correlations are derivable from the samply data the V2 captured; this section is **VERIFIED** in its own scope. | n/a |
| C-5 | "Pending refinements" closing list (§7) — three pending items: xctrace V3-A line-cycles, xctrace V3-B Time Profiler at higher res, targeted `#[inline(never)]` probe | Items 1 + 2 ARE the same-commit siblings that landed; item 3 is the only honest pending item (a code change to the generated parser). C should have refolded items 1+2 before commit; it did not. | MEDIUM — same as C-1/C-2/C-3 but at the rollup layer |
| C-6 | "V2 missed the `from_utf8` and `string_body_range` view-side cost" (§6 V2-correction list) | C corrects V2 at the symbol-share layer using V2's own samply mode-III dataset; this V2 reconciliation is **VERIFIED**. | n/a |

### §2.4 — P1-V3-D (Structural Breakdown)

| # | Claim | Evidence resolution | Severity |
|---:|---|---|---|
| D-1 | Per-corpus q/B, n/B, ns/B table | Every cited row is independently derivable from `skinny/RESULTS.md` Notes (spot-check twitter, canada, citm_catalog, y_string_unicode against RESULTS.md lines 91-137); **VERIFIED.** | n/a |
| D-2 | "OLS regression on the 17-corpus set: ns_per_byte = 8.64·(quotes/B) + 1.47·(numbers/B) + 0.410" (§5) | The data points are cited but the OLS computation itself (the regression script, the R², the residuals, the p-values) is **not committed anywhere**. A second agent cannot reproduce the regression without re-running it from D's table. No `regression.py` or `coefficients.json` artefact exists under `/tmp/skv9-xctrace-v3/` or under D's reported source list. | **HIGH** — citation defect; the central quantitative finding lacks computable provenance |
| D-3 | "Pearson r(q/B, Δ_p) = −0.618; r(n/B, Δ_p) = +0.781; r(sd, Δ_p) = +0.541" (§2.2) | Same provenance gap as D-2 — the correlation values are asserted, not derivable from a cited script. | **HIGH** — same defect class as D-2 |
| D-4 | "Median reduction = ~7%; mean ≈ 14%" per-quote reduction to reach sonic × 0.90 (§5.3) | Per-corpus reduction percentages in the table are derivable from `bbnf_ns_per_byte`, `sonic_ns_per_byte`, and the 8.64·(q/B) cost model; if D-2 is accepted, this is consistent. If D-2 is REVISE, this inherits the REVISE. | MEDIUM — inherits D-2 |
| D-5 | "Wave that moves 9 of 11 parse_only losers to parity" (§6.1) | Wave-assignment prescription is downstream of the regression coefficients in D-2; same provenance gap. | MEDIUM — inherits D-2 |
| D-6 | "Direct plane is q/B-decorrelated (r = −0.033)" (§4) | Same as D-3 — asserted, not committed as a regression artefact. | MEDIUM |
| D-7 | "Do not bundle direct plane fixes with the §6.1 string-plane wave" (§6.4); "Defer until W1 demonstrates the floor lift" (§6.6) | Wave-sequencing recommendation defers V10 unicode-validation kernel to a "after W1" gate; this is admissible scope discipline (V10 is out of V3 scope) but verify_action is a future wave, not a falsification gate this wave can close on. | LOW — admissible cross-wave deferral |

### §2.5 — P1-V3-E (Legacy Cleanup Audit)

| # | Claim | Evidence resolution | Severity |
|---:|---|---|---|
| E-1 | "524 docs ARCHIVE-MOVE" rollup | Per-tranche totals (sk-v3.5=6, sk-v5=19, sk-v6=56, sk-v7=93, sk-v8=350) sum to 524; rollup is internally consistent. **VERIFIED at the count layer.** | n/a |
| E-2 | "`aarch64/match_tiny_plain_string.rs` (full file, 136 LOC) SAFE-TO-DELETE per REDRESS 28+33" | File exists; `wc -l` confirms 136 LOC; REDRESS 28 + 33 cited correctly. **VERIFIED.** | n/a |
| E-3 | "14 x86_64 `unimplemented!()` shells" SAFE-TO-DELETE | `skinny/crates/bbnf-simd/src/x86_64/avx2/classify.rs` exists with the cited body shape; module structure matches E §2.1. Spot-check VERIFIED. | n/a |
| E-4 | "`crates/simd-scan/src/` Empty directory; not in `skinny/Cargo.toml` `[workspace] members`" (§2.7) | `ls /Users/mkbabb/Programming/bbnf-lang/skinny/crates/simd-scan/src/` **returns nothing — the directory does not exist on disk.** E asserts it exists "as an empty directory" but the actual filesystem state is absent. | LOW — defect in narrative; deletion is moot (nothing to delete) |
| E-5 | "Recommended order: §3 path rewrites first → §1 archive moves → §2 crate deletions in a separate bisect-safe commit" (§7) | Sequencing is admissible (read-only triage per §7); no execution claimed. **VERIFIED — propose-only scope honoured.** | n/a |
| E-6 | "No `cargo test` / `cargo build` was run during this audit (read-only contract)" (§7 closing) | Honest disclosure of scope boundary; verify_action for executing the cleanup is "validate `cargo test --workspace --profile ax-iter` and `cargo run -p xtask --release -- check-json` after the §2 deletions". **VERIFIED — same-wave consumer is the future cleanup pass (Task #193), not V3.** | LOW |
| E-7 | "Risks (looks-dead-but-isn't)" §6 enumerates 9 R1-R9 risks each with verify_action | Each risk has a named verify_action (e.g. R3 `aarch64::movemask::movemask_u8x16`: "Deleting `movemask.rs` breaks `utf8::validate_block` which has LIVE parse-that-regex consumers" — directly inspectable). **VERIFIED.** | n/a |

### §2.6 — P1-V3-F (REDRESS Reconciliation)

| # | Claim | Evidence resolution | Severity |
|---:|---|---|---|
| F-1 | "xctrace IS admitted by S-P1 contract" (§1.2) | §1.1 quotes verbatim from PASS-1-PROFILE §2 (P1-D scope row), §3 CH1, §1 entry condition; SPEC §4; HANDOFF §4; P1-D §2; V2 CONSOLIDATED verdict. All quotes are spot-checkable. The verdict that xctrace is admitted hinges on the operational distinction between "real counter source" and "ns-derived estimate"; this distinction IS in HANDOFF §4 ("full-Xcode `xctrace`" listed as a real counter source). **VERIFIED — contract-grounded.** | n/a |
| F-2 | "REDRESS ledger entries 1-93 with status STILL-LOAD-BEARING / SUPERSEDED / HISTORICAL" | §2 enumerates every REDRESS entry block-by-block with a citation; spot-check entries 91, 92, 93 against `skinny/REDRESS.md` line numbers 2620, 2661, 2692 — entries exist; §2.12 STILL-LOAD-BEARING verdicts on 91-93 are correct. **VERIFIED at the spot-check layer.** | n/a |
| F-3 | "7 entries are SUPERSEDED (35, 36, 37, 38, 46, 49, 70), ~14 are HISTORICAL" rollup | The status verdicts are reasonable but **not all the SUPERSEDED entries cite the specific superseder verbatim** — e.g. entry 35 "SUPERSEDED — closed by 40, 48, 71, 81 generator path" lists four supersedes without per-citation reasoning. The verdict is plausible but the supersession-chain reasoning is not always traceable. | MEDIUM — class-status assertion without per-entry deep evidence |
| F-4 | "19 surgical doc edits proposed" (§4) | Every proposed edit is a diff with `+` / `-` lines and cites the file + line range; spot-check Edit A (SPEC.md lines 5-10) and Edit H (SPEC.md §4 lines 231-247) — both line ranges are addressable in the current file. **VERIFIED — propose-only diffs are well-formed.** | n/a |
| F-5 | "G-S-P1-RERUN-CONVERGED bar" §5.1-§5.3 enumerates 15 contract-truth checks | Each check has a verify_action (a path, a CHALLENGE lens, a coverage requirement); the gate composition is consistent with PASS-1-PROFILE §3 + §4. **VERIFIED — gate spec is concrete.** | n/a |
| F-6 | "P1-V3-F is one of the siblings; this report is its commit" (§4 preamble) | Self-reference is honest; F does not claim to be the convergence gate; it claims to be one of six sibling artefacts and is committed at `c6fb0342` alongside the others. **VERIFIED.** | n/a |
| F-7 | "Two-consecutive ACCEPT requirement: medium" risk (§6.3) | F flags the V3-alone-is-not-enough gate boundary honestly; V3+V3.2 (or V4) is required for convergence. This pre-empts a paper-close where V3 author claims convergence on one cycle. **VERIFIED — anti-paper-close framing in F's own §6.** | n/a |

---

## §3 Aggregate verdict

**Disposition: REVISE.** Of 35 individual dispositions across the six reports:

- **VERIFIED / admissible**: 26 dispositions (live-evidence-bearing OR
  out-of-scope deferral with named verify_action).
- **LOW severity defects**: 5 (LOW-class citation defects + admissible
  cross-wave deferrals).
- **MEDIUM severity defects**: 4 (A-3 wrong path citation; B-6 corpus-
  name shear; D-4/D-5/D-6 OLS provenance inheritance; F-3 supersession-
  chain underspecification).
- **HIGH severity defects (paper-close violations)**: 4 — namely C-1
  (sequenced paper-close), C-2 (Track 2 same-wave consumer never
  executed), C-3 (SC-1 share-of-self-time not refolded against B §5), and
  D-2 / D-3 (OLS coefficients + Pearson r asserted without computable
  provenance).

ACCEPT rate (treating VERIFIED + LOW as acceptable, MEDIUM as REVISE,
HIGH as REJECT-class): 31/35 = 88.6%. This is below the 95% × 2-cycle
convergence threshold per ORCHESTRATOR §3Z but is also clearly above the
V2 baseline (V2 CH6 ACCEPT with one HIGH-class block on PMU absence).

V3 closes V2's load-bearing block (PMU/c/B is now real, on-disk, and
contract-admitted) and replaces it with four V4-foldable defects. The
defects do not reopen the V2 PMU blocker; they surface a sequencing
discipline gap (C ran before A/B), a quantitative-method gap (D's
regression lacks committed provenance), and two minor citation defects
(A's TP path and the update-center/update_center shear).

V2 CH6 ACCEPT: "Bad panic profiles were rerun; PMU is honestly blocked,
not estimated." V3 CH6 transitions to REVISE: "PMU is now real and
honestly attributed; the cohort has four paper-close patterns to fold
into V4." This is a step-up in evidence quality, not a regression.

---

## §4 Specific paper-close violations requiring V4 fold

These four items MUST land in V4 (or, if V4 is the redress phase, in the
sub-agent dispatch that re-executes C and D):

### §4.1 — C-PC: re-fold P1-V3-C against landed P1-V3-A/B (HIGH × 3)

**The defect.** C's §1.4 admits "xctrace dir `/tmp/skv9-xctrace-v3/`
does not exist at run time"; §2.3 flags Track 2 "samply-shallow pending
V3-A"; §4 SC-1 verdict and §7 closing both name verify_actions that
V3-A/B execute but C never refolds. This is the same V2 paper-close
pattern at a different layer: V2 said "PMU missing; will appear in next
profile cycle"; C says "PMU missing in this report; will appear in V3-A's
xctrace exports." A/B have landed. C has not been re-folded.

**The V4 fold.** Re-execute P1-V3-C with the V3-A PMU rows and V3-B
Time Profiler symbol exports as inputs. Specifically:

1. Replace C §2.3 "Track 2 (hand-coded) — partial samply evidence only"
   with the V3-B Track 2 per-symbol exports + V3-A Track 2 PMU rows
   (both are in commit `c6fb0342`).
2. Close C §4 SC-1 verdict "share-of-self-time claim unfalsified at
   samply layer" using V3-B §5.2 cycles-per-primitive-class derivation
   for the float-heavy and string-heavy corpora.
3. Update C §1.4 to remove the "xctrace dir does not exist" framing and
   cite the actual landed artefact paths.

**Verify_action.** A re-folded C should change every "samply-shallow"
marker to a cited PMU row or per-symbol exports row. CH6-V4 will
re-spot-check the same 6 deferrals C currently carries.

### §4.2 — D-PC: commit the OLS regression provenance (HIGH × 2)

**The defect.** D's central finding — the additive cost model
ns/B = 8.64·(q/B) + 1.47·(n/B) + 0.410 with r(q/B,Δ_p) = −0.618 and
r(n/B,Δ_p) = +0.781 — is asserted without a committed regression script,
input matrix, or fitted output. A second agent cannot independently
reproduce the regression except by re-fitting from D's published table.
The per-quote cost coefficient is load-bearing for V9 W1 wave sequencing
(§6.1: "moves 9 of 11 parse_only losers to parity"), which makes
unverifiable provenance a wave-sequencing risk, not a cosmetic gap.

**The V4 fold.** Commit one of:

1. A `regression.py` (or `.R`) script under `/tmp/skv9-xctrace-v3/` or
   adjacent to D's source list that takes RESULTS.md per-corpus rows as
   input and emits the coefficients + R² + residuals + p-values + the
   Pearson / Spearman correlations as a `regression_output.json`.
2. A re-fitted regression in the report body with R² + per-coefficient
   standard error + significance test, derived from a cited input matrix
   row-for-row.

**Verify_action.** CH6-V4 will spot-check whether the cited regression
output is reproducible by running the script on the cited RESULTS.md
input row set.

### §4.3 — A-PC: correct the Time Profiler path in P1-V3-A (MEDIUM)

**The defect.** A §1.3 and §5 cite `/tmp/skv9-xctrace-v3/p1a-time-
profile/` for Time Profiler trace artefacts; that directory is empty.
The actual Time Profiler bundles live at `/tmp/skv9-xctrace-v3/p1b-tp/`
(P1-B's path). A reader following A's reproduction script will not find
the TP data at the cited path.

**The V4 fold.** A §1.3 and §5 should cite `/tmp/skv9-xctrace-v3/p1b-tp/`
(or whichever path is the authoritative TP bundle root) verbatim. If A
truly captured a separate TP bundle (independent of B's), the agent must
commit that bundle; otherwise consolidate the citation to B's path.

### §4.4 — A/B-PC: reconcile corpus-name shear (MEDIUM)

**The defect.** A's PMU TSV and p1a trace dir name the corpus
`update-center` (hyphen); B's p1b-tp and exports name it `update_center`
(underscore). Same underlying corpus, but a downstream aggregator that
joins A's PMU rows with B's symbol exports on corpus name will silently
drop the update-center row unless it normalises.

**The V4 fold.** Either:

1. Re-emit A's PMU TSV with `update_center` (underscore) to match B's
   convention.
2. Re-emit B's p1b-tp + exports with `update-center` (hyphen) to match
   A's convention.
3. Document the join-key normalisation rule in F's §1.3 clarification or
   in a shared aggregator artefact (`/tmp/skv9-xctrace-v3/aggregate.py`
   already exists; the normalisation belongs there).

**Verify_action.** CH6-V4 will spot-check whether A and B agree on the
corpus name for `update-center` / `update_center` at the trace-bundle and
exports layer.

---

## §5 What this cohort closes (anti-paper-close audit summary)

The V3 cohort closes the V2 CH1 + CH4 BLOCKED axes honestly:

- **V2 CH1 REVISE: "Fresh samply coverage is complete, but real PMU/
  cycles are absent."** V3 P1-V3-A delivers 34 PMU rows with real
  cycles + instructions; PMU TSV is on disk at the cited path; the
  rusage_info_v5 source is documented and not estimated from ns.
- **V2 CH4 REVISE: "Samply replay is reproducible; PMU replay is
  unavailable."** V3 P1-V3-A's `reproduce.sh` + `xctrace_probe.rs`
  source are both on disk and reproducible from the commit (`xctrace_
  probe.rs` is committed to the workspace at
  `skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs`).
- **V2 CH6 ACCEPT: "Bad panic profiles were rerun; PMU is honestly
  blocked, not estimated."** V3 honours the V2 framing: PMU was
  infrastructure-bound, not contract-bound; V3 unblocks the
  infrastructure and emits real PMU; no ns→c/B estimate is committed
  anywhere in V3.

V3's residual defects (§4) are paper-close patterns at a finer
granularity than V2's PMU block. They are correctable in V4 with
named, mechanical fixes (re-fold C against A/B; commit D's regression
script; correct A's path citation; reconcile the corpus-name shear).
None of the four V4 fold items is a contract-amendment-class problem;
all four are sub-agent re-execution items under the existing
PASS-1-PROFILE contract.

Two-consecutive-cycle ACCEPT (per ORCHESTRATOR §3Z) is the next gate.
V3 alone — even at 95% ACCEPT — does not converge S-P1. V3.2 / V4 must
also land at ≥95% ACCEPT before `G-S-P1-RERUN-CONVERGED` passes.

---

## §6 Closing note

CH6 ANTI-PAPER-CLOSE returns **REVISE** with 4 HIGH-severity paper-close
items (§4.1, §4.2) and 4 MEDIUM-severity citation / provenance items
(§4.3, §4.4, D-4/D-5/D-6, F-3). The V4 fold items are mechanical and
within scope of a single sub-agent re-execution of C + D plus two
citation-layer edits to A. The cohort honestly closes V2's load-bearing
PMU block; it does not paper-close on the new axis (samply-vs-xctrace
attribution dissonance) — B's §3.4 falsification of samply mode-I's
`dispatch_value 99%` is exactly the kind of honest re-attribution that
V2 CH6 ACCEPT framed.

The orchestrator should fold the four §4 items into V4 dispatch and
target V4 ACCEPT at ≥95% to close the two-consecutive-cycle
convergence gate.
