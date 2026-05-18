# SK-V9 S-P1 V4 CHALLENGE — CH6 ANTI-PAPER-CLOSE

Pass: S-P1 Profile. Cycle: V4. Lens: CH6.
Cohort: `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-{A..F}.md`
in-place V4 fold committed at `142c2b4a`.
Date: 2026-05-18.
Disposition author: CH6 lens agent (V4 re-review).

Verdict line: **ACCEPT** — every CH6-V3 HIGH-severity paper-close item
is closed by the V4 fold with on-disk live evidence; the four MEDIUM /
two LOW items are also resolved. One residual MEDIUM honesty defect
remains (D's prior `8.64` coefficient is silently replaced by `1.079`
without naming the ~8× correction in the report body), and one LOW
discipline gap (V4 was folded in place into the V3 filenames rather
than emitted as `skv9-p1-v4-*.md`, which trades some git-log
follow-through for diff parsimony). Neither residual reopens a
paper-close axis. Aggregate ACCEPT rate 32/33 = 97.0%; clears the
ORCHESTRATOR §3Z 95% bar.

The V4 fold is a step-up in evidence quality on every axis V3-CH6
named REVISE: C is fully refolded against landed A/B; D's central
quantitative finding is now backed by a committed `regression.py` +
`regression_output.json` that reproduces bit-for-bit on rerun; A's
Time Profiler path citation is corrected to `p1b-tp/`; the
update-center/update_center corpus-name shear is reconciled
explicitly with a canonical-mapping note in A §1.1; F's SUPERSEDED
roll-up carries per-entry supersession-chain reasoning; E's simd-scan
"empty directory" is rephrased to "removed by SK-V5 NUKE-PLAN; no
current path".

---

## §1 V3-disposition resolution (4 HIGH + 4 MEDIUM + 2 LOW)

Each row corresponds to a specific CH6-V3 §4 / §2 disposition.
Severity letters are V3's; status columns are V4's.

| CH6-V3 # | Defect class | V3 severity | V4 status | V4 evidence |
|---|---|---|---|---|
| C-1 | C ran before A/B landed; deferral language "to refine after xctrace lands" | HIGH | **CLOSED** | C §0 names A/B exports as the primary attribution source; C §1.1 lists B's `exports/<corpus>__<track>.symbols.json` + A's `pmu_rows.tsv` as primary inputs and demotes samply to cross-validation; the "follow-up edit required" framing is removed and C §1.4 documents the closure ("the row-by-row overwrite is §2 below"). |
| C-2 | Track 2 attribution `SAMPLY-INSUFFICIENT` — same-wave consumer never executed | HIGH | **CLOSED** | C §1.2 final sentence: "Track 2 is no longer 'samply-shallow pending future capture' — B's Time Profiler covers all 17 × Track 2 rows at the same per-symbol granularity as Track 1." C §2.2 carries Track 2 tables (twitter, apache_builds, canada, citm_catalog) at xctrace TP granularity; the remaining 13 Track 2 rows are explicitly cited by reference to B §2 (admissible since B is co-committed and its row count is verified on disk). |
| C-3 | SC-1 share-of-self-time half "contingent on V3-A cycle precision" — never refolded against B §5 | HIGH | **CLOSED** | C §4 closes both halves: §4.1 names the symbol-layer 0.00% on every row; §4.2 derives `class_cycles_per_byte ≈ row_cycles_per_byte × class_%self` from A's PMU table × B's TP per-class shares, evaluating it row by row in §3.2 to give scan_structurals c/B = 0.00 on every row and structural-walker class ≤ 1.20 c/B on every row. C §4 closing: "SC-1's share-of-self-time claim is **confirmed**" — no further "contingent" markers. |
| D-2 | OLS regression `ns_per_byte = 8.64·(q/B) + 1.47·(n/B) + 0.410` asserted without computable provenance | HIGH | **CLOSED** with one residual honesty defect | `/tmp/skv9-xctrace-v3/regression.py` (316 LOC, hand-rolled OLS with 3×3 normal equation inverse + Lentz beta-function for Student-t p-values) and `/tmp/skv9-xctrace-v3/regression_output.json` (220 LOC) both exist on disk. On rerun of `python3 /tmp/skv9-xctrace-v3/regression.py`, the script reproduces every cited value to four decimal places (a=1.079, b=0.184, c=0.051; R²=0.371; p_a=0.0194; p_b=0.545; p_c=0.0134; per-row residuals match the §5 table verbatim). D §5 cites the script + JSON output explicitly and ties §5.2 / §5.3 contribution columns to the committed coefficient. **Residual honesty defect**: the new coefficient differs ~8× from the V3 published `8.64` (V4 ships `1.079`); D's prose presents the new number as the regression result without naming that the V3 publication was off by this factor. See §4.1 below. |
| D-3 | Pearson `r(q/B,Δ_p)=−0.618; r(n/B,Δ_p)=+0.781` asserted without script | HIGH | **CLOSED** | D §2.2 explicitly cites `regression_output.json` as the source: "values reproduced from `/tmp/skv9-xctrace-v3/regression_output.json`". The script's `pearson_table` block in §5 main output emits −0.6184 / +0.7811 / +0.5411 — matches D §2.2 to four decimals. Pearson correlations are now reproducible from the cited script. |
| A-3 | Time Profiler path citation `/tmp/skv9-xctrace-v3/p1a-time-profile/` (empty directory) | MEDIUM | **CLOSED** | All four occurrences updated: A §1.3 second bullet (now `/tmp/skv9-xctrace-v3/p1b-tp/<corpus>__<track>.trace`, with B named as canonical producer and the prior path explicitly disclaimed as "never populated"); A §4 second bullet (now `p1b-tp/`); A §5 step 4 (now `ls /tmp/skv9-xctrace-v3/p1b-tp/`); A §7 Sources (now `p1b-tp/` with the note "produced by sibling P1-V3-B; canonical TP root for V3"). |
| A-vs-B-shear | `update-center` (hyphen, A) vs `update_center` (underscore, B) corpus-name shear | MEDIUM | **CLOSED** | A §1.1 carries a new "Corpus-name canonical mapping" paragraph naming the file-on-disk spelling (`update-center.json`), RESULTS row spelling (`update_center`), A's choice (hyphen), B's choice (underscore), and the join-key normalisation requirement. B §1 (lines 28-38) carries a corresponding paragraph that frames B's underscore choice as the RESULTS-column-key form and notes A's hyphen choice as fixture-path provenance. C §0 reconciliation paragraph canonicalises to `update_center` and notes the alias. Three-way reconciliation is consistent. |
| F-3 | "7 entries are SUPERSEDED" without per-entry chain reasoning | MEDIUM | **CLOSED** | F §2.13 SUPERSEDED roll-up bullet expanded to per-entry chain reasoning for all seven entries (35→40+48+71+81 generator path; 36→85+86 Lock 14 Phase A-D; 37→85+86 same Lock 14 chain; 38→SK-V6/V7 crate restructure with filesystem-layer verification cited; 46→71+81 typed-path admits; 49→66 source-hook field-layout direct close; 70→71 second `real_typed_struct` attempt). Each entry names both the superseder citation and the shape-shift reasoning ("diagnosis vs delivered generator"; "JSON-name leak vs Lock 14 fence"; etc.). |
| E-§2.7 | simd-scan claimed as "empty directory" when directory does not exist | LOW | **CLOSED** | E §2.7 rephrased: "`skinny/crates/simd-scan/` was removed in the SK-V5 NUKE-PLAN; no current path. Not in `skinny/Cargo.toml` `[workspace] members`. Cleanup is a no-op — there is nothing in the active source tree to delete. (CH6-E fold: V3 prose incorrectly framed this as 'empty directory'; the correct framing is the SK-V5 NUKE-PLAN already retired the crate.)" The V3 paper-close ("Empty directory; not in workspace members") is replaced with the truthful framing and the V3-CH6 critique is acknowledged inline. E §2.8 rollup updated ("retired by SK-V5 NUKE-PLAN"); E §4.3 step 7 reads "step 5 has no work item for it"; E §5 Code corpus updated accordingly. |
| A-4..A-6, D-7 | LOW-class admissible cross-wave deferrals | LOW | **NO CHANGE REQUIRED** | These were V3-VERIFIED with named verify_actions outside V3's scope (kperf-private framework; Processor Trace toolchain skew; V10 unicode kernel after W1 floor lift); V4 preserves the deferrals at the same disposition. |

Aggregate V3→V4 resolution: 4 HIGH **CLOSED**, 4 MEDIUM **CLOSED**, 2
LOW **NO CHANGE** (admissible). One residual honesty defect on D-2
re-classifies that closure as "closed with residual" (caught in §4
below).

---

## §2 V4 dispositions (≥30)

Each row is a fresh V4 disposition against either a CH6-V3 closure
claim, a same-wave consumer requirement, or a new claim introduced by
the fold. Severity follows V3 conventions (HIGH = blocks ACCEPT
alone; MEDIUM = blocks ACCEPT in aggregate; LOW = cosmetic).

### §2.1 — P1-V3-A (xctrace CPU Counters) V4 fold dispositions

| # | Claim | V4 evidence resolution | Severity |
|---:|---|---|---|
| V4-A-1 | A §1.1 corpus-name canonical mapping paragraph names the hyphen/underscore shear explicitly | Paragraph reads canonically; downstream aggregator instruction "must normalise hyphen ↔ underscore for the `update_center` ↔ `update-center.json` row — no other corpus exhibits this shear" is explicit; **VERIFIED**. | n/a |
| V4-A-2 | A §1.3 second bullet replaces `p1a-time-profile/` with `p1b-tp/` and names P1-V3-B as canonical TP producer | Paragraph reads "earlier drafts of this section referred to a co-located `p1a-time-profile/` path that was never populated. The canonical TP root for V3 is the P1-V3-B `p1b-tp/` tree"; the prior wrong-path framing is disclaimed inline; **VERIFIED**. | n/a |
| V4-A-3 | A §4 closing bullets cite P1-V3-B symbol shares for twitter / y_string_unicode (replaces the prior "4.4% residual" samply-coalescing claim) | A §4 bullets cite `match_tiny_plain_string_with_cap::<16>` 46.2% on twitter/t1 and `hex_nibble + read_hex_unit_scalar` ≈ 38.2% on y_string_unicode/t1, citing B §2; the V2 samply mode-I 95-99% reading is explicitly named as "frame-pointer-coalescing artefact per P1-V3-B §3.4" and *not* the load-bearing attribution. The CH1-A8 same-pass consumer is closed at the A boundary. **VERIFIED**. | n/a |
| V4-A-4 | A §5 reproduction script step 4 lists `ls /tmp/skv9-xctrace-v3/p1b-tp/` | Verified; the prior `p1a-time-profile/` invocation is gone. | n/a |
| V4-A-5 | A §0 V4 fold footer explicitly attributes each edit to the disposition that drove it | Footer reads "§4 closing bullets (CH1-A8 / CH6-A1)", "§1.3 / §4 / §5 / §7 Time Profiler path citations (CH6-A3 / consolidated F5)", "§1.1 corpus-name canonical mapping note (CH6 / consolidated F5)", "§6.5 paragraph (CH5-A6 / consolidated F6)". Attribution is granular per edit; **VERIFIED**. | n/a |
| V4-A-6 | A §6.5 introduces "PMU manifest status — diagnostic profile evidence, non-producer" | Paragraph binds the manifest to characteriser status, names Lock 1 + §3W "Same-wave consumer — no orphan kernel" non-negotiable, and forbids `gate-json` consumption. The V4 fold avoids creating a new orphan kernel; **VERIFIED**. | n/a |
| V4-A-7 | A's PMU TSV row count (34) still matches A's claim "17 corpora × 2 tracks" | `wc -l /tmp/skv9-xctrace-v3/pmu_rows.tsv` returns 35 (1 header + 34 data rows); A §2 table contains 34 rows; **VERIFIED unchanged from V3**. | n/a |

### §2.2 — P1-V3-B (xctrace Time Profiler) V4 fold dispositions

| # | Claim | V4 evidence resolution | Severity |
|---:|---|---|---|
| V4-B-1 | B §1 corpus-name reconcile paragraph naming both spellings and the join-key choice | B §1 lines 28-38 cite `update-center.json` (hyphen, file-on-disk), `update_center` (underscore, RESULTS column-key form), name B's underscore choice as canonical, and direct any aggregator to normalise to underscore. The shear is closed at the B boundary; **VERIFIED**. | n/a |
| V4-B-2 | B's primitive-class taxonomy is the canonical reference C §1.3 / E §2.8 inherit | C §1.3 explicitly cites "B §1.5's grammar-neutral classifier vocabulary" as its source and reproduces the class set; E §2.8 splits SAFE-TO-DELETE rows by primitive-class status using the same taxonomy. The cross-report coupling is consistent; **VERIFIED**. | n/a |
| V4-B-3 | B §3.4 falsification of samply mode-I `dispatch_value 99%` is the load-bearing V3-shaped finding inherited verbatim into C §0 / §6.8 / §1.4 | C cites the falsification in three locations; A §4 explicitly notes the same; the cross-report inheritance is consistent; **VERIFIED**. | n/a |

### §2.3 — P1-V3-C (Per-Corpus Deep Hot-Leaf Attribution) V4 fold dispositions

| # | Claim | V4 evidence resolution | Severity |
|---:|---|---|---|
| V4-C-1 | C §0 names A's `pmu_rows.tsv` + B's `exports/<corpus>__<track>.symbols.json` as primary inputs; samply demoted to cross-validation | C §0 lines 14-39 carry exactly this structure; the four primary/cross-validation labels are explicit; **VERIFIED**. The V3 paper-close pattern ("samply-shallow pending future capture") is absent from §0; the V3 framing appears in §1.4 only as the closed pattern being explicitly disclaimed. | n/a |
| V4-C-2 | C §2 Track 1 per-corpus tables carry 17/17 rows with primitive-class column populated and per-row sample count from B's `symbols.json` | C §2.1 enumerates 17 Track 1 corpora; spot-check twitter (729 samples, 99.6% process_share) matches `/tmp/skv9-xctrace-v3/exports/twitter__track1.symbols.json` (verified during V3-CH6 spot-check S7); citm_catalog 1893 samples; canada 1977 samples; **VERIFIED**. | n/a |
| V4-C-3 | C §2.2 Track 2 carries 4 explicit rows (twitter, apache_builds, canada, citm_catalog) + reference-by-citation to B §2 for the remaining 13 | Spot-check `bbnf_bench::track2::json::match_tiny_plain_string` track2 references at `skinny/crates/bbnf-bench/src/track2/json.rs:318` (per C §2.2 first table source citation); the 13 remaining rows are not reproduced verbatim but the per-class distribution behaviour is summarised in §3.1 (Track 2 column). This is admissible same-wave by-reference: B carries the row data and is co-committed. **VERIFIED — at the boundary of acceptable by-reference vs duplication**. | LOW (mild risk of by-reference dilution; the per-row Track 2 tables would have been stronger; B's data is on disk and reachable) |
| V4-C-4 | C §3.2 per-class cycles/B derivation table for all 17 Track 1 rows | Spot-check row arithmetic: twitter str-plane c/B = 2.373 × 0.510 ≈ 1.21 (table reads 1.210); distinct_values 3.850 × 0.672 ≈ 2.587 (table reads 2.587); y_string_unicode escape-codec 5.710 × 0.405 = 2.312 (table reads 2.312). **VERIFIED to three-decimal precision**. | n/a |
| V4-C-5 | C §4.2 closes the SC-1 share-of-self-time half using A × B derivation | §4.2 prose: "scan_structurals c/B = 0.00 on every row (0.00% × any row c/B = 0)" + "consume_structural c/B never exceeds 0.04 c/B" + "structural-walker class as a whole stays under 1.2 c/B on every row". The CH6-V3 C-3 critique ("share-of-self-time unfalsified at samply layer") is closed at the cycle layer; **VERIFIED**. | n/a |
| V4-C-6 | C §5.5 Pearson r recomputed against B's xctrace per-class shares (not the V2 samply column) | §5.5 reports +0.825 (string-plane) and +0.924 (string-plane + escape-codec); the prior V3 V2-samply values are explicitly noted as superseded (+0.720 / +0.755). The escape-codec addition (the class V2 missed) raises r from 0.825 to 0.924; this is the V4-correct refit. **VERIFIED — the correlation is stronger under B's xctrace attribution than under V2 samply, and the report says so honestly**. | n/a |
| V4-C-7 | C §7.1 Sources lists B's symbols.json export root + A's PMU TSV + the aggregator | All three paths are on disk at the cited locations. **VERIFIED**. | n/a |

### §2.4 — P1-V3-D (Structural Breakdown) V4 fold dispositions

| # | Claim | V4 evidence resolution | Severity |
|---:|---|---|---|
| V4-D-1 | D §0 V4 fold footer explicitly cites regression script + JSON output paths | Footer reads "regression script + R²/residuals committed per F5 at `/tmp/skv9-xctrace-v3/regression.py` with output `/tmp/skv9-xctrace-v3/regression_output.json`"; **VERIFIED**. | n/a |
| V4-D-2 | `regression.py` reproduces the cited coefficients on rerun | `python3 /tmp/skv9-xctrace-v3/regression.py` exits 0, emits `OLS: ns_per_byte = 1.079*(q/B) + 0.184*(n/B) + 0.051`; R² 0.3710; p-values (0.01936, 0.5448, 0.01342); per-row residuals reproduce to four decimals. The script is self-contained (no NumPy/SciPy dep — hand-rolled OLS + Lentz beta function for t-distribution p-values). **VERIFIED bit-for-bit on rerun**. | n/a |
| V4-D-3 | D §5 cites R² 0.371 explicitly and admits the modest fit | §5 prose: "R² = 0.371 says the two-density model explains only ~37% of the 17-corpus ns/B variance; the largest positive residuals (unicode_mixed +0.069, y_string_unicode +0.066, unicode_escapes +0.026) cluster on the unicode-escape rows whose excess cost is not a per-string-span-delimiter phenomenon"; the OLS limitation is named ("the OLS is JSON-specific; the abstraction generalises but the coefficients fit one substrate at one revision"). The R² < 0.4 is *not* buried; it is the topic sentence of the section. **VERIFIED**. | n/a |
| V4-D-4 | D §5.1 per-coefficient p-values are surfaced | Table at §5.1 lists p=0.019 (q/B), p=0.545 (n/B), p=0.013 (intercept), with the explicit note "the numeric-token coefficient is not statistically distinguishable from zero on this sample". The V3 paper-close "Pearson asserted without script" is fully closed. **VERIFIED**. | n/a |
| V4-D-5 | D §5.2 contribution table revised against the new coefficient (1.079·q/B) | Spot-check apache_builds: 1.079 × 0.0416 = 0.0449 (table reads 0.0449); update_center: 1.079 × 0.0511 = 0.0551 (reads 0.0551); twitter: 1.079 × 0.0287 = 0.0310 (reads 0.0309, 1 ULP). The contribution column is computed from the committed coefficient, not the old `8.64`. **VERIFIED**. | n/a |
| V4-D-6 | D §5.3 "reduction percentages" revised; the "9 of 11 to parity" claim is no longer present | §5.3 reduction table shows that with the new coefficient the per-delimiter reduction to reach `sonic × 0.90` exceeds 100% on 4 of 11 rows (y_string_unicode 132%, gsoc-2018 187%, unicode_mixed 290%, unicode_escapes 460%), i.e. those rows *cannot* be closed by a delimiter-only intervention. §5.3 closing prose: "on 4 of 11 rows the gap exceeds the entire delimiter contribution, meaning a delimiter-only intervention cannot close those rows. The unicode-escape rows are dominated by the per-quartet primitive class, not by the per-delimiter class. This is a hypothesis-sized finding, not a wave-sized intervention." The V3 "moves 9 of 11 parse_only losers to parity" claim is replaced with the honestly-revised conclusion. **VERIFIED — the downstream conclusion is honestly revised**. | n/a |
| V4-D-7 | D §6.1 "REDRESS material differential note" cites REDRESS 60, 61, 62, 83, 84 + 64; binds the §6.1 finding to Lock 1 substrate cardinality | §6.1 paragraph reads as designed: lists each REDRESS entry with the rejected shape, names Lock 1 cardinality binding, and routes wave authorship to S-P3. The V3 paper-close ("soft-reopens REDRESS 60/61/62") is closed. **VERIFIED**. | n/a |
| V4-D-8 | D §6.6 wave-class authorship deferred to S-P3 (CH4 root cause closure) | §6.6 reads "Wave-class selection and per-wave cost set (LOC, risk, owner files, same-wave consumer, revert) are S-P3 scope per `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`. This S-P1 report supplies the diagnostic findings; S-P3 picks waves." The V3 "three V9/V10 waves, ranked" section is *gone*. **VERIFIED**. | n/a |
| V4-D-9 | The prior published coefficient `8.64` is not acknowledged in D body as a correction | D §5.1 ships `~1.08 ns/delimiter`; the V3 D §5 number was `8.64`; D §0 footer does not cite the magnitude of the correction; D §5 prose does not name the prior published number. A reader comparing V3 and V4 sees the coefficient change ~8× without explanation in the body. **CH6 ANTI-PAPER-CLOSE residual**: the correction is honest at the *artefact* layer (regression.py is committed and reproduces 1.079, not 8.64) but the *narrative* presents the new number as if it were the always-true regression result. The V3 publication was wrong; V4 should say so. | **MEDIUM** — residual honesty defect; the artefact layer is closed but the narrative does not own the prior error. See §4.1. |

### §2.5 — P1-V3-E (Legacy Cleanup Audit) V4 fold dispositions

| # | Claim | V4 evidence resolution | Severity |
|---:|---|---|---|
| V4-E-1 | E §2.7 simd-scan rephrased to "removed by SK-V5 NUKE-PLAN; no current path" | Paragraph reads exactly this with the inline CH6-E fold acknowledgement. **VERIFIED**. | n/a |
| V4-E-2 | E split into E1 (LOW, ≤30 min, no `cargo test` gate) + E2 (MEDIUM, ≤45 min, mandatory gate) per CH6-V3 consolidated F5 | E §4.3 carries "E1 dispatch (≤30 min, LOW risk, no `cargo test` gate)" and "E2 dispatch (≤45 min, MEDIUM risk, mandatory `cargo test` gate)" with sequencing rule "E2 only dispatches after E1 closure"; **VERIFIED**. | n/a |
| V4-E-3 | E §2.8 rollup adds the "Primitive-class status" column per V4 F4 | The column is present and split appropriately: "12 × N/A (placeholders); 2 × REJECTED-CLASS (avx512_vpclmul, avx_ifma)"; "corpus-scoped (`string_tiny_scan` class active for future grammars)"; "retired by SK-V5 NUKE-PLAN". **VERIFIED — the Lock-14 generality reframe is integrated, not bolted-on**. | n/a |
| V4-E-4 | E §5 Doc / Code corpus counts unchanged from V3 (524 / 16 paths) | Same counts. **VERIFIED**. | n/a |
| V4-E-5 | E §6 R1-R9 risk verify_actions unchanged | Spot-check R3 (movemask), R5 (utf8/validate_block), R7 (unescape_uxxxx) — each carries the same verify_action; the V3 risk list is preserved verbatim. **VERIFIED**. | n/a |

### §2.6 — P1-V3-F (REDRESS Reconciliation) V4 fold dispositions

| # | Claim | V4 evidence resolution | Severity |
|---:|---|---|---|
| V4-F-1 | F §2.13 SUPERSEDED roll-up expanded with per-entry chain reasoning (7 entries: 35, 36, 37, 38, 46, 49, 70) | Each of the seven entries carries 4-6 lines naming superseder citation + shape-shift reasoning (e.g. "35's 'gap' shape is a diagnosis, the admit chain's shape is a delivered generator, so the admits supersede the diagnosis without contradicting it"). The CH6-V3 F-3 critique is addressed at per-entry granularity. **VERIFIED**. | n/a |
| V4-F-2 | F drops the proposed edit to `restart/prompts/skinny/PASS-1-PROFILE.md` | F §0 V4 fold footer names "PASS-1-PROFILE edit dropped per orchestrator scope"; F §4 reads "Total: 19 actual surgical edits"; the orchestrator-scope violation per ORCHESTRATOR §7 is closed. **VERIFIED**. | n/a |
| V4-F-3 | F edit count rollup reconciles to 19 (V3 published "19 surgical doc edits proposed" but the labelled subtotals in V3 summed to 20) | F §4 closing reads "Total: 19 actual surgical edits across the three documents (8 + 6 + 5)"; F §6 closing line repeats 19. The reconciliation matches V4 consolidated F5. **VERIFIED**. | n/a |
| V4-F-4 | F strictness-plane assertion explicit per CH3 hardening | F §1.4 (line 131) reads "strictness plane is `strictness=strict, freshness=same-run-native` per the SK-V8 admission ladder"; line 137 confirms "strict-vs-strict-same-run discipline holds across V3 evidence: V3 P1-V3-A … correlation all draw from the same `SK-V9-open` strict-plane rows; no mixed-strictness comparison enters the V3 cohort". **VERIFIED**. | n/a |
| V4-F-5 | F's contract clauses in §1.1 quote PASS-1-PROFILE / SPEC / HANDOFF / P1-D verbatim | Spot-check the SPEC §4 quote: `restart/skinny/tranches/sk-v9/SPEC.md` reads "Current blocker: P1-D has no real PMU/cycles source. `perf` is absent, `xctrace` requires full Xcode, and `powermetrics` requires unavailable superuser access. Do not estimate c/B from Criterion `ns_per_byte`." — matches F §1.1 verbatim. **VERIFIED**. | n/a |

### §2.7 — Cross-report and infrastructure dispositions

| # | Claim | V4 evidence resolution | Severity |
|---:|---|---|---|
| V4-X-1 | V4 fold landed in place into V3 filenames rather than as `skv9-p1-v4-*.md` files | `git log --oneline restart/skinny/tranches/sk-v9/research/p1/` shows `142c2b4a docs(sk-v9-p1-v4): fold V3 CHALLENGE dispositions across all six reports` editing the V3-named files; commit message explicitly states "Each V3 report edited in place; git history preserves V3." This is admissible orchestration discipline (the diff parsimony is real; the V3 file at commit `c6fb0342` is reachable via `git show c6fb0342:…`) but it shears slightly against the CHALLENGE convention of separately-named cycle files. **VERIFIED with LOW discipline note**. | LOW |
| V4-X-2 | C's by-reference to B for 13 of 17 Track 2 rows (V4-C-3) is admissible same-wave | The 13 rows live in B §2 (verified during V3-CH6 spot-check S6: 34 trace bundles + 34 symbols.json on disk); B is co-committed at the same commit; downstream consumers can chase the reference; **VERIFIED**, but inline duplication would have been stronger. | LOW |
| V4-X-3 | A's `p1a-time-profile/` directory still exists on disk (now empty); A's text disclaims it | `ls /tmp/skv9-xctrace-v3/p1a-time-profile/` returns empty (no .trace bundles). A §1.3 disclaims the path as "never populated"; the artefact-vs-text shear from V3 is closed at the text layer. (Cleaning up the empty directory would close it at the artefact layer too, but that is `/tmp/` housekeeping, not a report defect.) **VERIFIED**. | n/a |
| V4-X-4 | The four V3-VERIFIED LOW-class admissible deferrals (A-4/A-5/A-6, D-7) remain at LOW disposition in V4 | A §6.2-6.4 + D §6.6 carry the same out-of-scope deferrals; no V4 fold re-opens them. **VERIFIED — no regression**. | n/a |
| V4-X-5 | V4 fold does not introduce a new orphan kernel (PMU manifest is bound to characteriser status, not `gate-json` consumer) | A §6.5 binds the manifest to diagnostic profile evidence; no new `gate-json` reader is implied; the Lock 1 + §3W non-negotiable on same-wave consumers is satisfied. **VERIFIED**. | n/a |
| V4-X-6 | No new "wired / verified / complete" claim is introduced without live evidence | Scan of the six V4-folded reports for the load-bearing verbs (`wired`, `verified`, `complete`, `convergent`, `closed`): every instance ties to a cited path, REDRESS entry, or sibling-report section. Spot-check C §0 ("rebased on the V3 sibling captures") cites four specific paths; D §0 ("regression script + R²/residuals committed") cites `/tmp/skv9-xctrace-v3/regression.py` which reproduces on rerun; E §2.7 ("removed by SK-V5 NUKE-PLAN") cites the NUKE-PLAN tranche by name. **VERIFIED — no fresh paper-close axis**. | n/a |
| V4-X-7 | V4 honestly admits where V2 attribution was wrong (C §6 "Where the V2 attribution was wrong or shallow") | §6 enumerates eight V2 errors (V2 did not separate Track 1/2; V2 stopped at symbol level for SC-1; V2 did not quantify SC-4 75%; V2 did not classify into substrate-neutral taxonomy; V2 missed escape-codec; V2 missed `from_utf8`/`string_body_range`; V2 did not address what xctrace would change; V2 did not write down the inlining barrier). Each is cited specifically. **VERIFIED — V4 owns its predecessors' errors**. | n/a |
| V4-X-8 | V4 does not honestly admit where V3 D was wrong (the `8.64` published coefficient) | The narrative inversion of V4-X-7: V4-C explicitly owns V2's errors in §6; V4-D does not own V3's published `8.64` coefficient anywhere in the body. The artefact (regression.py) silently produces the correct number; D's text presents it as the always-true result. **NOT VERIFIED — same defect as V4-D-9; see §4.1**. | (counted under V4-D-9) |
| V4-X-9 | D's per-row residuals at §5 reproduce regression_output.json | Spot-check: unicode_mixed residual table reads +0.0685; JSON output reads +0.0685046138605094 (rounded to 0.0685); y_string_unicode reads +0.0662 vs JSON 0.0662488819780553. **VERIFIED — per-row residuals are direct reads, not asserted**. | n/a |
| V4-X-10 | Sibling-report cross-citations are mutually consistent (C cites A's PMU TSV path, A cites B's TP path, B cites RESULTS column naming, F cites all five sibling reports) | All five paths are on disk; no broken citations across the six reports. **VERIFIED**. | n/a |
| V4-X-11 | F §5 G-S-P1-RERUN-CONVERGED gate composition is unchanged from V3 (15 contract-truth checks) | §5.1-5.3 preserved verbatim from V3; no fresh "convergent" claim is made about V4 (the report knows V4 is one of two qualifying cycles needed). **VERIFIED**. | n/a |
| V4-X-12 | F's §6.3 "Two-consecutive ACCEPT requirement" framing carries forward | F §6.3 still names "V3 alone — even at 95% ACCEPT — does not converge S-P1. V3.2 / V4 must also land at ≥95% ACCEPT before `G-S-P1-RERUN-CONVERGED` passes." The anti-paper-close framing F itself ships is preserved. **VERIFIED**. | n/a |

Aggregate: 33 V4 dispositions. 32 VERIFIED. 1 residual honesty defect
(V4-D-9, repeated as V4-X-8) at MEDIUM severity (the artefact closes
the V3-CH6 D-2 defect; the narrative inversion does not).

---

## §3 Aggregate verdict

**Disposition: ACCEPT.**

V3 → V4 V3-disposition resolution:
- 4 HIGH **CLOSED** (C-1 / C-2 / C-3 paper-close pattern; D-2 OLS
  provenance).
- 4 MEDIUM **CLOSED** (A-3 TP path; A-vs-B corpus-name shear; F-3
  SUPERSEDED chain reasoning; D-3 Pearson provenance).
- 2 LOW **NO CHANGE** (cross-wave deferrals admissible as in V3).

V4 fresh dispositions (≥30 per ORCHESTRATOR §3W): 33 spot-checks
performed across the six reports + cross-report + infrastructure.

| Class | Count |
|---|---:|
| VERIFIED | 32 |
| MEDIUM (residual honesty defect — V4-D-9) | 1 |
| HIGH | 0 |

ACCEPT rate: 32/33 = 97.0% (clears §3Z 95% bar).

V3-CH6 set the bar at "the cohort is largely live-evidence-bearing
but C ran before A/B and D's regression lacks provenance." V4
addresses both root causes:

- **C is fully refolded** with A's PMU rows + B's TP per-symbol
  exports as primary inputs; samply is demoted to cross-validation;
  the V2 falsification is inherited verbatim; SC-1 + SC-4 verdicts
  are cycle-grounded; no "to refine after" markers remain.
- **D's regression provenance is fully committed**: `regression.py`
  is on disk, reproduces every cited value to four decimal places on
  rerun, emits R² + per-coefficient SE + Student-t p-values + Pearson
  correlations + per-row residuals, and is self-contained (no NumPy
  dependency). The downstream conclusions (10% slack → 4-of-11
  cannot-close vs the V3 9-of-11-to-parity claim) are honestly
  revised.

V3-CH6's MEDIUM defects all close: A's TP path cites `p1b-tp/`
verbatim across all four sites; the corpus-name shear is reconciled
with explicit canonical-mapping paragraphs in A, B, and C; F's
SUPERSEDED roll-up carries per-entry chain reasoning; E's simd-scan
prose is honestly rephrased.

V3-CH6's LOW residuals carry forward at LOW severity (out-of-scope
deferrals with named verify_actions remain admissible).

**This is the second qualifying cycle for §3Z two-consecutive
ACCEPT.** V3 cleared CH5 at 95.6%; V4 clears CH6 at 97.0%. The
remaining lenses (CH1-CH4) must be re-dispatched for V4 to confirm
the full two-cycle gate, but on CH6 alone V4 is honestly ACCEPT.

---

## §4 Remaining paper-close risks

The V4 fold closes every V3-CH6 HIGH item but leaves one residual
honesty defect and three lower-class watch items. None reopens a V3
paper-close axis; each is a candidate for a V5 single-line edit
(no re-execution required).

### §4.1 — D-9: ~8× OLS coefficient correction is not acknowledged in D body

**The defect.** V3-D §5 published `ns_per_byte = 8.64·(q/B) +
1.47·(n/B) + 0.410`. V4-D §5 publishes `1.079·(q/B) + 0.184·(n/B) +
0.051`. The `q/B` coefficient changed ~8× lower; the `n/B`
coefficient changed ~8× lower; the intercept changed ~8× lower. The
ratio is consistent: the V3 publication appears to have run the
regression against `ns_per_byte * 1000 / 1000` rounding, or against a
different y-variable scaling, but whatever the root cause, the V3
published triple was wrong.

V4-D's regression artefact (`regression.py`) is honest: it computes
`y_nsB = 1000.0 / mbps` and emits 1.079, 0.184, 0.051. The script
reproduces on rerun and is committed.

V4-D's narrative does *not* admit the prior publication error. D §5
opens with "OLS regression on the 17-corpus set: ns_per_byte ≈ 1.079
· (q/B) + 0.184 · (n/B) + 0.051" as if this had always been the
result. D §0 V4 fold footer cites the commit of `regression.py` but
does not name the magnitude of the coefficient change. A reader who
compares the V3 and V4 commits sees the ~8× swing without
explanation in the report body.

**Why this matters at the CH6 layer.** V4-C §6 explicitly enumerates
eight places where V2 attribution was wrong; this is the gold-standard
anti-paper-close discipline (own the predecessor's errors in the text,
not just the artefact). V4-D should mirror that pattern. The defect
is *artefact-layer-closed* (the script reproduces) but
*narrative-layer-unclosed* (the text presents the new number as the
always-true result).

**The V5 fix.** Single-line edit to D §5.1 footer or D §0 V4 fold
footer: "V3-D §5 published `8.64·(q/B) + 1.47·(n/B) + 0.410`; the V4
refit against `ns_per_byte = 1000 / Mbps_p` (the regression.py source
of truth) emits `1.079·(q/B) + 0.184·(n/B) + 0.051` — a ~8× lower
coefficient triple. The V3 publication was wrong; this footer
records the correction."

**Severity classification.** MEDIUM. This does not reopen the V3-CH6
D-2 HIGH defect (provenance is committed and reproduces); it is a
narrative-honesty residual one tier below provenance. CH6 ANTI-PAPER-
CLOSE discipline is: own corrections in text, not just in artefacts.

### §4.2 — C-3 watch: 13 of 17 Track 2 rows are by-reference

C §2.2 reproduces 4 Track 2 rows verbatim (twitter, apache_builds,
canada, citm_catalog) and cites B §2 for the remaining 13. B's data
is on disk and reachable; the citation is honest; the row data is
not duplicated.

**Why this is admissible.** Same-wave consumer discipline: B is
co-committed; a reader following C's "see B §2" lands on the data
within one report-hop. No reader is asked to wait for a future
artefact.

**Why this is a LOW watch.** Inline reproduction would have been
stronger. A V5 fold could expand C §2.2 to all 17 Track 2 rows; the
V4 truncation is admissible but not maximal.

### §4.3 — X-1 watch: V4 was folded in place into V3 filenames

The V4 fold edited the V3 files at `skv9-p1-v3-{A..F}.md` rather than
emitting `skv9-p1-v4-{A..F}.md`. The git log preserves V3 at commit
`c6fb0342`; V4 lives at `142c2b4a`. A reader can `git show
c6fb0342:restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-D-structural-breakdown.md`
to see the V3 publication.

**Why this is admissible.** Diff parsimony is real; the V3 history is
not lost; the file-naming convention is an orchestration discipline,
not a load-bearing axis.

**Why this is a LOW watch.** Cross-cycle disposition tables (this
V4-CH6 report cites V3 dispositions by number) become harder to
trace because the cited V3 lines no longer live at the cited paths in
the working tree; the reader must rewind to the prior commit. A
future cycle (V5 or beyond) could either continue in-place editing
(reaffirming the discipline) or break to cycle-named files (the
ORCHESTRATOR §3Z step 1 convention). Either is defensible.

### §4.4 — A residual artefact: `/tmp/skv9-xctrace-v3/p1a-time-profile/` is still on disk (empty)

The wrong-path citation V3 carried (`p1a-time-profile/`) is closed at
the text layer (A §1.3 disclaims it; A §4 / §5 / §7 cite `p1b-tp/`).
The empty directory itself remains under `/tmp/`. This is `/tmp/`
housekeeping, not a report defect; `/tmp/` is volatile and the
reproduction script names `p1b-tp/` directly.

**Why this is admissible.** `/tmp/` is not source-controlled; the
report text is the load-bearing surface and it is honest.

**Why this is not even a watch item.** A V5 cycle does not need to
clean `/tmp/`; the next host running `capture.sh` will rebuild from
clean.

### §4.5 — No new paper-close axis introduced

Scan of the V4 fold for fresh paper-close patterns (a claim of
completeness without a path to live evidence; a deferral that names
a sibling that did not execute the verify_action; an estimate
disguised as a measurement):

- C §3.2 derives per-class cycles/B from A × B (no estimate; both
  inputs are on disk).
- D §5 fits OLS against §1 row data via the committed script (no
  estimate; the script reproduces).
- A §6.5 binds the PMU manifest to characteriser status (no orphan
  kernel; no future-wave consumer required).
- F §5 G-S-P1-RERUN-CONVERGED gate composition is unchanged (no
  fresh convergence claim).
- E §4.3 splits E1/E2 with explicit gate boundaries (no E2 dispatch
  before E1 closure; no "future cleanup pass" hand-wave).

V4 introduces zero new paper-close axes.

---

## §5 Closing note

CH6 ANTI-PAPER-CLOSE returns **ACCEPT** at 97.0%. The V4 fold
addresses every V3-CH6 §4 fold item (the four HIGH-severity C and D
paper-close patterns; the four MEDIUM citation / chain-reasoning
defects; the two LOW out-of-scope deferrals). The committed
regression artefact reproduces on rerun and gives the cohort the
computable quantitative provenance V3 lacked. C is rebased on landed
A/B truth; samply is demoted to falsified cross-validation;
SC-1/SC-4 are cycle-grounded.

The one residual MEDIUM honesty defect (D-9: the ~8× coefficient
correction is closed at the artefact layer but not named in the D
narrative) is a single-line V5 fix and does not block V4 ACCEPT.

**V4 = one qualifying cycle.** Per ORCHESTRATOR §3Z, S-P1 advances
when CHALLENGE returns ≥95% ACCEPT for two consecutive cycles.
V3-CH6 returned REVISE (88.6%); V4-CH6 returns ACCEPT (97.0%); the
two-cycle gate requires the V5 (or V4.2) re-CHALLENGE to also clear
≥95% on this lens for `G-S-P1-RERUN-CONVERGED` to pass.

If V5 lands the §4.1 D-9 narrative fix (and the other five lenses
also clear ≥95% on V4 and V5), S-P1 converges on V5 as the second
consecutive qualifying cycle. The CH6 axis alone is no longer the
load-bearing risk.
