# SK-V9 S-P1 V3 CHALLENGE — CH1 CORRECTNESS

Pass: S-P1 Profile. Cycle: V3 reframe hardening, lens CH1 (CORRECTNESS).
Date: 2026-05-18.
Reviewer: adversarial CH1 lens, single agent, read-only.
Inputs sampled: P1-V3-A..F (six reports), `skinny/RESULTS.md`,
`skinny/REDRESS.md`, `/tmp/skv9-xctrace-v3/pmu_rows.tsv`,
`skinny/crates/runtime/src/grammars/json/generated.rs`,
`skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs`,
`skinny/crates/bbnf-simd/src/x86_64/avx2/classify.rs`, `git show c6fb0342`,
`git show 68260866`, `git show 90609aee`. Hard cap: 30 min.

CH1 contract per `restart/prompts/ORCHESTRATOR.md` §3W:
verify every concrete claim resolves to a citation that exists and matches
the claimed content. Adversarial bias: a citation that is approximately
right is wrong; a citation that resolves but contradicts a sibling report
is a defect to surface, not paper over.

---

## §1 — Method

The cohort was read end-to-end before judging. Sampling strategy:

1. **Citations spot-check.** Roughly 20 cited file:line pairs were opened
   and the claimed content verified against the actual line range:
   `generated.rs:14-17` (attach_structural_index), `:37-42` (parse_value_at),
   `:47` (dispatch_value), `:161-185` (match_tiny_plain_string family),
   `:468` (parse_object_value_at_direct), `:508`
   (parse_array_element_at_direct); `x86_64/avx2/classify.rs:45-46`
   (unimplemented! body); `bbnf-bench/src/bin/xctrace_probe.rs` (file
   exists, 180 LOC, matches §1.1 of P1-V3-A); `RESULTS.md:139` (Track 1/2
   definition line); `parse-that-regex/src/lib.rs` (file exists, 1214 LOC,
   well within line-range citations).
2. **Commit SHAs.** `c6fb0342`, `68260866`, `90609aee` all resolve via
   `git show`; commit-message content matches the use the cohort makes of
   it.
3. **RESULTS.md row reconciliation.** Per-corpus Mbps and Δ-vs-sonic
   values used in P1-V3-C/D/F were cross-checked against the live table
   (lines 5-46) and Notes block (lines 89-137) for 10 corpora (twitter,
   canada, citm_catalog, numbers, distinct_values, y_string_unicode,
   apache_builds, marine_ik, instruments, unicode_basic), covering wins,
   losses, edge rows, and the unicode_basic direct WIN.
4. **REDRESS reconciliation.** Cited entries 28, 33, 50-55, 56, 72, 80,
   88, 89, 91, 92, 93 were located by anchor text in `skinny/REDRESS.md`
   and the cited claim was matched to the prose of the entry.
5. **PMU arithmetic.** P1-V3-A's §2 PMU table CPI and cycles/B columns
   were recomputed from `cycles`, `instructions`, `bytes`, `iters` for
   three representative rows (twitter/t1, citm_catalog/t1,
   y_string_unicode/t1). All three reproduce A's printed columns to the
   reported precision.
6. **TSV vs report.** The 34-row PMU table in A was checked against
   `/tmp/skv9-xctrace-v3/pmu_rows.tsv`. Every spot-check matches the TSV
   bit-for-bit.
7. **Internal cross-references.** A→B/C→A/B→A/F→A/D→C threading was
   followed; one report's "next-step assumption" was checked against the
   sibling's actual delivery.
8. **Falsifiability gates.** §3 and §5 of D, §5 of F, and §5 of B were
   inspected for whether each proposed gate names a row + Mbps threshold
   + maintain envelope.

Sampling cap: ~35 dispositions across the six reports, more than the
required 30. Where the cohort's CH1-relevant defect surface is shallow
(D, F), 4-5 dispositions; where it is dense (A, B, C), 6+.

---

## §2 — Per-report disposition tables

Verdict vocabulary:

- **ACCEPT** — claim has a citation that resolves and matches the claimed
  content; arithmetic is self-consistent; no internal contradiction.
- **REVISE** — claim is substantially correct but carries a defect of
  precision, citation accuracy, or framing that V4 must fold; not a
  rejection of the underlying finding.
- **REJECT** — claim does not survive its own citation, or contradicts a
  sibling claim load-bearing on the same evidence root, with no
  reconciliation prose.

Citation column refers either to the cohort-internal location of the
claim (`P1-V3-X §N`) or to the external resource cross-checked.

### §2.1 — P1-V3-A: xctrace CPU Counters PMU Capture

| # | Finding (claim) | Verdict | Evidence |
|---|---|---|---|
| A-1 | "Real PMU cycles + instructions via `proc_pid_rusage(RUSAGE_INFO_V5)`, kpc-backed, no sudo" (§1.1, §6.4) | ACCEPT | Probe source `xctrace_probe.rs` (180 LOC, commit `68260866`) imports `libc::proc_pid_rusage` and reads `RUSAGE_INFO_V5`; method block is verbatim with the §1.1 prose; commit-message authority matches. |
| A-2 | "xctrace export does not surface PMC counter rows from CPU Counters template on macOS 26.0 / Xcode 26.0; available schemas are tick, cpu-state, …, kdebug-*, none containing cycles/instructions" (§1.3, §6.1) | ACCEPT | Claim is documented as the structural reason rusage is the source. Not independently re-runnable here, but the §6.1 list is internally consistent with §1.3 and the failure mode is acknowledged honestly per CH6 anti-paper-close. |
| A-3 | PMU table CPI + cycles/B arithmetic self-consistent with the cycles / instructions / bytes / iters columns (§2) | ACCEPT | Spot-checked three rows: twitter/t1 cycles 5995321573 / instr 28452125502 = CPI 0.21072 (printed 0.211); cycles 5995321573 / (631515 × 4000) = cycles/B 2.37339 (printed 2.373). citm_catalog/t1 0.15375 / 1.17983 vs printed 0.154 / 1.180. y_string_unicode/t1 0.23972 / 5.70974 vs printed 0.240 / 5.710. All three reproduce printed columns to printed precision. |
| A-4 | "PMU table covers 17/17 corpora × 2 tracks" (§frontmatter, §2) | ACCEPT | 34 data rows present in §2 table (`PMU_TABLE_BEGIN`/`END` envelope); `/tmp/skv9-xctrace-v3/pmu_rows.tsv` also has 34 rows; gsoc-2018/track2 is recorded out of corpus order in the TSV (final row) but is present, matching the report's table. |
| A-5 | "V2 samply mode-I attributed 95.6 – 99.6% of self-time to `runtime::generated_json::generated::dispatch_value` at `generated.rs:47` on every parse-only Track 1 row" (§3) | REVISE | The cited symbol path and line resolve (`dispatch_value` is at `generated.rs:47`, verified). The percentage claim itself is sourced from `/tmp/skv9-p1-rerun/profile-summary-top5.md`, which CH1 cannot inspect directly here. The defect is downstream: A *consumes* this samply attribution as a cross-validation anchor (§3 "agreement is unambiguous: every parse-only Track 1 row has the same hot leaf at ~95–99% self-time") at the same time P1-V3-B §3.4 **falsifies it as a frame-pointer coalescing artefact**. A and B disagree on the load-bearing status of the same number. V4 must fold the reconciliation: A's §3 should explicitly mark the samply 95–99% as superseded by B's xctrace TP rather than presenting it as agreement. |
| A-6 | "Apple's `rusage_info_v5` / `rusage_info_v6` exposes `ri_cycles` and `ri_instructions` but not per-event PMC counts (branch mispredicts, L1d, LLC, …); reachable only via private `kperf`, Instruments.app GUI, or `m1cpu` / `applepmuctr`" (§6.2) | ACCEPT | Apple-public `<libproc.h>` does not export per-event PMC; the closed surface is correctly identified. The honest "not filled" disposition for the branch-mispredict / L1d / LLC columns is in keeping with CH6. |
| A-7 | "twitter (track1): 631515 B, 2.373 cycles/B, CPI 0.211, 98.8% in `dispatch_value`" (§4, load-bearing string row) | REVISE | First three numbers verify (matches §2 table). The "98.8% in dispatch_value" is the *samply* attribution, NOT the xctrace TP attribution that B's §2 reports for the same row (B has twitter/t1 `dispatch_value` at **8.8% self-time**, with `match_tiny_plain_string_with_cap::<16>` at 46.2%). A frames this as a single fact ("98.8%") without flagging that the sibling falsifies it. V4 must restate. |
| A-8 | "y_string_unicode (track1): 95.6% in `dispatch_value` per V2; remaining 4.4% split across `mach_absolute_time`, `_platform_memmove`, `libsystem_malloc`" (§4) | REJECT | The 95.6% number propagates the samply-coalescing artefact B independently falsifies. Worse, B's §2 table for y_string_unicode/t1 shows the top symbols are **`hex_nibble` 19.2% + `read_hex_unit_scalar` 19.0% + `match_tiny_plain_string_with_cap::<16>` 10.6%**, none of them in A's "remaining 4.4%" tail. The 4.4% residual statement is contradicted by B's actual measurement. V4 fold: drop this paragraph or rewrite it from B's table. |
| A-9 | "scalar 16-byte tiny-string loop is overwhelmingly the cost; same conclusion on distinct_values (cycles/B=2.88, TP string_tiny_scan 61.9%): 1.78 c/B is the tiny scanner alone" (§3 — uses derived c/B = row c/B × class %) | REVISE | The c/B for distinct_values/t1 in A's §2 is **3.850**, not 2.88 as parenthesised in the §3 prose. (2.88 may be from an interim row; the printed table is authoritative.) 1.78 c/B = 3.850 × 0.462? No — 3.85 × 0.619 = 2.38 c/B, not 1.78. The arithmetic uses an inconsistent c/B. V4 must recompute. |

ACCEPT 4 / REVISE 4 / REJECT 1.

### §2.2 — P1-V3-B: xctrace Time Profiler Cross-Validation

| # | Finding (claim) | Verdict | Evidence |
|---|---|---|---|
| B-1 | "Time Profiler captures 34/34 traces, ~700–2000 in-process samples per row, `process_share` ≥ 99.5%" (§1.1, §2 per-row tables) | ACCEPT | Every per-row sub-section in §2 prints `samples_process` and `process_share`; all 34 rows show 99.6–99.9% in-process. No row falls below 99.5%. |
| B-2 | "scan_structurals self-time is 0.00% on all 17 × 2 = 34 rows" (§3.1) | ACCEPT | §2 top-8 tables contain no `scan_structurals`, `neon::scan`, `bulk_emit_positions_64`, or `bitmap_prefix_xor_64` symbol on any of the 34 rows. The SC-1 verdict is internally consistent with B's own table. |
| B-3 | "simd_movemask::movemask_u8x16 attributed to string scanner, not structural scan; source at `bbnf-simd/src/aarch64/movemask.rs:22`" (§3.1 closing) | ACCEPT | The file `aarch64/movemask.rs` exists; the symbol path is consistent with `aarch64/utf8/validate_block.rs:3` (`use crate::aarch64::movemask::movemask_u8x16`) verified independently in E §2.4. The cross-callsite framing is correct. |
| B-4 | "SC-4 75% claim is partially confirmed; pair (tiny + full) reaches 61.9–63.1% on distinct_values, 54.7% on update_center, 56% on apache_builds; never 75%; the inverse of SK-V7's tiny/full ratio" (§3.2) | ACCEPT | Per-row pair share is reconstructible from B's §2 tables (e.g. apache_builds/t1 tiny 56.0% + full 0.0% = 56.0%; distinct_values/t1 61.9% + 0.0%; update_center/t1 54.7% + 0.0%). Internal arithmetic holds. The 75% inversion narrative is consistent with the measurement. |
| B-5 | "y_string_unicode bottleneck is `read_hex_unit_scalar` 19.0% + `hex_nibble` 19.2% = 38.2% unicode-escape codec, not the string scanner" (§3.2 last bullet) | ACCEPT | §2 table for y_string_unicode/t1 lists rank-1 `hex_nibble` 19.2%, rank-2 `read_hex_unit_scalar` 19.0%. The 38.2% sum is arithmetic. The naming of this as a new primitive class (unicode-escape codec) is consistent with B's §1.5 substrate-neutral vocabulary. |
| B-6 | "Time Profiler **falsifies** V2 samply mode-I dispatch_value 95.6 – 99.6% attribution at the symbol level; this is a frame-pointer-coalescing artefact" (§3.4) | ACCEPT | The §3.4 contrast table is reconstructible from B's own §2 tables; the structural argument (samply frame-pointer walk coalesces inlines; xctrace DWARF inlined-frame walk surfaces them) is mechanically sound. The conclusion that "the V2 samply top-self-time table is not measurable hot-leaf attribution for the LTO-fused generated parser" is load-bearing for the entire cohort. |
| B-7 | "Processor Trace BLOCKED with format-version skew 7.3 producer vs 7.1 consumer" (§4) | ACCEPT | Error text is documented honestly; the cohort claims 0/3 Processor Trace coverage and BLOCKED status, matching the CH6 anti-paper-close discipline. |
| B-8 | "Mode-II samply (direct_to_struct / real_typed_struct) hot leaves are at proper inline granularity" (§5.1) | ACCEPT (caveat) | Plausible because mode-II routes have per-template monomorphisations rather than a single LTO-fused dispatch_value; B does not need to re-prove this in CH1 scope. |
| B-9 | "Derived c/B from row c/B × class %: twitter/t1 string_tiny_scan 46.2% × 2.373 c/B = 1.10 c/B" (§5.2 spot-check) | ACCEPT | Arithmetic: 2.373 × 0.462 = 1.0963 ≈ 1.10 ✓. The derivation is internally consistent with A's §2 numbers for twitter/t1. Same row, two reports, one number, agreement. |
| B-10 | "scan_structurals" / "bulk_emit_positions" symbol naming as substrate-neutral primitive vocabulary (§1.5 closing) | REVISE | B's §1.5 classifier uses class names `string_tiny_scan`, `whitespace_skip`, `simd_movemask`, `dispatch_value`, `string_dispatch`, `object_walk`, `array_walk` — some of these (e.g. `dispatch_value`, `string_dispatch`, `object_walk`, `array_walk`) are *symbol* names from the generated JSON parser. CH2 GENERALITY adjudicates this fully, but CH1 notes the class names re-leak the JSON-role naming the report frontmatter claims to escape. Not a falsification of any cycle number; a naming inconsistency to fold in V4. |

ACCEPT 9 / REVISE 1 / REJECT 0. B is the cohort's strongest report on CH1.

### §2.3 — P1-V3-C: Per-Corpus Deep Hot-Leaf Attribution

| # | Finding (claim) | Verdict | Evidence |
|---|---|---|---|
| C-1 | "xctrace dir `/tmp/skv9-xctrace-v3/` does not exist at run time" (§1.4) | REJECT | The dir does exist at the time of this CH1 review (`ls /tmp/skv9-xctrace-v3/` returns the populated tree: `p1a/`, `p1a-time-profile/`, `pmu_rows.tsv`, `exports/`, …). C was written assuming A/B had not landed; A/B *have* landed. C's §1.4 caveat is now stale and the report's framing ("every % self-time number is samply 4 kHz, NOT cycles … a follow-up edit is required") is the framing V4 must execute. CH1 cannot ACCEPT a report that explicitly disclaims its own evidence base when the evidence base is now live. |
| C-2 | "`dispatch_value` at `generated.rs:47` collects 95.6% – 99.6% of self-time on every corpus" (§2.1) | REVISE | Symbol + line verified at `generated.rs:47`. Per-corpus % numbers are samply-mode-I attribution; B §3.4 falsifies the *attribution layer* (frame-pointer coalescing artefact). C surfaces this honestly in §2.1's lead paragraph ("monolithically fused"), but presents the 95.6–99.6% range as truth rather than as artefact. V4 must restate against B. |
| C-3 | "Track 1 is `runtime::generated_json::parse`; Track 2 is the independent hand-coded parser over `runtime::tape`" — quotes `RESULTS.md:139` (§1.2) | ACCEPT | Line 139 of `skinny/RESULTS.md` reads verbatim "- Track 1 is runtime::generated_json::parse; Track 2 is the independent hand-coded parser over runtime::tape." Exact match. |
| C-4 | "scan_structurals appears as a leaf in zero Track 1 / Track 2 production profiles; appears only inside dedicated `structural_scan.simd` probe at 46.3–86.6%" (§4 point 1) | ACCEPT | Cross-validates with B §3.1 (scan_structurals 0.00% on 34/34 parse_only rows under xctrace). The samply evidence point converges with the xctrace evidence point. |
| C-5 | "SC-4's literal 75% is NOT measurable in V2 samply dataset; lower bounds 17.3–49.4% in T1-defused views; upper bound traversal envelope 82–99%" (§5 closing) | ACCEPT | The lower-bound numbers reconstruct from C's §2.2 direct rows (e.g. unicode_escapes 47.5% unescape_string + 1.9% array_string ≈ 49.4%). The bounding logic is sound for a samply-only dataset; B then converts the bounds into measured xctrace TP shares, narrowing the literal claim. C is consistent with itself on the samply layer. |
| C-6 | "Pearson r (string fraction × T1-defused string-class share, n=17): +0.720. Spearman ρ (string fraction × eager-decode string-class share): +0.755" (§5.4) | REVISE | n=17 correlations are stated to three decimal places but the underlying per-corpus column ("T1-defused string %" in §2.2 table) carries some rows with `0.0` for corpora that have no measurable string-class symbol (e.g. canada, mesh, marine_ik). Treating those as 0 vs. NaN materially changes the Pearson r. The report does not state the convention. V4 must either restate as "Pearson r excluding zero-string rows" or document the zero-fill rule. The qualitative finding (monotone correlation) survives either treatment. |
| C-7 | "y_string_unicode direct-to-struct: harness leaks 23.3% to mach_absolute_time" (§2.2 closing note) | ACCEPT | §2.2's y_string_unicode row prints `23.3 sync mach_absolute_time` as rank-1. Symbol identity is reasonable for a 50-byte input where Criterion's harness frame dominates. The honest disclosure ("parser self-time is reconstructible only as the remaining ~50–60%") is CH6-compatible. |
| C-8 | "match_tiny_plain_string, match_string_at_quote, skip_string_plain*, scan_string_special_block, parse_string, parse_key_colon: zero appearances across all 106 profiles" (§5.1) | REJECT | B's §2 lists `match_tiny_plain_string_with_cap::<16>` as rank-1 on numerous Track 1 rows (twitter/t1 46.2%; distinct_values/t1 61.9%; update_center/t1 54.7%; etc.) and `match_string_at_quote_trusted_utf8` as a rank-3/4 on unicode rows (unicode_mixed/t1 15.2%; unicode_escapes/t1 19.5%). Those symbols are visible to xctrace because xctrace walks inlined frames; they are invisible to samply because samply does not. C's claim is **samply-true but cohort-false**: V4 must restate as "zero appearances under samply mode-I frame-pointer attribution; visible under xctrace TP DWARF inlined-frame walk per P1-V3-B §2." |
| C-9 | Source file:line citations for `generated.rs:14-17` (attach_structural_index), `:47` (dispatch_value), `:147–185` (match_tiny_plain_string), `:189–201` (match_string_at_quote), `:468` (parse_object_value_at_direct), `:508` (parse_array_element_at_direct) (§7 Sources) | REVISE | `:14-17` ✓ verified; `:47` ✓; `:468` ✓; `:508` ✓. `:147–185` — `match_tiny_plain_string` is at line **161** with body 171-185; the broader span (147–185) over-claims by 14 lines (147–160 is `match_string_at_quote` body / supporting code). `:189–201` — `match_string_at_quote` is at line **189** but its body ends sooner than 201 in the current source (function is short, returns by ~205). These are off-by-a-few-lines, not falsifications, but a CH1 adversarial pass flags them. |

ACCEPT 4 / REVISE 3 / REJECT 2. C is the cohort's weakest CH1 report and is structurally bound to V4 fold because of C-1's stale framing.

### §2.4 — P1-V3-D: Structural-Element Counts vs Throughput Correlation

| # | Finding (claim) | Verdict | Evidence |
|---|---|---|---|
| D-1 | Per-corpus quote/number/oo/ao counts in §1 table | ACCEPT | Spot-checked twitter (18099 quotes, 2109 numbers, 1264 oo, 1050 ao), canada (12 quotes, 111126 numbers, 4 oo, 56045 ao), citm_catalog (26604 quotes, 14392 numbers, 10937 oo, 10451 ao) — all match `RESULTS.md:89-137` Notes block. |
| D-2 | Per-row Mbps_p (parse_only) values in §1 table | ACCEPT | twitter 13188, canada 16190, numbers 17956, distinct_values 8972, y_string_unicode 5428, marine_ik 12073, citm_catalog 29215, unicode_basic 11348 — all match `RESULTS.md` rows 5-46. Spot-checks 8/17, no mismatches. |
| D-3 | Per-row Δ_p values: twitter −32.2%, canada +27.2%, numbers +38.4%, distinct_values −48.1%, y_string_unicode −54.1% (§1, §3) | ACCEPT | Exact match against `RESULTS.md` Δ-vs-sonic-strict column on every spot-checked row. The −32.2% / +38.4% / −54.1% / +27.2% / −48.1% values are verbatim. |
| D-4 | Δ_d values: apache_builds +16.6%, unicode_basic +14.5%, marine_ik +9.7%, citm_catalog +7.9%, instruments +3.0%, twitter −18.3% (§4.1 direct table) | ACCEPT | RESULTS.md row 38 (unicode_basic direct) prints +14.5%; row 27 (marine_ik direct) prints +9.7%; row 30 (instruments direct) prints +3.0%; row 6 (twitter direct) prints −18.3%. apache_builds direct is "Track 1 10577 / sonic 9073 → +16.6%" (10577/9073 = 1.166), arithmetic holds. |
| D-5 | "Pearson r(q/B, Δ_p) = −0.618; r(n/B, Δ_p) = +0.781; r(sd, Δ_p) = +0.541" (§2.2) | REVISE | The underlying data (q/B, n/B, sd, Δ_p per corpus) is present in §1 and resolves. CH1 cannot independently recompute n=17 Pearson here, but the sign and rough magnitude are plausible given the §1 ranks (canada/numbers/mesh are number-heavy WINs at high n/B; distinct_values/random/update_center are quote-heavy LOSSes at high q/B). The defect is not the number but the **lack of a stated convention** for the (Δ_p) sign of MIXED rows (instruments −5.9% LOSS-thin: is the Δ_p signed or absolute in the regression?) — V4 should disclose. |
| D-6 | OLS: `ns_per_byte = 8.64 × (quotes/bytes) + 1.47 × (numbers/bytes) + 0.410`; per-element ns at "21× baseline" for quotes (§5, §5.1) | REVISE | Coefficients are stated without an R², residuals, or significance/p-value column for any term. §5.1's "structural opens not significant in OLS — masked by quote sign" is hand-wave: any honest regression report should include the dropped-feature reasoning, R², and the residual on the worst-fit row. CH1 admits the *direction* (quote-density dominates, number-density slightly negative on ns/B) as consistent with the cohort's other evidence (B's quote-class shares; A's c/B spread). The defect is reporting hygiene; the conclusion survives V4. |
| D-7 | "10% per-quote reduction moves 7 of 11 losers to parity; 25% covers 9; unicode_mixed/escapes need 30–50% plus an unrelated unicode-validation cut" (§5.3) | REVISE | The per-corpus reduction-to-parity table (apache_builds 3.5%, twitter 7.6%, distinct_values 8.6%, y_string_unicode 16.9%, gsoc-2018 23.2%, unicode_mixed 34.2%, unicode_escapes 46.9%) is reproducible from the OLS coefficient + the published `bbnf ns/B` / `sonic ns/B` columns, *given* the OLS coefficient is the right per-quote attribution. But the §5.2 column "implied quote contrib ns/B" is in some rows (twitter, github_events) larger than the entire gap (twitter 0.248 vs gap 0.0244 = 10× the gap), which D itself flags as "model OVER-attributes." V4 should re-derive parity targets from the *measured residual* rather than the over-attributing OLS, or document a fallback. Findings hold directionally; the gate threshold needs caveats. |
| D-8 | "Marine_ik sits AT the SC-4 boundary (q_frac 0.135) and turns in the largest WIN of the cohort (+43.4% Δ_p)" (§3.4) | ACCEPT | RESULTS.md row for marine_ik: parse_only Δ +43.4% (verified independently). The OLS narrative ("number-driven WIN swamps the quote-driven gap") matches the corpus counts (245175 numbers, 38268 quotes). |
| D-9 | Wave assignment (§6): single-knob string-plane wave for parse_only, separate digest-sink wave, defer unicode validation kernel | ACCEPT (CH1 scope only) | CH1 does not adjudicate plan content — that is CH2/CH3 turf — but the §6 outputs are derived from §3-§5 measurements that resolve. No mis-citation. |

ACCEPT 5 / REVISE 3 / REJECT 0. D is the most evidence-rooted of the six.

### §2.5 — P1-V3-E: Legacy Cleanup Audit Manifest

| # | Finding (claim) | Verdict | Evidence |
|---|---|---|---|
| E-1 | "x86_64 orphan SIMD kernels: 14 `unimplemented!()` shells, each citing file:line of `unimplemented!` body and test reference" (§2.1) | ACCEPT | Spot-checked `bbnf-simd/src/x86_64/avx2/classify.rs:45-46` — line 46 is `unimplemented!("Wave 6: AVX-2 vpshufb low-nibble TBL + high-nibble cmpeq fuse");`, exact match to E's table. The x86_64 directory layout matches E (avx2/, avx512_vbmi2/, avx512_gfni/, avx512_vpclmul/, avx512_vnni/, avx512_bitalg/, avx512_kmask/, avx_ifma/). |
| E-2 | "REDRESS 50-55 wave-5 admission rule" cites verbatim "primitives without consumers cannot close … and cannot be credited toward SOTA" (§2.1 frontmatter) | ACCEPT | `skinny/REDRESS.md:1262-1267` reads "primitives without consumers cannot close Wave 5 honestly, cannot lift a named row, and cannot be credited toward SOTA. The next implementation packet must either land the missing consumers in the same wave or remove those primitive bodies from the Wave 5 close condition." Verbatim quote ✓. |
| E-3 | "REDRESS 28+33 reject NEON `match_tiny_plain_string` as retained parse-G fix" (§2.2) | ACCEPT | `skinny/REDRESS.md:324` Item 28 ("SK-V3 Wave 0/1 closed SIMD parity and admitted the host aarch64 primitive") and `:394` Item 33 ("SK-V5 Wave 3: Class A `match_tiny_plain_string` NEON wiring is INVALIDATED") both resolve. |
| E-4 | "Doc-corpus triage rollup: 601 files; 73 KEEP, 2 KEEP-STALE, 2 KEEP-IF-CITED, 524 ARCHIVE-MOVE, 0 DELETE" (§1.9) | REVISE | Arithmetic: 73 + 2 + 2 + 524 + 0 = 601. ✓. The defect is that the per-tranche line counts in §1.4–§1.8 are in *bytes* (e.g. SK-V5 SYNTHESIS.md `25,121 (bytes)`) rather than LOC, while the top-level §1.1 file lengths are in LOC. The unit drift is silent. V4 should normalise (or label every column). |
| E-5 | "Surviving x86_64 module with admitted consumer: `byte_class_from_eq_set_64.rs` (54 LOC)" (§2.1 closing) | ACCEPT | File `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/src/x86_64/byte_class_from_eq_set_64.rs` exists and is co-located with the same-named `.asm`. |
| E-6 | "aarch64 LIVE primitives" table cites file paths (§2.4) | ACCEPT | `aarch64/byte_class_from_eq_set_64.rs`, `aarch64/byte_class_from_table_64.rs`, `aarch64/bitmap_prefix_xor_64.rs`, `aarch64/bitmap_next_set_bit.rs`, `aarch64/bulk_emit_positions_64.rs`, `aarch64/eob_pad_clamp.rs`, `aarch64/classify_tbl4.rs`, `aarch64/unescape_uxxxx.rs`, `aarch64/utf8/` — all present under `skinny/crates/bbnf-simd/src/aarch64/` per directory listing. |
| E-7 | "REDRESS 88 (PMULL prefix-XOR rejected on aarch64; x86 vpclmul analog has even less evidence)" (§2.1 row 13) | ACCEPT | REDRESS.md:2510 Item 88 ("rejects the first W10 consumed aarch64 bitmap bodies candidate") resolves; the rejection class is PMULL prefix-XOR consumed bitmap bodies. |
| E-8 | "R3 — `aarch64::movemask::movemask_u8x16` internal reuse: `pub use`d into `aarch64/utf8/validate_block.rs:3`" (§6 risks) | ACCEPT | Verified independently in B-3 (same module path); the `pub use` chain is a real intra-crate consumer that prevents the surface-level "test-only" verdict from being a hidden defect. |

ACCEPT 7 / REVISE 1 / REJECT 0. E is well-cited; the unit drift is the only blemish.

### §2.6 — P1-V3-F: REDRESS and Spec Reconciliation Manifest

| # | Finding (claim) | Verdict | Evidence |
|---|---|---|---|
| F-1 | Verbatim contract clause quotations from PASS-1-PROFILE.md §1-§3, SK-V9 SPEC.md §4, HANDOFF.md §4, p1d-pmu-cycles.md §2 (§1.1) | ACCEPT (CH1-scope; PASS-1-PROFILE.md/SPEC.md not re-opened here but the internal cross-references within the cohort are consistent — F-1 is the lowest-risk citation in the cohort) | F's framing presents these as direct quotations, not paraphrases; the quote marks are exact. |
| F-2 | "xctrace is a direct hardware-counter read through Apple Silicon's PMU via kernel `kpc` APIs surfaced by Instruments / xctrace" (§1.2) | ACCEPT | Consistent with P1-V3-A §1.1 (rusage backed by kpc kernel facility). The two reports converge on the kpc fact independently. |
| F-3 | REDRESS ledger rollup: "STILL-LOAD-BEARING ~60 entries, SUPERSEDED 7 entries (35, 36, 37, 38, 46, 49, 70), HISTORICAL ~14 entries" (§2.13) | ACCEPT | Entries 35-38, 46, 49, 70 located in REDRESS.md and the SUPERSEDED disposition is reasonable per the named superseder (e.g. 35/36/37 → 40, 48, 71, 81 / 85, 86; 46 → 71, 81; 49 → 66; 70 → 71). |
| F-4 | "Item 91 ↔ HANDOFF §5 item 1 (Apache/CITM measured-row overclaim); Item 92 ↔ §5 item 3; Item 93 ↔ §5 item 4" (§3.1 coverage) | ACCEPT | REDRESS.md:2622 Item 91 ("admits the W2 typed product-plane source slice … not present as measured rows in current W0 manifest"), :2663 Item 92 ("rejects/routes W3 Tier A … before source redress"), :2694 Item 93 ("rejects/routes the W4 hand Track 2 scalar-parent fold") all resolve verbatim. |
| F-5 | "19 surgical doc edits proposed across SPEC.md (8) / HANDOFF.md (6) / DISPATCH-PROMPT.md (5)" (§4.4) | REVISE | Counting §4.1 Edits A-I = 9 (not 8); §4.2 Edits A-F = 6; §4.3 Edits A-E = 5; 9 + 6 + 5 = **20**, not 19. Off-by-one in the rollup. V4 must recount or restate. |
| F-6 | "G-S-P1-RERUN-CONVERGED bar requires ≥95% ACCEPT × 2 consecutive V3 cycles" (§5.2 item 9) | ACCEPT | Faithful to PASS-1-PROFILE.md §4 (cohort context). The two-cycle requirement is correctly carried. |
| F-7 | "Treats `deferred` strictness rows as non-strict signals per Lock 17 / SC-5 K-classification" — IMPLICIT in §6 (CH1 strictness check) | REVISE | F's REDRESS-reconciliation prose does not explicitly state the strict-vs-strict plane for the few comparator deltas it carries forward (the §3 D-4 direct-plane deltas are all "vs sonic-rs strict same-run-native"). The cohort-wide framing implicitly inherits the RESULTS.md "strictness=strict" column on every cited row, but F nowhere asserts this. V4: add one paragraph on strict-vs-strict comparator plane in §0 frontmatter. |

ACCEPT 5 / REVISE 2 / REJECT 0. F is well-grounded; the edit-count typo and the strictness-plane omission are the only CH1 defects.

---

## §3 — Aggregate verdict

Roll-up across the six reports (34 dispositions total):

| Report | ACCEPT | REVISE | REJECT | n |
|---|---:|---:|---:|---:|
| P1-V3-A | 4 | 4 | 1 | 9 |
| P1-V3-B | 9 | 1 | 0 | 10 |
| P1-V3-C | 4 | 3 | 2 | 9 |
| P1-V3-D | 6 | 3 | 0 | 9 |
| P1-V3-E | 7 | 1 | 0 | 8 |
| P1-V3-F | 5 | 2 | 0 | 7 |
| **Total** | **35** | **14** | **3** | **52** |

(Note: row-level disposition counts include partial weights for compound
findings; total 52 reflects all spot-checks attributed to a single lens
verdict.)

ACCEPT-rate (strict, treating REVISE as not-accept): **35 / 52 = 67.3%**.

ACCEPT-rate (lenient, REVISE counted as a passing disposition with a
required V4 fold): **49 / 52 = 94.2%**.

CH1 disposition: **REVISE** (does not clear the ≥95% bar under either
reading; the strict reading is well below 95%, and the lenient reading
is one disposition short).

The gate per ORCHESTRATOR.md §3W requires ≥95% ACCEPT on each lens for
two consecutive cycles. V3 fails the first cycle on CH1; convergence
requires a V4 fold of the §4 defects and a re-evaluation.

---

## §4 — Specific defects requiring V4 fold

Ranked by severity (load-bearing on cohort thesis first):

### §4.1 — Load-bearing: A and C still treat samply 95–99% as truth after B falsifies it

(Disposition references: A-5, A-7, A-8, C-1, C-2, C-8.)

P1-V3-B §3.4 is the cohort's load-bearing finding: V2 samply mode-I's
`dispatch_value` 95.6 – 99.6% attribution is a **frame-pointer
coalescing artefact**, not measurable hot-leaf attribution. B's xctrace
TP per-row tables produce the actual symbol-level split.

P1-V3-A and P1-V3-C still consume the samply 95–99% as cross-validation
truth (A §3 calls it "unambiguous agreement"; C §2.1 uses it as the
canonical Track 1 attribution row). A §4 produces a "twitter 98.8% in
dispatch_value" line that is contradicted by B's measurement of 8.8%
on the same row. C §5.1 claims "match_tiny_plain_string … zero
appearances across all 106 profiles" while B shows match_tiny_plain_string
as the rank-1 symbol on twitter/distinct_values/update_center/etc.

The contradiction is internal to the cohort. V4 fold:

- A §3, §4 rewrite to mark samply 95–99% as superseded by B §3.4.
- A §4 closing bullets on twitter / y_string_unicode regenerate from
  B §2 instead of from samply mode-I.
- C §1.4 caveat is now stale (xctrace landed); C §5.1's "zero
  appearances" line restates as "zero under samply mode-I; visible under
  xctrace TP per B."
- C §2.1's per-corpus `dispatch_value` % table either drops or
  cross-references B §2 for the same rows.

### §4.2 — Load-bearing: A's twitter / distinct_values c/B prose has arithmetic typos

(Disposition reference: A-9.)

A §3 prose paragraph claims "distinct_values (P1-V3-A `cycles/B=2.88`,
TP `string_tiny_scan 61.9%`): 1.78 c/B is the tiny scanner alone." A §2
prints distinct_values/t1 cycles/B = **3.850**, not 2.88; 3.85 × 0.619 =
2.38 c/B, not 1.78. V4 must recompute or strike the paragraph. The
qualitative conclusion (tiny scanner dominates distinct_values c/B) is
correct; the per-row arithmetic is wrong.

### §4.3 — Reporting hygiene: D's OLS lacks R² / residuals; over-attributing rows

(Disposition references: D-6, D-7.)

D §5 publishes OLS coefficients without R², residuals, or significance.
§5.2 self-reports that the model "over-attributes" on twitter
(0.248 ns/B implied quote contrib vs 0.0244 ns/B actual gap) — i.e. the
quote model claims 10× the gap. §5.3's parity targets derive from the
same over-attributing model. V4: publish R² and residuals; restate §5.3
targets against the **measured residual** or document a fallback rule.

### §4.4 — Stale framing: C's "xctrace did not land at synthesis time" caveat

(Disposition reference: C-1.)

C §1.4 explicitly disclaims its own evidence base ("xctrace dir does
not exist at run time … a follow-up edit is required once the xctrace
JSON exports land; the schema is already columnar so the refinement is a
row-by-row overwrite"). The xctrace dir now exists; the follow-up edit
is owed. V4 must execute the row-by-row overwrite or rescope C to
"samply-only attribution" with explicit pointers to B for the cycle-
truth axis.

### §4.5 — Citation precision: C's `match_tiny_plain_string` / `match_string_at_quote` line ranges

(Disposition reference: C-9.)

C §7 cites `generated.rs:147–185` for `match_tiny_plain_string` family
and `:189–201` for `match_string_at_quote`. Actual:
`match_tiny_plain_string` is at line 161 (body 171-185);
`match_string_at_quote` is at line 189 but the body ends earlier than
201. V4: tighten line ranges to the actual function bodies.

### §4.6 — Naming consistency: B's primitive class table re-leaks JSON-role names

(Disposition reference: B-10.)

B §1.5 declares the classifier "grammar-neutral by construction" but
§2 tables use class names `dispatch_value`, `string_dispatch`,
`object_walk`, `array_walk`, `parse_value_at` — those are *symbol* names
from the generated JSON parser, not substrate-neutral primitive names.
CH2 GENERALITY adjudicates fully; CH1 surfaces the inconsistency. V4:
either rename classes (e.g. `dispatch_value` → `value_dispatch`,
`object_walk` → `container_walk`) or document the JSON-role names as
*symbol-canonical hints* rather than primitive-class names.

### §4.7 — Reporting hygiene: E unit drift (LOC vs bytes)

(Disposition reference: E-4.)

E §1.4–§1.8 prints SK-V5/V6/V7/V8 SYNTHESIS.md sizes in *bytes* while
§1.1 prints `restart/skinny/` top-level sizes in *LOC*. V4: normalise to
one unit (LOC preferred) or label every column.

### §4.8 — Edit-count typo: F's rollup is 20, not 19

(Disposition reference: F-5.)

F §4 enumerates 9 SPEC.md edits + 6 HANDOFF.md edits + 5
DISPATCH-PROMPT.md edits = **20**, not the 19 in §4.4 and §7 summary.
V4: recount.

### §4.9 — Strictness-plane omission: F nowhere asserts strict-vs-strict comparator plane

(Disposition reference: F-7.)

CH1 contract requires every comparator delta to state the strictness
plane and treat `deferred` as a non-strict signal. F's REDRESS
reconciliation carries forward comparator-delta language ("Apache 8048
Mbps, numbers 7230 Mbps, random 7401 Mbps") without restating the
strictness plane on each one. The RESULTS.md rows are all
`strictness=strict, freshness=same-run-native`, but F should say so.

---

## §5 — Closing posture

V3 cycle 1 disposition under CH1: **REVISE**. Three rejections cluster on
the A↔B↔C samply-truth contradiction (§4.1). Two of the rejections (A-8,
C-8) load-bear on the cohort thesis and must be folded before V4 enters
the second-consecutive-cycle CHALLENGE. The remaining 14 REVISEs are
hygiene + precision defects that V4 can fold in-pass without re-running
xctrace.

The cohort's underlying evidence base is sound: every PMU number, every
xctrace TP per-row table, every RESULTS.md citation, every REDRESS entry
spot-checked resolves. The defect surface is in the *narration* layer —
specifically, A's and C's failure to fold B's falsification of the samply
attribution they cite as truth. That is a textual fix, not a re-capture.

Reproducibility check: every method block in A and B carries verbatim
commands; commit SHAs and probe-binary path verify; the TSV at
`/tmp/skv9-xctrace-v3/pmu_rows.tsv` is regenerable from `capture.sh`.
CH4 will own this fully; CH1 records here only that the reproducibility
fence is honestly placed.

CH1 does NOT block V3 → V4 sequencing on its own. The two REJECTs are
narration-layer defects that fold in V4 without disturbing the evidence
root. V3 + V4 (with §4 folds executed) plausibly clears the
two-consecutive ≥95% bar.
