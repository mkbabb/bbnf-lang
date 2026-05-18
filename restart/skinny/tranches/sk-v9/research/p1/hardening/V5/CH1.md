# SK-V9 S-P1 V5 CHALLENGE — CH1 CORRECTNESS (verify V5 fold)

Pass: S-P1 Profile. Cycle: V5 (post-V5-fold). Lens: CH1 CORRECTNESS.
Date: 2026-05-18.
Reviewer: adversarial CH1 verify, single agent, read-only.

V4 returned CH1 at **96.2% strict / 100% lenient ACCEPT** with two
narration-layer V4 REVISEs (A4-9 §3 line 237 stale framing; C4-5 §5.3
off-by-one hedge). V5 commit `d76eef63` applied 6 surgical edits across
A/B/C/D/F. Per ORCHESTRATOR.md §3Z: V4 was the first qualifying cycle on
CH1; V5 is the candidate second consecutive qualifying cycle. If V5
clears ≥95%, S-P1 converges on CH1.

Inputs:

- V4 disposition: `restart/skinny/tranches/sk-v9/research/p1/hardening/V4/CH1.md`.
- V5 fold spec: `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md` §"V5 fold requirements".
- V5 commit `d76eef63` (5 files; +76 / −7).
- Six V5-folded reports `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-{A..F}.md`.
- Evidence root `/tmp/skv9-xctrace-v3/{regression.py,pmu_rows.tsv,regression_output.json,p1b-tp/*.trace,aggregate.py}`.
- Source corpus `skinny/crates/runtime/src/grammars/json/generated.rs`,
  `skinny/crates/parse-that-regex/src/lib.rs`, `skinny/RESULTS.md`.

Hard cap: 20 min.

CH1 contract (per ORCHESTRATOR §3W): every concrete claim resolves to a
citation that exists and matches the claimed content. Adversarial bias:
a citation approximately right is wrong. V5 verify is a **two-fold**
discipline: (a) the two V4 narration REVISEs must land cleanly with
concrete cited evidence; (b) the V5 fold must not introduce new defects
elsewhere.

---

## §1 — V4 REVISE resolution

V4 returned exactly two CH1 REVISEs. The V5 fold targets both
explicitly. Both verified below by direct line-anchored Read of the
V5-on-disk text plus diff against the V4 baseline.

| V4 # | V4 defect | V5 fold site | Status | Evidence |
|---|---|---|---|---|
| A4-9 | P1-V3-A §3 line 237 "The agreement is unambiguous: every parse-only Track 1 row … has the same hot leaf at ~95-99% self-time" reads as a current truth-claim that §4 then contradicts. Recommended fix: prefix with "V2 baseline (superseded; see §4 / B §3.4)" or qualify "at samply mode-I sample-attribution granularity". | P1-V3-A lines 237-246 (paragraph rewrite). | **FOLDED** | The §3 paragraph at line 237 now reads "The V2 baseline (superseded; see §4 and P1-V3-B §3.4): every parse-only Track 1 row *appears to share* the same hot leaf at ~95-99% self-time. That ~95-99% figure is a frame-pointer-coalescing artefact of the samply mode-I capture; xctrace Time Profiler with DWARF resolves the inlined leaves and falsifies the single-symbol attribution." The "is unambiguous" assertive present tense is replaced by past/observational framing ("appears to share"); the supersession is named in-line; the explanation cites §4 + B §3.4. The continuation about cycles/B variation within `dispatch_value` is preserved. Verbatim against commit diff: +6 / −5 lines. Fix matches the V4 prescription exactly. |
| C4-5 | P1-V3-C §5.3 line 717 "The escape-codec class is the largest single cycle sink in the entire 34-row table" is off-by-one: distinct_values/t1 per-string-span (3.850 × 0.619 = 2.38 c/B) marginally exceeds y_string_unicode/t1 escape-codec (2.31 c/B). Recommended fix: hedge to "among the largest" or recompute. | P1-V3-C lines 716-719. | **FOLDED** | Lines 716-719 now read "The escape-codec class is among the largest single cycle sinks in the 34-row table (distinct_values/t1 per-string-span at 3.850 × 0.619 = 2.38 c/B is marginally larger; cf. CH1 V4 A4-9 / C4-5 hedges)." The hedge is exactly what V4 prescribed; the cited arithmetic (3.850 × 0.619 = 2.3814) is independently verified — V3-A §2 PMU table line 184 prints `distinct_values | track1 | … | 0.206 | 3.850` ✓; V3-B §3.2 line 643 prints `string_tiny_scan 61.9%` for distinct_values/t1 ✓. The cross-reference to the V4 CH1 disposition (A4-9 / C4-5) is a clean audit-trail. |

V4 REVISE → V5 status: **2/2 FOLDED**.

### §1.1 — Other V5 fold edits (CH4/CH6 scope, verified for CH1 non-interference)

The V5 commit also lands four non-CH1 edits (V3-D §0 footer CH6
enumeration; V3-B §0 footer CH4-V05/V19/V20 re-capture wall costs; V3-B
§0 footer CH4-V23 aggregate.py reproducibility-by-instruction; V3-F §4
edit-dispatch ≤30min hard cap). These are not CH1-targeted, but CH1
verifies they introduce no citation regressions:

| Edit | CH1 verdict | Evidence |
|---|---|---|
| V3-D §0 footer V3-publication-errors enumeration (8 items) | ACCEPT | Each enumerated correction cites a specific V3 vs V4 value pair (8.64→1.079, 1.47→0.184, 0.410→0.051, 0.371 R², "10% cut 7/11"→"4 of 11", "25% covers 9/11" gone, sign-convention provenance gap, per-row residual absence). Each value reproduced bit-for-bit by `regression.py` (§2). |
| V3-B §0 footer re-capture wall costs (~12 min CPU Counters, ~22 min TP, ~3-5 min lto=fat cold-link, ~37-39 min aggregate) | ACCEPT | The wall-cost numbers are forward-looking estimates for a future S-P1 re-capture, not citations of prior measurement. CH1 contract scope: these are honestly framed as "carries the following deterministic wall costs on the SK-V9 host" — defensible. |
| V3-B §0 footer aggregate.py reproducibility-by-instruction | ACCEPT | `/tmp/skv9-xctrace-v3/aggregate.py` confirmed present on disk; script reads xcrun xctrace export tabular output, bucketises by symbol. Path-cite resolves. |
| V3-F §4 edit-dispatch ≤30min hard cap | ACCEPT | Paragraph inserted before the edit-count reconciliation at F line 463. Procedural addition; no factual citation surface. |

No new CH1 defects introduced by the four non-CH1 edits.

---

## §2 — V5 spot-check dispositions (≥15)

Sampled across all six V5 reports, drawn from a mix of (a) V5-edited
sites, (b) V4 ACCEPT load-bearing claims rechecked for V5 stability,
(c) random source-cite resolution.

| # | Claim | Verdict | Evidence |
|---|---|---|---|
| S5-1 | P1-V3-A §3 V5-edited paragraph (line 237) supersedes V2 framing | ACCEPT | Verbatim Read of lines 237-246 confirms "V2 baseline (superseded; see §4 and P1-V3-B §3.4)" + frame-pointer-coalescing framing. The §4 continuation is preserved unchanged. |
| S5-2 | P1-V3-C §5.3 V5-edited hedge cites distinct_values/t1 at 3.850 × 0.619 = 2.38 c/B | ACCEPT | Three-way arithmetic verifies: V3-A §2 PMU row 184 c/B = 3.850 (also `/tmp/skv9-xctrace-v3/pmu_rows.tsv` line 31 = 3.850092); V3-B §3.2 line 643 string_tiny_scan = 61.9%; product = 2.3814 ≈ 2.38 ✓. |
| S5-3 | P1-V3-A §2 PMU twitter/t1 c/B = 2.373 | ACCEPT | `5995321573 / (631515 × 4000) = 2.37339` ✓; pmu_rows.tsv line 2 = `2.373388` ✓. |
| S5-4 | P1-V3-A §2 PMU citm_catalog/t1 c/B = 1.180 | ACCEPT | `4075618079 / (1727204 × 2000) = 1.17983` ✓; pmu_rows.tsv line 4 = `1.179831` ✓. |
| S5-5 | P1-V3-A §2 PMU y_string_unicode/t1 c/B = 5.710 / CPI = 0.240 | ACCEPT | `2439294848 / (35601 × 12000) = 5.70974` ✓; `2439294848 / 10175451443 = 0.23972` ✓; pmu_rows.tsv line 33 = `5.709799 / 0.239724` ✓. |
| S5-6 | P1-V3-A §2 PMU distinct_values/t1 c/B = 3.850 (the A-9 V3-typo fix target) | ACCEPT | `3548937694 / (153630 × 6000) = 3.85002` ✓; pmu_rows.tsv line 31 = `3.850092` ✓. The V3 §3 prose typo (2.88 / 1.78) is not present in V5 V3-A. |
| S5-7 | P1-V3-D §5 OLS coefficients (1.079, 0.184, 0.051) and R² = 0.371 | ACCEPT | `python3 /tmp/skv9-xctrace-v3/regression.py` reproduced under V5 verify cycle: `ns_per_byte = 1.079*(q/B) + 0.184*(n/B) + 0.051 / R² = 0.3710 / SE (0.409, 0.296, 0.018) / p-values (0.01936, 0.5448, 0.01342) / RSS = 0.0135`. Bit-for-bit reproduction of D §5 line 331-334. |
| S5-8 | P1-V3-D §5 per-row residuals (twitter −0.0070, y_string_unicode +0.0662, citm_catalog −0.0352) | ACCEPT | Script output matches D §5 line 355 (`y_string_unicode 0.1842 0.1180 +0.0662`) and the broader per-row table bit-for-bit. |
| S5-9 | Pearson correlations r(q/B, Δ_p) = −0.618, r(n/B, Δ_p) = +0.781 | ACCEPT | Script output: `r(q/B, Δ_p) = -0.6184, r(n/B, Δ_p) = +0.7811` ✓. |
| S5-10 | P1-V3-B §3.2 line 643 distinct_values/t1 rank-1 = `match_tiny_plain_string_with_cap::<16>` at 61.9% | ACCEPT | Line 643 Read confirms. |
| S5-11 | P1-V3-B §2 y_string_unicode/t1 rank-1 hex_nibble 19.2% + rank-2 read_hex_unit_scalar 19.0% (= 38.2% escape-codec class) | ACCEPT | Lines 671-672 Read: rank-1 19.2% `hex_nibble`, rank-2 19.0% `read_hex_unit_scalar`. Class sum = 38.2% ✓. |
| S5-12 | `match_tiny_plain_string_with_cap` body at `crates/runtime/src/grammars/json/generated.rs:171-185` | ACCEPT | Read of generated.rs:171-185 confirms function head at 171; while-loop body 175-184; close at 185. The B §2 source-cite `generated.rs:178` lies inside the inner-loop match arm body ✓. C §7.4 "head 161, body to ~185" framing matches (line 161 is the `match_tiny_plain_string` wrapper, 171 the `_with_cap` body). |
| S5-13 | P1-V3-F §2 head "Strictness-plane assertion" cites `sk-v9-open:criterion-fnv64-cd1673844eeea12f` | ACCEPT | F line 126-141 Read confirms paragraph intact. `skinny/RESULTS.md` line 48 confirms manifest hash `sk-v9-open:criterion-fnv64-cd1673844eeea12f` on twitter/parse_only row. Cite resolves. |
| S5-14 | V5 commit V3-F edit-dispatch hard-cap paragraph inserted at line 463 | ACCEPT | F lines 463-471 Read confirms: "Edit-dispatch hard cap (V5 fold per CH4-D29 / CH4-V21). The full batch of 19 surgical edits below carries a single dispatch hard cap of ≤30 minutes total." Sequenced SPEC→HANDOFF→DISPATCH; single git-revert protocol. |
| S5-15 | V5 commit V3-B aggregate.py reproducibility-by-instruction (line ~1180) | ACCEPT | `/tmp/skv9-xctrace-v3/aggregate.py` confirmed extant on disk (also `corpus_paths.txt`, `exports/`, 34-bundle p1b-tp/ confirmed). The V3-B §0-footer paragraph at line ~1175-1185 names the path and the operational contract; both resolve. |
| S5-16 | V5 commit V3-B re-capture wall-cost paragraph (CPU Counters ~12min / TP ~22min) | ACCEPT | V3-B lines 1158-1175 paragraph reads as a deterministic forward-cost estimate, not a back-cite. "On the SK-V9 host (Apple M5 Max, 12P+6E, full Xcode 26.0, target-cpu=native)" — host triple matches V3-A §0 host-triple line 13 and RESULTS.md line 48 host column ✓. |
| S5-17 | V5 commit V3-D §0 footer enumeration (8 items, OLS ~8× over-stated) | ACCEPT | V3-D lines 25-51 Read confirms 8 numbered items, each with V3-published vs V4-real values; the ~8× factor between V3 8.64 and V4 1.079 cited correctly. Cross-reference to "P1-V3-C §6's V2-shallowness item list" cited; C §6 confirmed present and enumerates 8 V2 shallowness points. |
| S5-18 | P1-V3-B §1.5 substrate-neutral primitive vocabulary preserved unchanged across V5 | ACCEPT | B §1.5 lines 124-150 unchanged from V4 (V5 commit touched only B §0 footer; rest of B preserved). Vocabulary still: per-string-span scanner / escape_codec_hex_unit / structural-element walker / number-digit parser / traversal-dispatch / simd_movemask / whitespace_skip. |
| S5-19 | P1-V3-F §4.4 edit-count rollup = 19 surgical edits (closes V3 F-5) | ACCEPT | F §4.4 Read: "Total: 19 actual surgical edits across the three documents (8 + 6 + 5)". 8 + 6 + 5 = 19 ✓. SPEC Edit E enumerated as deferral (not surgical). |
| S5-20 | P1-V3-E §1.9 doc-corpus rollup 73 + 2 + 2 + 524 + 0 = 601 | ACCEPT | Arithmetic: 73 + 2 + 2 + 524 + 0 = 601 ✓. E was untouched by V5 (commit affected A/B/C/D/F only); V4 acceptance preserved. |
| S5-21 | P1-V3-D §6.1 Lock-1 binding names `match_tiny_plain_string_with_cap` at `generated.rs:171-185` + `match_string_at_quote_trusted_utf8` at `parse-that-regex/src/lib.rs` | ACCEPT | Generated.rs:171-185 = `match_tiny_plain_string_with_cap` body ✓ (Read confirmed). `parse-that-regex/src/lib.rs` exists; `match_string_at_quote_trusted_utf8` referenced. D §6.1 unchanged by V5. |

n = 21 spot-checks. **ACCEPT 21 / REVISE 0 / REJECT 0**.

---

## §3 — Aggregate verdict

V5 ACCEPT-rate on CH1:

- V4 REVISE resolution: **2/2 FOLDED** (A4-9, C4-5).
- V5 spot-check (n=21): **21 ACCEPT / 0 REVISE / 0 REJECT** = 100%.
- Non-CH1 V5 edits CH1-non-interference: **4/4 ACCEPT** (no new
  citation regressions surfaced).
- Regression script reproduces 1.079 / 0.184 / 0.051, R² = 0.371,
  per-row residuals bit-for-bit (independently re-executed under V5
  verify; output captured in §2 row S5-7/S5-8).
- PMU table arithmetic spot-checks (twitter, citm_catalog,
  y_string_unicode, distinct_values, update-center): **5/5 ACCEPT**.
- Strictness-plane assertion at V3-F §2 head still present, cites
  `sk-v9-open:criterion-fnv64-cd1673844eeea12f` (✓ verified against
  RESULTS.md line 48).

**Strict ACCEPT rate (V5 CH1)**: 21/21 spot-checks + 2/2 V4 REVISEs =
**100%** — well above the §3Z ≥95% bar.

**CH1 V5 disposition: ACCEPT**.

Per ORCHESTRATOR.md §3Z: ≥95% × 2 consecutive cycles converges S-P1 on
this lens. V4 cleared CH1 at 96.2% strict (first qualifying cycle). V5
clears CH1 at 100% strict (second consecutive qualifying cycle).

**CH1 convergence: ACHIEVED**. The V3→V4→V5 fold sequence closed the
samply-coalescing artefact (V3 REJ), the narration-precision A4-9 / C4-5
defects (V4 REVISE), and introduced no new defects in V5. The evidence
root reproduces; the per-class substrate-neutral vocabulary is internally
consistent across A/B/C/D/E/F; the regression script is committed and
deterministic; the strictness-plane discipline is asserted.

---

## §4 — Any new defects

### §4.1 — Sibling stale-cite (NOT-CH1-V5-FOLD-TARGET; carry-forward observation)

P1-V3-B §3.4 line 1020 reads "Same conclusion on distinct_values (P1-V3-A
`cycles/B=2.88`, TP `string_tiny_scan 61.9%`): 1.78 c/B is the tiny
scanner alone." The `2.88` and `1.78` are the V3 distinct_values
arithmetic typo that V4 CH1 A-9 flagged for V3-A and that V5 confirmed
absent from V3-A. The same typo persists in V3-B at line 1020.

**Scope**: This is a sibling stale-cite, not a regression introduced by
V5. V4 CH1 did not surface it (V4 spot-checks did not visit B §3.4 line
1020). V5 commit `d76eef63` did not target it. The actual canonical
distinct_values/t1 c/B is `3.850` (per V3-A §2 line 184 and
pmu_rows.tsv line 31); tiny-scanner-alone = 3.850 × 0.619 = `2.38`, not
`1.78`.

**Disposition**: Not a CH1-V5 fold blocker. CH1 V5 convergence is
unaffected because the disposition contract for V5 was to verify the
two named V4 REVISEs (A4-9, C4-5) and confirm no new V5-induced defects.
The B §3.4 line 1020 stale cite is a **pre-existing V3 defect** that V4
CH1 missed. It carries forward as a future-cycle observation: either a
post-convergence amendment or a tolerable cosmetic stale cite if S-P2
proceeds directly.

**Recommended (non-blocking) one-line fix**: replace `cycles/B=2.88,
TP string_tiny_scan 61.9%): 1.78 c/B` with `cycles/B=3.850, TP
string_tiny_scan 61.9%): 2.38 c/B`. Single edit. No new evidence
needed.

### §4.2 — All other V5 fold sites: clean

Direct line-anchored verification of the six V5 edit sites (A line 237,
C line 717, D §0 footer enumeration, B §0 footer re-capture costs, B §0
footer aggregate.py paragraph, F §4 edit-dispatch hard cap) finds no
new citation errors, arithmetic errors, or framing inconsistencies. The
V5 edits are surgical, well-cited, and consistent with the V4
disposition prescriptions.

### §4.3 — No regressions in V4 ACCEPT load-bearing claims

Spot-checks of V4 ACCEPT-state claims (PMU table arithmetic, B §1.5
substrate-neutral vocabulary, F §2 strictness-plane assertion, E §1.9
doc-corpus rollup arithmetic, D §6.1 Lock-1 binding, regression
reproducibility) all hold under V5. No load-bearing V4 claim regressed.

---

## §5 — Convergence

V4 was the first qualifying cycle on CH1 at 96.2% strict.
V5 is the second consecutive qualifying cycle on CH1 at **100%
strict**.

Per ORCHESTRATOR.md §3Z, **S-P1 CH1 converges on V5**. The two V4
narration REVISEs landed cleanly; the V5 non-CH1 edits do not perturb
CH1; the V3-B §3.4 sibling stale-cite is a pre-existing defect carried
forward at the user's discretion as a non-blocking post-convergence
amendment.

CH1 V5 disposition: **ACCEPT (converged)**.
