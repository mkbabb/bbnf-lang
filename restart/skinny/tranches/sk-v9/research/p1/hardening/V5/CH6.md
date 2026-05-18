# SK-V9 S-P1 V5 CHALLENGE — CH6 ANTI-PAPER-CLOSE (V5 verify)

Pass: S-P1 Profile. Cycle: V5. Lens: CH6.
Cohort: `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-{A..F}.md`
in-place V5 fold committed at `d76eef63`.
Date: 2026-05-18.
Disposition author: CH6 lens agent (V5 re-verify of V4 residual MEDIUM).

Verdict line: **ACCEPT** — the single V4 residual MEDIUM (D should
mirror C §6's V2-error enumeration) is FOLDED at V3-D §0 with eight
honestly-enumerated V3 publication errors, each spot-checkable against
the committed `regression.py` artefact and against the V3 publication
text preserved at commit `c6fb0342`. The five sibling V5 edits (V3-A
§3 line 237, V3-C §5.3 line 717, V3-B §0 footer wall-cost + aggregate
note, V3-F §4 hard cap) introduce no fresh "wired / verified /
complete" claim without backing live evidence. One LOW watch item
remains (V3-D §6.1 carries a stale "9 of 11 rows lives inside the
delimiter contribution" sentence that contradicts the V5-enumerated
"4 of 11 cannot be closed" — a pre-existing V3 inconsistency that
the V5 fold neither created nor closed; the §0 footer is now the
authoritative count and §5.3 already agrees). No new paper-close
axis introduced.

---

## §1 V4 residual MEDIUM resolution — FOLDED

V4-CH6 §4.1 ("D-9: ~8× OLS coefficient correction is not acknowledged
in D body") prescribed a single-line edit to D §0 or D §5.1 footer
naming the V3 publication error. V5 delivered a stronger fold: an
eight-item enumeration mirroring C §6's V2-error list, committed at
D §0 footer (lines 25-51 of the post-V5 file).

### §1.1 Honesty + mirror-of-C-§6 — spot-check on each of the 8 items

V5-D §0 footer item-by-item against the regression.py artefact + the
V3 publication preserved at commit `c6fb0342`:

| # | V5 claim | V3 publication (commit c6fb0342) | regression.py output | Verdict |
|---:|---|---|---|---|
| 1 | V3 a=8.64; real fit 1.079; ~8× over-stated | V3 D §5 line 256: `ns_per_byte = 8.64 * (quotes / bytes) + 1.47 * (numbers / bytes) + 0.410` | OLS a=1.079 (run on disk: 1.079) | **VERIFIED** — both the V3 number and the V4 number are accurate; ratio 8.64/1.079 = 8.01, "~8×" is correct. |
| 2 | V3 b=1.47; real fit 0.184; ~8× over-stated | same V3 line — 1.47 | OLS b=0.184 | **VERIFIED** — ratio 1.47/0.184 = 7.99, "~8×" is correct. |
| 3 | V3 c=0.410; real fit 0.051; ~8× over-stated | same V3 line — 0.410 | OLS c=0.051 | **VERIFIED** — ratio 0.410/0.051 = 8.04, "~8×" is correct. The ~8× ratio's consistency across all three coefficients points to a y-variable-scaling issue in the V3 hand-computation (e.g. `ns / Mbps` vs `1000 / Mbps`), not three independent errors. The footer's diction "OLS coefficients off ~8×" is honest about the magnitude without overclaiming a root-cause diagnosis. |
| 4 | R² absent in V3; V4 publishes 0.371; coefficient b not significant at p=0.545 | V3 D §5 has no R² value anywhere | regression.py: `R^2 = 0.3710`; `p-values (a, b, c) = (0.01936, 0.5448, 0.01342)` | **VERIFIED** — V3 publication has no R²; V4 publishes 0.371 (rounded from 0.3710); p_b = 0.545 (rounded from 0.5448); the not-significant qualifier is honest. |
| 5 | "10% per-quote cut clears 7/11 LOSS rows" V3 forecast superseded; real picture 4 of 11 (unicode_mixed, unicode_escapes, y_string_unicode, gsoc-2018) cannot be closed by a delimiter-only intervention | V3 D §5.3 line 329-330: "**Median reduction = ~7%; mean ≈ 14%.** A 10% cut in per-quote substrate cost moves 7 of 11 losers to parity" | V5-D §5.3 reduction table: y_string_unicode 132%, gsoc-2018 187%, unicode_mixed 290%, unicode_escapes 460% — 4 rows >100% | **VERIFIED** — the V3 "10% / 7 of 11" forecast is quoted accurately; the V5 "4 of 11 cannot be closed" matches the V5-D §5.3 table exactly (the four >100% rows). |
| 6 | "25% covers 9/11" forecast V3-derived; superseded | V3 D §5.3 line 330: "a 25% cut moves 9" | V5-D body absorbs this into the §5.3 table | **VERIFIED** — V3 publication quoted accurately; the V5 fold drops this forecast since it was derived from the inflated coefficients. |
| 7 | OLS sign-convention provenance gap — V3 lacked the script; V4 commits it, reproducible bit-for-bit | V3 publication carries no `regression.py` reference | `/tmp/skv9-xctrace-v3/regression.py` (11395 bytes) + `regression_output.json` (6017 bytes) present on disk; rerun reproduces a=1.079 / b=0.184 / c=0.051 / R²=0.3710 / per-row residuals identical to V5 D §5 table | **VERIFIED** — script reproduces bit-for-bit on rerun. Note: the V5 footer item-7 wording "sign-convention provenance gap" is slightly more decorative than the underlying issue ("V3 lacked the producing script"); the substance is correct but the label is not strictly the issue the footer text actually owns. **LOW cosmetic only — not a paper-close.** |
| 8 | Per-row residual table absent in V3; V4 publishes per-row residuals; uncloseable rows exceed 130-460% of regression's full per-byte budget | V3 D has no per-row residual table | regression.py output lists per-row residuals (unicode_mixed +0.0685, y_string_unicode +0.0662, etc.) | **VERIFIED** — the 130-460% range is the V5-D §5.3 reduction column's range (132% / 187% / 290% / 460%); reading it as "the four uncloseable rows exceed 130-460%" is honest. |

**Mirror-of-C-§6 pattern check.** C §6 enumerates 8 items, each
naming "V2 did NOT do X" specifically and citing where V4 closes it.
D §0 footer enumerates 8 items, each naming "V3 published X; real
fit Y" and citing the regression.py artefact as the V4 closure. The
list structure is parallel; the diction is parallel; the cardinality
is parallel (8 / 8). The mirror is faithful at the form layer.

### §1.2 — Resolution disposition

| V4-CH6 §4.1 prescription | V5 delivery | Status |
|---|---|---|
| Single-line edit to D §5.1 or D §0 footer naming the prior publication error and its magnitude | D §0 footer (26 lines, 8-item enumeration) | **FOLDED + exceeded** — the V5 fold delivers more than V4-CH6 required (the prescription named "single-line edit"; V5 ships an 8-item enumeration matching C §6's pattern). |
| Own the V3 coefficient correction in narrative, not just artefact | Items 1, 2, 3 each name V3 published value + real fit + ~8× over-statement | **FOLDED** |
| Mirror C §6 V2-error enumeration | 8 items mirroring C §6's 8-item structure | **FOLDED** |

V4-CH6 residual MEDIUM (V4-D-9 / V4-X-8) is **CLOSED** in V5.

---

## §2 V5-edits paper-close audit

The V5 fold is six surgical edits across five files (V3-A, V3-B,
V3-C, V3-D, V3-F). Each edit is audited below for fresh "wired /
verified / complete / convergent / closed" claims without backing
live evidence.

### §2.1 — V3-A §3 line 237 (CH1-A4-9 fold)

| V5 claim | Live evidence cited | Verdict |
|---|---|---|
| "The V2 baseline (superseded; see §4 and P1-V3-B §3.4)" | §4 of the same file + P1-V3-B §3.4 (frame-pointer-coalescing falsification) | **VERIFIED** — both citations resolve. §4 of V3-A exists and discusses the dispatch_value 95-99% being inside the fused symbol body. P1-V3-B §3.4 is the canonical falsification of samply mode-I (cited consistently across A/C/F in V4). |
| "every parse-only Track 1 row appears to share the same hot leaf at ~95-99% self-time" (qualified by "appears") | The §3 samply-V2 table immediately above (cites 95.6-99.6%) | **VERIFIED** — the "appears" hedge is properly anchored to V2 data; the next sentence frames this as a frame-pointer-coalescing artefact, not a load-bearing finding. |
| "xctrace Time Profiler with DWARF resolves the inlined leaves and falsifies the single-symbol attribution" | P1-V3-B §3.4 (the canonical falsification) | **VERIFIED** — no new claim; this is the V3-B-shaped finding inherited verbatim. |

No new paper-close axis introduced. The edit transforms a previously
load-bearing "agreement is unambiguous" assertion into a properly-
hedged, properly-superseded V2-baseline framing.

### §2.2 — V3-C §5.3 line 717 (CH1-C4-5 fold)

| V5 claim | Live evidence cited | Verdict |
|---|---|---|
| "among the largest single cycle sinks in the 34-row table" (replaces "the largest single cycle sink") | distinct_values/t1 per-string-span at 3.850 × 0.619 = 2.38 c/B; cf CH1 V4 A4-9 / C4-5 hedges | **VERIFIED** — the arithmetic checks: distinct_values/t1 row in V5-D §1 table is q/B = 0.0638 × Mbps_p = 8,972; bbnf-bench cycles/B for this row from V3-A §2 table is 3.850; per-string-span class share in C §3 for distinct_values is 0.619; product 3.850 × 0.619 = 2.384 ≈ 2.38. The y_string_unicode escape-codec c/B 2.31 is now correctly framed as "among the largest" not strictly "the largest". |

The hedge is an honest narrowing (the prior claim was wrong by a
small margin — 2.31 vs 2.38 — so the hedge is the correct fix, not
a paper-close evasion). The reference to CH1 V4 A4-9 / C4-5 makes
the disposition trail traceable.

### §2.3 — V3-B §0 footer wall-cost addition (CH4-V05/V19/V20 fold)

| V5 claim | Live evidence cited | Verdict |
|---|---|---|
| "xctrace CPU Counters template ~12 min wall for 34 captures" | Implied: P1-V3-A `capture.sh` + p1a/ trace bundles | **VERIFIED at the artefact layer** — `/tmp/skv9-xctrace-v3/capture.sh` exists; the p1a/ directory contains 34 captured trace bundles. The wall-cost figure is reconstruction-by-experience, not directly cited from a timing log on disk, but it is a constraint (not a "wired" claim) and is admissible as scheduling guidance. |
| "xctrace Time Profiler template ~22 min wall for 34 captures" | Implied: P1-V3-B `capture_p1b.sh` + p1b-tp/ trace bundles | **VERIFIED at the artefact layer** — same as above for Time Profiler; the 34 bundles exist. |
| "lto=fat cold-link cost ~3-5 min one-time" | No direct citation; experiential | **WATCH (LOW)** — this is a one-time cost claim with no direct evidence in the report. Admissible as planning guidance, not as a measured value. The "one-time" qualifier prevents this from being a paper-close. |
| "Aggregate ~37-39 min wall" | Sum of the three above | **VERIFIED** — 12 + 22 + 3-5 = 37-39 min, arithmetic is correct. |

No "wired / verified / complete" assertion is introduced; the
language is uniformly cost-estimation, not capability-claim.

### §2.4 — V3-B §0 footer aggregate.py reproducibility note (CH4-V23 fold)

| V5 claim | Live evidence cited | Verdict |
|---|---|---|
| "`aggregate.py` lives at `/tmp/skv9-xctrace-v3/aggregate.py`" | File exists at the cited path (11188 bytes, 304 LOC, verified) | **VERIFIED** |
| "deterministically regenerates `exports/<corpus>__<track>.symbols.json`" | exports/ directory contains 34 + index symbols.json files on disk | **VERIFIED** — `ls /tmp/skv9-xctrace-v3/exports/` lists 34 corpus__trackN.symbols.json files plus the aggregator's index files. |
| "reproducible-by-instruction" | The instruction is: invoke `aggregate.py` against the captured `.trace` bundles | **VERIFIED with MINOR cosmetic** — the prose says aggregate.py "reads the exported `.xml` from `xcrun xctrace export --type tabular --output <out> --input <trace>`". The script *itself* invokes `xcrun xctrace export` (line 116 of aggregate.py: `subprocess.run(["xcrun", "xctrace", "export", ...])`); it does not consume pre-exported XML from a sibling step. The instruction-as-prose conflates "invokes the export call" with "reads pre-exported output"; the substance is correct (running aggregate.py against the trace bundles is sufficient to reproduce the symbols.json output) but the documented workflow is slightly off. **LOW cosmetic only — does not block reproduction.** |

The reproducibility-by-instruction is admissible per ORCHESTRATOR
discipline (CH4-V23 explicitly permits this admission shape). The
script being on disk + executing on rerun is the load-bearing
evidence; the prose framing imprecision is a documentation defect,
not a paper-close.

### §2.5 — V3-F §4 hard cap (CH4-V21 fold)

| V5 claim | Live evidence cited | Verdict |
|---|---|---|
| "≤30 minutes total" for 19 surgical edits | None — instruction, not measurement | **VERIFIED as instruction-shape** — the cap is a constraint on the future dispatch, not a claim about a past dispatch. CH4-V21 prescribed this as an instruction; V5 delivers it as written. |
| Realism check: 19 edits ÷ 30 min = ~1.6 min per edit | Spot-check F §4.1 Edit A: 6-line diff replacement; Edit C: 14-line diff; Edit D: 1-line; Edit E: explicit deferral (no edit); etc. | **REALISTIC** — the edits are diff-shaped (the diff is written verbatim in F §4); applying a verbatim diff to a markdown file is a sub-minute operation. 19 edits × 1.5 min = ~28-30 min including file-switching + commit overhead. The cap is achievable. |
| "Sequenced commits SPEC.md (8) → HANDOFF.md (6) → DISPATCH-PROMPT.md (5)" | Three documents per CH3 / CH4 / CH5 hardening | **VERIFIED** — the sequencing matches F §4.1 / §4.2 / §4.3 partitioning. |
| "Single `git revert` on the batch commit" | Standard git semantics | **VERIFIED** — admissible revert protocol. |

No fresh paper-close. The cap is honestly achievable given the
diff-shaped, no-compilation-cost nature of the edits.

### §2.6 — Cross-edit scan for fresh "wired/verified/complete" claims

Scan of the V5 diff (76 line insertions across 5 files) for the
load-bearing verbs `wired`, `verified`, `complete`, `convergent`,
`closed`, `confirmed`:

- D §0 footer item 4: "V4 publishes 0.371" — backed by regression.py output (verified bit-for-bit on rerun).
- D §0 footer item 7: "V4 commits it, reproducible bit-for-bit" — backed by regression.py + regression_output.json on disk (verified).
- D §0 footer closing: "satisfies the V4 CH6 residual MEDIUM disposition" — meta-claim about CH6 closure, admissible.
- B §0 footer: "deterministically regenerates" — backed by aggregate.py on disk + exports/ directory on disk (verified).
- F §4: no load-bearing verb introduced; the hard cap is instruction-shape.
- A §3 line 237 fold: "falsifies the single-symbol attribution" — backed by P1-V3-B §3.4 (inherited verbatim).
- C §5.3 fold: "among the largest" — hedge, not strengthening claim.

No fresh "wired / verified / complete" claim is introduced without
backing live evidence.

---

## §3 Aggregate verdict

**Disposition: ACCEPT.**

V4-CH6 §4.1 D-9 residual MEDIUM: **FOLDED** at V3-D §0 footer with
8-item enumeration mirroring C §6's V2-error list; each item
spot-checked against regression.py + V3 publication preserved at
commit `c6fb0342`. The V5 fold delivers more than V4-CH6 prescribed.

V5 surgical edits paper-close audit (6 edits across 5 files):

| Edit | Fold disposition | Paper-close audit |
|---|---|---|
| V3-A §3 line 237 (CH1-A4-9) | FOLDED | No fresh paper-close — cites §4 + B §3.4 (live). |
| V3-C §5.3 line 717 (CH1-C4-5) | FOLDED | No fresh paper-close — hedge with arithmetic spot-check (3.850 × 0.619 = 2.38 c/B). |
| V3-D §0 footer (CH6-D V4 residual) | FOLDED + exceeded | No fresh paper-close — every item backed by regression.py output. |
| V3-B §0 footer wall-cost (CH4-V05/V19/V20) | FOLDED | One LOW watch — `lto=fat` cold-link cost is experiential; admissible as scheduling guidance. |
| V3-B §0 footer aggregate.py (CH4-V23) | FOLDED | One LOW cosmetic — prose conflates aggregate.py invoking xctrace with consuming pre-exported XML; substance correct, workflow doc slightly off. |
| V3-F §4 hard cap (CH4-V21) | FOLDED | No fresh paper-close — instruction-shape; 19 edits × ~1.5 min = ~30 min is realistic. |

Aggregate count: 1 V4-residual MEDIUM **CLOSED**, 6 V5 edits all
admissible, 2 LOW watch items (1 experiential cost figure + 1 prose
workflow imprecision), 0 fresh paper-close axes, 0 fresh "wired /
verified / complete" claims without backing.

**On the CH6 axis alone, V5 holds ACCEPT at 100% (no MEDIUM or HIGH
residual).** The two LOW watch items are sub-MEDIUM and do not block.
Combined with V4's 97.0% on the same lens, V5 is the second
consecutive qualifying cycle for CH6, satisfying the §3Z two-cycle
gate on this axis. (The full convergence requires all six lenses
to clear ≥95% on V5.)

---

## §4 Any new paper-close introduced by V5

**None.**

Systematic scan of the V5 fold for fresh paper-close patterns:

- **Completeness claims without live-evidence path.** None. Every
  V5 claim that asserts "V4 publishes X" or "reproducible Y" is
  backed by a file on disk that reproduces or by a sibling-report
  section that exists.
- **Deferrals naming siblings that did not execute.** None. The
  V3-F §4 hard cap is forward-instruction; the §6.6 wave-class
  authorship deferral to S-P3 is unchanged from V4; no new orphan
  consumer is created.
- **Estimates disguised as measurements.** Two LOW watch items: the
  `lto=fat` cold-link 3-5 min cost (experiential, framed as planning
  guidance) and the aggregate.py prose workflow (substance correct,
  documentation slightly off). Neither is a measurement-disguised-as-
  truth; both are admissible.
- **Convergence claims without two-cycle gate.** None. The V5
  fold preserves V4-F §6.3's "Two-consecutive ACCEPT requirement"
  framing; no V5 edit asserts S-P1 convergence.

**§4.1 — One residual structural inconsistency (pre-existing, not
created by V5)**

V5-D §0 footer item 5 ("4 of 11 ... cannot be closed by a delimiter-
only intervention") matches V5-D §5.3 table (4 rows >100%
reduction). V5-D §6.1 still reads "the gap on 9 of 11 rows lives
inside the delimiter contribution; the other 2 (unicode_mixed,
unicode_escapes) sit outside it" — this is the prior V3-fold reading
that conflicts with §5.3's 4-of-11 count. The V5 fold did NOT
touch §6.1; the §0 footer is now the authoritative count and §5.3
agrees with it; §6.1 is a stale prose pocket.

**Severity classification.** LOW — this is a within-report prose
inconsistency, not a paper-close. The authoritative count (4 of 11)
appears in two of the three locations (§0, §5.3); §6.1 is stale.
A V6 cycle (if dispatched) could trivially fix §6.1 to read "4 of
11 rows the gap exceeds the delimiter contribution; 7 of 11 sit
inside it." Not blocking on CH6 because the V5 fold is closing a
*different* defect (the V4 residual MEDIUM) and the V5 fold's
footer is internally consistent with §5.3.

**§4.2 — Closing note**

CH6 ANTI-PAPER-CLOSE returns **ACCEPT** on V5. The V4 residual
MEDIUM (D-9 ~8× coefficient narrative gap) is closed at the D §0
footer with an 8-item enumeration that mirrors C §6's V2-error
pattern, with each item spot-checkable against `regression.py` +
V3 publication. The five sibling V5 edits introduce no fresh
paper-close axis. Two LOW watch items remain (one experiential cost
estimate; one prose workflow imprecision); neither blocks ACCEPT
nor reopens any V4-closed axis.

**V5 = second qualifying cycle on CH6.** Combined with V4's
97.0% ACCEPT on this lens, V5's clean fold satisfies the §3Z
two-consecutive-ACCEPT gate on CH6. Convergence on the full
S-P1 cycle requires the remaining five lenses (CH1-CH5) to also
clear ≥95% on V5.
