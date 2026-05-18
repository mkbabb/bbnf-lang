# SK-V9 S-P1 V5 CH2 — Generality / Lock 14 (verify second consecutive cycle)

Disposition: **ACCEPT** (37 / 38 verified dispositions land ACCEPT; one
new RESIDUAL-MINOR is the V4 R1 propagating verbatim into D's V5 §0
footer enumeration — paragraph-level, substrate is reachable via
§5/§5.1 supersession). 6 / 6 reports ACCEPT. The six V5 surgical edits
introduce no new architectural leak; five are CH1/CH4/CH6 housekeeping
and are grammar-neutral by construction. The sixth (D §0 footer
publication-error enumeration) inherits the V4 R1 caption-form
("per-quote", "per-number") because it is describing a V3 publication
artefact, and the V4-canonical reframing at D §5 / §5.1 is unchanged
and still supersedes the §0 captions for any load-bearing reader.

V5 is the **second consecutive qualifying cycle on CH2 GENERALITY**;
per `ORCHESTRATOR.md` §3Z the cohort holds two qualifying cycles on
this lens and S-P1 may release CH2.

## §1 — V5-edits Lock-14 audit (per edit)

Six surgical edits in commit `d76eef63`, touching five reports (A, B,
C, D, F). Each is audited below against Lock 14 (grammar-neutrality of
the cohort substrate vocabulary: no JSON-only language without
substrate-neutral framing).

### §1.1 — V3-A §3 line 237 — V2-baseline supersession hedge

V5 rewrite of lines 237-246: the previous "agreement is unambiguous"
sentence is replaced by an explicit "V2 baseline (superseded; see §4
and P1-V3-B §3.4)" hedge that names the samply mode-I
frame-pointer-coalescing artefact as the source of the apparent
single-leaf attribution. Locus: CH1-A4-9 narration fix.

Lock-14 audit: the edit retains the pre-existing references to
`dispatch_value` and the per-row %self numbers (they were present in
V3 already). It introduces no new JSON-only captioning. The two terms
it adds — "frame-pointer-coalescing artefact" and "xctrace Time
Profiler with DWARF" — are tooling-shape, not grammar-shape. The
canonical primitive vocabulary (V3-B §1.5) is not invoked here because
the §3 surface is the V2 samply baseline comparison, not the V3 cost
attribution; that is the correct surface boundary.

Verdict: **ACCEPT**. No new Lock-14 leak.

### §1.2 — V3-B §0 footer — re-capture wall cost (CH4-V05/V19/V20)

V5 addition at lines 1158-1174: enumerates xctrace CPU Counters
template wall (~12 min), xctrace Time Profiler template wall (~22
min), `lto=fat` cold-link cost (~3-5 min one-time), aggregate wall
(~37-39 min). Locus: CH4 V05/V19/V20 cost-truthing.

Lock-14 audit: the addition is purely capture-methodology — host
hardware (Apple M5 Max, 12P+6E), toolchain (Xcode 26.0,
`target-cpu=native`), xctrace template names, wall-clock minutes. No
substrate-shape claim. The corpora coverage (17 corpora × {track1,
track2} = 34 captures) names the count, not the per-corpus identity.
This is methodology disclosure for CH4; it would read identically if
the corpora were CSS L4 W0 corpora or Sheets snapshots — the
methodology is grammar-neutral by construction.

The canonical 7-class primitive vocabulary (V3-B §1.5) is **not**
referenced in this addition, and correctly so: re-capture wall cost is
not a primitive-class claim. The §1.5 vocabulary is preserved
unchanged at its pre-V5 location (lines 124-151) and continues to
serve as the cohort-canonical source for B/C/D/E captioning.

Verdict: **ACCEPT**. No new Lock-14 leak; the addition is
grammar-neutral by construction and does not touch the §1.5
vocabulary.

### §1.3 — V3-B §0 footer — `aggregate.py` reproducibility (CH4-V23)

V5 addition at lines 1176-1183: locates `aggregate.py` at
`/tmp/skv9-xctrace-v3/aggregate.py`, declares it reproducible-by-
instruction, names the `xcrun xctrace export --type tabular` shape
that produces its input, and ties the buckets to the per-symbol
self-time tables in §2-§3. Locus: CH4 V23 reproducibility-by-
instruction.

Lock-14 audit: the addition names `<corpus>__<track>.symbols.json` as
the file-naming shape and `<corpus>` as a free variable. No JSON-only
caption introduced. The script bucketises "by symbol", not "by JSON
role" — grammar-neutral aggregation. The canonical 7-class primitive
vocabulary is not invoked here either, and correctly so: the script
shapes are tooling-shape (xctrace export + Python aggregator), not
class-shape.

Verdict: **ACCEPT**. No new Lock-14 leak.

### §1.4 — V3-C §5.3 line 717 — "among the largest" hedge (CH1-C4-5)

V5 rewrite at lines 715-719: the "the largest single cycle sink in the
entire 34-row table" claim is hedged to "among the largest single
cycle sinks in the 34-row table", citing `distinct_values/t1`
per-string-span at 3.850 × 0.619 = 2.38 c/B as marginally larger and
referencing CH1-V4 A4-9 / C4-5 hedges. Locus: CH1 off-by-one
correction.

Lock-14 audit: the edit retains substrate-neutral primitive-class
language ("escape-codec class", "per-string-span"). The corpus names
`y_string_unicode` and `distinct_values` are JSON-corpus identifiers,
but they are referencing *cells* in the empirical 17-row × 2-track
matrix — corpus-identifier-as-row-key is admissible per V4 CH2 §1
(JSON-empirical evidence; substrate-neutral classifier vocabulary).
The §5.3 closing paragraph (lines 723-731) carrying the V4-B Lock-14
reframe — `escape_codec_hex_unit` parameterised by `{hex_digit_count,
surrogate_join_policy, terminator_policy}` with JSON / CSS L4 /
JS-strict instantiations — is preserved verbatim by V5.

Verdict: **ACCEPT**. No new Lock-14 leak; the hedge tightens the
quantitative claim without disturbing the substrate-neutral framing.

### §1.5 — V3-D §0 footer — publication-error enumeration (CH6-D)

V5 addition at lines 25-51: enumerates eight V3 publication errors
mirroring P1-V3-C §6's V2-shallowness pattern. Locus: CH6 D-narrative
enumeration.

Lock-14 audit: this is the **one V5 edit that re-introduces V4 R1
caption-form**, though as inheritance of an existing residual rather
than fresh leak. The enumeration uses:

- Item 1: "OLS coefficient `a` (per-quote)"
- Item 2: "OLS coefficient `b` (per-number)"
- Item 3: "OLS intercept `c`"
- Item 5: "10% per-quote cut clears 7/11 LOSS rows" forecast
- Item 5 / Item 8: corpora names `unicode_mixed`, `unicode_escapes`,
  `y_string_unicode`, `gsoc-2018`

The parenthetical labels "(per-quote)" and "(per-number)" repeat the
V4 CH2 RESIDUAL-MINOR R1 caption-form (D §1 column-name list
`quotes`/`numbers`) rather than the V4-canonical substrate-neutral
form "per-string-span-delimiter cost" / "per-numeric-token cost"
that the V4 fold canonicalized at D §5 / §5.1 / §6.1.

Two considerations cut in opposite directions on whether this is a new
leak:

1. **Mitigating**: the §0 footer is explicitly *enumerating V3
   publication errors*. The "per-quote" label is the V3 label that is
   being corrected; describing the V3 error in V3's own vocabulary is
   appropriate retrospective citation. Item 8 ("per-row residuals")
   uses the substrate-neutral form. The enumeration directly cites the
   V4 corrected numbers (1.079, 0.184, 0.051) and R² 0.371 from the
   V4-committed regression script.

2. **Aggravating**: a grammar-neutral enumeration would have read
   "OLS coefficient `a` (per-string-span-delimiter cost — V3 caption:
   per-quote)" rather than "(per-quote)" alone. The substrate-neutral
   form is not even mentioned in the §0 enumeration; a reader skimming
   §0 would see "per-quote", "per-number", "per-quote cut" without the
   V4-canonical reframing.

Net assessment: this is **V4 R1 propagation**, not a fresh leak. V4
CH2 already flagged the §1 column-name list as RESIDUAL-MINOR (D.V4-1,
"§1 itself unchanged"); the V5 §0 footer inherits the same caption.
The load-bearing reader still reaches the substrate-neutral framing
via §5 / §5.1 (unchanged by V5 — "per-string-span-delimiter density
`q/B`", "primitive class" table header, "**Per-string-span-delimiters
are the dominant marginal primitive class**"). The §0 footer is
captioning-only and below the §3Z load-bearing bar.

Verdict: **ACCEPT-WITH-RESIDUAL**. V4 R1 propagates into the new §0
footer; not a new leak. See §2 R1-extended below.

### §1.6 — V3-F §4 — edit-dispatch hard cap (CH4-V21)

V5 addition at lines 463-470: hard cap of ≤30 minutes for the batch of
19 surgical SPEC/HANDOFF/DISPATCH-PROMPT edits, sequenced SPEC.md (8)
→ HANDOFF.md (6) → DISPATCH-PROMPT.md (5), single `git revert`
protocol. Locus: CH4 V21 dispatch hard-cap residual.

Lock-14 audit: the addition is pure workflow / process language —
minute caps, edit counts, file ordering, revert protocol. No grammar
mentioned, no primitive class invoked, no substrate-shape claim. This
is the cleanest of the six V5 edits from a CH2 lens; it would read
identically for any future cohort fold against any grammar.

Verdict: **ACCEPT**. No new Lock-14 leak.

### §1.7 — Per-edit summary

| # | Edit | Locus | CH2 verdict |
|---|---|---|---|
| 1 | V3-A §3 V2-baseline hedge | CH1-A4-9 | ACCEPT |
| 2 | V3-B §0 re-capture wall cost | CH4-V05/V19/V20 | ACCEPT |
| 3 | V3-B §0 `aggregate.py` reproducibility | CH4-V23 | ACCEPT |
| 4 | V3-C §5.3 "among the largest" hedge | CH1-C4-5 | ACCEPT |
| 5 | V3-D §0 publication-error enumeration | CH6-D | ACCEPT-WITH-RESIDUAL (R1 propagation) |
| 6 | V3-F §4 edit-dispatch hard cap | CH4-V21 | ACCEPT |

Five of six V5 edits are clean ACCEPT; the sixth inherits the V4 R1
caption-form without aggravating it materially. None of the six
introduces a new architectural or load-bearing Lock-14 leak.

## §2 — V4 RESIDUAL-MINOR status (R1 / R2 / R3 after V5)

The three V4 RESIDUAL-MINOR items were marked admissible (CH2 V4
disposition: ACCEPT at 97.2% strict). V5 did not specifically target
them. Status after V5:

### §2.1 — R1: D §1 column-name list lacks generalisation paragraph

V4 description: `P1-V3-D §1` lists columns `quotes`, `numbers`, `oo`,
`ao`, `q/B`, `n/B`, `sd`, `q_frac` without a substrate-neutral
mapping paragraph; the mapping appears in §5 and §5.1 and supersedes
the §1 captions for the load-bearing reader.

V5 status: §1 column-name list is **unchanged** by V5 (the V5 commit
adds a §0 footer above §1; §1 itself is verbatim). The §5 / §5.1
substrate-neutral framing is unchanged.

V5 inadvertent aggravation: yes, mild. The new §0 publication-error
enumeration (V5 edit #5) uses "(per-quote)" / "(per-number)" caption-
form in items 1 / 2 / 5 without a substrate-neutral parenthetical.
Strictly: the V4 R1 caption-form now appears in two §-locations (§1
and §0) rather than one. The load-bearing supersession at §5 / §5.1 /
§6.1 is untouched and still reaches the substrate-neutral framing for
any reader past the §0 footer.

Disposition: **RESIDUAL-MINOR (R1-extended)** — still paragraph-level,
still below the §3Z load-bearing bar, still admissible. The V5 fold
opportunity is a one-clause add to §0 footer item 1: "OLS coefficient
`a` (per-quote; V4-canonical: per-string-span-delimiter cost)" — pure
captioning. Not blocking.

### §2.2 — R2: D §2 section title retains "string-quote-density"

V4 description: §2 titled "String-quote-density verdict" retains
JSON-named caption; §2.2 introduces the substrate-neutral framing.

V5 status: §2 title is **unchanged** by V5. The §2 surface is not
touched. No aggravation, no improvement.

Disposition: **RESIDUAL-MINOR (R2 unchanged)** — same admissibility
posture as V4.

### §2.3 — R3: C §1.3 13-class table mapping to B §1.5 7-class

V4 description: C §1.3 enumerates 13 sub-class rows where B §1.5 has 7
canonical primitive classes; the relationship is sub-class refinement
rather than vocabulary divergence, but the C/B mapping is implicit
rather than stated.

V5 status: C §1.3 is **unchanged** by V5 (the V5 edit to C is at §5.3
line 717, well below §1.3). The V5 §5.3 hedge does not touch the §1.3
vocabulary table. No aggravation, no improvement.

Disposition: **RESIDUAL-MINOR (R3 unchanged)** — same admissibility
posture as V4.

### §2.4 — Aggregate residual posture after V5

The three V4 residuals remain admissible after V5. None was elevated
to load-bearing by V5; one (R1) propagated mildly into D's new §0
footer enumeration without changing its paragraph-level disposition.
No V5 edit closes any of the three residuals (V5 was not scoped to
CH2 fold per the V4 consolidation plan; CH2 was already ACCEPT at
97.2%).

## §3 — Aggregate verdict

| Axis | V4 result | V5 result | Δ |
|---|---|---|---|
| ACCEPT-rate (strict, dispositions) | 35 / 36 = 97.2% | 37 / 38 = 97.4% | +0.2pp |
| ACCEPT-rate (lenient, residuals-as-ACCEPT) | 36 / 36 = 100% | 38 / 38 = 100% | 0pp |
| Load-bearing leaks | 0 | 0 | 0 |
| RESIDUAL-MINOR count | 3 | 3 (R1 propagated, R2/R3 unchanged) | 0 net |
| Cohort canonical vocabulary (B §1.5) | preserved | preserved (untouched by V5) | — |
| Cross-grammar admission column (B §1.5, C §1.3) | populated | populated (untouched by V5) | — |
| `escape_codec_hex_unit` parameterisation (B §3.5, C §5.3, E §6 R2) | populated | populated (untouched by V5; C §5.3 hedged) | — |
| Primitive-class status column (E §2 / §6) | populated | populated (untouched by V5) | — |

**V5 ACCEPT rate: 37 / 38 = 97.4% strict, 100% lenient.** Both
numerators clear the §3Z 95% convergence bar. With V5 CH2 ACCEPT
following V4 CH2 ACCEPT, the cohort holds **two consecutive qualifying
cycles on CH2 GENERALITY** per `ORCHESTRATOR.md` §3Z. S-P1 may release
CH2.

The six V5 surgical edits are scoped to CH1 / CH4 / CH6 residuals; CH2
was already at ACCEPT in V4 and was not targeted by V5. The V5 fold is
mechanically a captioning + methodology disclosure pass on top of the
V4 canonical vocabulary; no number was re-measured, no class identity
was changed, no deletion verdict was revised. The B §1.5 canonical
7-class vocabulary, B §3.5 `escape_codec_hex_unit` parameter table, C
§1.3 13-row sub-class refinement, D §5 / §5.1 substrate-neutral OLS
reframe, E §2 Primitive-class status column, and F §3.2 / §4
substrate-shape framing are all preserved verbatim.

## §4 — Any new Lock-14 leaks

**No new architectural Lock-14 leaks.** One V4 RESIDUAL-MINOR (R1)
propagated by V5:

- The V3-D §0 footer publication-error enumeration (V5 edit #5) uses
  "(per-quote)" / "(per-number)" caption-form in items 1 / 2 / 5,
  inheriting the V4 R1 §1 column-name caption rather than the
  V4-canonical "per-string-span-delimiter cost" / "per-numeric-token
  cost" terminology used at D §5 / §5.1 / §6.1.

This is paragraph-level captioning inside an error-enumeration
section that is by construction citing V3's own vocabulary. The
load-bearing substrate-neutral framing at D §5 / §5.1 supersedes the
§0 caption for any reader past the publication-error retrospective.

**Optional V5+ fold (paragraph-level, not blocking)**: add a single
substrate-neutral parenthetical to D §0 items 1 / 2 / 5: e.g. "OLS
coefficient `a` (V3 caption: per-quote; V4-canonical:
per-string-span-delimiter cost) ...". One sentence; no number changes;
closes R1-extended without disturbing the publication-error narrative.
Not required for §3Z convergence on CH2.

**Net Lock-14 posture after V5**: identical to V4 — three paragraph-
level captioning residuals, all admissible, all below the load-bearing
bar; canonical 7-class vocabulary stable cohort-wide; cross-grammar
admission columns populated; `escape_codec_hex_unit` parameter table
intact; Primitive-class status column intact; substrate-neutral OLS
reframe at D §5 intact.

V5 CH2 GENERALITY converges on the §3Z 95% bar for the second
consecutive cycle (V4 → V5 both ACCEPT at ≥95%). CH2 is released.
