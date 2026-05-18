# SK-V9 P1-V3-D: Structural-Element Counts vs Throughput Correlation

Pass: S-P1 Profile. Cycle: V3 (sibling of V3-A/B xctrace lanes; V2 closed BLOCKED on PMU).
Date: 2026-05-18.
Scope: Per-corpus correlation of structural-element counts against Track-1
throughput on the SK-V9-open W0 corpus. Findings are diagnostic; wave
authorship belongs to S-P3.
Output: this file.
Baseline: SK-V9-open at run `sk-v9-open:criterion-fnv64-cd1673844eeea12f`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `profile=bench;rustflags=-C target-cpu=native;target_cpu=native`.
Profile tool: structural-count extraction from `skinny/RESULTS.md` Notes
section + per-row Mbps/ns_per_byte from main table + W0 telemetry manifest;
no samply/PMU input needed for the correlation question.
Corpus coverage: 17/17.

## §0 V4 fold footer

V4 fold: wave authorship deferred to S-P3 per F1; REDRESS material
differentials cited per F3; Lock-1 cardinality binding per F6; regression
script + R²/residuals committed per F5 at
`/tmp/skv9-xctrace-v3/regression.py` with output
`/tmp/skv9-xctrace-v3/regression_output.json`.

**V3 publication errors enumerated (V5 fold per CH6-D):** The V4 fold
revealed eight load-bearing V3 errors in this report that the
regression-script commit surfaced and corrected:

1. **OLS coefficient `a` (per-quote)**: V3 published 8.64; real fit
   1.079. ~8× over-stated.
2. **OLS coefficient `b` (per-number)**: V3 published 1.47; real fit
   0.184. ~8× over-stated.
3. **OLS intercept `c`**: V3 published 0.410; real fit 0.051. ~8×
   over-stated.
4. **R² absent in V3**: V4 publishes 0.371 (modest fit; coefficient `b`
   is not statistically significant at p=0.545).
5. **"10% per-quote cut clears 7/11 LOSS rows" forecast**: V3 derived
   from inflated coefficients; gone in V4. Real picture: 4 of 11
   (unicode_mixed, unicode_escapes, y_string_unicode, gsoc-2018) cannot
   be closed by a delimiter-only intervention because the throughput
   gap exceeds the entire delimiter contribution.
6. **"25% covers 9/11" forecast**: V3-derived; superseded.
7. **OLS sign-convention provenance gap**: V3 lacked the script that
   produced the coefficients; V4 commits it, reproducible bit-for-bit.
8. **Per-row residual table absent in V3**: V4 publishes per-row
   residuals showing the four uncloseable rows exceed 130-460% of the
   regression's full per-byte budget — a hypothesis-sized finding, not
   a wave-sized intervention.

This enumeration mirrors P1-V3-C §6's V2-shallowness item list and
satisfies the V4 CH6 residual MEDIUM disposition.

## §1 The correlation table (all 17 corpora)

Columns (definitions):
- `bytes` — corpus byte length from W0 manifest `bytes=` field.
- `quotes`, `numbers`, `oo` (object opens), `ao` (array opens) — from `skinny/RESULTS.md`
  Notes section "lazy tape materialization" lines.
- `q/B`, `n/B`, `sd` — densities (count / corpus bytes). `sd` = total structural
  elements / bytes where total = oo + ao + closes + quotes + numbers + literals.
- `q_frac` = `quotes / (quotes + numbers + literals)` (SC-4's "string fraction").
- `Mbps_p / Mbps_d / Mbps_t` — parse_only / direct_to_struct /
  real_typed_struct throughput, Track 1.
- `Δ_p` — parse_only delta vs sonic-rs strict (same-run-native anchor) in %.
- `pred_outcome` — SC-4 step-function prediction by q_frac
  (≤0.135 WIN, ≥0.726 LOSS, else MIXED).
- `actual` — WIN if Δ_p > +5%, LOSS if Δ_p < −5%, PARITY otherwise.

| corpus | bytes | quotes | q/B | numbers | n/B | oo | ao | tot_struct | sd | q_frac | Mbps_p | Mbps_d | Mbps_t | Δ_p | pred | actual |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|---|
| numbers          |  150,124 |      0 | 0.0000 |  10,001 | 0.0666 |     0 |     1 |  10,003 | 0.067 | 0.000 | 17,956 | 12,177 | n/a    | +38.4% | WIN | WIN |
| canada           | 2,251,051 |     12 | 0.0000 | 111,126 | 0.0494 |     4 | 56,045 | 223,236 | 0.099 | 0.000 | 16,190 |  9,475 | n/a    | +27.2% | WIN | WIN |
| mesh             |   723,597 |     11 | 0.0000 |  73,013 | 0.1009 |     3 |  3,610 |  80,250 | 0.111 | 0.000 | 12,435 |  8,489 |  8,919 | +10.2% | WIN | WIN |
| unicode_escapes  | 1,050,797 |  5,636 | 0.0054 |   1,877 | 0.0018 | 1,879 |     1 |  11,274 | 0.011 | 0.750 | 12,047 |  4,821 | n/a    | −33.6% | LOSS | LOSS |
| gsoc-2018        | 3,327,831 | 34,128 | 0.0103 |       0 | 0.0000 | 3,793 |     0 |  41,714 | 0.013 | 1.000 | 22,184 | 14,362 | n/a    | −51.0% | LOSS | LOSS |
| marine_ik        | 2,983,466 | 38,268 | 0.0128 | 245,175 | 0.0822 | 9,680 | 28,377 | 359,563 | 0.121 | 0.135 | 12,073 |  8,696 | 11,259 | +43.4% | WIN(edge) | WIN |
| citm_catalog     | 1,727,204 | 26,604 | 0.0154 |  14,392 | 0.0083 | 10,937 | 10,451 |  85,035 | 0.049 | 0.630 | 29,215 | 20,229 | n/a    | +23.8% | MIXED | WIN |
| unicode_mixed    | 1,053,086 | 25,121 | 0.0239 |   8,371 | 0.0079 | 4,187 |     2 |  41,870 | 0.040 | 0.750 |  6,803 |  4,215 | n/a    | −53.1% | LOSS | LOSS |
| twitter          |   631,515 | 18,099 | 0.0287 |   2,109 | 0.0033 | 1,264 |  1,050 |  29,573 | 0.047 | 0.726 | 13,188 | 11,166 | 14,761 | −32.2% | LOSS(edge) | LOSS |
| github_events    |    65,132 |  1,891 | 0.0290 |     149 | 0.0023 |   180 |    19 |   2,526 | 0.039 | 0.889 | 14,302 | 11,430 | n/a    | −33.0% | LOSS | LOSS |
| instruments      |   220,346 |  6,889 | 0.0313 |   4,935 | 0.0224 | 1,012 |   194 |  14,793 | 0.067 | 0.556 | 16,189 | 11,327 | n/a    |  −5.9% | MIXED | LOSS(thin) |
| apache_builds    |   127,275 |  5,289 | 0.0416 |       2 | 0.0000 |   884 |     3 |   7,068 | 0.056 | 0.999 | 11,917 | 10,577 | n/a    | −23.3% | LOSS | LOSS |
| update_center    |   533,178 | 27,229 | 0.0511 |       0 | 0.0000 | 1,896 |  1,937 |  35,281 | 0.066 | 0.986 |  9,857 |  7,245 | 11,345 | −37.6% | LOSS | LOSS |
| unicode_basic    | 1,048,586 | 57,590 | 0.0549 |  11,518 | 0.0110 | 5,759 |  5,760 |  92,146 | 0.088 | 0.833 | 11,348 |  8,179 | n/a    | −23.4% | LOSS | LOSS |
| y_string_unicode |    35,601 |  2,200 | 0.0618 |       0 | 0.0000 |     0 |     1 |   2,202 | 0.062 | 1.000 |  5,428 |  4,583 | n/a    | −54.1% | LOSS | LOSS |
| distinct_values  |   153,630 |  9,796 | 0.0638 |     440 | 0.0029 |   440 |     1 |  11,118 | 0.072 | 0.957 |  8,972 |  5,761 | n/a    | −48.1% | LOSS | LOSS |
| random           |   510,476 | 33,005 | 0.0647 |   5,002 | 0.0098 | 4,001 |  1,001 |  49,011 | 0.096 | 1.000 |  9,382 |  7,590 | n/a    | −38.1% | LOSS | LOSS |

Rows sorted by `q/B` ascending; the WIN→LOSS transition is sharply visible
between marine_ik (0.0128, +43%) / citm_catalog (0.0154, +24%) and the
quote-dense block above 0.024.

## §2 String-quote-density verdict

### §2.1 Reconfirmation of SC-4's `q_frac` (quotes / element_tokens) step

Applied to the SK-V9-open Mbps in §1 the SC-4 thresholds yield 13/13 correct
verdicts among the non-mid corpora:

- `q_frac ≤ 0.135` predicts WIN: numbers (0.000, +38.4%), canada (0.000,
  +27.2%), mesh (0.000, +10.2%), marine_ik (0.135, +43.4%) — 4/4.
- `q_frac ≥ 0.726` predicts LOSS: twitter (0.726, −32%),
  unicode_mixed/escapes (0.750, −53%/−34%), unicode_basic (0.833, −23%),
  random/gsoc-2018/y_string_unicode (≥0.846, −38%/−51%/−54%),
  github_events (0.889, −33%), distinct_values (0.957, −48%),
  update_center (0.986, −38%), apache_builds (0.999, −23%) — 9/9.
- Mid band `0.135 < q_frac < 0.726`: instruments (0.556, −5.9%) and
  citm_catalog (0.630, +23.8%) — SC-4 explicitly disclaims a verdict here
  and §3 explains why citm splits up.

The step function survives the SK-V9-open rerun **without flipping a single
non-mid row**. The flip point is unchanged in q_frac space: `≤0.135` wins,
`≥0.726` loses.

### §2.2 The same step in `q/B` (string-span-delimiters per corpus byte) space

q_frac depends on element-token mix, which is not directly seen by the
substrate. The substrate sees per-byte string-span-delimiter density.
Sorting by `q/B`:

- **WINs**: numbers (0.0000), canada (0.0000), mesh (0.0000),
  marine_ik (0.0128), citm_catalog (0.0154). All ≤ 0.0154.
- **First clear LOSS**: unicode_escapes (0.0054, q_frac 0.750, −34%).
- **All LOSSes from 0.0239 up**: twelve consecutive corpora.

So in `q/B` the threshold is **noisy** because unicode_escapes and gsoc-2018
slip below the WIN region in q/B (0.0054 and 0.0103) but their q_frac is
0.75 and 1.00. The substrate-visible density that better predicts the
verdict is **`delimiter_density / total_element_density`** = q_frac, i.e.
the relative composition matters, not the per-byte rate alone. Mechan-
istically: the per-byte string-span scan cost scales with q/B *and* the
per-byte non-span work that bbnf wins on (numeric-token FSM, structural-
element emit) scales with the other elements; what flips the verdict is
whether the non-delimiter work is enough to offset the delimiter work.

Quantified Pearson correlations with `Δ_p` (parse_only delta vs sonic;
values reproduced from `/tmp/skv9-xctrace-v3/regression_output.json`):

- `r(q/B, Δ_p) = −0.618` — strong negative.
- `r(n/B, Δ_p) = +0.781` — stronger positive (numeric-token density is
  the actual WIN driver).
- `r(sd,  Δ_p) = +0.541` — total structural-element density tracks WIN
  weakly, because high-sd corpora tend to be numeric-token-heavy (canada,
  mesh, marine_ik).

Numeric tokens are bbnf's **lift signal**; string-span delimiters are the
**anchor**. The verdict flip is set by which one dominates per byte.

### §2.3 Banded mean `Δ_p` by `q/B`

| q/B band         | n | mean Δ_p | composition |
|---|---:|---:|---|
| [0.000, 0.050)   | 12 | −7.4% | mixed (3 WINs offset 9 LOSSes); the SC-4 step is hiding inside this band, gated by q_frac/n_frac |
| [0.050, 0.100)   |  5 | −40.3% | uniform LOSS: update_center, random, unicode_basic, distinct_values, y_string_unicode |

q/B alone is **insufficient** as the admission predicate; q_frac is.

## §3 Breakers + additional axes

SC-4's step function admits two MIXED-band rows. SK-V9-open separates them:

### §3.1 citm_catalog: low q/B + mid q_frac → +24% WIN (mid-band upward breaker)

- q_frac 0.630 sits inside SC-4's mid band but the row WINs at +23.8%.
- The structural mix is unusual: **10,937 oo + 10,451 ao** vs 26,604
  string-span delimiters — i.e. a ~0.43 structural-element-to-delimiter
  ratio, the highest of any span-bearing corpus.
- Explanatory axis: **structural-element-emit-per-delimiter**. citm has
  roughly one object/array open per 2.5 delimiters, so a sizeable share
  of its substrate cost is on the structural-element-emit path (a bbnf
  strength: lazy-tape offsets) rather than the string-span scan (a bbnf
  weakness).
- Empirically: q/B is 0.0154, well below the 0.024 noise floor where
  LOSSes begin, and the row's `ns_per_byte = 0.274` is the lowest in
  the corpus (parsing at 29.2 GB/s while sonic does 23.6 GB/s).
- citm wins because it is **structurally dense relative to its string-
  span density**. q_frac is misleading because element_tokens here
  include literals and numeric tokens, but the *structural-element*
  count (oo+ao+closes = 32,776) is 23% above the element_tokens count
  (42,259).

### §3.2 instruments: mid q_frac → −5.9% mild LOSS (mid-band downward breaker)

- q_frac 0.556, q/B 0.0313 — both squarely in mid band but row lands
  inside the noise floor of SC-4 ("win thin").
- Loses ~6% — closer to parity than any other LOSS in the cohort.
- Explanatory axis: **mean string-span length**. instruments averages
  ~64 bytes/span, which is comparable to twitter (70) and below citm
  (130). The escape/unicode density is low (no unicode corpus markers in
  the Notes). The −6% likely reflects ordinary per-string-span-delimiter
  scan overhead rather than escape/unicode validation pathology.

### §3.3 Axes that explain residual variance among LOSSes

When the SC-4 step says LOSS, the *magnitude* still varies from −23% to
−54%. The dispersion correlates with two axes:

1. **mean bytes per string-span-delimiter pair** (proxy for unicode-escape
   primitive amortisation):
   - unicode_escapes: 373 bytes/span, gsoc-2018: 195, marine_ik: 156,
     citm 130 — long-span corpora lose less per delimiter because the
     per-span fixed cost amortises.
   - distinct_values 31, y_string_unicode 32, random 31, unicode_basic 36
     — short-span corpora lose hardest because the per-delimiter-pair
     fixed cost dominates.
   - Pattern: y_string_unicode (q_frac 1.000, 32 bytes/span, −54%) and
     distinct_values (q_frac 0.957, 31 bytes/span, −48%) are the worst
     LOSS magnitudes after unicode_mixed.
2. **non-ASCII (unicode/escape) byte density**:
   - unicode_mixed (−53%) is uniquely bad in q_frac 0.750 band because
     its spans carry validated unicode rather than ASCII; the substrate
     pays for `escape_complete=yes` validation that sonic strict does too,
     but bbnf's per-byte scan is slower.
   - unicode_escapes is shielded by its 373-byte mean span (only −34%
     despite q_frac 0.750).

**REDRESS material differential note (F3, CH3 D-3).** Any candidate
intervention motivated by these unicode rows touches the per-quartet /
per-segment unicode-escape classifier class rejected on the exact rows
above: REDRESS 82 closed the four-`\uXXXX` AArch64 classifier on
`unicode_escapes` / `unicode_mixed` / `y_string_unicode`, and REDRESS 59
permanently rejected the UTF-8 fusion class on the close route. A
successor intervention is admissible only with a same-row falsification
gate (the differential vs each rejected shape, the rows that must
improve, the rows that must not regress, and the hot-leaf threshold that
flags the gate) — wave-class authoring belongs to S-P3 per F1.

### §3.4 Marine_ik: predicted-edge WIN, biggest absolute lift

marine_ik sits AT the SC-4 boundary (q_frac 0.135) and turns in the
**largest WIN of the cohort** (+43.4% Δ_p, +25% Δ_t). It carries 245,175
numeric tokens (the second-largest absolute numeric-token count after
canada) and 38,268 string-span delimiters; the numeric-token-driven WIN
swamps the delimiter-driven gap. This is the canonical "lift signal" row:
when bbnf's numeric-token FSM lights up at scale, the row WINs regardless
of having tens of thousands of string-span delimiters.

## §4 Direct + typed-plane correlation

Pearson `r(q/B, Δ vs sonic)` per plane:

| plane | r | n | comment |
|---|---:|---:|---|
| parse_only         | −0.618 | 17 | strong q/B anti-correlation |
| direct_to_struct   | −0.033 | 17 | near-zero — q/B does NOT predict the digest gap |
| real_typed_struct  | −0.566 |  4 | weak negative on a tiny sample |

### §4.1 Direct plane decouples from string-span-delimiter density

Sorted by Δ_d vs sonic (direct):

| corpus | q_frac | q/B | Δ_d |
|---|---:|---:|---:|
| apache_builds   | 0.999 | 0.0416 | **+16.6%** WIN-direct |
| unicode_basic   | 0.833 | 0.0549 | **+14.5%** WIN-direct |
| marine_ik       | 0.135 | 0.0128 | +9.7% WIN-direct |
| citm_catalog    | 0.630 | 0.0154 | +7.9% WIN-direct |
| instruments     | 0.556 | 0.0313 | +3.0% near parity |
| numbers         | 0.000 | 0.0000 | −1.3% parity |
| mesh            | 0.000 | 0.0000 | −9.1% mild LOSS |
| random          | 0.846 | 0.0647 | −12.7% LOSS |
| twitter         | 0.726 | 0.0287 | −18.3% LOSS |
| canada          | 0.000 | 0.0000 | −18.9% LOSS |
| github_events   | 0.889 | 0.0290 | −22.9% LOSS |
| update_center   | 0.986 | 0.0511 | −25.1% LOSS |
| gsoc-2018       | 1.000 | 0.0103 | −36.1% LOSS |
| y_string_unicode| 1.000 | 0.0618 | −42.6% LOSS |
| distinct_values | 0.957 | 0.0638 | −46.5% LOSS |
| unicode_mixed   | 0.750 | 0.0239 | −57.6% LOSS |
| unicode_escapes | 0.750 | 0.0054 | **−64.6%** worst |

Direct-plane verdicts contradict SC-4 in **both directions**:

- apache_builds (q_frac 0.999) WINS direct (+16.6%) — q_frac is anti-predictive here.
- unicode_basic (q_frac 0.833) WINS direct (+14.5%) — same.
- canada (q_frac 0.000) LOSES direct (−18.9%) — number-heavy WIN flips to LOSS.
- mesh (q_frac 0.000) LOSES direct (−9.1%) — same flip.

Mechanistically the digest sink pre-allocates a fixed-size hasher and feeds
it raw bytes; the cost is **byte-walk dominated, not element-dominated**.
The parse_only plane runs a structural-emit producer over a lazy tape; the
typed plane projects a typed struct. Their substrate cost profiles are
different and **the q_frac step function only describes the parse_only
plane**.

**REDRESS material differential note (F3, CH3 D-2).** The direct-plane
decorrelation is a *diagnostic finding*; it is not a proposal to redesign
the digest path. REDRESS 66–69 close the digest-sink-redesign class
(direct source-hook field-folding, parser-owned decoded scratch,
byte-output `unescape`, DirectBuild semantic-string-fact) — all REJECTED.
REDRESS 93 routes any further direct-guard-row work to a dedicated
direct-output-contract or control-path tranche. The finding admits a
*profile-only* follow-up; no structural intervention is admissible
without that tranche, and wave-class authoring belongs to S-P3 per F1.

### §4.2 Typed plane (n=4) — partial decoupling

| corpus | q_frac | q/B | Δ_t |
|---|---:|---:|---:|
| marine_ik     | 0.135 | 0.0128 | +25.2% WIN |
| twitter       | 0.726 | 0.0287 | +0.7% parity |
| mesh          | 0.000 | 0.0000 | +4.5% parity |
| update_center | 0.986 | 0.0511 | −4.5% parity |

All four typed rows admit (GO). The typed plane mostly **beats or matches
sonic-rs strict**. n=4 is too small for confident regression; the
qualitative pattern is that the typed plane absorbs the per-string-span-
delimiter-cost penalty because the projection collapses string-span
offsets into the destination struct in one pass rather than emitting
onto a tape then reprojecting.

The take-away: **q_frac is a parse_only-substrate signal**, not a universal
bbnf signal. The same corpus can lose parse_only, win direct, and parity
typed.

## §5 Marginal-cost analysis: which primitive class is the highest-cost element

Provenance: the OLS coefficients, R², residuals, p-values, and Pearson
correlations below are emitted by the committed regression script at
`/tmp/skv9-xctrace-v3/regression.py`; the run output lives at
`/tmp/skv9-xctrace-v3/regression_output.json`. The 17 input rows are the
§1 correlation table verbatim with `ns_per_byte = 1000 / Mbps_p`. Per F5
the script is the regression's source of truth; the per-row residuals in
this section are a direct read of `regression_output.json`.

OLS regression on the 17-corpus set (predictors:
per-string-span-delimiter density `q/B` and per-numeric-token density
`n/B`; response: parse_only `ns_per_byte`):

```
ns_per_byte ≈ 1.079 · (q/B) + 0.184 · (n/B) + 0.051
                 SE=0.409     SE=0.296      SE=0.018
                 p=0.0194     p=0.5448      p=0.0134
R²  = 0.371      df_resid = 14      RSS = 0.0135
```

Per-row residuals (`y − ŷ`) — sorted by §1 row order:

| corpus | y (ns/B) | ŷ (ns/B) | resid |
|---|---:|---:|---:|
| numbers          | 0.0557 | 0.0635 | −0.0078 |
| canada           | 0.0618 | 0.0604 | +0.0014 |
| mesh             | 0.0804 | 0.0699 | +0.0106 |
| unicode_escapes  | 0.0830 | 0.0574 | +0.0256 |
| gsoc-2018        | 0.0451 | 0.0623 | −0.0173 |
| marine_ik        | 0.0828 | 0.0802 | +0.0026 |
| citm_catalog     | 0.0342 | 0.0694 | −0.0352 |
| unicode_mixed    | 0.1470 | 0.0785 | +0.0685 |
| twitter          | 0.0758 | 0.0828 | −0.0070 |
| github_events    | 0.0699 | 0.0830 | −0.0131 |
| instruments      | 0.0618 | 0.0891 | −0.0274 |
| apache_builds    | 0.0839 | 0.0961 | −0.0122 |
| update_center    | 0.1015 | 0.1064 | −0.0050 |
| unicode_basic    | 0.0881 | 0.1126 | −0.0245 |
| y_string_unicode | 0.1842 | 0.1180 | +0.0662 |
| distinct_values  | 0.1115 | 0.1206 | −0.0092 |
| random           | 0.1066 | 0.1229 | −0.0163 |

The R² = 0.371 says the two-density model explains only ~37% of the
17-corpus ns/B variance; the largest positive residuals (unicode_mixed
+0.069, y_string_unicode +0.066, unicode_escapes +0.026) cluster on the
unicode-escape rows whose excess cost is not a per-string-span-delimiter
phenomenon. The OLS is JSON-specific; the abstraction (per-primitive-class
marginal cost) generalises across grammars, but the coefficients fit one
substrate at one revision.

Column-wise Pearson correlations (full table at `regression_output.json`):

- `r(q/B, ns/B)` = **+0.595** (largest positive — per-string-span density
  drives ns/B up).
- `r(n/B, ns/B)` = −0.240 (numeric-token density slightly REDUCES ns/B,
  consistent with bbnf's WIN rows being numeric-token-driven).
- `r(oo/B, ns/B)` = −0.049 (negligible).
- `r(ao/B, ns/B)` = −0.260 (negligible / anti-correlated).
- `r((oo+ao)/B, ns/B)` not separately fit; per §5.4 structural-element
  opens are not the hot class.

### §5.1 Implied per-primitive-class costs

Reading the OLS coefficients as additive per-event cost contributions
(with the R²=0.371 / p_b=0.54 caveat that the numeric-token coefficient is
not statistically distinguishable from zero on this sample):

| primitive class                         | implied marginal ns | ratio vs baseline byte | p-value |
|---|---:|---:|---:|
| per-string-span-delimiter cost          | ~1.08 ns/delimiter  | ~21× baseline (~0.051 ns/B) | 0.019 |
| per-numeric-token cost                  | ~0.18 ns/token      | ~3.6×  | 0.545 (not significant) |
| baseline non-element byte               | ~0.051 ns/byte      | 1×     | 0.013 |
| per-structural-element open/close       | not significant in OLS — masked by string-span-delimiter sign |  |  |

**Per-string-span-delimiters are the dominant marginal primitive class
on the parse_only plane.** At ~1.08 ns per delimiter (amortised over a
string-span scan with escape validation + view-boundary UTF-8 check), the
string-span plane consumes more wall-clock than every other primitive
class combined on the LOSS corpora. The 21× ratio is the diagnostic
signature; the OLS R² is modest (0.371) so any successor intervention
must demonstrate the marginal cost on out-of-sample fixtures before
admitting the coefficient as a wave knob (per F5 falsifiability
discipline).

### §5.2 String-span-delimiter cost share of substrate gap on the worst losers

For each LOSS corpus, partition bbnf's ns/B excess over sonic-rs strict
into per-string-span-delimiter-driven (`1.079 * q/B`) and non-delimiter-
driven contributions. The contribution column reads directly from the
committed regression coefficient:

| corpus | bbnf ns/B | sonic ns/B | gap ns/B | delimiter contrib (1.079·q/B) | delimiter-share of gap |
|---|---:|---:|---:|---:|---:|
| twitter         | 0.0758 | 0.0514 | 0.0244 | 0.0309 | 127% (over-attributes; bbnf cheap on non-delimiter bytes) |
| apache_builds   | 0.0839 | 0.0644 | 0.0195 | 0.0449 | 230% — same |
| github_events   | 0.0699 | 0.0468 | 0.0231 | 0.0313 | 135% — same |
| update_center   | 0.1015 | 0.0633 | 0.0382 | 0.0551 | 144% — same |
| random          | 0.1066 | 0.0659 | 0.0407 | 0.0698 | 172% — same |
| gsoc-2018       | 0.0451 | 0.0221 | 0.0230 | 0.0111 |  48% (bbnf undershoots; non-delimiter cost still cheaper than sonic) |
| unicode_mixed   | 0.1470 | 0.0689 | 0.0781 | 0.0258 |  33% — delimiter contribution alone undershoots; unicode-escape primitive co-load is the rest |
| unicode_escapes | 0.0830 | 0.0552 | 0.0279 | 0.0058 |  21% — same |
| unicode_basic   | 0.0881 | 0.0675 | 0.0207 | 0.0593 | 287% — over-attributes |
| distinct_values | 0.1115 | 0.0578 | 0.0537 | 0.0688 | 128% — over-attributes |
| y_string_unicode| 0.1842 | 0.0846 | 0.0996 | 0.0667 |  67% — delimiter contribution undershoots; unicode-escape primitive picks up the residual |

The over-attribution on 7 of 11 rows means bbnf's substrate is **already
faster per non-delimiter byte** than sonic-rs strict; the gap is **almost
entirely the string-span-delimiter plane** on those rows. Where the
delimiter coefficient *undershoots* (gsoc-2018, unicode_mixed,
unicode_escapes, y_string_unicode) the OLS fails to absorb the unicode-
escape per-quartet primitive cost — those rows demand a separate
primitive class beyond the q/B + n/B regression's reach (the R²=0.371
caveat lives here).

### §5.3 What per-string-span-delimiter reduction lifts the worst losers to parity

Diagnostic target: bring each LOSS row within **sonic-strict × 0.90**
(within 10% slack, the same gate the main RESULTS table uses). Reduction
percentages are computed against the OLS per-delimiter coefficient:

| corpus | reduction in per-string-span-delimiter cost to reach sonic × 0.90 |
|---|---:|
| apache_builds   |  29% |
| unicode_basic   |  24% |
| twitter         |  68% (gap exceeds delimiter contribution; non-delimiter work also moves) |
| github_events   |  60% (same) |
| update_center   |  60% (same) |
| random          |  44% (same) |
| distinct_values |  68% (same) |
| y_string_unicode| 132% (gap > delimiter contribution; unicode-escape primitive dominates) |
| gsoc-2018       | 187% (same) |
| unicode_mixed   | 290% (same) |
| unicode_escapes | 460% (same) |

**Diagnostic finding.** The per-string-span-delimiter reduction needed to
bring even the cheaper losers to parity exceeds the coefficient's own
confidence interval; on 4 of 11 rows the gap exceeds the entire
delimiter contribution, meaning a delimiter-only intervention cannot
close those rows. The unicode-escape rows (y_string_unicode,
unicode_mixed, unicode_escapes) are dominated by the per-quartet
primitive class, not by the per-delimiter class. This is a hypothesis-
sized finding, not a wave-sized intervention.

The hot per-string-span-delimiter work that could plausibly be reduced:

- view-boundary UTF-8 validation on every span (per-span fixed cost).
- escape-complete scan (per-byte branch over `b'\\'` and `<0x20`).
- structural-emit handshake (lazy-tape offset write per delimiter pair).

Each item touches a substrate route REDRESS has previously rejected. Per
F3, REDRESS 60 (boundary collapse), 61 (always-wide retained trusted
scan), 62 (delayed-wide retained trusted scan), 83 (StringBlock16 tiny
probe), 84 (object-pair value-byte control compaction) all closed the
string-scanner-widening class on the same rows. REDRESS 64 closed the
retained Unicode-escape run validator. Any successor intervention must
demonstrate a material differential against each cited rejection on a
same-row falsification gate — wave authorship belongs to S-P3 per F1.

### §5.4 What about the structural-element-open class?

oo+ao density barely correlates with ns_per_byte (r = −0.260) and the OLS
coefficient on per-structural-element-open/close opens/closes is not
distinguishable from zero. Structural elements cost **nearly free under
the lazy tape** — the offset write is amortised through the same cache
line as the byte scan. **The structural-element plane is not the
bottleneck.**

### §5.5 What about the numeric-token class?

Numeric-token density correlates **positively** with bbnf's WIN delta
(r = +0.781) and slightly negatively with ns_per_byte (r = −0.240).
Numeric tokens are **net free or net beneficial**. The numeric-token FSM
is bbnf's currently strongest sub-plane and needs no immediate work.

## §6 Diagnostic findings against the 17-row admission map

This section catalogues the *findings* §3–§5 surface, against the 17-row
admission map. Each finding is a candidate input to S-P3 wave authoring
— not a wave itself.

### §6.1 Parse_only LOSS-block finding: per-string-span-delimiter cost dominates

The 11 parse_only LOSS rows cluster on the per-string-span-delimiter
plane: the OLS coefficient at §5.1 puts ~1.08 ns/delimiter at ~21× the
baseline byte cost (p=0.019). The §5.3 reduction table shows the gap on
9 of 11 rows lives inside the delimiter contribution; the other 2
(unicode_mixed, unicode_escapes) sit outside it.

**REDRESS material differential note (F3, CH3 D-1; CH5 §4.1, F6).** A
candidate intervention on this finding REPLACES the existing string-
scanner pair on the production hot path — `match_tiny_plain_string_with_cap`
at `runtime/src/grammars/json/generated.rs:171-185` and
`match_string_at_quote_trusted_utf8` at `parse-that-regex/src/lib.rs` —
running alongside the existing scanner constitutes a sidecar producer
and fails Lock 1 (substrate cardinality stays at one; per `LOCKS.md` Lock
1 a "SIMD mask stream is a transient producer, not a retained sidecar").
The string-scanner-widening class on these same rows was rejected by
REDRESS 60 (boundary collapse), 61 (always-wide retained trusted scan),
62 (delayed-wide retained trusted scan), 83 (StringBlock16 tiny probe),
and 84 (object-pair value-byte control compaction). The retained
Unicode-escape run validator was rejected by REDRESS 64. Any successor
intervention must demonstrate a material differential against each
cited rejection on a same-row falsification gate; this report stops at
the diagnostic, and wave-class authoring belongs to S-P3 per F1.

### §6.2 Unicode-row finding: per-quartet primitive class dominates residual

unicode_mixed and unicode_escapes residuals at §5.1 (+0.069, +0.026) and
the §5.3 reduction table (290%, 460% per-delimiter cuts needed) say the
delimiter coefficient does not absorb the unicode cost on these rows.
The unicode-escape primitive is a distinct class.

**REDRESS material differential note (F3, CH3 D-3).** REDRESS 82
rejected the four-`\uXXXX` AArch64 classifier on exactly these rows.
REDRESS 59 permanently rejected the UTF-8 fusion class on the close
route. Any successor intervention must articulate the differential
against each cited entry on a same-row falsification gate. Wave-class
authoring belongs to S-P3 per F1.

### §6.3 Rows with no LOSS finding

- **citm_catalog, canada, mesh, marine_ik, numbers**: WIN unconditionally
  on parse_only. citm depends on the lazy-tape structural-element-emit
  advantage; canada/mesh/marine_ik/numbers depend on the numeric-token
  FSM. The §6.1 / §6.2 findings name no intervention that would touch
  these planes; any successor wave must guard them. REDRESS 71 (twitter,
  update_center typed-GO) and REDRESS 81 (mesh, marine_ik typed-GO)
  bind the admitted-row guard.
- **instruments**: −5.9% sits inside the noise floor.

### §6.4 Direct-plane finding: q/B decoupled from digest gap

The direct plane is `q/B`-decorrelated (r = −0.033). The direct LOSSes
(canada −19%, gsoc-2018 −36%, unicode_mixed −58%, unicode_escapes −65%,
distinct_values −46%, y_string_unicode −43%) do not load on the
per-string-span-delimiter plane; they live in the digest-sink-producer
cost profile (see §4.1's separate decorrelation analysis). Per the §4
F3 note, the digest-sink-redesign class is closed by REDRESS 66–69 + 93;
any further direct-plane work routes to a dedicated direct-output-
contract or control-path tranche.

### §6.5 Typed-plane finding: 4/4 measured rows admit

All 4 measured typed rows admit (GO). Track 2 oracle parity at 14,977
(twitter) and 9,796 (marine_ik) confirms structural soundness. The
finding admits a *horizontal* follow-up — run `real_typed_struct` on
more corpora — and forbids a substrate-change follow-up. No new
`BackendShape` is proposed; per CH5 §4.5 the substrate union holds.

### §6.6 Wave authorship deferred to S-P3

Wave-class selection and per-wave cost set (LOC, risk, owner files,
same-wave consumer, revert) are S-P3 scope per
`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`. This S-P1 report
supplies the diagnostic findings; S-P3 picks waves.

## §7 Sources

- `skinny/RESULTS.md` lines 3–42 (main verdict table) and Notes lines
  85–137 ("lazy tape materialization" per-corpus structural counts);
  W0 Telemetry Manifest lines 44–86 (`bytes=`, `ns_per_byte=`).
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md`
  §2 "Quantified Correlation" (quote-fraction thresholds 0.135 / 0.726).
- `restart/skinny/tranches/sk-v9/research/p1/p1d-pmu-cycles.md`
  (V2 BLOCKED on PMU; this report uses structural-count correlation as the
  substitute lane).
- `restart/skinny/tranches/sk-v9/research/p1/p1f-results-delta.md`
  (SK-V9-open Δ-vs-SK-V8 deltas; consulted to ensure §1 Mbps are the W0
  rerun numbers and not stale).
- `/tmp/skv9-xctrace-v3/regression.py` (V4 F5 — OLS regression script
  emitting the §5.1 coefficients, R², residuals, p-values, and Pearson
  correlations from the §1 row data).
- `/tmp/skv9-xctrace-v3/regression_output.json` (regression run output;
  per-row residuals and per-coefficient SE / t / p directly readable).
- `skinny/REDRESS.md` entries 59, 60, 61, 62, 64, 66–69, 82, 83, 84, 93
  (material-differential anchors cited in §3.3, §4, §5.3, §6.1, §6.2,
  §6.4 per V4 F3).
- `restart/locks/LOCKS.md` Lock 1 substrate union (substrate cardinality
  binding for §6.1 per V4 F6).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` (wave authorship
  scope per V4 F1; §6.6).
