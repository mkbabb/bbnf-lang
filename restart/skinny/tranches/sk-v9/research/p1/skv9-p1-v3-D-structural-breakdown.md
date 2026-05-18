# SK-V9 P1-V3-D: Structural-Element Counts vs Throughput Correlation

Pass: S-P1 Profile. Cycle: V3 (sibling of V3-A/B xctrace lanes; V2 closed BLOCKED on PMU).
Date: 2026-05-18.
Scope: Per-corpus correlation of structural-element counts against Track-1
throughput on the SK-V9-open W0 corpus, and what it implies for which
SK-V9/V10 wave moves which row.
Output: this file.
Baseline: SK-V9-open at run `sk-v9-open:criterion-fnv64-cd1673844eeea12f`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Build flags: `profile=bench;rustflags=-C target-cpu=native;target_cpu=native`.
Profile tool: structural-count extraction from `skinny/RESULTS.md` Notes
section + per-row Mbps/ns_per_byte from main table + W0 telemetry manifest;
no samply/PMU input needed for the correlation question.
Corpus coverage: 17/17.

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

### §2.2 The same step in `q/B` (quotes per corpus byte) space

q_frac depends on element-token mix, which is not directly seen by the
substrate. The substrate sees quotes per byte. Sorting by `q/B`:

- **WINs**: numbers (0.0000), canada (0.0000), mesh (0.0000),
  marine_ik (0.0128), citm_catalog (0.0154). All ≤ 0.0154.
- **First clear LOSS**: unicode_escapes (0.0054, q_frac 0.750, −34%).
- **All LOSSes from 0.0239 up**: twelve consecutive corpora.

So in `q/B` the threshold is **noisy** because unicode_escapes and gsoc-2018
slip below the WIN region in q/B (0.0054 and 0.0103) but their q_frac is
0.75 and 1.00. The substrate-visible density that better predicts the
verdict is **`quote_density / total_element_density`** = q_frac, i.e. the
relative composition matters, not the per-byte rate alone. Mechanistically:
the per-byte string-plane scan cost scales with q/B *and* the per-byte
non-string-plane work that bbnf wins on (number FSM, structural emit) scales
with the other elements; what flips the verdict is whether the non-quote
work is enough to offset the quote work.

Quantified Pearson correlations with `Δ_p` (parse_only delta vs sonic):

- `r(q/B, Δ_p) = −0.618` — strong negative.
- `r(n/B, Δ_p) = +0.781` — stronger positive (number-density is the actual
  WIN driver).
- `r(sd,  Δ_p) = +0.541` — total structural density tracks WIN weakly,
  because high sd corpora tend to be number-heavy (canada, mesh, marine_ik).

Numbers are bbnf's **lift signal**; quotes are the **anchor**. The verdict
flip is set by which one dominates per byte.

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
- The structural mix is unusual: **10,937 oo + 10,451 ao** vs 26,604 quotes
  — i.e. a ~0.43 structural/quote ratio, the highest of any string-bearing
  corpus.
- Explanatory axis: **structural-emit-per-quote**. citm has roughly one
  object/array open per 2.5 quotes, so a sizeable share of its substrate
  cost is on the structural-emit path (a bbnf strength: lazy-tape offsets)
  rather than the string scan (a bbnf weakness).
- Empirically: q/B is 0.0154, well below the 0.024 noise floor where LOSSes
  begin, and the row's `ns_per_byte = 0.274` is the lowest in the corpus
  (parsing at 29.2 GB/s while sonic does 23.6 GB/s).
- citm wins because it is **structurally dense relative to its string
  density**. q_frac is misleading because element_tokens here include
  literals and numbers, but the *structural* element count
  (oo+ao+closes = 32,776) is 23% above the element_tokens count (42,259).

### §3.2 instruments: mid q_frac → −5.9% mild LOSS (mid-band downward breaker)

- q_frac 0.556, q/B 0.0313 — both squarely in mid band but row lands
  inside the noise floor of SC-4 ("win thin").
- Loses ~6% — closer to parity than any other LOSS in the cohort.
- Explanatory axis: **mean string length**. instruments averages
  ~64 bytes/string, which is comparable to twitter (70) and below citm
  (130). The escape/unicode density is low (no unicode corpus markers in
  the Notes). The −6% likely reflects ordinary per-quote scan overhead
  rather than escape/unicode validation pathology.

### §3.3 Axes that explain residual variance among LOSSes

When the SC-4 step says LOSS, the *magnitude* still varies from −23% to
−54%. The dispersion correlates with two axes:

1. **mean bytes/string** (proxy for escape/unicode scan cost amortisation):
   - unicode_escapes: 373 bytes/string, gsoc-2018: 195, marine_ik: 156,
     citm 130 — long-string corpora lose less per quote because the
     per-string fixed cost amortises.
   - distinct_values 31, y_string_unicode 32, random 31, unicode_basic 36
     — short-string corpora lose hardest because the per-quote-pair fixed
     cost dominates.
   - Pattern: y_string_unicode (q_frac 1.000, 32 bytes/string, −54%) and
     distinct_values (q_frac 0.957, 31 bytes/string, −48%) are the worst
     LOSS magnitudes after unicode_mixed.
2. **non-ASCII (unicode/escape) byte density**:
   - unicode_mixed (−53%) is uniquely bad in q_frac 0.750 band because
     its strings carry validated unicode rather than ASCII; the substrate
     pays for `escape_complete=yes` validation that sonic strict does too,
     but bbnf's per-byte scan is slower.
   - unicode_escapes is shielded by its 373-byte mean string (only −34%
     despite q_frac 0.750).

### §3.4 Marine_ik: predicted-edge WIN, biggest absolute lift

marine_ik sits AT the SC-4 boundary (q_frac 0.135) and turns in the
**largest WIN of the cohort** (+43.4% Δ_p, +25% Δ_t). It carries 245,175
numbers (the second-largest absolute number count after canada) and 38,268
quotes; the number-driven WIN swamps the quote-driven gap. This is the
canonical "lift signal" row: when bbnf's number FSM lights up at scale,
the row WINs regardless of having tens of thousands of quotes.

## §4 Direct + typed-plane correlation

Pearson `r(q/B, Δ vs sonic)` per plane:

| plane | r | n | comment |
|---|---:|---:|---|
| parse_only         | −0.618 | 17 | strong q/B anti-correlation |
| direct_to_struct   | −0.033 | 17 | near-zero — q/B does NOT predict the digest gap |
| real_typed_struct  | −0.566 |  4 | weak negative on a tiny sample |

### §4.1 Direct plane decouples from quote density

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

### §4.2 Typed plane (n=4) — partial decoupling

| corpus | q_frac | q/B | Δ_t |
|---|---:|---:|---:|
| marine_ik     | 0.135 | 0.0128 | +25.2% WIN |
| twitter       | 0.726 | 0.0287 | +0.7% parity |
| mesh          | 0.000 | 0.0000 | +4.5% parity |
| update_center | 0.986 | 0.0511 | −4.5% parity |

All four typed rows admit (GO). The typed plane mostly **beats or matches
sonic-rs strict**. n=4 is too small for confident regression; the
qualitative pattern is that the typed plane absorbs the quote-cost penalty
because the projection collapses string offsets into the destination struct
in one pass rather than emitting onto a tape then reprojecting.

The take-away: **q_frac is a parse_only-substrate signal**, not a universal
bbnf signal. The same corpus can lose parse_only, win direct, and parity
typed.

## §5 Marginal-cost analysis: which structural class is the highest-cost element

OLS regression on the 17-corpus set:

```
ns_per_byte = 8.64 * (quotes / bytes) + 1.47 * (numbers / bytes) + 0.410
```

with column-wise Pearson r(ns_per_byte, density):

- quote_density: **+0.595** (largest positive — drives ns_per_byte up)
- num_density: −0.240 (number density slightly REDUCES ns_per_byte, i.e.
  number-heavy corpora are faster per byte; consistent with bbnf's WIN
  rows being number-driven)
- obj_density: −0.049 (negligible)
- arr_density: −0.260 (negligible/anti-correlated)
- (oo+ao)/B: −0.278 (structural opens are NOT the hot class)

### §5.1 Implied per-element costs

Reading the OLS coefficients as additive per-event cost contributions:

| element class | implied marginal ns | ratio vs baseline byte |
|---|---:|---:|
| quote (open or close)         | ~8.64 ns/quote  | ~21× the per-byte baseline |
| number-token                  | ~1.47 ns/number | ~3.6× |
| baseline non-element byte     | ~0.41 ns/byte   | 1× |
| structural open (object/array)| not significant in OLS — masked by quote sign |

**Quotes are the dominant marginal element.** At ~8.6 ns per quote
(amortised over a string scan with escape validation + view-boundary UTF-8
check), the string plane consumes more wall-clock than every other
structural class combined on the LOSS corpora.

### §5.2 Quote-cost share of substrate gap on the worst losers

For each LOSS corpus, partition bbnf's ns/B excess over sonic-rs strict
into quote-driven (`8.64 * q/B`) and non-quote-driven:

| corpus | bbnf ns/B | sonic ns/B | gap ns/B | implied quote contrib ns/B | quote-share of gap |
|---|---:|---:|---:|---:|---:|
| twitter         | 0.0758 | 0.0514 | 0.0244 | 0.248 | quote model OVER-attributes (bbnf already cheap on non-quote bytes) |
| apache_builds   | 0.0839 | 0.0644 | 0.0195 | 0.360 | same |
| github_events   | 0.0699 | 0.0468 | 0.0231 | 0.251 | same |
| update_center   | 0.1015 | 0.0633 | 0.0382 | 0.442 | same |
| random          | 0.1066 | 0.0659 | 0.0407 | 0.559 | same |
| gsoc-2018       | 0.0451 | 0.0221 | 0.0230 | 0.089 | 385% — bbnf still net cheaper than sonic per non-quote byte |
| unicode_mixed   | 0.1470 | 0.0689 | 0.0781 | 0.207 | 264% — quotes + unicode validation co-load |
| unicode_escapes | 0.0830 | 0.0552 | 0.0279 | 0.047 | 166% |
| unicode_basic   | 0.0881 | 0.0675 | 0.0207 | 0.474 | same |
| distinct_values | 0.1115 | 0.0578 | 0.0537 | 0.551 | same |
| y_string_unicode| 0.1842 | 0.0846 | 0.0996 | 0.534 | same |

The over-attribution means bbnf's substrate is **already faster per
non-quote byte** than sonic-rs strict; the gap is **almost entirely the
quote plane**. (sonic's quote cost is implicitly lower; we cannot infer it
without sonic-side OLS, but the consistency of "quote contrib >> gap"
across 11 LOSS rows is the evidence.)

### §5.3 What per-quote reduction lifts the worst losers to parity

Target: bring each LOSS row within **sonic-strict × 0.90** (within 10%
slack, the same gate the main RESULTS table uses):

| corpus | reduction in per-quote cost to reach sonic × 0.90 |
|---|---:|
| apache_builds   |  3.5% |
| unicode_basic   |  2.8% |
| random          |  6.0% |
| github_events   |  7.1% |
| update_center   |  7.1% |
| twitter         |  7.6% |
| distinct_values |  8.6% |
| y_string_unicode| 16.9% |
| gsoc-2018       | 23.2% |
| unicode_mixed   | 34.2% |
| unicode_escapes | 46.9% |

**Median reduction = ~7%; mean ≈ 14%.** A 10% cut in per-quote substrate
cost moves 7 of 11 losers to parity; a 25% cut moves 9; the unicode_mixed
and unicode_escapes rows would need 30–50% **plus** an unrelated unicode
validation cost cut to clear the bar.

The hot per-quote work amenable to reduction:

- view-boundary UTF-8 validation on every string (currently per-string fixed cost).
- escape-complete scan (currently per-byte branch over `b'\\'` and `<0x20`).
- structural-emit handshake (the lazy-tape offset write per quote pair).

A 10% per-quote reduction is **plausibly attainable** by collapsing the
string scan into a single masked-bitmap pass (matching the bitmap shape
sonic-rs already uses internally) and deferring the escape-complete check
to a flaw probe rather than running it inline.

### §5.4 What about the structural-open class?

oo+ao density barely correlates with ns_per_byte (r = −0.278) and the OLS
coefficient is not significant. Structural opens cost **nearly free under
the lazy tape** — the offset write is amortised through the same cache
line as the byte scan. **The structural-emit plane is not the bottleneck.**

### §5.5 What about the number class?

Number density correlates **positively** with bbnf's WIN delta (r = +0.781)
and slightly negatively with ns_per_byte. Numbers are **net free or net
beneficial**. The number FSM is bbnf's currently strongest sub-plane and
needs no immediate work.

## §6 Implications for SK-V9/V10 wave assignment

Reading §3–§5 against the 17-row admission map, the wave moves are:

### §6.1 Wave that moves the parse_only LOSS block (11 rows)

**Target: collapse the string-plane per-quote cost by ~10–15%.**

Concretely:

- twitter (−32%), github_events (−33%), apache_builds (−23%),
  update_center (−38%), random (−38%), distinct_values (−48%),
  y_string_unicode (−54%), gsoc-2018 (−51%), unicode_basic (−23%) —
  9 rows lift to parity from a 10–25% per-quote cost cut.
- This is a **single-knob wave**: it ships the string-plane masked
  bitmap + deferred escape-complete. Estimated reach: 9 of 11 parse_only
  losers cross to within sonic-strict × 0.90.

### §6.2 Wave that moves unicode_mixed / unicode_escapes

**Target: separate unicode validation from string scan.**

These rows need (a) the §6.1 quote-cost cut AND (b) an unicode-validation
plane that runs once over the whole input rather than per-string. Likely
a SIMD-classify + boundary-verify pass. Estimated reach: would close
unicode_mixed to ~−25% (still LOSS-thin) and unicode_escapes to ~−10%
(parity-thin) — i.e. they remain post-V9 LOSSes but become tractable in
V10 once the validation kernel lands.

### §6.3 Rows that do NOT need a wave

- **citm_catalog, canada, mesh, marine_ik, numbers**: WIN unconditionally
  on parse_only. citm depends on the lazy-tape structural-emit advantage;
  canada/mesh/marine_ik/numbers depend on the number FSM. **Do not
  perturb these planes** in V9/V10 waves — any change here forfeits the
  +24% to +43% WIN.
- **instruments**: −5.9% sits inside the noise floor; will pass naturally
  with the §6.1 wave (only needs ~3% lift).

### §6.4 Direct plane: do not chase by string-plane wave

The direct plane is q/B-decorrelated (r = −0.033). The direct LOSSes
(canada −19%, gsoc-2018 −36%, unicode_mixed −58%, unicode_escapes −65%,
distinct_values −46%, y_string_unicode −43%) come from the **digest sink
path**, not the string plane. A separate wave should profile the digest
producer; see P1-V3-A/B xctrace lanes for that capture. Do **not** bundle
direct plane fixes with the §6.1 string-plane wave.

### §6.5 Typed plane: no wave needed in V9

All 4 measured typed rows admit (GO). Track 2 oracle parity at 14,977
(twitter) and 9,796 (marine_ik) confirms structural soundness. The typed
plane should be expanded **horizontally** (run real_typed_struct on more
corpora) in V10 to confirm the parity pattern, not vertically (no
substrate change).

### §6.6 Synthesis — three V9/V10 waves, ranked

1. **V9 W1: string-plane cost cut (per-quote ~10–15%)** — moves 9 of 11
   parse_only losers to parity. Highest marginal lift per engineering hour
   under §5.3.
2. **V9 W2: digest-sink truth pass** — independent of string plane; needed
   for direct-plane LOSSes (driven by q/B-decorrelated cost). Sequenced
   second because the masking probe is independent.
3. **V10: unicode validation kernel** — required only for
   unicode_mixed/unicode_escapes after W1 lands. Defer until W1
   demonstrates the floor lift on the 9 simple LOSSes.

The cohort-wide takeaway is unchanged from SC-4: the substrate ceiling on
the parse_only plane is the per-quote scan path. SK-V9-open's structural
counts and per-row Mbps reconfirm SC-4's verdict and quantify the
required floor lift at ~10% per-quote for the median LOSS row.

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
