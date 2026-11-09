# Bench Honesty Audit — SK-V20 Research Pass (agent 14)

Scope: every >SOTA claim in the skinny proving tree, traced from claim sites in
tranche docs back to the bench harnesses that produce the numbers. Sources inspected:
`skinny/crates/bbnf-bench/benches/json_parity.rs`,
`skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs`,
`skinny/crates/bbnf-bench/src/sonic_skipper.rs`,
`skinny/crates/bbnf-bench/src/nonjson_css_l4.rs`,
`skinny/crates/bbnf-bench/src/css_l4_w8.rs`,
G6-LEDGER.md, H1-LEDGER.md, FRAMING.md (sk-v18/waves/h1/),
skv18 research/p1/a0-rebaseline-ledger.md,
skv17 W0/W2/W5 ledgers, sk-v17/sk-v18 SYNTHESIS docs, skinny/RESULTS.md,
restart/corpora/SOTA.md.

---

## 1 — CSS claim: `track1_rich` vs lightningcss

### What each side measures

`track1_rich` (`css_canon_bench.rs:111-124`) calls `parser::rich_summary()` which:
(a) parses the stylesheet into the single sealed offset tape, then (b) walks the tape
lazily via `ValueRef` spans, re-deriving every typed field from `(source, offset)`. No
node is allocated to the payload arena — `write_count == 0`, asserted in runtime tests.
The nine returned fields are aggregate counts: rules, at_rules, qualified_rules,
declarations, selectors, dimensions, numbers, colors, functions.
`CssTypedValue::classify` (`nonjson_css_l4.rs:204-226`) identifies the value head token
from the **leading byte** only. Selectors are counted by scanning for top-level commas
in the prelude span.

`lightningcss` (`css_canon_bench.rs:126-129`) calls
`StyleSheet::parse(input, ParserOptions::default())` and feeds only
`sheet.rules.0.len()` (a single `usize`) to `black_box`. The parse itself builds an
EAGER, OWNED CSSOM: every property value decomposed to typed variants, every selector
component list fully constructed, all nodes heap-allocated via SmallVec and CowArcStr.
The `black_box(sheet.rules.0.len())` idiom keeps the parse live without returning the
sheet — this is the standard optimizer-barrier pattern, not an under-measurement
of lightningcss's work.

### The asymmetries — what H1 FRAMING.md discloses

FRAMING.md (`waves/h1/FRAMING.md:1-43`) names both asymmetries explicitly:

1. **Retention**: `track1_rich` retains nothing (lazy re-derivation from tape, zero
   arena writes). lightningcss retains the full owned CSSOM.
2. **Depth**: `track1_rich` classifies the LEADING token of each declaration value
   and counts top-level selector commas. lightningcss walks the full owned value
   and selector trees.

FRAMING.md also scopes the parity oracle: the 9-field cssparser equality oracle
(`assert_rich_strict_equality`, `nonjson_css_l4.rs:128-136`) uses the same
head-level definition as the tape projection — `record_leading_typed_value` at
`nonjson_css_l4.rs:204`. The oracle certifies head-level depth agreement, NOT full-tree
depth parity with lightningcss. This is stated in FRAMING.md lines 33-43.

The H1-LEDGER.md (`waves/h1/H1-LEDGER.md:1-15`) quotes the disclosure and marks the
claim as `materialization_framing = lazy-rich-vs-eager-cssom` with both asymmetries
(retention AND depth) disclosed. The SK-V18 SYNTHESIS §C2 characterises it as
"lazy-rich-summary beats eager-full-CSSOM."

### The figures and their thermal status

G6-LEDGER §5 figures (N=100, fat-LTO, opt-level=3, cold per-parse):

```
corpus                    track1_rich   lightningcss   ratio
bootstrap                 2357.5 MB/s   1120.8 MB/s    2.10×
tailwindcss               2737.3 MB/s    837.8 MB/s    3.27×
material-components-web   2413.4 MB/s   1286.3 MB/s    1.88×
animate                   1824.8 MB/s   1217.0 MB/s    1.50×
```

**All committed runs are load-depressed.** The A0 re-baseline ran under loadavg
4.35/6.03/5.70 (`a0-rebaseline-ledger.md:§LOAD CAVEAT`) and is explicitly labelled
non-re-lockable. The H1 figures ran at loadavg 6.79 (`H1-LEDGER.md:29`) and are
marked DIRECTIONAL. The G6-LEDGER.md does not record host_loadavg for its figures.
The W0 quiet baseline (`skv17-W0-baseline-ledger.md §2`) locked the lightningcss
denominator at bootstrap 1112.4 / tailwindcss 841.3 / material 1292.3 / animate
1218.7 Mbps from a genuinely quiet run — but that baseline does not have a matching
quiet `track1_rich` figure because `track1_rich` did not exist at W0 (the workload
was created by the bit-rot fix `784ceb418`, noted in `a0-rebaseline-ledger.md:§CANONICAL-BENCH`).

No quiet-clean (loadavg < 1.0) `track1_rich` run exists in any committed document.
Every absolute Mbps figure for `track1_rich` is load-depressed.

The H1-LEDGER explicitly acknowledges this and flags outcome S, pending upgrade to A
on a quiet re-capture. But no such re-capture has occurred.

### CSS claim grade

**DISCLOSED-RESIDUAL.** Both asymmetries (retention and depth) are named in FRAMING.md.
The claim is honestly scoped as "lazy-rich head-level vs eager full-CSSOM." The binding
H1 close remains an open residual: no quiet-clean `track1_rich` absolute Mbps exists;
ratio claims are directionally valid (load affects both planes uniformly in same-run);
absolute claims are not certifiable from the current corpus. The MASTER-PLAN.md and
some BENCH.md passages cite the ratios without the depth/retention qualifier inline —
a reader seeing those sites does not automatically land in FRAMING.md.

---

## 2 — JSON claim: tape offset vs sonic-rs Skipper

### What each side measures (parse_only plane)

`track1_generated` (`json_parity.rs:53-73`) calls
`runtime::generated_json::parse_only(black_box(input))` and asserts
`result.is_ok()`. The `parse_only` function is the distinct no-tape path: it
recognises JSON structure and returns `Ok(())` with no tape build and no allocation
(`skinny/RESULTS.md` rows record `warmup_iters=0; output plane = parse_only`).

`sonic_rs_skipper` (`json_parity.rs:97-111`) calls
`bbnf_bench::sonic_skipper::parse_only(black_box(&fixture.bytes))` which executes
(`sonic_skipper.rs:3-7`):

```rust
serde::de::IgnoredAny::deserialize(&mut sonic_rs::Deserializer::from_slice(bytes))?;
deserializer.end()
```

`IgnoredAny` drives sonic-rs to validate structure and consume every token while
discarding all values. This is labeled `materialisation = "skip_checked"` in the
`write_competitor_row` call at `json_parity.rs:108`. Both sides produce no output
and retain nothing.

**Within the parse_only lane the comparison is symmetric**: neither side allocates a
DOM, tape, or typed value. The lane is recognizer vs recognizer. This is correct and
fair for the parse_only admission.

### The undisclosed retention asymmetry at the lane boundary

The RESULTS.md `parse_only` rows are cited as evidence that "JSON >SOTA" holds.
However:

- `parse_only` on Track 1 is the recognizer-only path: no tape built.
- The `direct_to_struct` and `real_typed_struct` lanes DO build the full offset
  tape and then project from it. Those lanes are the tape-vs-typed comparisons.

The RESULTS.md rows are labeled `Output plane = parse_only` and the BENCH.md
calls them the "unconditional proof." But sonic-rs in the broader comparison
(direct, real-typed) does not use `skip_checked` — it uses the full struct
deserializer. So "Track 1 beats sonic-rs Skipper on parse_only" and "Track 1
beats sonic-rs full-deserialize on direct_to_struct" are two distinct claims
that should be distinguished.

The SK-V18 SYNTHESIS §280-283 groups them as a single "+1.4%–164.7%" range
without separating parse_only (recognizer-vs-recognizer) from typed (tape-vs-full-
deserialize). `parse_only` is the widest margin (unicode_escapes +164.7%);
real_typed_struct is narrower (e.g. mesh +5.5%, github_events +11.2%,
twitter +19.6%). Presenting the full range as a single "JSON beats sonic-rs" claim
obscures that the largest margins are on the recognizer-only plane.

No consumer-facing framing document analogous to the CSS FRAMING.md states:
"On the parse_only plane, Track 1 is a recognizer-only path and sonic-rs Skipper
is a validate-and-discard path — both produce no output. On the direct/typed
planes, Track 1 builds a tape then projects; sonic-rs deserialises directly to
the same struct type in one pass. The parse_only margin and the typed margin
are structurally distinct and should not be combined into a single range."

### The Criterion warm vs cold-per-parse gap

The JSON bench uses Criterion (`json_parity.rs:637-648`):

```rust
config = Criterion::default()
    .warm_up_time(Duration::from_secs(3))
    .measurement_time(Duration::from_secs(5))
    .sample_size(100)
```

Criterion warms up for 3 seconds, then estimates slope over 100 samples in
steady-state — measuring warm-I-cache / warm-data-cache throughput.

The CSS bench (`css_canon_bench.rs:158-190`) uses N=200 independent single-parse
samples with only one pre-touch of the source buffer (to settle page faults) before
the timed window. This is warm-I-cache but cold-data (no parser state reused, result
dropped each sample). The harness asserts `N >= 50`.

The RESULTS.md admitted rows use `profile_direct-cold` with `warmup_iters=0`, which
is the genuine cold measurement harness. But the gate function
`w13_typed_strict_sonic_plus_one_passes` (gate.rs, referenced in BENCH.md) consumes
Criterion Mbps, not profile_direct numbers, for the final gate admission.

There is therefore a two-tier situation: admitted rows carry `profile_direct-cold`
numbers, but the gate signal is Criterion warm throughput. The CSS >SOTA gate uses
cold-per-parse median. These are not the same thermal discipline.

No document at the primary claim sites (MASTER-PLAN.md, combined >SOTA narrative
summaries) notes that JSON figures are Criterion warm throughput while CSS figures
are cold per-parse median.

### JSON claim grade

**ACCEPTABLE FOR TYPED PLANES, DISCLOSURE GAP FOR PARSE_ONLY + THERMAL MISMATCH.**

The direct_to_struct and real_typed_struct lanes are output-symmetric: both sides
produce a typed struct; Track 1 absorbs a two-pass overhead (tape build then
projection) while sonic-rs is one-pass. A win here is a genuine win.

The parse_only plane is a recognizer-vs-recognizer comparison — fair within-lane
but the +164.7% headline margin lives on this lane and the framing "beats sonic-rs"
conflates it with the typed-output-plane comparison.

The thermal mismatch (Criterion warm for JSON vs cold-per-parse for CSS) means any
combined "JSON+CSS both >SOTA" assertion comparing absolute Mbps figures
cross-domain is not on the same thermal plane.

---

## 3 — Claim table

| Claim | Our side | Their side | Asymmetries | Disclosed where | Grade |
|---|---|---|---|---|---|
| CSS `track1_rich` beats lightningcss 1.5–3.3× (G6) | Lazy head-level typed projection; 9 aggregate counts from tape; zero arena writes | Eager full CSSOM build; every value/selector node allocated and retained | Retention (none vs owned); Depth (head-classify vs full-tree) | FRAMING.md (`waves/h1/FRAMING.md`); H1-LEDGER §1 | DISCLOSED-RESIDUAL — honest in FRAMING.md; not co-located at primary claim sites; no quiet-clean absolute Mbps |
| JSON parse_only beats sonic-rs Skipper +1.4%–164.7% | Recognizer: structure valid, no output | Recognizer: structure valid, no output | None within-lane | RESULTS.md metadata `comparator_plane=skip_checked` | FAIR WITHIN-LANE |
| "JSON beats sonic-rs +1.4%–164.7%" (combined range) | Recognizer-only (parse_only, no tape) + tape-then-project (direct/typed) | Validate-and-discard (parse_only lane) + full serde deserialize (typed lanes) | Recognizer-vs-recognizer and tape-vs-direct conflated into single range | Not disclosed in consumer-facing prose | UNDISCLOSED — requires framing analogous to CSS FRAMING.md |
| JSON direct_to_struct / real_typed_struct beats sonic-rs | Two-pass: tape build + typed projection | One-pass: direct serde deserialise into same struct | Two-pass overhead absorbed by Track 1 | Not disclosed | ACCEPTABLE — output symmetric; two-pass gap informational |
| CSS cold-per-parse N=200 and JSON Criterion warm as equivalent >SOTA evidence | CSS: cold-per-parse N=200 median | JSON: Criterion warm 3s warmup, slope N=100 | Thermal discipline incompatible | Not disclosed | UNDISCLOSED |
| CSS >SOTA binding close complete | H1-LEDGER records S outcome, pending quiet re-capture | N/A | No quiet-clean absolute CSS Mbps exists in any committed doc | H1-LEDGER §2 marks outcome S explicitly | OPEN RESIDUAL — correctly acknowledged in H1-LEDGER, but not reflected in MASTER-PLAN/BENCH summaries |

---

## 4 — Items that are clean

- FRAMING.md is thorough: names retention, depth, and oracle-scope limits precisely.
- H1-LEDGER correctly marks outcome S and forbids absolute Mbps claims under load.
- W0 baseline locked lightningcss denominator under genuine quiet conditions.
- A0 re-baseline explicitly labels its figures as directional and non-re-lockable.
- G6-LEDGER correctly flags the ax-iter profile as invalid for NEON magnitude.
- Cold discipline for CSS is standard hot-I-cache / cold-data, applied symmetrically
  to all four workloads in the same run (no cross-plane thermal advantage).
- The four CSS corpora are real, unmodified, publicly available stylesheets.
- `black_box(sheet.rules.0.len())` correctly keeps the lightningcss parse live.
- JSON direct_to_struct and real_typed_struct lanes are output-symmetric comparisons.
- The parse_only within-lane comparison (recognizer vs recognizer) is fair.

---

## 5 — Required disclosures (analogous to CSS FRAMING.md)

### 5.1 JSON parse_only framing document

A document parallel to CSS FRAMING.md should state:

- On the `parse_only` plane: Track 1 is a structure-recognizing path with no output
  (`parse_only()` retains nothing). sonic-rs Skipper (`IgnoredAny::deserialize`) is
  likewise a validate-and-discard path with no output. Both sides produce no retained
  structure. The parse_only margin (+1.4%–164.7%) reflects recognizer-vs-recognizer
  throughput.
- On the `direct_to_struct` and `real_typed_struct` planes: Track 1 builds a full
  offset tape and then projects typed output from it (two passes). sonic-rs
  deserialises directly to the same struct type (one pass). Track 1 absorbs the
  tape-build overhead; a win here reflects genuine per-schema speed after that cost.
- These two planes should not be presented as a single margin range without naming
  which plane the extreme figures (164.7%) come from.

### 5.2 Thermal mismatch disclosure

Any document presenting JSON Mbps and CSS Mbps as co-equal >SOTA evidence must note:
JSON figures are Criterion steady-state warm-cache throughput; CSS figures are
cold-per-parse N≥200 median. The absolute values are not on the same thermal plane;
combined "both >SOTA" claims are valid only if this difference is named.

### 5.3 CSS quiet re-capture

Until a loadavg < 1.0 run of `css_canon_bench` is committed with both `track1_rich`
and `lightningcss` in the same binary session, the CSS >SOTA absolute Mbps claim
remains directional. The ratios (being same-run) are load-fair. H1-LEDGER already
acknowledges this correctly; the MASTER-PLAN.md summary should be updated to reflect
the S (residual) outcome rather than treating the close as complete.

---

## 6 — Summary verdict

The CSS >SOTA claim is the better-disclosed of the two: FRAMING.md names the depth
and retention asymmetries; H1-LEDGER records honest load caveats. The principal
remaining CSS gap is the absence of a quiet-clean absolute Mbps re-capture, acknowledged
as an open residual in H1-LEDGER but not surfaced in summary narratives.

The JSON claim has two undisclosed issues. First, the +1.4%–164.7% range combines
the recognizer-only plane (where the largest margins live and no output is produced)
with the typed-output planes (where both sides produce equivalent output) — no
consumer-facing framing document distinguishes them. Second, the JSON bench uses
Criterion warm-cache throughput while the CSS bench uses cold-per-parse median;
combined narratives presenting both as equivalent SOTA evidence are comparing numbers
from incompatible thermal regimes without disclosure. Both gaps require a framing
document analogous to CSS FRAMING.md before any external publication of the combined
JSON+CSS >SOTA claim.
