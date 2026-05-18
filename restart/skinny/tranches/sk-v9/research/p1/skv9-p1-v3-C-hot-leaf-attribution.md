# SK-V9 P1-V3-C: Per-Corpus Deep Hot-Leaf Attribution

Pass: S-P1 Profile. Cycle: V3 reframe.
Date: 2026-05-18.
Scope: per-corpus deep hot-leaf attribution across 17 JSON corpora × {Track 1,
Track 2}, with SC-1 and SC-4 hypothesis adjudication.
Output: this file.
Baseline: SK-V9-open at commit `90609aee`, run
`sk-v9-open:criterion-fnv64-cd1673844eeea12f`.
Host triple: `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`.
Profile tools: `samply 0.13.1` (106 profile/sidecar pairs); xctrace P1-V3-A/B
captures had not landed at the time of this synthesis (see §1.3 caveat).

## §1 Method

### 1.1 Sources

- 17 × `parse_only.track1_generated` profiles (`profile-lazy`, fused parse-only
  surface). One profile per corpus.
- 17 × `direct_to_struct.track1` profiles (`profile_direct ... track1`, the
  direct/SinkOnly route via `JsonDigestSink`). One per corpus.
- 17 × `host_call_eager_decode` profiles (Criterion bench; eager-decode
  diagnostic — used to de-fuse `dispatch_value` because eager decode forces the
  string body and view accessors back out of inline scope).
- 17 × `cold_first_parse` profiles (Track 1 cold; reveals `bbnf_bench::track2::
  json::Parser::parse_value_at` — the hand-coded Track 2 baseline — as a
  measurable tail leaf).
- 17 × `structural_scan.simd` profiles (isolated SC-1 named-kernel probe).
- 17 × `alternate_scalar_plan` profiles (serde_json comparator; diagnostic
  only).
- 4 × `real_typed_struct.track1` profiles (W0-admitted typed rows;
  out-of-scope here, owned by P1-B).

Aggregated symbol table:
`/tmp/skv9-p1-rerun/profile-summary.json` (106 entries, top-20 per profile
with file/line metadata).

### 1.2 Track mapping (important — V2 conflated this)

`RESULTS.md:139` states "Track 1 is `runtime::generated_json::parse`; Track 2
is the independent hand-coded parser over `runtime::tape`." This V3-C report
honours that definition:

- **Track 1 (generated)**: `parse_only.track1_generated` + `direct_to_struct.
  track1` profiles. Both run the generated parser; the first measures parse
  cost alone, the second measures parse+sink projection.
- **Track 2 (hand-coded)**: `bbnf_bench::track2::json::Parser::parse_value_at`.
  Track 2 does NOT have its own dedicated samply binary in the V2 capture set.
  It appears as a tail leaf only inside the `cold_first_parse` Criterion bench
  (1–2% of cold samples; e.g. apache_builds 1.3%, random 1.6%, instruments
  1.9%). For deep self-time, Track 2 cycle precision must come from the
  in-flight P1-V3-A xctrace capture; this synthesis records the samply
  evidence available now and flags Track 2 rows as samply-shallow.

For Track 1 the report uses `parse_only.track1_generated` as the canonical
Track 1 numeric row and treats `direct_to_struct.track1` as the de-fused
Track 1 evidence (because direct sink projection prevents some of the
aggressive inlining present in the parse-only binary). The
`host_call_eager_decode` rows are used as an additional de-fusion lens for
SC-4 evidence in §5.

### 1.3 Attribution rule and structural classifier

Each leaf symbol is mapped to one of the agreed classes:

| Class | Rule (substring or file match) |
|---|---|
| `structural_scan` | `scan_structurals`, `structural_offsets_simd`, `bulk_emit_positions`, `bitmap_prefix_xor`, `consume_structural`, `consume_container_next`, `consume_array_next`, `consume_quote_at_cursor`, `consume_delimiter`, `skip_ws` |
| `string_scan` | `match_tiny_plain_string`, `match_string_at_quote`, `skip_string_plain`, `string_special_mask`, `scan_string_special_block`, `string_body_range`, `parse_string`, `parse_key_colon`, `StringSpecial` |
| `number_parse` | `parse_number`, `match_number`, `materialize_f64`, `materialize_u64`, `parse_vec_cap_*_scalar_f64`, `parse_int` |
| `escape_handling` | `unescape_string`, `unescape_uxxxx`, `parse_escape`, `parse_unicode_escape`, `validate_string_escape` |
| `tape_write` | `push_plain_offset`, `patch_flags`, `TapeBuilder`, `emit_plain_offset`, `Tape::push` |
| `allocation` | `_platform_memmove`, `_platform_memset`, `libsystem_malloc.*`, `libsystem_c.*`, `RawVecInner::reserve`, `IndexMap::*`, `BTreeMap::*`, `hashbrown::*`, serde_json comparator leaves |
| `sync_overhead` | `mach_absolute_time`, `criterion::*`, `black_box` |
| `traversal_other` | `dispatch_value`, `parse_value_at`, `parse_object`, `parse_array`, `parse_literal`, `DirectParser::skip_value`, `parse_type_*`, `JsonNodeKind::at_cursor`, `next_sibling_cursor`, `JsonDigestSink::*`, `parse_object_value_at_direct`, `parse_array_element_at_direct`, `parse_type_plugin`, `core::str::from_utf8`, `parse_string_direct` |

The classifier is conservative: when a leaf is a fused dispatch surface
(`dispatch_value`, `parse_object_value_at_direct`, `parse_array_element_at_
direct`), it is bucketed `traversal_other`, NOT split into string/number/
structural sub-claims. The shallowness this introduces is the central V2
gap and is explicitly preserved here so §6 can quantify it.

### 1.4 V3-A / V3-B sibling status

xctrace dir `/tmp/skv9-xctrace-v3/` does not exist at run time. The
cycle-precision (CPU Counters) and per-symbol-self-time (Time Profiler)
xctrace columns are therefore absent from §2/§3. Every "% self-time"
number in this report is samply 4 kHz on-CPU sample share, NOT cycles.
A follow-up edit is required once the xctrace JSON exports land; the
schema is already columnar so the refinement is a row-by-row overwrite.

## §2 Per-Corpus Per-Track Top-8 Symbols

`%` is samply self-time share of the named profile (4 kHz sampler,
main-thread-only). `cls` uses the §1.3 classifier.

### 2.1 Track 1, `parse_only.track1_generated`

The Track 1 parse-only surface is **monolithically fused**: `dispatch_value` at
`generated.rs:47` collects 95.6%–99.6% of self-time on every corpus, and no
inner-loop symbol (match_tiny_plain_string, match_string_at_quote,
scan_structurals, consume_structural, push_plain_offset, …) appears as its own
leaf in any of the 17 profiles. The top-8 below is identical in shape across
all corpora; only the tail magnitudes shift.

| Corpus | #1 % | #1 symbol (cls) | tail (#2–#8 ≈) |
|---|---:|---|---|
| `twitter` | 98.8 | `dispatch_value` (traversal_other) | memmove 0.3 / mach_time 0.2 / malloc 0.2 / other ~0.1 |
| `citm_catalog` | 98.9 | `dispatch_value` (traversal_other) | memmove 0.3 / mach_time 0.2 / malloc ~0.1 |
| `canada` | 99.6 | `dispatch_value` (traversal_other) | memmove 0.1 / mach_time 0.1 / malloc ~0.1 |
| `apache_builds` | 98.6 | `dispatch_value` (traversal_other) | memmove 0.6 / mach_time 0.2 / memset/malloc ~0.1 |
| `github_events` | 98.2 | `dispatch_value` (traversal_other) | memmove 0.5 / mach_time 0.3 / malloc ~0.1 |
| `update_center` | 99.0 | `dispatch_value` (traversal_other) | memmove 0.3 / mach_time 0.2 / malloc ~0.1 |
| `mesh` | 99.3 | `dispatch_value` (traversal_other) | memmove 0.3 / mach_time 0.1 |
| `random` | 99.3 | `dispatch_value` (traversal_other) | memmove 0.3 / mach_time 0.1 |
| `gsoc-2018` | 99.1 | `dispatch_value` (traversal_other) | memmove 0.2 / mach_time 0.1 |
| `marine_ik` | 99.4 | `dispatch_value` (traversal_other) | memmove 0.1 / mach_time 0.1 |
| `instruments` | 98.0 | `dispatch_value` (traversal_other) | memmove 1.0 / mach_time 0.4 |
| `numbers` | 96.7 | `dispatch_value` (traversal_other) | memmove 1.8 / mach_time 0.5 |
| `unicode_mixed` | 99.2 | `dispatch_value` (traversal_other) | memmove 0.2 / mach_time 0.1 |
| `unicode_escapes` | 99.0 | `dispatch_value` (traversal_other) | memmove 0.3 / mach_time 0.1 |
| `unicode_basic` | 99.0 | `dispatch_value` (traversal_other) | memmove 0.2 / mach_time 0.1 |
| `distinct_values` | 98.3 | `dispatch_value` (traversal_other) | memmove 1.0 / mach_time 0.2 |
| `y_string_unicode` | 95.6 | `dispatch_value` (traversal_other) | mach_time 1.2 / memmove 1.0 / malloc/memset ~0.2 |

### 2.2 "Track 1 de-fused" — `direct_to_struct.track1`

The direct profile gives more useful split because the sink-shaped function
(`parse_object_value_at_direct::<JsonDigestSink>` at `generated.rs:468` and
`parse_array_element_at_direct::<JsonDigestSink>` at `generated.rs:508`)
survives inlining alongside the sink closure `JsonDigestSink::array_string` at
`direct_struct.rs:124`. Number-materialization symbols also escape inlining
(`number::materialize_u64`, `number::materialize_f64`). Unescape escapes inlining
on escape-bearing rows (`parse_that_regex::unescape_string`).

Top-8 (% / class / symbol-short):

| Corpus | top-8 |
|---|---|
| `twitter` | 72.4 traversal `parse_object_value_at_direct` / 11.3 traversal `parse_array_element_at_direct` / 8.2 traversal `JsonDigestSink::array_string` / 1.9 other `memchr_aligned` / 1.9 sync `mach_absolute_time` / 1.2 escape `unescape_string` / 0.8 alloc memmove / 0.3 number `materialize_u64` |
| `citm_catalog` | 55.5 traversal `parse_array_element_at_direct` / 41.4 traversal `parse_object_value_at_direct` / 2.0 number `materialize_u64` / 1.0 traversal `array_string` / 0.1 other run_once / 0.0 alloc malloc / 0.0 other read / 0.0 traversal `from_utf8` |
| `canada` | 87.5 traversal `parse_array_element_at_direct` / 12.4 number `materialize_f64` / 0.0 traversal `parse_object_value_at_direct` / 0.0 other read / 0.0 traversal `from_utf8` / 0.0 alloc malloc / 0.0 other run_once |
| `apache_builds` | 38.1 traversal `parse_object_value_at_direct` / 32.7 traversal `array_string` / 28.5 traversal `parse_array_element_at_direct` / 0.3 other run_once / 0.1 escape `unescape_string` / 0.1 sync `mach_absolute_time` / 0.0 alloc memmove / 0.0 alloc malloc |
| `github_events` | 72.8 traversal `parse_object_value_at_direct` / 15.5 traversal `array_string` / 8.6 traversal `parse_array_element_at_direct` / 1.4 escape `unescape_string` / 0.5 sync `mach_absolute_time` / 0.3 alloc memmove / 0.2 other run_once / 0.1 number `materialize_u64` |
| `update_center` | 68.3 traversal `parse_object_value_at_direct` / 18.9 traversal `array_string` / 8.7 traversal `parse_array_element_at_direct` / 1.1 escape `unescape_string` / 0.9 sync `mach_absolute_time` / 0.5 other `memchr_aligned` / 0.3 alloc memmove / 0.1 alloc memset |
| `mesh` | 75.9 traversal `parse_array_element_at_direct` / 15.2 traversal `parse_object_value_at_direct` / 5.0 number `materialize_f64` / 3.8 number `materialize_u64` / 0.1 other run_once / 0.0 alloc malloc / 0.0 other read |
| `random` | 39.5 traversal `parse_object_value_at_direct` / 37.2 traversal `parse_array_element_at_direct` / 22.3 traversal `array_string` / 0.9 number `materialize_u64` / 0.0 alloc malloc / 0.0 traversal `from_utf8` / 0.0 alloc malloc / 0.0 other run_once |
| `gsoc-2018` | 61.7 traversal `parse_object_value_at_direct` / 20.7 traversal `array_string` / 7.1 escape `unescape_string` / 4.0 sync `mach_absolute_time` / 1.9 alloc memmove / 1.8 other `memchr_aligned` / 0.4 other run_once / 0.3 alloc memset |
| `marine_ik` | 73.6 traversal `parse_array_element_at_direct` / 19.2 traversal `parse_object_value_at_direct` / 4.2 number `materialize_f64` / 2.9 number `materialize_u64` / 0.0 traversal `array_string` / 0.0 sync `mach_absolute_time` / 0.0 other read / 0.0 traversal `from_utf8` |
| `instruments` | 59.3 traversal `parse_array_element_at_direct` / 37.4 traversal `parse_object_value_at_direct` / 2.4 number `materialize_u64` / 0.8 traversal `array_string` / 0.1 other run_once / 0.0 sync `mach_absolute_time` / 0.0 alloc malloc / 0.0 alloc malloc |
| `numbers` | 77.7 traversal `parse_array_element_at_direct` / 11.9 number `materialize_f64` / 10.4 other run_once / 0.0 sync `mach_absolute_time` / 0.0 alloc malloc |
| `unicode_mixed` | 54.4 traversal `parse_object_value_at_direct` / 23.4 escape `unescape_string` / 7.1 alloc memmove / 5.4 traversal `parse_array_element_at_direct` / 3.7 traversal `array_string` / 1.9 sync `mach_absolute_time` / 1.1 other `memchr_aligned` / 0.4 alloc memset |
| `unicode_escapes` | 47.5 escape `unescape_string` / 42.8 traversal `parse_object_value_at_direct` / 2.8 sync `mach_absolute_time` / 1.9 traversal `array_string` / 1.4 alloc memmove / 0.8 traversal `parse_array_element_at_direct` / 0.5 alloc memset / 0.2 other `memchr_aligned` |
| `unicode_basic` | 44.6 traversal `parse_object_value_at_direct` / 36.5 traversal `parse_array_element_at_direct` / 17.3 traversal `array_string` / 0.8 other run_once / 0.7 number `materialize_u64` / 0.1 traversal `from_utf8` / 0.0 other read / 0.0 sync `mach_absolute_time` |
| `distinct_values` | 50.1 traversal `parse_array_element_at_direct` / 28.2 traversal `parse_object_value_at_direct` / 21.0 traversal `array_string` / 0.3 other run_once / 0.2 number `materialize_u64` / 0.0 alloc malloc / 0.0 sync `mach_absolute_time` / 0.0 traversal `from_utf8` |
| `y_string_unicode` | 23.3 sync `mach_absolute_time` / 21.5 escape `unescape_string` / 16.8 traversal `parse_array_element_at_direct` / 3.4 traversal `array_string` / 2.3 alloc malloc / 2.1 alloc malloc / 2.1 other panic.rs / 1.2 alloc malloc |

Note on `y_string_unicode`: the Criterion harness leaks 23.3% to
`mach_absolute_time` and ~5–10% to malloc/panic.rs because each parse iteration
is extremely short (input is 50 B) and the harness frame dominates. The honest
parser self-time is reconstructible only as the remaining ~50–60%; the
in-flight xctrace Time Profiler capture is needed to clean this.

### 2.3 Track 2 (hand-coded) — partial samply evidence only

The Track 2 hand-coded parser
(`bbnf_bench::track2::json::Parser::parse_value_at`) appears only as a tail
leaf in `cold_first_parse` profiles (1.1%–1.9% on 8/17 corpora; rest are
sub-1.0% tail). No dedicated Track 2 samply binary was captured in S-P1 V2.
Track 2 hot-leaf attribution is therefore SAMPLY-INSUFFICIENT and is flagged
"to refine after sibling xctrace captures land" — the V3-A CPU Counters
capture against `bbnf-bench --bench json_parity` filtered to track 2
benchmark rows is the required input. Until those land, Track 2 hot-leaf
claims must rest on architectural inspection (track2/json.rs) plus the
Track 1 de-fused evidence in §2.2 as the closest proxy.

## §3 Per-Corpus Structural-Class Share

Sum of top-8 self-time per class. Rows are ordered by ascending string
fraction (§5). "T1-fused" = `parse_only.track1_generated`; "T1-defused"
= `direct_to_struct.track1`. Track 2 columns are intentionally omitted
pending xctrace V3-A.

T1-fused share (top-8 sum; rest ≤ 1% scattered tail):

| corpus | trav | alloc | sync | other |
|---|---:|---:|---:|---:|
| numbers | 96.7 | 1.9 | 0.5 | 0.0 |
| mesh | 99.3 | 0.3 | 0.1 | 0.0 |
| canada | 99.6 | 0.2 | 0.1 | 0.0 |
| marine_ik | 99.4 | 0.1 | 0.1 | 0.0 |
| instruments | 98.0 | 1.0 | 0.4 | 0.0 |
| citm_catalog | 98.9 | 0.4 | 0.2 | 0.0 |
| unicode_escapes | 99.0 | 0.4 | 0.1 | 0.0 |
| unicode_mixed | 99.2 | 0.3 | 0.1 | 0.0 |
| twitter | 98.8 | 0.5 | 0.2 | 0.1 |
| github_events | 98.2 | 0.6 | 0.3 | 0.0 |
| random | 99.3 | 0.3 | 0.1 | 0.0 |
| unicode_basic | 99.0 | 0.4 | 0.1 | 0.0 |
| apache_builds | 98.6 | 0.6 | 0.2 | 0.0 |
| update_center | 99.0 | 0.4 | 0.2 | 0.0 |
| distinct_values | 98.3 | 1.0 | 0.2 | 0.0 |
| gsoc-2018 | 99.1 | 0.3 | 0.1 | 0.0 |
| y_string_unicode | 95.6 | 1.4 | 1.2 | 0.0 |

T1-defused share (top-8 sum; "escape" includes `unescape_string`; "trav"
includes `parse_object_value_at_direct`, `parse_array_element_at_direct`,
`JsonDigestSink::array_string`, `parse_string_direct`, `from_utf8`):

| corpus | trav | escape | number | alloc | sync | other |
|---|---:|---:|---:|---:|---:|---:|
| numbers | 77.7 | 0.0 | 11.9 | 0.0 | 0.0 | 10.4 |
| mesh | 91.2 | 0.0 | 8.7 | 0.0 | 0.0 | 0.1 |
| canada | 87.5 | 0.0 | 12.4 | 0.0 | 0.0 | 0.0 |
| marine_ik | 92.8 | 0.0 | 7.1 | 0.0 | 0.0 | 0.0 |
| instruments | 97.5 | 0.0 | 2.4 | 0.0 | 0.0 | 0.1 |
| citm_catalog | 97.9 | 0.0 | 2.0 | 0.0 | 0.0 | 0.1 |
| unicode_escapes | 45.5 | 47.5 | 0.0 | 1.9 | 2.8 | 0.2 |
| unicode_mixed | 63.5 | 23.4 | 0.0 | 7.5 | 1.9 | 1.1 |
| twitter | 92.0 | 1.2 | 0.3 | 0.8 | 1.9 | 1.9 |
| github_events | 96.9 | 1.4 | 0.1 | 0.3 | 0.5 | 0.2 |
| random | 99.0 | 0.0 | 0.9 | 0.0 | 0.0 | 0.0 |
| unicode_basic | 98.5 | 0.0 | 0.7 | 0.0 | 0.0 | 0.8 |
| apache_builds | 99.3 | 0.1 | 0.0 | 0.1 | 0.1 | 0.3 |
| update_center | 96.0 | 1.1 | 0.0 | 0.4 | 0.9 | 0.5 |
| distinct_values | 99.4 | 0.0 | 0.2 | 0.0 | 0.0 | 0.3 |
| gsoc-2018 | 82.3 | 7.1 | 0.0 | 2.2 | 4.0 | 2.3 |
| y_string_unicode | 20.3 | 21.5 | 0.0 | 5.6 | 23.3 | 2.1 |

Note: `string_scan`/`structural_scan`/`tape_write` columns are uniformly zero
in both views and intentionally omitted; their absence is the central finding
in §6.

## §4 SC-1 Verdict — Structural-Scan Non-Fusion

**SC-1 claim restated** (`SC-1-offset-tape-teardown.md` §1.3):
`scan_structurals` exists and is fast, but its output is never consumed —
`attach_structural_index` at `generated.rs:14-17` is `let _ = state;`. The
recursive-descent parser re-classifies every structural byte itself. The
parser pays for a second structural traversal.

**V3-C verdict: SC-1 is structurally TRUE but the in-parser cost is
samply-invisible at the symbol level. Cycle-level falsification requires
xctrace V3-A.**

Direct evidence in this dataset:

1. `scan_structurals` (the named SIMD kernel
   `runtime::generated_json::scan::scan_structurals` and its NEON helpers
   `neon::scan`, `bulk_emit_positions_64_neon`, `bitmap_prefix_xor_64_neon`)
   appears as a leaf in **zero** Track 1 / Track 2 production profiles. It
   appears only inside the dedicated `structural_scan.simd` probe, where
   `bbnf_bench::scan::structural_offsets_simd` is 46.3%–86.6% of self-time and
   `bulk_emit_positions_64_neon` is 5.2%–45.5%. That probe is a synthetic
   isolation: the SC-1 claim "scan output discarded" is consistent with this
   non-appearance.

2. `runtime::generated_json::scan::scan_tail` shows up as a 0.9%–2.0% tail
   leaf in `structural_scan.simd` profiles but never in
   `parse_only.track1_generated`. That confirms the production parse-only
   binary does not call `scan_structurals` / `scan_tail` on the hot path
   under default `CapacityPlan::GrowOnly`, matching `SC-1 §1.3` line by line.

3. The "structurally-equivalent scalar rediscovery pass" the claim predicts
   (in-parser `skip_ws`, `consume_structural`, `consume_container_next`,
   `consume_array_next`, `consume_quote_at_cursor`) is **invisible at symbol
   resolution**. None of those symbols appears as a samply leaf in any of
   the 95 `dispatch_value`-dominated profiles; they are all inlined into
   `generated.rs:47` via `#[inline]` and aggressive bench-profile inlining.
   Samply cannot prove or disprove cycle-level "second traversal" cost from
   on-CPU sample share alone. The V2 attribution stopped here and called
   it shallow; V3-C agrees.

4. **Falsification path**: the V3-A CPU Counters xctrace capture (PMU
   cycles per instruction at the source-line level) is the only available
   instrument that can isolate the re-classification cost without a code
   change. The alternative is a same-line `#[inline(never)]` build (a
   change to the generated parser) that breaks one of `consume_structural`
   / `skip_ws` out for one corpus and measures the delta in cycles/byte.
   This V3-C synthesis cannot perform the falsification; it can only
   confirm the named-kernel absence.

**Net SC-1 verdict (V3-C)**: SC-1's *non-fusion* claim holds (the SIMD
scan symbols are non-producers, present only under synthetic probes); SC-1's
*share-of-self-time* claim is unfalsified at the samply layer and remains
contingent on V3-A cycle-precision. The V2 attribution did not separate
those two halves — V3-C does.

## §5 SC-4 Verdict — String-Plane 75% Share

**SC-4 claim restated** (`SC-4-string-plane-gap.md` §1.3 quoting SK-V7
`SYNTHESIS.md`): `match_tiny_plain_string` + `match_string_at_quote` ≈
**75% of total self-time** on string-heavy rows
(twitter, gsoc-2018, update_center, unicode_*, y_string_unicode).

**V3-C verdict: the literal "75% on Track 1" share is NOT measurable in the
V2 samply dataset, because the string scanners are inlined into
`dispatch_value`. The de-fused evidence (direct + eager-decode probes)
proves the string class IS the dominant non-traversal cost on string-loss
corpora, but the share is 21%–62%, not 75%, in the only views the data
permits.**

### 5.1 Where the named scanners are visible

`match_tiny_plain_string`, `match_string_at_quote`, `skip_string_plain*`,
`scan_string_special_block`, `parse_string`, `parse_key_colon`: **zero
appearances** across all 106 profiles. They are inlined into
`dispatch_value` (Track 1 fused), `parse_object_value_at_direct` /
`parse_array_element_at_direct` (Track 1 de-fused / direct sink), and into
the hand-coded Track 2 `parse_value_at` (which itself only appears as a
~1.5% tail leaf in `cold_first_parse`).

The string class IS visible as the following symbols that *did* escape
inlining:

- `parse_that_regex::unescape_string` at `parse-that-regex/src/lib.rs:718`
  — only fires when a string has `\` escapes; rate depends on escape
  density.
- `bbnf_bench::direct_struct::JsonDigestSink::array_string` at
  `direct_struct.rs:124` — the sink callback that consumes the parsed
  string bytes; its self-time is downstream string processing
  (copy/digest/etc.) and is a defensible proxy for "post-scan string
  cost" but not for the scan itself.
- `runtime::generated_json::view::string_body_range` at `view.rs` — only
  appears under the eager-decode probe (the production parse-only path
  does not eagerly access string bodies).
- `core::str::converts::from_utf8` — only appears under cold / eager probes
  (production parse skips UTF-8 validation per W0 boundary).

### 5.2 Quantified per-corpus string share (corrected)

T1-defused (`direct_to_struct.track1`) string-class self-time =
`unescape_string + array_string + parse_string_direct + string_body_range`.
Sorted by RESULTS-derived string fraction
(`string_quotes / (quotes + numbers + literals)`):

| Corpus | string fraction | T1-defused string % | eager-decode string % | RESULTS Δ vs sonic-strict |
|---|---:|---:|---:|---:|
| numbers | 0.000 | 0.0 | 4.8 | +51.2% (win) |
| canada | 0.000 | 0.0 | 3.9 | +27.9% (win) |
| mesh | 0.000 | 0.0 | 4.0 | +21.4% (win) |
| marine_ik | 0.135 | 0.0 | 9.3 | +37.0% (win) |
| instruments | 0.556 | 0.8 | 22.0 | +10.6% (thin win) |
| citm_catalog | 0.630 | 1.0 | 14.7 | +24.6% (win) |
| twitter | 0.726 | 9.4 | 28.2 | −25.1% |
| unicode_mixed | 0.750 | 27.1 | 45.5 | −50.3% |
| unicode_escapes | 0.750 | 49.4 | 61.6 | −34.6% |
| unicode_basic | 0.833 | 17.3 | 41.4 | −26.8% |
| random | 0.846 | 22.3 | 28.8 | −36.4% |
| github_events | 0.889 | 16.9 | 30.8 | −34.0% |
| distinct_values | 0.957 | 21.0 | 34.7 | −61.2% |
| update_center | 0.986 | 20.1 | 32.9 | −43.1% |
| apache_builds | 0.999 | 32.9 | 34.9 | −28.2% |
| gsoc-2018 | 1.000 | 27.8 | 43.4 | −53.3% |
| y_string_unicode | 1.000 | 25.0 | 44.3 | −54.1% |

(The eager-decode column adds `string_body_range` and `from_utf8`, both
view-side cost not paid by parse-only Track 1 production, and is meant as
a de-fusion upper bound, NOT a Track 1 admission number.)

### 5.3 Adjudication of the literal "75%"

- The SK-V7 §3.4 number cited by SC-4 ("`match_string_at_quote` ~47% +
  `match_tiny_plain_string` ~28% ≈ 75%") was a Track 1 attribution made
  before the V2 inlining regime tightened. The current build's
  `dispatch_value` consumes 95.6%–99.6% of Track 1 sample share and
  contains the same instructions — the 75% is *inside* that 99%, but the
  symbols are no longer separable in samply.
- The 75% is **upper-bounded** by the corpus's traversal share (since the
  string scanners live inside traversal) and **lower-bounded** by the
  de-fused string share. For the named SC-4 loss corpora the lower bound
  is:
  - twitter 9.4% (T1-defused) / 28.2% (eager); upper bound ≤ 92% (the
    fused traversal class).
  - gsoc-2018 27.8% / 43.4%; upper bound ≤ 82%.
  - update_center 20.1% / 32.9%; upper bound ≤ 96%.
  - unicode_escapes 49.4% / 61.6%; upper bound ≤ 47.5%+45.5% = 93%.
  - unicode_mixed 27.1% / 45.5%; upper bound ≤ 87%.
  - unicode_basic 17.3% / 41.4%; upper bound ≤ 98.5%.
  - distinct_values 21.0% / 34.7%; upper bound ≤ 99.4%.
  - apache_builds 32.9% (almost all `array_string` sink); upper bound
    ≤ 99.3%.
  - y_string_unicode 25.0% / 44.3% (harness-noisy); upper bound ≤ 41.8%.
- The "≈75%" claim is therefore **plausible on a per-corpus basis** but
  cannot be confirmed at 75% literal precision in this dataset. The
  largest *confirmed* lower-bound is unicode_escapes at 49.4%, which
  comes from the `unescape_string` leaf (escape decoding) plus the
  containing direct dispatch — and unicode_escapes is by construction
  the most escape-heavy corpus. The non-escape string-loss corpora
  (twitter, update_center, github_events) have T1-defused string share
  only 17%–33%; their remaining loss must hide inside traversal
  (= inlined `match_tiny_plain_string` + `match_string_at_quote` +
  `consume_quote_at_cursor` + `skip_ws`).
- **Therefore: the SC-4 string-plane diagnosis is directionally correct
  but the literal 75% is not measurable post-W0 without xctrace cycles.
  Until V3-A lands, the honest range is "string-class is the dominant
  non-traversal cost on every string-loss corpus, with confirmed
  share 17%–49% in de-fused views and an upper bound of the traversal
  envelope (82%–99%)".**

### 5.4 String-fraction correlation (SC-4 §2 claim test)

Pearson r (string fraction × T1-defused string-class share, n=17):
**+0.720**. Spearman ρ (string fraction × eager-decode string-class share,
n=17): **+0.755**.

Both confirm SC-4's monotone correlation hypothesis — string-quote density
*does* predict the string class's profile share. The diagnostic is therefore
defensible as JSON-corpus `RecognizerFacts`/`CostFacts` telemetry, as SC-4
recommends in its §2 closing. It is not, however, a clean step function:
the mid-band (instruments 0.556 / +10.6% win, citm_catalog 0.630 / +24.6%
win) shows that string density alone does not predict the strict-sonic
verdict; the wins continue past the SC-4 §2 "0.135 ≤ knee" until at least
0.630, with the universal loss boundary at ~0.726 (twitter).

## §6 Where the V2 Attribution Was Wrong or Shallow

The V2 attribution (`p1e-hot-leaf-attribution.md` §2 table) said:

- "Parse-only Track 1: `dispatch_value` at `generated.rs:47` dominates every
  corpus at 95.6%–99.6%" — accurate, but stopped there.
- "Direct object-heavy rows: `parse_object_value_at_direct` dominates" —
  accurate at the symbol level.

V2's §4 already named the shallowness ("P1-A remains too fused: `dispatch_
value` is a real symbol, but not a primitive-level split"). What V2 did
NOT do, and V3-C now does:

1. **V2 did not separate Track 1 from Track 2.** V2 treated `parse_only.
   track1_generated` as "Track 1" and `direct_to_struct.track1` as a
   second Track 1 surface, never citing the hand-coded Track 2 (which
   `RESULTS.md:139` defines). V3-C names the gap and tags Track 2
   "samply-insufficient pending V3-A".

2. **V2 stopped at the symbol level for SC-1.** V2 did not test the SC-1
   claim that `scan_structurals` is non-consumed in the production
   parse-only path. V3-C confirms via cross-profile symbol absence: the
   SIMD scan symbols appear only inside the dedicated `structural_scan.
   simd` probe (`structural_offsets_simd` 46–87%) and never inside
   `parse_only.track1_generated` (all 17 rows). This is consistent
   evidence for SC-1's non-fusion claim — at least at the symbol layer.

3. **V2 did not quantify the SC-4 75% share.** V2 said "the fused symbol
   does not by itself authorize a primitive" and stopped. V3-C names the
   upper/lower bounds per corpus (§5.3) and runs the Pearson + Spearman
   correlation against string fraction (§5.4, r=+0.720 / ρ=+0.755).

4. **V2 did not classify into the agreed structural-class taxonomy.** V2
   used surface phrases ("direct sink object projection", "string/unicode
   materialization"). V3-C maps every leaf into the 8-class set agreed
   in the V3-C scope.

5. **V2 missed the `from_utf8` and `string_body_range` view-side cost.**
   These are 14%–30% of self-time on `cold_first_parse` / `host_call_
   eager_decode` for string-heavy corpora — they are not production
   parse-only cost but they ARE production eager-decode cost. V2's §2
   line for "Eager decode probes" was correct but not quantified per
   corpus; §5.2 here gives the per-corpus number.

6. **V2 did not flag the harness-noise outlier.** `y_string_unicode`
   direct-to-struct has 23.3% in `mach_absolute_time` because the input
   is tiny (50 bytes; one iteration runs in nanoseconds). V2 listed
   "no single parser leaf dominates" without explaining; V3-C names
   the harness frame and notes the parser-self-time must be
   reconstructed from xctrace.

7. **V2 did not address what xctrace would change.** V3-C does (§1.4,
   §4 falsification path, §5 closing): cycle-precision V3-A is the only
   way to convert the inlined `dispatch_value`'s 99% into an
   actionable per-source-line split.

8. **V2 did not write down the inlining barrier explicitly.** The
   reason `match_tiny_plain_string` is invisible is the bench-profile
   inlining policy, not their absence from the code. V3-C names that
   explicitly so the next move (cycle-precision xctrace, OR a
   targeted `#[inline(never)]` probe build, OR `cargo asm` to read
   `dispatch_value`'s machine code) is on the page.

## §7 Sources

- `/tmp/skv9-p1-rerun/profile-summary.json` (106 samply summaries with
  top-20 leaf, file/line, sample count).
- `/tmp/skv9-p1-rerun/profiles/p1a/*.profile.json.gz` (17 parse-only
  Track 1 profiles).
- `/tmp/skv9-p1-rerun/profiles/p1b/*.profile.json.gz` (17 direct-to-struct
  + 4 real-typed Track 1 profiles).
- `/tmp/skv9-p1-rerun/profiles/p1c/*.profile.json.gz` (17 × 4 probe
  profiles: cold, eager, alternate scalar, structural scan).
- `restart/skinny/tranches/sk-v9/research/p1/p1a-samply-mode-1.md`
  (`dispatch_value` 95.6–99.6% baseline).
- `restart/skinny/tranches/sk-v9/research/p1/p1b-samply-mode-2.md`
  (direct sink top symbols).
- `restart/skinny/tranches/sk-v9/research/p1/p1c-samply-mode-3.md`
  (eager decode + cold + scan tables).
- `restart/skinny/tranches/sk-v9/research/p1/p1e-hot-leaf-attribution.md`
  (V2; superseded shallowly).
- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md`
  (V2 disposition; CH1/CH4 REVISE on PMU).
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md`
  §1.3 / §2 (non-fusion claim).
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md`
  §1.3 / §2 (75% scanner pair, quote-density correlation).
- `skinny/RESULTS.md:91–137` (per-corpus element census + tape ratios)
  and `:139` (Track 1 vs Track 2 definition).
- `skinny/crates/runtime/src/grammars/json/generated.rs:14-17`
  (`attach_structural_index` no-op), `:47` (`dispatch_value`), `:147–185`
  (`match_tiny_plain_string`), `:189–201` (`match_string_at_quote`),
  `:468` (`parse_object_value_at_direct`), `:508` (`parse_array_element_
  at_direct`).
- `skinny/crates/runtime/src/grammars/json/scan.rs:22–30,207–275`
  (`scan_structurals` + NEON path) and `:38–45` (`scalar_parity_report`).
- `skinny/crates/runtime/src/grammars/json/view.rs:309–381`
  (`at_cursor`, `string_body_range`, `next_sibling_cursor`).
- `skinny/crates/parse-that-regex/src/lib.rs:718` (`unescape_string`),
  `:547–574` (`skip_string_plain_trusted`), `:162–209`
  (`match_string_at_quote_trusted_utf8`).
- `skinny/crates/bbnf-bench/src/direct_struct.rs:124`
  (`JsonDigestSink::array_string` sink).
- `skinny/crates/bbnf-bench/src/scan.rs` (`structural_offsets_simd`
  probe entry).

### Pending refinements

- xctrace V3-A (CPU Counters) per-source-line cycles to convert the
  inlined `dispatch_value` (99%) into measured shares for
  `match_tiny_plain_string`, `match_string_at_quote`,
  `consume_structural`, `skip_ws`, `push_plain_offset`, etc.
- xctrace V3-B (Time Profiler) per-symbol self-time at higher
  resolution than samply 4 kHz, especially for the tiny-input
  `y_string_unicode` direct profile where the Criterion harness
  dominates samply share.
- A targeted `#[inline(never)]` probe build for one symbol on one
  corpus is the cheapest non-xctrace falsification of the SC-1
  "second traversal" share.
