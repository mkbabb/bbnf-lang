# perf-01 — json_monolithic samply attribution (AW-III.P1)

AW-III.W4 viability profile for the `json_monolithic` bench family.
Samply self-time attribution across the five bench entries (three
parsed to completion; two blocked by Category A parse failures).
All claims cite artefacts under
`.profiles/samply/json_monolithic/<entry>/`.

## Methodology

- Wave: prepared via `scripts/prepare-profile-wave.sh` on commit
  `b58d14614` (`build(prepare-profile-wave): rename json_value →
  json_monolithic_value (bench target correction)`).
  Wave contract: `.profiles/samply/prebuild/wave.tsv`.
- Shared target: `.profiles/shared-target/` (single-target across
  the wave, per PROFILING.md).
- Bench binary:
  `.profiles/shared-target/release/deps/json_monolithic-dda276c29cb57503`
  (rebuilt 2026-04-17 00:19 during prepare).
- Profile runs: `scripts/profile-bench-headless.sh` with ports
  3130 / 3131 (record / load) reused serially across the five
  entries. All artefacts written to
  `.profiles/samply/json_monolithic/<entry>/`.
- Symbolication: `profile.json.syms.json` module symbol tables
  joined against `frameTable.address` RVAs in
  `profile.json.gz`. Self-time is leaf-frame sample count. Caller
  attribution traces the `stackTable.prefix` chain back one
  frame. Category aggregation groups `bbnf_tape::driver::*` as
  *driver*, the `DtaDfaScanner::scan` + `parse_that::cached_dfa` +
  `HashMap::get` + `Sip13` chain as *scanner*,
  `bbnf_tape::psi::*` + dec2flt + `f64::from_str` as *psi*,
  `bbnf_tape::finaliser::*` + `TapeBuilder::finish` +
  `DtaTable::rule_entry_for` as *finalise*.
- `data_xl` required a manual `samply load` completion after
  `wait_for_record_artifacts` in the script timed out (60 s loop,
  but the 262 ms/iter bench plus pre-warmup took ~3 min before
  `profile.json.gz` was flushed). Record artefact is
  `profile.json.gz` written 2026-04-17 00:29 by the
  `samply record` process; `load.txt` and `syms-proof.txt`
  regenerated 2026-04-17 00:36 from that unchanged `.gz`.

## Per-entry results

Input sizes: `data.json` 35 491 B, `twitter.json` 631 514 B,
`citm_catalog.json` 1 727 204 B, `canada.json` 2 251 051 B,
`data_xl.json` 21 281 177 B (`wc -c data/json/*.json`).

### data — Category A blocked

`.profiles/samply/json_monolithic/data/bench.txt`:

```
running 2 tests
test data_s  ...
thread 'main' (10618282) panicked at crates/core/benches/json/monolithic.rs:52:1:
data.json: parse failed: Syntax { offset: 35490, rule: None }
```

Panic offset 35 490 on a 35 491-byte input — one byte short of
EOF, i.e. trailing-whitespace / EOF-handling in the DTA walker
(not a structural JSON issue). Bencher filter `--bench data`
matched both `data_s` and `data_xl`; the `data_s` entry panicked
first, killing the run before `data_xl` could execute under this
filter path. Consequence for this profile: no `profile.json.gz`
written for this wave; the artefact directory retains stale Apr 15
profile data and is not used for attribution. No AW-IV lever
addresses this — it is a correctness gap inside AW-III's W2 scope
(Cluster A parse completeness).

### twitter — 5 251 828 ns/iter, 120 MB/s

`.profiles/samply/json_monolithic/twitter/bench.txt` line 2:

```
test twitter ... bench:   5,251,828 ns/iter (+/- 296,999) = 120 MB/s
```

5 743 self-time samples (1 ms interval).

Top-20 hot functions by self-time (from
`profile.json.gz` × `profile.json.syms.json`, RVA-joined):

| Rank | Self % | Samples | Function |
|-----:|-------:|--------:|----------|
|  1 | 26.50 | 1522 | `<DtaDfaScanner as RegexScanner>::scan` |
|  2 | 22.03 | 1265 | `bbnf_tape::driver::dispatch_one` |
|  3 |  9.02 |  518 | `bbnf_tape::driver::advance_or_pop_with` |
|  4 |  7.84 |  450 | `bbnf_tape::driver::reserve_compound` |
|  5 |  6.11 |  351 | `bbnf_tape::finaliser::finalise` |
|  6 |  5.61 |  322 | `bbnf_tape::driver::dta_run` |
|  7 |  4.84 |  278 | `bbnf_tape::driver::emit_leaf` |
|  8 |  3.55 |  204 | `core::str::converts::from_utf8` |
|  9 |  3.15 |  181 | `bbnf_tape::driver::close_compound` |
| 10 |  2.99 |  172 | `SipHasher13::write` |
| 11 |  2.30 |  132 | `parse_that::scanners::cached_dfa` |
| 12 |  1.76 |  101 | `_platform_memcmp` |
| 13 |  1.24 |   71 | `HashMap<String, Arc<Dfa>>::get::<str>` |
| 14 |  0.75 |   43 | `dec2flt::parse_number` |
| 15 |  0.57 |   33 | `<f64 as FromStr>::from_str` |
| 16 |  0.49 |   28 | `bbnf_tape::psi::write_decoded` |
| 17 |  0.38 |   22 | `DtaTable::rule_entry_for` |
| 18 |  0.30 |   17 | `_platform_memmove` |
| 19 |  0.26 |   15 | `PayloadStream::fill_columns` |
| 20 |  0.16 |    9 | `json_monolithic-dda276c29cb57503:0x778d8` (unresolved) |

Category attribution
(`.profiles/samply/json_monolithic/twitter/profile.json.gz`):

- driver   52.48% (3014 samples)
- scanner  33.03% (1897 samples)
- finalise  6.49% (373)
- utf8      3.55% (204)
- libc      2.16% (124)
- psi       2.07% (119)
- alloc    ≈ 0

Caller attribution of the scanner leaf:
`cached_dfa` is called 100 % by
`<DtaDfaScanner as RegexScanner>::scan`; `dispatch_one` is called
100 % by `bbnf_tape::driver::dta_run`. This is the textbook
state-machine interpreter signature: one long hot stack
`dta_run → dispatch_one → (Regex arm) → scan → cached_dfa →
HashMap::get → Sip13::write`.

Bottleneck classification:

- **State-machine dispatch baseline (≈ 52 %)**: the
  `bbnf_tape::driver::*` cluster. `dispatch_one` is the
  `match table.states[state_idx]` pivot; every input byte visits
  ≥ 1 state transition. `advance_or_pop_with` +
  `reserve_compound` + `close_compound` + `emit_leaf` are the
  frame / column bookkeeping arms. `dta_run` is the outer loop.
  Twitter is an 85 % string-payload workload (tweet text,
  hashtags, short keys) with shallow nesting, so the Regex arm
  dominates and compound arms (Seq / Ref) are relatively cheap.
- **Scanner closure (≈ 33 %)**: every Regex state calls
  `cached_dfa(pattern)` with a `String` key lookup on a global
  `HashMap<String, Arc<Dfa>>` guarded by `RwLock`
  (`crates/parse-that/rust/parse_that/src/scanners.rs:30`). The
  `SipHasher13::write` and `HashMap::get::<str>` rows are the
  per-scan hashmap lookup cost, not a miss — the cache is warm.
  `DtaDfaScanner::scan` itself is the wrapper:
  `cached_dfa(pattern).find_at(input, offset)`
  (`crates/core/src/backend/rust/emitter/grammar.rs:244`).
- **Frame / counter update (inside driver 52 %)**: no separate
  frame-depth hotspot emerges; the 9 %/7 % `advance_or_pop_with`
  + `reserve_compound` rows are the compound-frame life cycle.
- **PSI write (≈ 2 %)**: only 2 % self-time. Twitter's string
  payloads write Span records at emit-leaf time and enqueue PSI
  jobs, but the actual f64 decode + column write runs in a batch
  at `finalise`; twitter's low numeric density keeps PSI cheap.
- **Alt-branch enumeration / Ref-chase**: no distinct hot frames
  — collapsed into `dispatch_one`'s branch-predicted arms.

AW-IV lever mapping (per AW-IV.md §W1–W5):

| Lever | Addresses | Expected magnitude |
|-------|-----------|--------------------|
| **Scanner closure** (eliminate `cached_dfa` hashmap lookup per scan; hoist `Arc<Dfa>` onto the `DtaState::Regex` payload at lift time) | 33 % scanner cluster | strong |
| **PSI rayon stage-B** (AW-IV.W1.1) | only 2 % on twitter; marginal | mild |
| **ShapeRef runtime dispatch** (AW-IV.W1.2; collapse same-shape Seq frames) | 9–8 % `advance_or_pop_with` + `reserve_compound` share | moderate |
| **PHF + SIMD keyword** (AW-IV.W2.1/W2.2) | JSON has 3 literal keywords (`true`/`false`/`null`); minor | mild |
| **Bloom + GADT dedup** (AW-IV.W3) | compound dedup on repeated shapes | mild |
| **Pratt / selector classifier / reduce_column** | off-topic for JSON | n/a |

Viability signal: scanner closure (33 %) is the single largest
recoverable block on twitter. Combined with ShapeRef collapse
(moderate), twitter's 5–40 × gap vs post-AU should close
substantially. PSI rayon is not the lever twitter needs.

### citm — 12 054 674 ns/iter, 143 MB/s

`.profiles/samply/json_monolithic/citm/bench.txt` line 2:

```
test citm    ... bench:  12,054,674 ns/iter (+/- 140,495) = 143 MB/s
```

4 911 self-time samples.

Top-20 hot functions by self-time:

| Rank | Self % | Samples | Function |
|-----:|-------:|--------:|----------|
|  1 | 31.89 | 1566 | `bbnf_tape::driver::dispatch_one` |
|  2 | 11.93 |  586 | `bbnf_tape::driver::reserve_compound` |
|  3 | 11.40 |  560 | `bbnf_tape::driver::advance_or_pop_with` |
|  4 |  7.55 |  371 | `bbnf_tape::finaliser::finalise` |
|  5 |  7.33 |  360 | `bbnf_tape::driver::dta_run` |
|  6 |  7.07 |  347 | `<DtaDfaScanner as RegexScanner>::scan` |
|  7 |  5.40 |  265 | `bbnf_tape::driver::emit_leaf` |
|  8 |  4.17 |  205 | `bbnf_tape::driver::close_compound` |
|  9 |  2.52 |  124 | `SipHasher13::write` |
| 10 |  2.22 |  109 | `parse_that::scanners::cached_dfa` |
| 11 |  1.83 |   90 | `core::str::converts::from_utf8` |
| 12 |  1.79 |   88 | `_platform_memcmp` |
| 13 |  1.16 |   57 | `HashMap<String, Arc<Dfa>>::get::<str>` |
| 14 |  0.83 |   41 | `<f64 as FromStr>::from_str` |
| 15 |  0.67 |   33 | `dec2flt::parse_number` |
| 16 |  0.63 |   31 | `DtaTable::rule_entry_for` |
| 17 |  0.51 |   25 | `bbnf_tape::psi::write_decoded` |
| 18 |  0.22 |   11 | `read` |
| 19 |  0.22 |   11 | `_platform_memmove` |
| 20 |  0.18 |    9 | `PayloadStream::fill_columns` |

Category attribution
(`.profiles/samply/json_monolithic/citm/profile.json.gz`):

- driver   72.12% (3542)
- scanner  12.97% (637)
- finalise  8.19% (402)
- libc      2.36% (116)
- psi       2.20% (108)
- utf8      1.83% (90)
- alloc    ≈ 0

Bottleneck classification:

- **State-machine dispatch (72 %)** is the defining signature here.
  citm is deeply-nested: the grammar walks through
  nested objects (`events.{id}.prices.{price}.seats.{seat}`) where
  each level is a Seq → KvPair promotion and every scalar
  terminates with a Regex match or Literal. `dispatch_one`'s
  32 % self-time plus `reserve_compound` + `advance_or_pop_with` +
  `close_compound` (28 % combined) gives 60 % inside the compound
  life cycle alone. The regression manifests here more than on
  twitter: the walker's per-state dispatch cost multiplies across
  the deeper frame stack.
- **Scanner (13 %)** is smaller than twitter because citm has
  far more compound structure per scalar — the object/array
  skeleton dominates over the terminal strings/numbers. The
  per-scan HashMap lookup remains a visible hot slice.
- **Finalise (8 %)**: `bbnf_tape::finaliser::finalise` does the
  final-pass fixups (column-length stamping, frame-depth
  reconstruction when needed). citm's high record count
  (≈ 10⁵+ records) amplifies this over twitter.
- **PSI (2 %)**: same character as twitter — low numeric density
  per byte.

AW-IV lever mapping:

| Lever | Addresses | Expected magnitude |
|-------|-----------|--------------------|
| **ShapeRef runtime dispatch** (AW-IV.W1.2) | The 28 % reserve/advance/close compound cluster; citm's deep nesting of repeated Seq shapes is the canonical ShapeRef beneficiary | strong |
| **Scanner closure** | 13 % scanner | moderate |
| **PSI rayon stage-B** | ≈ 2 %; marginal | mild |
| **PHF + SIMD keyword** | `true`/`false`/`null` only; residual | mild |
| **Bloom + GADT dedup** | citm's repeated compound shapes are prime candidates (every `event` is the same shape) | moderate |

Viability signal: ShapeRef + bloom are the dominant recoverable
blocks on citm; the 32 % `dispatch_one` has no direct lever
except via reduction in transitions per byte, which ShapeRef
provides by collapsing Seq-frame scaffolding around same-shape
sub-trees.

### canada — Category A blocked

`.profiles/samply/json_monolithic/canada/bench.txt`:

```
running 1 test
test canada  ...
thread 'main' (10621885) panicked at crates/core/benches/json/monolithic.rs:55:1:
canada.json: parse failed: Syntax { offset: 2251050, rule: None }
```

Panic at offset 2 251 050 on a 2 251 051-byte input — same
one-byte-from-EOF pattern as `data.json`. canada.json is a
single geospatial coordinate array dominated by floats; the
failure is structural / EOF-handling in the DTA walker, shared
aetiology with the `data` failure. No profile collected;
no AW-IV lever addresses it — AW-III.W2 (Cluster A) owns the
fix.

### data_xl — 262 030 758 ns/iter, 81 MB/s

`.profiles/samply/json_monolithic/data_xl/bench.txt` line 2:

```
test data_xl ... bench: 262,030,758 ns/iter (+/- 371,578,030) = 81 MB/s
```

133 817 self-time samples (highest-sample run in the set;
longest iteration time pushes the profile into the densest
signal.)

Top-20 hot functions by self-time:

| Rank | Self % | Samples | Function |
|-----:|-------:|--------:|----------|
|  1 | 25.29 | 33843 | `bbnf_tape::driver::dispatch_one` |
|  2 | 15.20 | 20334 | `<DtaDfaScanner as RegexScanner>::scan` |
|  3 | 10.61 | 14198 | `bbnf_tape::driver::advance_or_pop_with` |
|  4 |  9.22 | 12344 | `bbnf_tape::driver::reserve_compound` |
|  5 |  6.72 |  8994 | `bbnf_tape::driver::dta_run` |
|  6 |  6.56 |  8781 | `bbnf_tape::finaliser::finalise` |
|  7 |  4.46 |  5970 | `bbnf_tape::driver::emit_leaf` |
|  8 |  4.30 |  5757 | `SipHasher13::write` |
|  9 |  3.30 |  4422 | `bbnf_tape::driver::close_compound` |
| 10 |  2.60 |  3473 | `core::str::converts::from_utf8` |
| 11 |  2.52 |  3375 | `_platform_memcmp` |
| 12 |  2.16 |  2890 | `parse_that::scanners::cached_dfa` |
| 13 |  2.13 |  2851 | `HashMap<String, Arc<Dfa>>::get::<str>` |
| 14 |  1.29 |  1725 | `dec2flt::parse_number` |
| 15 |  1.22 |  1627 | `<f64 as FromStr>::from_str` |
| 16 |  0.67 |   896 | `bbnf_tape::psi::write_decoded` |
| 17 |  0.48 |   645 | `DtaTable::rule_entry_for` |
| 18 |  0.39 |   525 | `madvise` |
| 19 |  0.34 |   454 | `_platform_memmove` |
| 20 |  0.23 |   314 | `PayloadStream::fill_columns` |

Category attribution
(`.profiles/samply/json_monolithic/data_xl/profile.json.gz`):

- driver   59.61% (79771)
- scanner  23.79% (31832)
- finalise  7.04% (9426)
- psi       3.41% (4562)
- libc      2.96% (3961)
- utf8      2.60% (3473)
- alloc    ≈ 0

Bottleneck classification:

- **State-machine dispatch (60 %)**: same signature as citm /
  twitter. `dispatch_one` 25 % + compound-life-cycle 30 % =
  55 % inside driver proper. `dta_run` 7 % is the outer loop.
  data_xl is a mix: repeated records with inner numeric arrays
  plus string-keyed maps. The dispatch share is between
  twitter's 52 % and citm's 72 %.
- **Scanner (24 %)**: higher absolute count than twitter
  (20 334 vs 1 522) because data_xl is 34 × larger by bytes,
  but scanner share (24 % vs 33 %) is lower because each scan
  covers a longer token run on average (longer strings / longer
  numbers).
- **Finalise (7 %)**: similar 7 % share as twitter; the PSI
  batch has more numeric payloads to decode on data_xl
  (`dec2flt::parse_number` 1.3 %, `f64::from_str` 1.2 %), so
  PSI share climbs from twitter's 2 % to data_xl's 3.4 %.
- **PSI (3.4 %)**: rises with numeric density. data_xl is the
  only entry where PSI is non-trivially actionable, though
  still small in absolute share.
- **madvise (0.4 %)** surfaces here: the 21 MB input triggers
  transparent-hugepage / page-fault paths. Not a lever target.

AW-IV lever mapping:

| Lever | Addresses | Expected magnitude |
|-------|-----------|--------------------|
| **Scanner closure** (hoist `Arc<Dfa>` onto `DtaState::Regex`) | 24 % scanner cluster — the per-scan hashmap lookup is a concrete cut | strong |
| **ShapeRef runtime dispatch** (AW-IV.W1.2) | 30 % compound life cycle; data_xl's record-array pattern is a ShapeRef fit | moderate–strong |
| **PSI rayon stage-B** (AW-IV.W1.1) | 3.4 % PSI + 0.4 % numeric decode; the Rayon parallelisation targets `fill_sequential` → `fill_columns` — directly applicable, but small share | mild |
| **Bloom + GADT dedup** | record-array of same-shape rows — canonical dedup target | moderate |
| **PHF + SIMD keyword** | `true`/`false`/`null` only | mild |

Viability signal: scanner closure + ShapeRef are the two
load-bearing levers for data_xl. PSI rayon is directionally
correct but capped at the 3.4 % share visible here.

## Cross-entry synthesis

The three profiled entries share one dominant signature and
differ in a predictable secondary:

1. **Driver self-time is the ceiling on all three entries**
   (52 % / 72 % / 60 %). `dispatch_one` is the state-machine
   pivot and the 4–5 sibling driver functions
   (`advance_or_pop_with`, `reserve_compound`, `close_compound`,
   `emit_leaf`, `dta_run`) each draw 3–12 % self-time per entry.
   The `dta_run → dispatch_one → arm` stack accounts for every
   byte of input.
2. **Scanner closure ranks #1 on twitter, #5–6 on citm/data_xl**.
   `cached_dfa → HashMap::get → Sip13::write` is the
   always-present overhead per Regex scan; its share scales
   with the Regex-state frequency in the grammar path and
   inversely with scan length. Twitter is string-heavy so the
   Regex arm fires often on short matches — 33 % scanner.
   citm is compound-heavy so compound arms fire more — 13 %
   scanner. data_xl is the middle ground at 24 %.
3. **Compound-frame life cycle is the other fat band**.
   `reserve_compound` + `advance_or_pop_with` + `close_compound`
   combined is 20 % / 27 % / 22 % across twitter / citm /
   data_xl. This is what ShapeRef runtime dispatch
   (AW-IV.W1.2) targets — collapsing the Seq frame scaffolding
   around same-shape sub-trees eliminates the per-level
   reserve/advance/close triplet for dedupable shapes.
4. **PSI is a small slice everywhere** (2 % / 2 % / 3.4 %).
   AW-IV.W1.1's PSI rayon stage-B applies but the recovery
   ceiling is capped at the `fill_columns` share plus the
   numeric decode (`dec2flt` + `f64::from_str`).
5. **Finalise is 6–8 % everywhere**. `finaliser::finalise` is
   the final-pass fixup; its share is proportional to record
   count, not content shape.
6. **Parse failures at EOF − 1 on data + canada** share a single
   aetiology (DTA walker's EOF / trailing-ws handling). One fix
   in AW-III.W2 should close both.
7. **Utf8 validation (2–4 %)** is `core::str::converts::from_utf8`
   called from the scanner adapter path (string payload framing).
   Small but addressable if the scanner closure lever also takes
   a byte-slice fast path (skipping an extra utf8 validation).

## Recommendation

Activate these AW-IV levers first, in this order:

1. **Scanner closure** — hoist the `Arc<Dfa>` onto the
   `DtaState::Regex` payload at lift time so `dispatch_one`'s
   Regex arm goes straight to `dfa.find_at(input, offset)`.
   This eliminates the global-cache HashMap lookup (and the
   `String` → hash path) on every scan. Directly addresses the
   33 % twitter / 24 % data_xl / 13 % citm scanner cluster.
   **Expected strong recovery** on all three entries (dominant
   lever on twitter).
2. **ShapeRef runtime dispatch (AW-IV.W1.2)** — collapses
   same-shape Seq compounds so the
   `reserve_compound → emit_child → close_compound` triplet
   doesn't fire at every nesting level. Directly addresses
   the 20–28 % compound-life-cycle cluster. **Expected
   moderate–strong** on citm (deep nesting), moderate on
   twitter + data_xl.
3. **Bloom + GADT dedup (AW-IV.W3)** — complements ShapeRef by
   catching same-shape compounds across rules (not just within).
   Moderate recovery on citm / data_xl (repeated record rows).
4. **PSI rayon stage-B (AW-IV.W1.1)** — directionally correct
   for data_xl's numeric decode share (3.4 %) but capped low.
   Ship after the two big levers; don't defer.

Not applicable for `json_monolithic`:

- **PHF + SIMD keyword** (W2.1/W2.2) — JSON grammar has three
  keywords (`true`/`false`/`null`); no material benefit.
- **Selector classifier** (W2.3) — CSS-specific.
- **Pratt generalisation** (W3.3) — expression-operator specific.
- **reduce_column** (W5.1) — consumer-side, not parse hot path.

Parse correctness work in AW-III.W2 unblocks the full matrix:
the `data` and `canada` failures sit outside AW-IV's scope
(per the Category A classification in AW-III.W2).

## Artefact index

All artefacts under `.profiles/samply/json_monolithic/`:

| Entry | bench.txt | profile.json.gz | syms.json | syms-proof | load.txt | record.txt | build.txt |
|-------|:---------:|:---------------:|:---------:|:----------:|:--------:|:----------:|:---------:|
| data    | present (panic) | stale Apr 15 | stale Apr 15 | stale Apr 15 | stale Apr 15 | stale Apr 15 | fresh |
| twitter | fresh | fresh | fresh | fresh | fresh | fresh | fresh |
| citm    | fresh | fresh | fresh | fresh | fresh | fresh | fresh |
| canada  | present (panic) | stale Apr 15 | stale Apr 15 | stale Apr 15 | stale Apr 15 | stale Apr 15 | fresh |
| data_xl | fresh | fresh | fresh | fresh (manual) | fresh (manual) | fresh | fresh |

"fresh" = 2026-04-17 00:24–00:36 timestamp from this wave.
"stale" = Apr 15 prior-wave artefact, retained in directory
but not used for attribution (Category A parse failure means
no profile was collected for this wave).
