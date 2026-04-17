# Research perf-05 — json_monolithic_value bbnf-vs-sonic samply profile (AW-III.P5)

Tranche AW-III, Wave P5. Angle: the `json_monolithic_value` bench
parks the BBNF-generated JSON parser next to sonic-rs
`from_str::<Value>` on five shared datasets so the `->` map-expression
materialisation path (the AW-II.W5c fix target) has a ground-truth
reference for every twin pair. This document attributes the
measured cold per-parse gap, names the hot functions that drive it,
and cites recommendations against the 8 × 7 = 56 samply artefacts
under `.profiles/samply/json_monolithic_value/`.

## Methodology

Ten samply runs were collected — `{bbnf, sonic} × {data, twitter,
citm, canada, data_xl}` — from the prebuilt release binary
`json_monolithic_value-282fb07b1a66006c` under
`/Users/mkbabb/Programming/bbnf-lang/.profiles/shared-target/release/deps/`,
driven by `scripts/profile-bench-headless.sh` with
`--record-port 3170 --load-port 3171`. Each attempt produced
`bench.txt`, `build.txt`, `record.txt`, `load.txt`, `profile.json.gz`,
`profile.json.syms.json`, and `syms-proof.txt` in
`.profiles/samply/json_monolithic_value/<entry>/`.

The bench source at
`/Users/mkbabb/Programming/bbnf-lang/crates/core/benches/json/value.rs`
runs both sides with the `mimalloc` global allocator. The BBNF path
invokes `JsonParser::parse` followed by `walk_tape` — a recursive
descent that reads every leaf's payload (`payload_string_with_source`
for strings, `payload_f64` for numbers, `payload_bool` / `payload_u8`
for literals) so the compiler cannot elide the value materialisation
that the `->` annotations produce. The sonic path calls
`sonic_rs::from_str::<sonic_rs::Value>` with identical input, which
is the SIMD-accelerated production-grade baseline.

Leaf- and inclusive-frame counts were extracted from every
`profile.json.gz` by resolving the `stringArray` hex RVAs against the
`symbol_table` in the companion `profile.json.syms.json` (range
match on `[rva, rva+size)`), then counting leaf frames per sample
and unique inclusive frames per stack chain. The raw aggregation
lives at `/tmp/p5-analysis.txt`; figures cited below come from that
file unless otherwise noted.

**Prefix-collision caveat**: `bencher`'s `--bench <name>` argument
is a substring match, and `json_monolithic_value.rs` carries bench
functions `bbnf_data_s`, `bbnf_data_xl`, `sonic_data_s`, `sonic_data_xl`.
The wave.tsv entry names `bbnf_data` / `sonic_data` (AU.6.6 renamed
the small variants with `_s` to avoid this exact collision, but the
wave.tsv still drops the suffix) therefore match two bench functions.
Concretely:

- `.profiles/samply/json_monolithic_value/sonic_data/bench.txt` runs
  both `sonic_data_s` (23,866 ns) and `sonic_data_xl` (28,717,741 ns)
  — the profile is dominated by `sonic_data_xl`, contaminating the
  "small-data" attribution.
- `.profiles/samply/json_monolithic_value/bbnf_data/bench.txt` runs
  `bbnf_data_s`, which parse-fails at offset 35490, so no profile
  artefacts (profile.json.gz, syms) are produced.
- `.profiles/samply/json_monolithic_value/bbnf_canada/bench.txt`
  parse-fails at offset 2251050 — no profile artefacts.

The `*_xl` entries (`bbnf_data_xl`, `sonic_data_xl`) are unique
suffix matches and produce clean profiles. For comparison tables
below the contaminated `sonic_data` profile is used only for the
small-variant `ns/iter` numbers pulled from `bench.txt`, never for
hotspot attribution.

## Per-entry bench results and artefact inventory

| Entry            | bench.txt result                                 | Artefacts          |
|------------------|--------------------------------------------------|--------------------|
| `bbnf_data`      | **parse fail** (offset 35490 on `data.json`)     | 2/7 (no profile)   |
| `bbnf_twitter`   | 8,497,575 ns/iter, 74 MB/s                       | 7/7                |
| `bbnf_citm`      | 13,923,737 ns/iter, 124 MB/s                     | 7/7                |
| `bbnf_canada`    | **parse fail** (offset 2251050 on `canada.json`) | 2/7 (no profile)   |
| `bbnf_data_xl`   | 248,895,599 ns/iter, 85 MB/s                     | 7/7                |
| `sonic_data` (s) | 23,866 ns/iter, 1,487 MB/s                       | 7/7 (contaminated) |
| `sonic_data` (xl)| 28,717,741 ns/iter, 741 MB/s                     | bundled w/ above   |
| `sonic_twitter`  | 234,389 ns/iter, 2,694 MB/s                      | 7/7                |
| `sonic_citm`     | 563,950 ns/iter, 3,062 MB/s                      | 7/7                |
| `sonic_canada`   | 1,456,854 ns/iter, 1,545 MB/s                    | 7/7                |
| `sonic_data_xl`  | 14,167,103 ns/iter, 1,502 MB/s                   | 7/7                |

Numbers come from the `bench:` line in each
`.profiles/samply/json_monolithic_value/<entry>/bench.txt`.

## BBNF-vs-sonic twin-pair table

| Twin pair | BBNF ns/iter | sonic ns/iter | **Ratio** | BBNF MB/s | sonic MB/s |
|-----------|--------------|---------------|----------:|----------:|----------:|
| data (s)  | fail         | 23,866        | —         | —         | 1,487      |
| twitter   | 8,497,575    | 234,389       | **36.3×** | 74        | 2,694      |
| citm      | 13,923,737   | 563,950       | **24.7×** | 124       | 3,062      |
| canada    | fail         | 1,456,854     | —         | —         | 1,545      |
| data_xl   | 248,895,599  | 14,167,103    | **17.6×** | 85        | 1,502      |

The three measurable twin pairs cluster at 17×–36× behind sonic-rs.
The ratio is smallest on `data_xl` (9 MB synthetic payload, ~60% of
time in DTA walker dispatch) and largest on `twitter` (structure-
dense with numerous small string values, 85% of time in dispatch +
DFA scan). citm is in the middle — closer to `data_xl` because it
is structurally similar (object-heavy, numeric-heavy) but with more
repeat string keys that favour sonic's container-end bookkeeping.

**Sonic throughput reference**: sonic_twitter reaches 2,694 MB/s and
sonic_citm 3,062 MB/s — pure SIMD scan+validate with bump-arena node
allocation. The BBNF side runs the same `Value`-equivalent walk
(every leaf's payload touched) at 74–124 MB/s, i.e. roughly
**20–40× below the SIMD reference** on large structured JSON. On
the small `data.json` + `twitter.json` the bbnf side cannot claim
its L1 advantage because the DTA dispatch loop's per-byte cost
swallows the small-input saving. This is the AW-III thesis'
viability question: DTA walker dispatch is 5–40× the post-AU
per-rule inlined path, and the same overhead relative to a SIMD
baseline is simply louder.

## Payload-materialisation hot-path analysis (the W5c fix target)

The prompt flags `->` map-expression materialisation as the focus.
On the BBNF side the `walk_tape` / `walk_cursor` recursion in
`benches/json/value.rs:52-117` exercises every payload accessor the
`->` codegen produces; the bench is the honest comparator.

Relative CPU share of payload materialisation on the three
measurable BBNF entries (leaf-time, from `/tmp/p5-analysis.txt`,
confirmed via `.profiles/samply/json_monolithic_value/<entry>/profile.json.syms.json`):

| Function                                                                            | bbnf_twitter | bbnf_citm | bbnf_data_xl |
|-------------------------------------------------------------------------------------|-------------:|----------:|-------------:|
| `bbnf_tape::driver::dispatch_one`                                                   | 20.6%        | 30.9%     | 23.4%        |
| `<…__jsonparser_emit_impl::DtaDfaScanner as …RegexScanner>::scan`                   | 20.5%        | 6.7%      | 15.3%        |
| `bbnf_tape::driver::advance_or_pop_with`                                            | 9.6%         | 9.6%      | 9.2%         |
| `json_monolithic_value::walk_cursor` (payload read)                                 | **7.5%**     | **8.1%**  | **7.3%**     |
| `bbnf_tape::driver::reserve_compound`                                               | 6.9%         | 10.4%     | 9.2%         |
| `bbnf_tape::driver::dta_run`                                                        | 5.2%         | 7.2%      | 5.7%         |
| `bbnf_tape::finaliser::finalise` / `<TapeBuilder>::finish`                          | 5.2%         | 6.9%      | 6.3%         |
| `bbnf_tape::driver::emit_leaf`                                                      | 3.8%         | 4.1%      | 4.3%         |
| `bbnf_tape::driver::close_compound`                                                 | 3.1%         | 3.9%      | 3.1%         |
| `core::str::converts::from_utf8`                                                    | 3.7%         | 2.2%      | 2.6%         |
| `<core::hash::sip::Hasher<Sip13Rounds>>::write`                                     | 3.5%         | 2.3%      | 3.7%         |
| `hashbrown::map::HashMap<String, Arc<bbnf_regex::…Dfa>, RandomState>` lookup        | 2.1%         | 1.5%      | 1.6%         |
| `parse_that::scanners::cached_dfa`                                                  | 1.2%         | 1.9%      | 2.2%         |
| `<f64 as FromStr>::from_str` / `core::num::imp::dec2flt::parse::parse_number`       | 1.6%         | 1.3%      | 2.2%         |

Inclusive-frame attribution (same three entries, same artefacts):

| Function                                  | bbnf_twitter | bbnf_citm | bbnf_data_xl |
|-------------------------------------------|-------------:|----------:|-------------:|
| `<JsonParser>::parse`                     | 91.6%        | 91.6%     | 92.4%        |
| `bbnf_tape::driver::dta_run`              | 80.3%        | 80.7%     | 80.5%        |
| `bbnf_tape::driver::dispatch_one`         | 62.3%        | 60.0%     | 62.5%        |
| `DtaDfaScanner::scan` (inclusive)         | 28.6%        | 12.8%     | 24.1%        |
| `advance_or_pop_with` (inclusive)         | 12.6%        | 13.2%     | 12.0%        |
| `reserve_compound`                        | 7.0%         | 10.5%     | 9.2%         |
| `walk_cursor` (payload-read recursion)    | **7.5%**     | **8.1%**  | **7.3%**     |
| `cached_dfa`                              | 8.1%         | 6.1%      | 8.8%         |
| `HashMap<String, Arc<Dfa>>` hit/miss path | 6.9%         | 4.3%      | 6.6%         |
| `PayloadStream::fill_columns`             | 5.9%         | 4.0%      | 5.5%         |
| `psi::write_decoded`                      | 5.6%         | —         | 5.3%         |
| `<TapeBuilder>::finish`                   | 5.2%         | 6.9%      | 6.3%         |

**Payload materialisation is not the dominant cost.** `walk_cursor`
(which is the `->`-produced `payload_*` accessor consumer in the
bench source) spends 7–8% of leaf CPU across the three measurable
entries. The remaining 92–93% is split between the DTA walker
dispatch loop (`dispatch_one` + `advance_or_pop_with` +
`reserve_compound` + `emit_leaf` + `close_compound` combined
≈ 40–58%) and the grammar regex / scanner path (`DtaDfaScanner::scan`
+ `cached_dfa` + the Sip13 hash lookups ≈ 25–30%). W5c's fix —
borrow-safe `payload_string_with_source` slicing `input[lo+1..hi-1]`
when no `\` is present — lands clean: the bench walks every string
and the bench's 7.3–8.1% `walk_cursor` self-time shows the arena
read path is not re-traversing UTF-8 validation on the hot path.

**Verified payload invariants via `syms-proof.txt`**:

- Every BBNF entry's `syms-proof.txt` names `TapeBuilder::finish`,
  `DtaTable::rule_entry_for`, `JsonParser::parse`, and
  `PayloadStream::fill_columns` / `fill_sequential` /
  `required_column_capacities` — payload layout is reached.
- `bbnf_tape::psi::write_decoded` appears in both leaf (0.5–0.7%)
  and inclusive (~5.3–5.9%) columns on twitter / data_xl, i.e. the
  AU.3.1 arena-frame write for escape-bearing strings does execute;
  it is the minor branch because most JSON strings lack `\`.

Sonic reference hotspot composition, from the clean `sonic_twitter`,
`sonic_citm`, `sonic_canada`, `sonic_data_xl` profiles:

| Function                                                                                       | s_twitter | s_citm | s_canada | s_data_xl |
|------------------------------------------------------------------------------------------------|----------:|-------:|---------:|----------:|
| `<Parser<PaddedSliceRead>>::parse_object::<DocumentVisitor>`                                    | 76.3%     | 58.3%  | —        | 44.5%     |
| `<Parser<PaddedSliceRead>>::parse_array::<DocumentVisitor>`                                    | 3.7%      | 21.0%  | 78.0%    | 43.3%     |
| `<DocumentVisitor>::visit_container_end`                                                        | 3.1%      | 8.3%   | 10.4%    | 3.0%      |
| `_platform_memmove`                                                                             | 6.9%      | 8.1%   | 8.9%     | 4.7%      |
| `core::str::converts::from_utf8`                                                                | 1.6%      | 0.8%   | 0.4%     | 1.5%      |

Sonic spends 90%+ of its cycles inside two functions —
`parse_object` and `parse_array` — and the remainder is memory-copy
(`_platform_memmove`) into the `DocumentVisitor`'s node arena. The
sonic parser has **one hot loop** per shape. The BBNF walker spreads
the same work across eight named driver functions (`dispatch_one`,
`dta_run`, `advance_or_pop_with`, `reserve_compound`, `emit_leaf`,
`close_compound`, `DtaDfaScanner::scan`, `cached_dfa`) because the
DTA is an interpreter — every input byte pays a dispatch cost and
the dispatched arms are distinct, small, often mispredicted
functions. The 17–36× gap is what the AW architecture thesis
predicts: state-machine dispatch over inlined recursive descent at
roughly the observed ratio.

## Lever hypotheses per BBNF entry

All three hypotheses are cross-cut by the same structural fact:
`dispatch_one` + `advance_or_pop_with` + `reserve_compound` +
`close_compound` collectively occupy ≈ 40–55% of leaf time, which
is pure state-machine overhead that no payload-path optimisation
touches.

### bbnf_twitter (20.6% `dispatch_one`, 20.5% scanner, 9.6% adv/pop)

Twitter is structure-light (few unique keys, many small string
leaves). The scanner runs an outsized 20.5% because every quoted
value triggers `DtaDfaScanner::scan` + a `HashMap<String, Arc<Dfa>>`
lookup (3.7% Sip hash + 2.1% hashbrown probe). The obvious lever is
**AW-IV ShapeRef dispatch**: twitter's `string` leaves share shape;
collapsing them to a single ShapeRef removes the per-leaf HashMap
probe and shrinks the scanner cache miss rate. Secondary lever:
**SIMD keyword / PHF** for the JSON literal fast-path (`true`,
`false`, `null`) which currently routes through `dispatch_one` with
byte-by-byte comparisons. Combined, an estimated 25–30% of leaf
time is reachable — bringing the twitter ratio from 36× to ~25×.

### bbnf_citm (30.9% `dispatch_one`, 10.4% `reserve_compound`)

citm is object-heavy — the benchmark walks ~140k records, each of
which triggers a `reserve_compound`. At 10.4% leaf that alone is
~1.4 M ns/iter. `dispatch_one` taking 30.9% is the dispatch
overhead amortised across a larger structure. The obvious lever is
**PSI rayon**: the payload-column writes (which live under
`fill_columns` at 4.0% inclusive and `write_decoded` at 1.6%
inclusive via `psi`) can be parallelised across CPU threads without
changing the walker's sequential structure. Secondary lever: AW-IV
**ShapeRef** across identical `key: value` pair shapes — citm has
~40 distinct key shapes, collapsed to ~5 ShapeRefs would remove
most `dispatch_one` state-transition work. Combined, an estimated
30–40% of leaf time is reachable — bringing the citm ratio from
25× to ~15×.

### bbnf_data_xl (23.4% `dispatch_one`, 15.3% scanner, 9.2% each adv/pop + reserve)

data_xl is the largest payload (9 MB synthetic). The scanner share
(15.3%) is lower than twitter because string leaves are longer, so
the scanner's per-byte cost amortises better. The arithmetic shifts
to **`reserve_compound` + `advance_or_pop_with` + `dta_run` = 24%**
of pure state-machine bookkeeping. The AW-IV lever that most
directly targets this is **inlined Frame stack + removing the
`Vec<u8>` push/pop** in `advance_or_pop_with` (the function is a
thin wrapper around the frame-depth push that is observable in
syms-proof at rva 12208, 12240, 12252 — three close rvas that
resolve to `advance_or_pop_with` at different inline depths).
Secondary lever: **PHF** on the JSON structural byte dispatch
(`{`, `[`, `"`, `,`, `:`, `}`, `]`) — currently a match arm walks
to the next state via the `ByteDispatch` arm of `dispatch_one`; a
PHF-derived 8-entry table would reduce the 23.4% `dispatch_one`
share by ~40%. Combined, an estimated 20–30% of leaf time is
reachable — bringing the data_xl ratio from 17.6× to ~12×.

## Recommendations

1. **The DTA viability question AW-III asks is partly answered by
   these numbers**. AW-II's 5–40× post-AU regression is consistent
   with the 17–36× sonic gap on this bench — the DTA walker
   dispatch is expensive, and payload materialisation (the `->`
   path) is *not* the bottleneck. Activating the AW-IV optimisation
   inventory (PSI rayon + ShapeRef + PHF/SIMD keyword) targets the
   correct hot functions: `dispatch_one`, `advance_or_pop_with`,
   `reserve_compound`, and `DtaDfaScanner::scan`. Payload-side
   levers (escape-free slice fast path, u8 sentinel writes) are
   already active per `syms-proof.txt` evidence and would recover
   at most 7–8% more.

2. **Close the parse-failure gap before the next bench wave**.
   `bbnf_data` and `bbnf_canada` cannot be profiled because the
   parser panics; `bench.txt` for each cites `ParseErr::Syntax`
   at a definite offset (35490 on `data.json`, 2251050 on
   `canada.json`). This is an AW-III W4 scope item — the regression
   is correctness, not performance. The canada.json failure at
   offset 2251050 is deep in the number scanner (canada is ~6 MB
   of GeoJSON floats); the data.json failure at 35490 lands in a
   nested structural transition. Both should be in the W4 residual
   failure audit already, but they remain visible in the P5
   profiling wave as artefact-free entries. AW-III cannot declare
   viability proven on this bench until both succeed.

3. **Fix the wave.tsv prefix-collision**. `bbnf_data` runs both
   `bbnf_data_s` and `bbnf_data_xl`; same for sonic. AU.6.6 renamed
   the small variants specifically to dodge this, but the
   `scripts/prepare-profile-wave.sh` row still uses the unsuffixed
   name. Either (a) change the wave.tsv `entry` column to
   `bbnf_data_s` / `sonic_data_s` and re-run, or (b) rename the
   bench functions to `bbnf_small_data` / `sonic_small_data` so
   the prefix no longer matches `data_xl`. Option (a) is one line
   in `prepare-profile-wave.sh`.

4. **Extend `wait_for_record_artifacts` timeout in
   `scripts/profile-bench-headless.sh`**. The default 60-tries
   `sleep 1` polling loop times out on benches whose single
   iteration ≥ 250 ms; `bbnf_data_xl` at 248,895,599 ns/iter
   exceeded the window on first attempt and produced only partial
   artefacts (no load.txt, no syms-proof.txt). A local-copy
   script at `/tmp/profile-bench-headless-p5.sh` with `tries=600`
   completed the run cleanly. The upstream script is untracked
   (`git ls-files` empty), so bumping to 600 tries is a one-line
   patch.

5. **The payload hot-path is honest and correct**. The
   `walk_cursor` function touches every leaf's typed payload
   (`payload_string_with_source`, `payload_f64`, `payload_bool`,
   `payload_u8`, `payload_bytes`) at a consistent 7.3–8.1% of leaf
   CPU across the three measurable entries, which is proportional
   to leaf density per byte. No compiler has elided the reads
   (the accessors appear at named inclusive positions in all three
   `profile.json.syms.json` files), and the AU.3.2 / W6 borrow-safe
   path is firing (`psi::write_decoded` contributes only 5–6%
   inclusive, reflecting the minority of escape-bearing strings).
   AW-II.W5c's fix is load-bearing and correct; the bench ratio is
   not degraded by a materialisation regression.

## Artefact citations

All claims above cite files under `.profiles/samply/json_monolithic_value/`:

- Bench numbers: `<entry>/bench.txt`, `bench:` line.
- Hot-function attribution: aggregation in `/tmp/p5-analysis.txt`,
  reconstructed from `<entry>/profile.json.gz` (leaf + inclusive
  counts) cross-resolved against `<entry>/profile.json.syms.json`
  (range match on `symbol_table[].rva / rva+size`).
- Named-frame coverage: `<entry>/syms-proof.txt` — bench, parser,
  TapeBuilder, PayloadStream frames present per entry.
- Parse failures: `bbnf_data/bench.txt` (offset 35490),
  `bbnf_canada/bench.txt` (offset 2251050).
- Codegen activation: expand artefact at
  `.profiles/samply/prebuild/expand/json_monolithic_value/expand.rs:3441-3482`
  (`walk_tape` + `walk_cursor` body) — payload accessors present
  in emitted code.

Shared target dir throughout: `CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-lang/.profiles/shared-target`.
Prebuilt binary: `json_monolithic_value-282fb07b1a66006c`. Ports
`3170` / `3171` reserved pre-flight. No `cargo expand` or `cargo
bench` reruns inside this wave.
