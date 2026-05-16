# SK-V7 C3: real_typed_struct typed DirectBuild profile

Cohort C3 of the SK-V7 research wave. Profiles SK-V6 Wave 3 commit `ab06ff11`
"feat(sk-v6-wave3): lower host output schemas into generated typed DirectBuild".

Single-process cold throughput from `profile_direct` (5,000 iters, 16-iter
warm-up, then timed loop) and hot-leaf attribution from `samply record --save-only`
(8,000 iters) at `aarch64` (`apple-m*`) with the `release` profile. All raw
samply traces and per-trace hot-leaf reports live under
`/tmp/skv7-C3-profiles/`.

## 1. Workload infrastructure

Test/bench wiring lives in three places:

- `skinny/crates/bbnf-bench/src/real_typed_struct.rs` — fixture enum,
  borrowed `'a` host structs (`TwitterSearch<'a>`, `Tweet<'a>`,
  `UpdateCenter<'a>`, `UpdateCore<'a>`, `Plugin<'a>`, `PluginEntry<'a>`),
  Track 1 / Track 2 / sonic / serde adapters, oracle checksum, and the
  custom `deserialize_plugin_entries` visitor that materialises the
  `plugins` object as a `Vec<PluginEntry>` rather than a `HashMap`.
  Track 1 calls `crate::generated_real_typed::parse_twitter_search`
  /`parse_update_center` (lines 111-123); Track 2 delegates to
  `serde_typed` (lines 125-130); sonic calls `sonic_rs::from_slice`
  (lines 146-158).
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs` — the @generated
  module the codegen produced for the `sk-v6-real-typed-v1` schema
  (header line 3). Top-level entry points are `parse_twitter_search`
  (`:31`) and `parse_update_center` (`:42`); the inlined `DirectParser`
  state machine sits at `:351-597`.
- `skinny/xtask/src/real_typed_schema.rs` — the host-output schema
  (lines 7-98). Roots, struct shapes, presence (`Default` for every field
  in this packet), and the per-`Plugin` "ignored field" allowlist live
  here.
- Codegen entry point: `skinny/crates/codegen/src/json_typed_direct.rs`
  (`render` at `:9`, struct-body emitter at `:69-118`, helper emitter at
  `:298-355`, embedded `DirectParser` runtime template at `:357-646`).
- Bench wiring: `skinny/crates/bbnf-bench/benches/json_parity.rs`
  registers `track1_real_typed_struct` (`:262`), `track2_real_typed_struct`
  (`:286`), `sonic_rs_real_typed_struct` (`:310`),
  `serde_json_real_typed_struct` (`:331`) and the cross-track parity
  assertion at `:20-22`.

The profile binary is `bbnf-bench/src/bin/profile_direct.rs`. Mode
selectors `real_typed_track1|real_typed_track2|real_typed_sonic|real_typed_serde`
dispatch through `real_typed_checksum` (`profile_direct.rs:81-93`).

## 2. Cold throughput (single-process, this machine)

Reproduced via `CARGO_TARGET_DIR=/tmp/skv7-cargo/C3 cargo build --release -p bbnf-bench --bin profile_direct`
then `profile_direct 5000 <corpus> <mode>`:

| corpus        | track1 (Mbps) | track2 (Mbps) | sonic (Mbps) | T1/sonic | T1/T2  |
|---------------|--------------:|--------------:|-------------:|---------:|-------:|
| twitter       |        14,878 |        12,219 |       12,696 |    117% |   122% |
| update_center |         9,714 |         7,874 |        9,869 |     98% |   123% |

These are smaller than the criterion-reported values in `RESULTS.md`
(twitter 18,129 / sonic 11,969 / 151.5%, and update_center 12,044 /
sonic 12,144 / 99.2%) because criterion's run-tree alternates corpora and
serialises differently than the profile binary's tight loop. The
*ratios* are stable: T1 strongly beats sonic on twitter, ties sonic on
update_center.

## 3. Per-corpus hot leaves (samply, addr2line on dSYM)

### 3.1 twitter / Track 1 (`/tmp/skv7-C3-profiles/twitter-track1.hot.txt`)

Total profile_direct-attributed samples: 2,730 of 2,748 (99.3%).

| % of profile | leaf                                                                        |
|-------------:|-----------------------------------------------------------------------------|
|       33.52% | `DirectParser::skip_value`           `generated_real_typed.rs:496` (`b'{' => skip_object`) |
|       27.18% | `DirectParser::skip_value`           `generated_real_typed.rs:498` (`b'"' => skip_string_raw`) |
|       21.39% | `profile_direct::run_once`           `profile_direct.rs:64`                  |
|        3.96% | `DirectParser::skip_value`           `generated_real_typed.rs:491` (`self.ws()`) |
|        3.00% | `parse_option_scalar_string`         `generated_real_typed.rs:317`           |
|        2.45% | `DirectParser::skip_value`           `generated_real_typed.rs:499` (`b'-' | b'0'..=b'9' => number_span`) |
|        2.12% | `DirectParser::skip_value`           `generated_real_typed.rs:505` (number_span return) |
|        2.01% | `DirectParser::skip_value`           `generated_real_typed.rs:495` (`match byte` dispatch) |
|        1.21% | `DirectParser::skip_value`           `generated_real_typed.rs:490` (entry) |
|        0.59% | `DirectParser::skip_value`           `generated_real_typed.rs:501` (`f` literal) |
|        0.48% | `DirectParser::skip_array`           `generated_real_typed.rs:530`           |
|        0.40% | `DirectParser::skip_array`           `generated_real_typed.rs:536`           |
|        0.29% | `DirectParser::skip_array`           `generated_real_typed.rs:529`           |
|        0.26% | `parse_that_regex::unescape_json_string` `parse-that-regex/src/lib.rs:867` |

Aggregated, **74.5% of all samples are inside `DirectParser::skip_value`
or `skip_array`**, with only ~3% in actual string materialisation and
the rest in the timed-loop driver and minor leaves. There is essentially
no time in `parse_string`, `parse_u64`, or struct-construction code.

### 3.2 twitter / Track 2 (`/tmp/skv7-C3-profiles/twitter-track2.hot.txt`)

Track 2 routes through `serde_json::from_slice`. The dominant 38.25% leaf
is `IgnoredAny::next_value` (`serde_json/src/de.rs:1918`); skipping is
done through `<SliceRead>::skip_to_escape` (`read.rs:434-481`) plus
`Read::ignore_str`. So Track 2 *also* spends most time skipping, but it
pays for serde's visitor dispatch, UTF-8 validation (8.30% in
`core::str::converts::from_utf8`), and `IgnoredAny` instantiation.

### 3.3 twitter / sonic (`/tmp/skv7-C3-profiles/twitter-sonic.hot.txt`)

sonic-rs concentrates ~62% of samples in
`<sonic_rs::parser::Parser>::skip_one` (`parser.rs:1439-1484`) — sonic's
SIMD-tape skipper. It also runs `simdutf8::validate_utf8_basic_neon`
(3.90% at `mod.rs:18`) and a `Tweet::Deserialize::Visitor` (6.48%).

The Track 1 advantage on twitter is structural: sonic's `skip_one` is
SIMD-tape but goes through a `Parser::Read` indirection plus serde
visitor dispatch per kept field. The generated `DirectParser::skip_value`
is a byte-level `match byte {...}` over `&'i [u8]` with monomorphic
recursion, no traits, no SIMD-tape build, and no UTF-8 validation.

### 3.4 update_center / Track 1 (`/tmp/skv7-C3-profiles/update_center-track1.hot.txt`)

Total profile_direct samples: 3,442 of 3,444 (99.94%).

| % of profile | leaf                                                                        |
|-------------:|-----------------------------------------------------------------------------|
|       19.52% | `profile_direct::run_once`                  `profile_direct.rs:64`           |
|       11.80% | `parse_type_plugin`                         `generated_real_typed.rs:259` (`"excerpt" => skip_string_raw`) |
|        9.88% | `parse_option_scalar_string`                `generated_real_typed.rs:317` (`parser.parse_string()`) |
|        8.86% | `parse_type_plugin`                         `generated_real_typed.rs:238` (`parser.parse_string()` for key) |
|        7.73% | `DirectParser::skip_value`                  `generated_real_typed.rs:498` (`skip_string_raw`) |
|        4.94% | `DirectParser::skip_value`                  `generated_real_typed.rs:496` (`skip_object`) |
|        3.28% | `parse_type_plugin`                         `generated_real_typed.rs:268` (`"wiki" => skip_string_raw`) |
|        3.14% | `parse_type_plugin`                         `generated_real_typed.rs:260` (`"gav" => skip_string_raw`) |
|        2.32% | `parse_type_plugin`                         `generated_real_typed.rs:271` (`parser.ws()` post-arm) |
|        2.12% | `parse_type_update_center`                  `generated_real_typed.rs:159`     |
|        1.92% | `parse_type_plugin`                         `generated_real_typed.rs:267` (`"sha1" => skip_string_raw`) |
|        1.80% | `parse_type_plugin`                         `generated_real_typed.rs:264` (`"releaseTimestamp"` arm) |
|        1.66% | `DirectParser::skip_value`                  `generated_real_typed.rs:505`     |

Aggregating per-Plugin arms across `:255-268`: about **27% of samples
are spent on the per-`Plugin` `match key.as_ref()` arms** for the
ignored-string fields, plus another ~10% on the post-arm `parser.ws()`
and `parser.parse_string()` key probe. The dominant *true* cost is no
longer skipping anonymous values — it is per-key string identification
across the 14 known-ignored arms enumerated in
`xtask/src/real_typed_schema.rs:81-94`.

### 3.5 update_center / Track 2 (`/tmp/skv7-C3-profiles/update_center-track2.hot.txt`)

Track 2 (serde_json) shows 10.09% in `libsystem_malloc.dylib` and 19.49%
in `core::str::converts::from_utf8` — serde's allocations + UTF-8
validation are visibly hot, and `PluginEntriesVisitor` (`real_typed_struct.rs:259-278`)
re-allocates per call.

### 3.6 update_center / sonic (`/tmp/skv7-C3-profiles/update_center-sonic.hot.txt`)

sonic's allocator share is the largest on this row: 12.86%
`libsystem_malloc.dylib`, 8.67% `libsystem_platform.dylib`. The
`Plugin::Deserialize::Visitor` itself is 22.46% of profile samples.
sonic and Track 1 end up close because the work shifts to malloc and
per-`Plugin` visitor entry — paths Track 1 also pays for, but with a
specialised match arm rather than a generic visitor.

## 4. Why twitter PASSes at 151.5% (and how the win is realised)

The schema (`xtask/src/real_typed_schema.rs:23-36`) keeps only two
fields on `Tweet`: `id` and `text`. Every other key under each
`statuses[*]` object — `created_at`, `id_str`, `source`, `truncated`,
`user.*` (a 12-field nested object), `entities.*`, `retweeted_status`,
etc. — falls into the `UnknownFieldPolicy::Skip` arm at
`json_typed_direct.rs:104`, which emits `_ => parser.skip_value()?,` at
`generated_real_typed.rs:110` of the `Tweet` match. The twitter fixture
is *predominantly* unknown-field bytes, so the work the generated parser
*does not do* dwarfs the work it does.

The recorded hot leaves confirm this:

- 74.5% of samples are in `DirectParser::skip_value`/`skip_array` —
  a single-function structural skipper compiled into ~110 bytes of code
  at `generated_real_typed.rs:490-544`.
- The hot dispatch is a `match byte` (`:495-504`) on `b'{'` / `b'['` /
  `b'"'` / digit / `t`/`f`/`n` literals — no SIMD, no UTF-8 validation,
  no allocator traffic, no Visitor indirection.
- `skip_string_raw` (`:546-559`) chooses between a 96-byte plain probe
  (`skip_plain_string_end`, `:575-587`) and the trusted matcher
  `match_json_string_at_quote_trusted_utf8` from parse-that-regex; for
  twitter's strings (mostly short tweet content + ASCII identifiers),
  the plain probe handles the bulk inline.
- Allocator pressure is invisible (~0.25%); the borrowed-`Cow<'i, str>`
  contract from `parse_string` (`:407-429`) returns `Cow::Borrowed`
  whenever the string has no escapes, so the kept `text` field
  almost never copies.

Four kernels combine to produce the 151.5% number:

1. **Schema-driven dispatch** — Per-object work is a `match key.as_ref()`
   over a *small* arm count (2 for `Tweet`, 1 for `TwitterSearch`) plus
   a single fall-through to `skip_value`. There is no hash table over
   JSON keys, no serde Visitor table, no `EnumAccess` indirection.
2. **No intermediate boxing** — Values land directly in the struct's
   field slot through `Option<T> = Some(parse_*(parser)?)` at
   `:69-70, :104-108`. No `JsonValue` enum is ever materialised. (See
   the renderer at `json_typed_direct.rs:263-278`.)
3. **Structural skipper > sonic's SIMD skipper, on this shape** —
   sonic's `skip_one` (`parser.rs:1439-1484`) is SIMD-tape-shaped and
   pays for its `PinnedInput::as_ptr` indirection (3.77% of sonic's
   profile_direct samples) and per-skip serde visitor entry. The
   generated `skip_value` has no per-call setup cost; it inlines into
   the recursion, and the recursion stays in registers.
4. **View-boundary string pass-through** — `parse_string` (`:407-429`)
   uses `match_tiny_plain_string` first, returns `Cow::Borrowed`
   without copying when no escapes are present, and never validates
   UTF-8 (`from_utf8_unchecked`). sonic spends 3.90% of its time in
   `simdutf8::validate_utf8_basic_neon`; Track 1 spends 0%.

## 5. Why update_center barely PASSes at 99.2%

The update-center fixture is structurally different:

- ~570 `plugins[*]` keys, each pointing to a `Plugin` object with
  ~14 keys per plugin (4 retained, ~10 ignored). The schema enumerates
  the ignored arms explicitly (`xtask/src/real_typed_schema.rs:81-94`),
  so they emit `parser.skip_string_raw()?` / `parser.skip_array()?` at
  `generated_real_typed.rs:255-268`, not the generic `skip_value`.
- The retained:ignored ratio is much closer to 1:1 than twitter's 1:10.
- Per-`Plugin` key parse + match-arm dispatch is the dominant cost
  (~37% of profile in the per-arm leaves). Each `parser.parse_string()`
  for the key runs the tiny-plain probe and returns a `Cow`, but the
  *probe + match* pair is paid on every key — sonic amortises this
  with its tape pre-scan.
- Allocator share is invisible on Track 1 (0.03%) but visible on sonic
  (12.86%) and Track 2 (10.09%). Track 1's `Vec::with_capacity(768)` on
  the `plugins` map (`json_typed_direct.rs:340`,
  `generated_real_typed.rs:332`) means a single allocation for the
  outer vector; sonic pays per-entry allocation in the serde Visitor.
  *Track 1's win comes entirely from this — sonic loses the malloc
  delta but reclaims it in SIMD-tape skipping.*
- The kept Cow strings carry escapes more often than twitter's (URLs
  containing `\/` from JSONified Jenkins update metadata), so the
  unescape path at `parse-that-regex/src/lib.rs:867` shows up briefly.

The result is a near-tie: Track 1 closes its skip-vs-tape gap with
sonic, but does not open a structural advantage the way it does on
twitter.

## 6. Replicability for mesh

Mesh is **numeric-array-heavy**: the fixture (`skinny/test_data/mesh.json`,
723,597 bytes) is an outer object with `batches[]`, `morphTargets`,
`positions: Vec<f64>` (real numbers, ~25k+ elements), and similar.
First 800 bytes confirm: bare positional `f64` arrays.

The current `json_typed_direct.rs` Vec helper template
(`:306-315`) is:

```
let mut out: Vec<f64> = Vec::new();
parser.ws();
parser.expect(b'[')?;
parser.ws();
if parser.take(b']') { return Ok(out); }
loop {
    out.push(parser.parse_f64()?);
    parser.ws();
    if parser.take(b',') { parser.ws(); continue; }
    parser.expect(b']')?;
    return Ok(out);
}
```

For a `Vec<f64>` of ~25k elements this is a scalar per-element push:

- One `parser.ws()` per element entry, one after, plus delim-take
  (~3 branches per element).
- `parse_f64()` calls `match_number_span_from_first` + `materialize_f64`
  per element (`generated_real_typed.rs:466-477`, `:479-488`). Eisel-Lemire
  is fast, but the *outer cadence* is what hurts.
- `out.push(...)` is `Vec::push`; with no `Vec::with_capacity` hint
  emitted for the inner `Vec`, the loop pays geometric reallocations
  from 0 -> ~25k.
- No SIMD across multiple numbers; no SWAR digit accumulation across
  comma boundaries; no bulk-skip-and-locate for delimiters.

Concretely: the codegen has **no template specialisation for
`Vec<f64>`** at all — `DirectTypeRef::Vec(inner)` is rendered
identically whether `inner` is `Tweet` or `f64`. Compare
`json_typed_direct.rs:306-315` to the `MapEntriesVec` arm at
`:326-342`, which *does* honor a `capacity_hint`. The `Vec` arm uses
`Vec::new()` unconditionally.

Mesh sits at 121.1% retained vs sonic and 91.8% direct vs sonic
(`RESULTS.md:11, :35`). The hot leaves from cohort C2
(`restart/skinny/tranches/sk-v6/research/skv6-C2-direct-profile.md:57`)
attribute mesh direct to `parse_array_element_at_direct` 77.6%,
`materialize_f64` 4.8%, `materialize_u64` 2.6%. The C5 audit
(`skv6-C5-parse-that-gaps.md:33-35`) flags the gap as "generated
number-array materialize/emit shape", not the Eisel-Lemire algorithm.

**Verdict on mesh real_typed_struct under current codegen:** the
generated path would emit one `out.push(parse_f64()?)` per element with
a 3-branch cadence per number. That is roughly equivalent to the SinkOnly
direct path that currently sits at 91.8% of sonic (`RESULTS.md:35`).
Without a `numeric_array_materialize_emit` primitive (named in
`skv6-C5-parse-that-gaps.md:35` and `skv6-B5-primitive-gap-inventory.md:35`),
mesh real_typed_struct **will not clear 100% on first measurement** and
will be REJECTed against the design-doc gate.

The first-measurement falsifiability gate (target: 100% sonic) is
therefore unlikely to pass without B1 + B5 *also* shipping a
`Vec<f64>` / `Vec<u32>` specialisation in
`json_typed_direct.rs::render_helper`. Two concrete changes the
codegen needs:

- A `DirectTypeRef::Vec(scalar)` template that emits
  `Vec::with_capacity(n)` (parsed off the bracket-prescan) plus a
  primitive helper `parse_f64_array_into(parser, &mut out)` defined in
  parse-that-regex that drives the comma/whitespace/digit-block scan
  inline.
- Avoiding the per-element `parser.ws()` calls — at minimum collapse to
  a single `skip_ws_and_comma` primitive between elements.

This is the named gap from C2: "Candidate C2-DB1: add a generated mesh
real_typed_struct DirectBuild" (`skv6-C2-direct-profile.md:122-123`).
C2-DB1 has *not* been done; ab06ff11 lowered host schemas but did not
add a mesh schema and did not extend the Vec template.

## 7. Extension to other corpora

Existing real_typed_struct rows (twitter PASS, update_center PASS) hint
at the predictors:

- **Twitter pattern (high skip ratio + few kept scalars + short
  strings):** big wins. Likely next-best fits are `gsoc-2018` (currently
  53.6% retained, 67.5% direct two-track — string-heavy with many
  ignored fields), `random` (currently 85.8% direct — likely 85-95%
  ignored), and `distinct_values` (currently 53.7% direct — heavily
  truncatable). All three have the structural "small keep, huge skip"
  shape that drives twitter to 151%.
- **Update_center pattern (object-of-objects + selective keeps):**
  marginal wins. `citm_catalog` already passes at 99.3% direct
  (`RESULTS.md:29`); its real_typed shape would resemble update_center
  closely. `apache_builds` (112.6% direct, `:31`) and `github_events`
  (114.3%, `:32`) already pass on direct and would likely pass
  real_typed_struct too.
- **Numeric-array pattern (mesh, canada, marine_ik, numbers):**
  parametrically blocked by the missing Vec-scalar template (see §6).
  - `canada` is 148.3% retained but only 83.6% direct (`RESULTS.md:7, :30`)
    — a candidate iff the Vec helper picks up vectorisation.
  - `marine_ik` 136.0% retained / 106.8% direct (`RESULTS.md:14, :38`)
    — direct already passes; real_typed_struct should track the direct
    number unless a struct-of-arrays/array-of-struct mismatch is introduced.
  - `numbers` 97.3% direct (`:40`) and gsoc-2018 67.5% direct (`:37`)
    sit in different gaps.

Concrete recommended next admit-order *after* B1/B5 lands:

1. `gsoc-2018 real_typed_struct` — biggest twitter-shape ignored-share win.
2. `mesh real_typed_struct` — only with `Vec<f64>`/`Vec<u32>` codegen specialisation.
3. `citm_catalog real_typed_struct` — already passes direct.
4. `random real_typed_struct` — likely twitter-shape with random keys.

## 8. Architectural implications

The 151.5% twitter result demonstrates the architectural thesis: a
schema-driven typed DirectBuild can beat sonic-rs typed serde on shapes
where the schema *prunes* most JSON-input bytes. The lever is not SIMD
or a smarter Eisel-Lemire — it is the absence of:

- the JsonValue enum boxing layer,
- the serde Visitor table lookup,
- UTF-8 validation across non-kept regions,
- per-`Tweet` allocator traffic.

What does NOT scale to all 17 corpora:

- Corpora where most input is *kept* (mesh, canada, numbers,
  marine_ik) get no skip leverage.
- Corpora where the kept payload is escaped strings (unicode_* set)
  pay full unescape cost regardless of schema.
- Numeric arrays need their own codegen template; the generic
  `DirectTypeRef::Vec(inner)` rendering at
  `json_typed_direct.rs:306-315` is shape-blind.

Host-schema LOC budget per corpus, based on the two rendered helpers:

- Twitter schema: 14 lines in `real_typed_schema.rs:24-36`. Struct
  decls in `real_typed_struct.rs:15-27`: 12 lines plus a 12-line
  checksum at `:183-195`. Total ~40 LOC per simple corpus.
- update_center schema: 38 lines in `real_typed_schema.rs:37-95`. The
  `Plugin` struct + ignored-field allowlist drives the size; the
  generated parser body for `parse_type_plugin` ends up at 64 lines
  (`generated_real_typed.rs:221-284`). Total ~120 LOC per "skip-heavy
  object-of-objects" corpus.

Generated module size, per `wc -l`:
- `generated_real_typed.rs` covering both corpora: 597 lines.

A 17-corpus rollout under the current Vec-scalar limitation would land
in the ~3-5kLoC generated-module range — within reasonable LOC budget
but already overlapping the per-tranche generated-size-budget
discipline (see memory `generated-size-budget`). Without a Vec-scalar
specialisation, only ~8 of 17 corpora are productive admits.

## 9. Falsifiability gate for mesh

Target (per Wave B5 design): mesh real_typed_struct at >=100% sonic.

Pre-measurement prediction: **REJECT on first measurement under current
codegen.** Predicted band: 70-90% sonic, set by the per-element
`parse_f64()?` cadence and the missing `Vec::with_capacity` hint on
`Vec<f64>` (`json_typed_direct.rs:306-315`).

Recommended sequencing:

1. Land B1 (`per-\uXXXX TBL`) — closes more rows that are not
   numeric-array-bound.
2. Land a `DirectTypeRef::Vec(DirectScalar::F64)` specialisation in
   `json_typed_direct.rs::render_helper`, with a matching
   `parse_f64_array_into(parser, &mut Vec<f64>)` primitive in
   parse-that-regex.
3. *Then* admit a mesh schema in `real_typed_schema.rs` and re-measure.

If steps 1+2 are skipped and a mesh schema is admitted directly,
expect the result to be REJECT and the falsifiability gate to fire.

## 10. Wave alignment

C3 confirms two things relevant to SK-V7 Wave 3 (B5 mesh DirectBuild):

- The twitter PASS result is genuine, not artifactual — the dominant
  74.5% structural-skip leaf cannot be hidden by criterion machinery,
  and it is qualitatively absent from sonic's profile.
- B5 (mesh DirectBuild) should NOT be admitted before B1 lands AND
  the Vec-scalar specialisation lands. The current codegen cannot meet
  the falsifiability gate.

C3 also confirms cohort C2's named candidate "C2-DB1" (mesh
real_typed_struct DirectBuild) remains open. The path to closing it
is shape-specific codegen, not a generic primitive admit.

## Artifacts

- `/tmp/skv7-C3-profiles/twitter-track1.json.gz` and
  `/tmp/skv7-C3-profiles/twitter-track1.hot.txt`
- `/tmp/skv7-C3-profiles/twitter-track2.json.gz` and `.hot.txt`
- `/tmp/skv7-C3-profiles/twitter-sonic.json.gz` and `.hot.txt`
- `/tmp/skv7-C3-profiles/update_center-track1.json.gz` and `.hot.txt`
- `/tmp/skv7-C3-profiles/update_center-track2.json.gz` and `.hot.txt`
- `/tmp/skv7-C3-profiles/update_center-sonic.json.gz` and `.hot.txt`
- `/tmp/skv7-C3-profiles/hotleaf.py` — extractor used to symbolicate
  samples against `/tmp/skv7-cargo/C3/release/profile_direct.dSYM`.
