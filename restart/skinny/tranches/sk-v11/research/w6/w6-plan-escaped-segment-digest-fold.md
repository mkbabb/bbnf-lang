# SK-V11 W6 Plan - Escaped Segment Digest Fold

Pass: W6 Phase 2 plan.
Date: 2026-05-20.
Disposition: PROPOSED, pending mandatory CHALLENGE.

## Selected Intervention

Select a JSON direct-plane escaped-segment digest fold for
`G-W6-ESCAPE-SEGMENT-DIRECT`.

The source delta is not a wrapper around the existing `unescape_string` path and
not another proof that `unescape_uxxxx_x4_neon` is reachable. The implementation
candidate is a direct-output consumer in
`skinny/crates/bbnf-bench/src/direct_struct.rs`: override
`JsonDigestSink::{key_source,string_source,array_string_source,object_string_source}`
so generated Track 1 routes escaped raw string slices to a local decoded-byte
fold rather than allocating a decoded `String`/`Cow<str>` and then hashing it.
Mirror the hand Track 2 parser with an independent local string-digest/key-fold
path so Track 2 measures the same direct output contract without calling the
generated parser or generated sink helper symbols.

The plan does not edit `skinny/crates/runtime/src/grammars/json/sink.rs`. That
trait file is not in SPEC Section 10 owner paths. It also does not claim a new
production win from the existing `parse_that_regex::unescape_string ->
unescape_four_unicode_escapes -> unescape_uxxxx_x4_neon` route, which REDRESS
107/108 and SPEC Section 10 already pre-block.

## Target Rows

Primary selected row:

| Row | SK-V11-open Track 1 | SK-V11-open Track 2 | sonic-rs strict | W6 floor | Admission |
|---|---:|---:|---:|---:|---|
| `unicode_mixed/direct_to_struct` | 3753 | 2427 | 2846 | 2588 | Selected |

Scout rows:

| Row | SK-V11-open Track 1 | SK-V11-open Track 2 | sonic-rs strict | W6 floor | W6 treatment |
|---|---:|---:|---:|---:|---|
| `unicode_escapes/direct_to_struct` | 1345 | 1341 | 3785 | 3441 | Measure and record only unless probes clear both tracks |
| `y_string_unicode/direct_to_struct` | 1983 | 1029 | 4344 | 3950 | Measure and record only unless probes clear both tracks |

`unicode_mixed` is the only first-packet candidate whose gap is plausibly in
range: Track 1 already clears the floor and Track 2 needs a 161 Mbps lift
(6.6%). The row has a real escaped-string hot surface, but REDRESS 107 recorded
zero eligible fixed-width Unicode-escape x4 payload because its `\u` text is
escaped-backslash data, not valid JSON Unicode escape syntax. That points to
consumer allocation/fold cost, not a new x4 decode body.

## Owner Paths

Allowed redress owner paths for this plan:

- `skinny/crates/bbnf-bench/src/direct_struct.rs`.
- `skinny/crates/bbnf-bench/benches/json_parity.rs`, if new direct-digest
  escaped-fixture parity cases are needed.
- `skinny/crates/bbnf-bench/src/bin/gate.rs` and
  `skinny/crates/bbnf-bench/src/report.rs`, only to consume the W6 result fields
  already admitted by the SK-V11 schema.
- `skinny/RESULTS.md` on measured PASS only.
- `skinny/REDRESS.md` for the W6 disposition.
- `skinny/crates/parse-that-regex/src/lib.rs` only if CHALLENGE accepts a small
  scalar oracle/test extraction. It may not become a JSON policy leak in a
  generic crate.
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs` and
  `skinny/crates/bbnf-simd/tests/` only if CHALLENGE explicitly routes x4. The
  default plan does not route x4.

Rejected for this plan unless CHALLENGE returns REVISE with new authority:

- `skinny/crates/runtime/src/grammars/json/sink.rs`.
- `skinny/crates/codegen/src/`.
- `skinny/crates/runtime/src/grammars/json/generated.rs`.
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`.

## Same-Wave Consumer

Track 1 consumer: the already-generated `parse_direct` caller still routes
`ParsedString { raw, needs_unescape }` to `JsonDigestSink`, but the sink's
source-method overrides consume escaped raw slices directly in the digest plane.
The generated JSON parser remains the producer; the same-wave consumer is the
direct digest output sink.

Track 2 consumer: the hand parser uses a separate local escaped-string digest
path for the same direct output fields. It must not call generated code or a
Track 1-only helper. CHALLENGE must decide whether a single local helper in
`direct_struct.rs` is an acceptable output-plane scalar oracle shared by both
tracks; if not, Track 1 and Track 2 get duplicated local implementations with a
dedicated parity test.

## Material Differential From Pre-Blocks

This plan is REDRESS-adjacent and therefore HIGH risk.

Material differential from REDRESS 54/55/66-69:

- It does not add parser-owned decoded scratch.
- It does not add persistent decoded semantic facts, side tables, or a public
  substrate.
- It does not change parser control flow or the direct parser's ownership of
  string slices.
- It does not expose an output hash side channel to generic runtime crates.
- It does not alter `unescape_string` and does not claim a win through the
  existing `Cow<str>` materializer.
- It folds decoded bytes only into the already-existing `JsonDirectDigest`
  fields that the direct output contract already measures.

Material differential from REDRESS 64, 82, 107, and 108:

- It does not claim fixed-width Unicode escape validation as the W6 product.
- It does not route x4 production unless a new same-wave consumer and strict
  checkasm are added by CHALLENGE revision.
- It treats the existing x4 path as background proof only.

REDRESS 113 is carried forward: W6 does not close the SK-V11 non-JSON grammar
axis. REDRESS 116 is carried forward: W5 admitted no reusable span API or scalar
proof.

## Correctness Gate Before Measurement

Before any throughput claim, redress must pass release-mode tests for:

- Direct digest parity across generated Track 1, hand Track 2, `serde_json`, and
  `sonic-rs` on simple escapes, escaped quote, escaped backslash, slash,
  newline/tab/control escapes, valid Unicode scalar, valid surrogate pair,
  invalid escape, invalid hex, lone high surrogate, lone low surrogate,
  control-before-close, escaped key, root string, array string, and object
  string.
- Existing `direct_contract` coverage.
- A source-level or runtime assertion that Track 2 does not call generated parser
  helpers for the W6 escaped-string path.
- Gate/report schema consumption if the row is admitted: `wave_id=SK-V11-W6`,
  `redress_entry=REDRESS-117`, selected row `unicode_mixed/direct_to_struct`,
  and a same-wave consumer class naming direct digest source-method overrides.

## Probe And Measurement Plan

Probe before Criterion:

```bash
RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml \
  -p bbnf-bench --release --bin profile_direct -- unicode_mixed
RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml \
  -p bbnf-bench --release --bin profile_direct -- unicode_escapes
RUSTFLAGS="-C target-cpu=native" cargo run --manifest-path skinny/Cargo.toml \
  -p bbnf-bench --release --bin profile_direct -- y_string_unicode
```

Criterion is authorized only if repeated probes put `unicode_mixed` Track 2 at
or above 2620 Mbps while Track 1 remains above 2588 Mbps. Scout rows may be
reported but do not become admitted rows unless their own repeated probes clear
their floors on both tracks before Criterion.

Criterion packet on PASS candidate:

```bash
CRITERION_HOME=/tmp/skv11-w6-criterion RUSTFLAGS="-C target-cpu=native" \
  cargo bench --manifest-path skinny/Cargo.toml -p bbnf-bench \
  --bench json_parity -- \
  'json_(unicode_mixed|unicode_escapes|y_string_unicode|citm_catalog|apache_builds|marine_ik|unicode_basic)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)'
```

Guard rows:

- Direct: `citm_catalog`, `apache_builds`, `marine_ik`, `unicode_basic`.
- Typed: `twitter`, `citm_catalog`, `apache_builds`, `github_events`,
  `update_center`, `mesh`, `marine_ik`.
- No parse-only row is an SK-V11 target.

## Exit Gate

`G-W6-ESCAPE-SEGMENT-DIRECT` passes for this plan only if:

1. `unicode_mixed/direct_to_struct` Track 1 and Track 2 both measure at or above
   2588 Mbps in the Criterion packet.
2. Generated Track 1 and hand Track 2 direct digests are exactly equal on the W6
   escaped fixture set and production corpus row.
3. Same-run `serde_json` and `sonic-rs` strict direct comparators are present.
4. Direct and typed guard floors hold.
5. Gate/report consume the W6 result fields in the same wave.
6. The REDRESS entry states the material differential from REDRESS 54/55/66-69,
   64, 82, 107, 108, 113, and 116.

If the row misses, the wave records a measured REDRESS reject. If malformed
escaped-string parity fails, or if CHALLENGE decides the Track 2 implementation
is not independent enough, the wave rejects before measurement.

## Revert Protocol

On parity failure, missing new source delta, row-floor miss, guard regression,
schema leak, or policy leak:

1. Save the source delta at `/tmp/skv11-waveW6-rejected.patch`.
2. Revert source, gate, report, and result edits as one slice.
3. Record REDRESS 117 as a W6 reject with probe/Criterion evidence.
4. Do not move `skinny/RESULTS.md`.
