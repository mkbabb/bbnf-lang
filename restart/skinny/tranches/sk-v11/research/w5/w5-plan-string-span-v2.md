# SK-V11 W5 Plan V2 - JSON-Local Bounded String Span

Date: 2026-05-20.

Disposition: PLAN V2 after CHALLENGE V1 REVISE.

V2 keeps the selected W5 surface but folds CH1 and CH4:

- release-mode opening-quote guards are mandatory before both generated Track 1
  and hand Track 2 string helper/fallback paths;
- no `parse-that-regex` behavior source is edited, because CH2/CH5 make a
  shared generic helper blocked without a generated non-JSON proof;
- the probe trigger is floor-level: `random` must plausibly clear 7878 Mbps on
  both tracks before Criterion.

## Selected Intervention

Selected primitive: JSON-local generated direct bounded string span for cap 8.

Selected consumer: generated JSON direct `parse_string_direct`, including
object keys and string values.

Selected row: exactly `random/direct_to_struct/main`.

Selected floor: Track 1 and independent Track 2 both >= 7878 Mbps.

Non-selected W5 rows remain scout or residual rows. REDRESS 113 remains blocked.
No W3/W4 route is reopened.

## Source Shape

The implementation must stay JSON-local:

- `skinny/crates/codegen/src/sink_direct.rs` may emit a generated JSON-local
  helper such as `bounded_plain_string_span_direct`.
- `skinny/crates/runtime/src/grammars/json/generated.rs` changes only by
  regeneration from `sink_direct.rs`.
- `skinny/crates/bbnf-bench/src/direct_struct.rs` may add an independent hand
  Track 2 mirror, but it must not call generated Track 1 symbols.
- `skinny/crates/parse-that-regex/src/lib.rs` is not a behavior owner in V2.

Generated helper semantics:

```rust
fn bounded_plain_string_span_direct<'i>(
    input: &'i str,
    bytes: &'i [u8],
    cursor: &mut usize,
) -> Result<Option<ParsedString<'i>>, ParseError<'i>>;
```

Requirements:

- First check `bytes.get(*cursor) == Some(&b'"')` in release mode. If not, return
  `ExpectedValue` at `*cursor` and do not enter the helper or trusted fallback.
- For cap 8, return `Ok(Some(ParsedString { raw, needs_unescape: false }))`
  only if a closing quote is found within the current direct cap before a
  backslash or control byte.
- On escape, control, EOF, or cap miss, return `Ok(None)` without advancing
  `cursor`; then the caller runs `match_string_at_quote_trusted_utf8`.
- All offsets are absolute and cursor movement is exactly current behavior:
  bounded success advances to closing quote + 1; fallback advances to
  `span.raw_end`.
- The helper returns borrowed raw source only. It never returns decoded bytes,
  decoded stats, semantic facts, side tables, or hashes.

Hand Track 2 must add its own release-mode quote guard and may keep its local
cap-8 loop. It must not call generated Track 1 or generated helper symbols.

## Owner Paths

Behavior/source owner paths:

- `skinny/crates/codegen/src/sink_direct.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/REDRESS.md`

Conditional:

- `skinny/RESULTS.md` only on row-floor pass.
- `skinny/crates/bbnf-bench/benches/json_parity.rs` only if durable filter or
  metadata support is required.

Out of scope:

- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/bbnf-simd/src/**`
- generated typed source
- non-JSON runtime/codegen

## Required Tests Before Measurement

Correctness tests must prove:

- generated direct and hand Track 2 both reject non-quote string positions with
  stable errors instead of trusting a debug assertion;
- malformed object key/value and array string positions do not panic in release;
- cap boundary behavior: close before cap succeeds, close outside cap falls
  back and succeeds when valid;
- escape/control inside the cap falls back and preserves current error offsets
  and decode behavior;
- empty string and non-zero offset strings keep exact raw/content offsets;
- generated Track 1 and hand Track 2 produce equal digest output on valid rows;
- Track 2 source does not reference generated Track 1 helper names.

Commands:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- regen-json
RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- check-json
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench w5_string_span -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench direct_contract -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --bin gate w5 -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo check -p codegen -p runtime -p bbnf-bench
```

No parse-that-regex or bbnf-simd test is required for V2 unless redress violates
the out-of-scope list; such violation returns REVISE before implementation.

## Probe And Measurement

Build probe:

```sh
RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench --bin profile_direct
```

Before and after the patch:

```sh
for row in random citm_catalog apache_builds marine_ik unicode_basic unicode_escapes unicode_mixed y_string_unicode; do
  ./target/release/profile_direct 20000 "$row" track1
  ./target/release/profile_direct 20000 "$row" track2
done
```

Criterion is allowed only if the post-patch `random` probe shows both tracks at
or near the 7878 Mbps floor with plausible noise margin and no guard-threatening
regression. A mere 1% improvement is insufficient.

Criterion and gate:

```sh
CRITERION_HOME=/tmp/skv11-w5-criterion RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json_(random|citm_catalog|apache_builds|marine_ik|unicode_basic|unicode_escapes|unicode_mixed|y_string_unicode)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)'
CRITERION_HOME=/tmp/skv11-w5-criterion RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json_(twitter|citm_catalog|apache_builds|github_events|update_center|mesh|marine_ik)/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)'
CRITERION_HOME=/tmp/skv11-w5-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

## Exit Gate

Admission requires:

- `random/direct_to_struct` Track 1 >= 7878 and Track 2 >= 7878 in Criterion;
- same-run sonic-rs and serde_json direct comparator rows;
- direct and typed guard floors hold;
- Unicode residual monitors are recorded and not admitted;
- gate/report consume W5 provenance with `wave_id=SK-V11-W5`,
  `redress_entry=REDRESS-116`, and `sk_v9_open_delta=bounded-string-span`;
- REDRESS 113 is carried forward as blocked.

## Reject Protocol

Reject W5 and save `/tmp/skv11-waveW5-rejected.patch` if:

- opening-quote guard or malformed-string parity fails;
- generated output is stale;
- Track 1/Track 2 digest parity fails;
- probe does not put both `random` tracks near the 7878 Mbps floor;
- Criterion misses the floor;
- any direct or typed guard floor misses;
- Unicode residuals are silently admitted;
- redress edits parse-that, bbnf-simd, generated typed, or non-JSON source;
- REDRESS-preblocked decoded scratch, semantic facts, retained wrappers,
  string-block production, or sidecars appear.

On FAIL, restore source after saving the patch, leave `RESULTS.md` unchanged,
and record REDRESS 116 as a measured reject.
