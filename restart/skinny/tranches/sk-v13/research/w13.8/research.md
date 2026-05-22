# SK-V13 W13.8 Research - Unicode Escapes Typed Product Surface

Wave: W13.8 typed product surface completion.
Candidate row: `json/unicode_escapes/real_typed_struct/main`.
Scope: read-only research.

## Finding

`unicode_escapes` remains a missing `real_typed_struct` product row in the
rolling 51-row JSON universe. The corpus is a stable object envelope:

```text
{
  "meta": { "mode": "escapes", "ensure_ascii": true },
  "records": [{ "id": u64, "v": string }, ...]
}
```

The fixture is `skinny/test_data/unicode_escapes.json`, 1,050,797 bytes, with
1,877 records. The product surface is representable by the existing generated
typed DirectBuild schema: object, optional object, vector, u64, bool, and
borrowed string fields. No `UnknownFieldPolicy` extension is required.

## Material Differential

W13.8 is distinct from REDRESS 70-72/103-110 and from the W13.5-W13.7 rejects:

- It is not a direct digest, parse-only row, unicode codec proof, hidden sink,
  or synthetic fixture.
- It generates and measures a full document product with `meta` plus every
  `records[*].id` and `records[*].v` field.
- It does not replay W13.6's mixed-object row or W13.7's bare
  `Vec<Cow<'input, str>>` route.

The expected failure mode is throughput, not correctness: the row is
escape-heavy and prior profiles identify unicode/string decode as the hot leaf.
That makes the wave valuable even on reject because it produces strict typed
plane evidence for the escaped document surface.

## Owner Paths

- `skinny/xtask/src/real_typed_schema.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `skinny/REDRESS.md`
- W13.8 artifacts under `restart/skinny/tranches/sk-v13/research/w13.8/`

## Verification Surface

The row must prove strict equality across generated Track 1, serde Track 2,
sonic-rs strict, and serde_json typed lanes before measurement can admit.
Admission threshold is same-run `sonic_rs_real_typed_struct + 1 Mbps`.

Planned native Criterion filter:

```text
json/unicode_escapes/(track1_real_typed_struct|track2_real_typed_struct|sonic_rs_real_typed_struct|serde_json_real_typed_struct)
```

## Routed Alternatives

`distinct_values` remains a later typed-product candidate but needs a codegen
collector for dynamic `key_*` entries to avoid a paper product. `canada` needs
a larger geometry product model and has REDRESS 80 history. A second
`y_string_unicode` or `unicode_mixed` attempt in this tranche must name a fresh
decode-allocation or SIMD consumption differential because W13.6/W13.7 already
measured the straightforward generated product routes.
