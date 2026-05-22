# SK-V13 W13.4 Research - Instruments Typed Product Surface

Wave: W13.4 typed product surface completion.
Scope: `json/instruments/real_typed_struct/main`.
Date: 2026-05-22.

## Cohort

Six read-only slices were redeployed after W13.3:

- Slice A, `gsoc-2018`: structurally clean map-entry product, but string-only
  and likely a harder sonic+1 throughput target.
- Slice B, `instruments`: the old SK-V10 W4 patch already had a generated
  typed product and measured Track 1 above sonic; its rejection was an older
  Track 2 floor, not a parity or Track 1 failure.
- Slice C, unicode rows: `unicode_mixed`, `unicode_escapes`, and
  `y_string_unicode` fit the current schema surface, but every unicode-heavy
  row carries escape/allocation risk and should stay row-specific.
- Slice D, `distinct_values`: requires a mixed fixed-field plus dynamic-entry
  unknown collector to be honest; current schema would skip the dominant
  dynamic payload.
- Slice E, direct reopen routes: output digest SIMD and typed/direct crossover
  are direct-plane candidates, not typed product missing-surface work.
- Slice F, W13 gate surface: the W13 typed-product companion gate can be
  extended one row at a time and should continue to reject support-only rows.

## Fixture Shape

`skinny/test_data/instruments.json` is a root object with:

```text
{ name, version, instruments, patterns, samples }
```

The prior SK-V10 W4 rejected patch and current fixture inspection agree on the
capacities:

- `instruments`: 63 records.
- `patterns`: 240 records.
- `samples`: 70 records.
- `InstrumentEnvelope.nodes`: up to 8 records.
- `InstrumentPattern.data`: up to 1 event record.

The product model is a real nested host/API surface:

```text
InstrumentsDocument<'i> {
  instruments: Vec<Instrument<'i>>,
  name: Option<Cow<'i, str>>,
  patterns: Vec<InstrumentPattern<'i>>,
  samples: Vec<InstrumentSample<'i>>,
  version: Option<u32>,
}
```

Nested product types include `Instrument`, `InstrumentEnvelope`,
`InstrumentEnvelopeNode`, `InstrumentPattern`, `InstrumentPatternEvent`, and
`InstrumentSample`. The known schema intentionally leaves `note_map`,
`sample_map`, and `tuning` outside the product surface as unknown fields,
matching the previous host/API fixture contract rather than a hidden digest.

## Finding

`instruments` is the lowest-risk next W13 typed product target. REDRESS 103
measured the old Track 1 generated typed product around `20678` Mbps against
sonic typed strict around `15940` Mbps, while rejecting because Track 2 missed
that tranche's older floor. W13 admits typed product rows under the stricter
Track 1 `sonic + 1 Mbps` bar with serde Track 2/oracle independence, so the
old rejection is no longer binding if same-run parity and measurement pass.

## Revert Protocol

If W13.4 fails, revert the instruments typed root, generated parser, fixture
enum/output/checksum/bench routing, companion report extension, RESULTS and
rolling updates, and the REDRESS entry. Record whether the failure is schema
coverage, parity mismatch, sonic threshold miss, gate-consumption failure, or
Track 2 coupling.
