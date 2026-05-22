# SK-V13 W13.6 Research - Unicode Mixed Typed Product Surface

Wave: W13.6 typed product surface completion.
Scope: `json/unicode_mixed/real_typed_struct/main`.
Date: 2026-05-22.

## Cohort

Six scopes were redeployed after the W13.5 `gsoc-2018` measured reject:

- Slice A, unicode roots: `unicode_mixed` and `unicode_escapes` both fit the
  current generated typed schema; `unicode_mixed` is lower risk because it has
  no raw `\uXXXX` escape pressure in the checked fixture.
- Slice B, remaining simple roots: `y_string_unicode` is a top-level string
  vector, but the small corpus size and dense escaped Unicode make it a noisy
  throughput target.
- Slice C, `distinct_values`: honest admission needs a mixed fixed-field plus
  dynamic `key_*` collector in codegen; current schema would skip most payload.
- Slice D, `gsoc-2018`: W13.5 proved correctness but missed sonic+1, so any
  retry needs a fresh material differential.
- Slice E, direct/parse reopen routes: output-digest SIMD and unicode string
  decode routes are row-moving candidates outside this typed product wave.
- Slice F, gate surface: W13 typed rows remain one-row companion reports with
  same-run Criterion evidence and `gate-json` consumption.

## Fixture Shape

`skinny/test_data/unicode_mixed.json` is a 1,053,086-byte root object:

```text
{
  metadata: {
    purpose: string,
    classes: [string; 5],
    count: 4185
  },
  records: [
    { id: u64, type: string, value: string, n: u64 },
    ...
  ]
}
```

The checked fixture has 4,185 records with five balanced record classes:
`ascii`, `latin1`, `cjk`, `emoji`, and `mixed_escapes`. It stresses unicode
and escaped control/quote/backslash strings, but unlike `unicode_escapes` it
does not make raw `\uXXXX` decoding the dominant work.

## Finding

`unicode_mixed` is the next admissible missing typed surface because it uses
only existing `struct_root`, `vec_with_capacity`, `string`, `u64`, and
`Option<T>` schema constructs. It does not require a new directive, BIR shape,
unknown-field collector, public substrate API, or generic-crate edit.

The risk is throughput, not architecture: existing parse/direct rows are far
behind sonic, and the typed product will still allocate owned strings for
escaped values. The wave is therefore a measured admit-or-reject under the
strict W13 gate: Track 1 must exceed same-run sonic strict typed throughput by
at least 1 Mbps.

## Revert Protocol

If W13.6 fails, revert the `unicode_mixed` typed root, generated parser,
fixture enum/output/checksum routing, gate/report additions, RESULTS and
rolling updates, and REDRESS entry. Save the rejected source patch under
`/tmp/skv13-waveW13.6-rejected.patch` and record the measured Track 1,
Track 2, sonic, and serde evidence.
