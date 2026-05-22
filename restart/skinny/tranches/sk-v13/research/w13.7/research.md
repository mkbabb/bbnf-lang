# SK-V13 W13.7 Research - Y String Unicode Typed Product Surface

Wave: W13.7 typed product surface completion.
Scope: `json/y_string_unicode/real_typed_struct/main`.
Date: 2026-05-22.

## Cohort

The post-W13.6 sidecar review separates the remaining typed rows:

- `y_string_unicode` is a top-level array of 2,200 strings and fits the
  existing generated typed schema as `Vec<Cow<'i, str>>`.
- `unicode_escapes` is also structurally simple, but raw `\uXXXX` decode is
  dominant and should be a separate row after this smaller string-vector probe.
- `distinct_values` is not honest under the current schema because it needs a
  mixed fixed-field plus dynamic `key_*` entry collector.
- `unicode_mixed` and `gsoc-2018` have already produced measured typed-product
  rejects in W13.6 and W13.5; same-tranche retries require fresh material
  differentials.

## Fixture Shape

`skinny/test_data/y_string_unicode.json` is a 35,601-byte root array of 2,200
strings. The fixture includes Unicode combining marks, escaped quotes, raw
`\uXXXX` sequences, and astral decoded strings. The product surface is:

```text
Vec<Cow<'i, str>>
```

The row is intentionally small; Criterion noise and decode/allocation pressure
are the main risks.

## Finding

`y_string_unicode` is the smallest remaining generated typed product surface
that can be implemented without codegen machinery changes. It is suitable for
a measured admit-or-reject wave because it isolates the string vector case and
keeps `unicode_escapes` available for a later, larger decode-focused attempt.

## Revert Protocol

If W13.7 fails, revert the `y_string_unicode` typed root, generated parser,
fixture enum/output/checksum routing, gate/report additions, RESULTS and
rolling updates, and REDRESS entry. Save the rejected patch under
`/tmp/skv13-waveW13.7-rejected.patch` and record the same-run Track 1,
Track 2, sonic, and serde measurements.
