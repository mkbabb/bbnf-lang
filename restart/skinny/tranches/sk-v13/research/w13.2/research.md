# SK-V13 W13.2 Research - Unicode Basic Typed Product Surface

Wave: W13.2 typed product surface completion.
Scope: `json/unicode_basic/real_typed_struct/main`.
Date: 2026-05-22.

## Cohort

Six read-only slices were redeployed after W13.1:

- Slice A, numeric/geospatial rows: `canada` and `gsoc-2018` are feasible but
  require deeper nested coordinate/map-entry products and larger schema growth.
- Slice B, simple residual rows: `unicode_basic` is the smallest regular
  missing product row whose sibling direct row already beats same-plane sonic;
  `random` and `distinct_values` contain wider heterogeneous or dynamic-key
  objects.
- Slice C, existing/open typed rows: `update_center` already has a product
  surface but remains below the addendum's sonic+1 bar; it is an optimization
  wave, not a missing-surface wave.
- Slice D, unicode-heavy rows: `y_string_unicode`, `unicode_mixed`, and
  `unicode_escapes` are escape-heavy and likely need the later parse/string
  route; `unicode_basic` is mostly valid UTF-8 text with ordinary record
  fields.
- Slice E, codegen surface: the typed generator already supports top-level
  `Vec<T>` roots, borrowed strings, numbers, and nested vectors. No new
  directive, BIR variant, `BackendShape`, or substrate API is required.
- Slice F, gate/report surface: W13.1 proved the typed-product companion gate.
  W13.2 can extend the same gate class only if it remains row-specific,
  same-run, and consumed by `gate-json`.

## Fixture Shape

`skinny/test_data/unicode_basic.json` is a top-level array of 5,759 records.
The first record shape is:

```text
{ id: u64, script: string, text: string, len: u64, tags: Vec<string> }
```

The row can be expressed as
`Vec<crate::real_typed_struct::UnicodeBasicRecord<'i>>` with a capacity hint
of `5_759`. Its independent Track 2/oracle route is the existing typed
fixture harness backed by `serde_json`, with same-run `sonic_rs` typed strict
as the SOTA comparator.

## Finding

`unicode_basic` is the next admissible W13 missing typed product target. It is
regular enough to keep generated LOC bounded, exercises non-ASCII borrowed
strings without the escape-heavy `unicode_escapes`/`unicode_mixed` path, and
has a sibling direct row already above sonic strict in the rolling table. That
does not prove typed admission, but it is the lowest-risk row after `numbers`.

## Revert Protocol

If W13.2 fails, revert the unicode_basic typed root, generated parser, fixture
enum/output/checksum/bench routing, companion report extension, RESULTS and
rolling updates, and the REDRESS entry. Record whether the failure is schema
impossibility, parity mismatch, sonic threshold miss, gate-consumption failure,
or Track 2 coupling.
